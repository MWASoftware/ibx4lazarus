{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

{
  This unit has been almost completely re-written as the original code was
  not that robust - and I am not even sure if it worked. The IBPP C++ implementation
  was used for guidance and inspiration. A permanent thread is used to receive
  events from the asynchronous event handler. This then uses "Synchronize" to
  process the event in the main thread.

  Note that an error will occur if the TIBEvent's Registered property is set to
  true before the Database has been opened.
}

unit IBEvents;

{$Mode Delphi}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  Classes, IBHeader, IBExternals, IB, IBDatabase;

const
  MaxEvents = 15;

type

  TEventAlert = procedure( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean) of object;

  { TIBEvents }

  TIBEvents = class(TComponent)
  private
    FIBLoaded: Boolean;
    FBase: TIBBase;
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FEventHandler: TObject;
    FRegistered: boolean;
    FDeferredRegister: boolean;
    procedure EventChange(sender: TObject);
    function GetDatabase: TIBDatabase;
    function GetDatabaseHandle: TISC_DB_HANDLE;
    procedure SetDatabase( value: TIBDatabase);
    procedure ValidateDatabase( Database: TIBDatabase);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    procedure DoAfterDatabaseConnect(Sender: TObject);
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    procedure SetEvents( value: TStrings);
    procedure SetRegistered( value: boolean);

  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents;
    procedure UnRegisterEvents;
    property DatabaseHandle: TISC_DB_HANDLE read GetDatabaseHandle;
    property DeferredRegister: boolean read FDeferredRegister write FDeferredRegister;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
  end;


implementation

uses
  IBIntf, syncobjs, SysUtils;

type

  TEventHandlerStates = (
    stIdle,           {Events not monitored}
    stHasEvb,         {Event Block Allocated but not queued}
    stQueued,         {Waiting for Event}
    stSignalled       {Event Callback signalled Event}
   );

  { TEventHandler }

  TEventHandler = class(TThread)
  private
    FOwner: TIBEvents;
    FCriticalSection: TCriticalSection;   {protects race conditions in stQueued state}
    {$IFDEF WINDOWS}
    {Make direct use of Windows API as TEventObject don't seem to work under
     Windows!}
    FEventHandler: THandle;
    {$ELSE}
    FEventWaiting: TEventObject;
    {$ENDIF}
    FState: TEventHandlerStates;
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FRegisteredState: Boolean;
    FResultBuffer: PChar;
    FEvents: TStringList;
    FSignalFired: boolean;
    procedure QueueEvents;
    procedure CancelEvents;
    procedure HandleEventSignalled(length: short; updated: PChar);
    procedure DoEventSignalled;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TIBEvents);
    destructor Destroy; override;
    procedure Terminate;
    procedure RegisterEvents(Events: TStrings);
    procedure UnregisterEvents;
  end;

 {This procedure is used for the event call back - note the cdecl }

procedure IBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
begin
  if (ptr = nil) or (length = 0) or (updated = nil) then
    Exit;
  { Handle events asynchronously in second thread }
  TEventHandler(ptr).HandleEventSignalled(length,updated);
end;



{ TEventHandler }

procedure TEventHandler.QueueEvents;
var
  callback: pointer;
  DBH: TISC_DB_HANDLE;
begin
  if FState <> stHasEvb then
    Exit;
  FCriticalSection.Enter;
  try
    callback := @IBEventCallback;
    DBH := FOwner.DatabaseHandle;
    if (isc_que_events( StatusVector, @DBH, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(Self)) <> 0) then
      IBDatabaseError;
    FState := stQueued
  finally
    FCriticalSection.Leave
  end;
end;

procedure TEventHandler.CancelEvents;
var
  DBH: TISC_DB_HANDLE;
begin
  if FState in [stQueued,stSignalled] then
  begin
    FCriticalSection.Enter;
    try
      DBH := FOwner.DatabaseHandle;
      if (isc_Cancel_events( StatusVector, @DBH, @FEventID) <> 0) then
          IBDatabaseError;
      FState := stHasEvb;
    finally
      FCriticalSection.Leave
    end;
  end;

  if FState = stHasEvb then
  begin
    isc_free( FEventBuffer);
    FEventBuffer := nil;
    isc_free( FResultBuffer);
    FResultBuffer := nil;
    FState := stIdle
  end;
  FSignalFired := false
end;

procedure TEventHandler.HandleEventSignalled(length: short; updated: PChar);
begin
  FCriticalSection.Enter;
  try
    if FState <> stQueued then
      Exit;
    Move(Updated[0], FResultBuffer[0], Length);
    FState := stSignalled;
    {$IFDEF WINDOWS}
    SetEVent(FEventHandler);
    {$ELSE}
    FEventWaiting.SetEvent;
    {$ENDIF}
  finally
    FCriticalSection.Leave
  end;
end;

procedure TEventHandler.DoEventSignalled;
var
  i: integer;
  CancelAlerts: boolean;
  Status: array[0..19] of ISC_LONG; {Note in 64 implementation the ibase.h implementation
                                     is different from Interbase 6.0 API documentatoin}
begin
    if FState <> stSignalled then
      Exit;
    isc_event_counts( @Status, FEventBufferLen, FEventBuffer, FResultBuffer);
    CancelAlerts := false;
    if not FSignalFired then
      FSignalFired := true   {Ignore first time}
    else
    if assigned(FOwner.FOnEventAlert)  then
    begin
      for i := 0 to FEvents.Count - 1 do
      begin
        try
        if (Status[i] <> 0) and not CancelAlerts then
            FOwner.FOnEventAlert( self, FEvents[i], Status[i], CancelAlerts);
        except
            FOwner.FBase.HandleException(Self)
        end;
      end;
    end;
    FState := stHasEvb;
  if  CancelAlerts then
      CancelEvents
    else
      QueueEvents
end;

procedure TEventHandler.Execute;
begin
  while not Terminated do
  begin
    {$IFDEF WINDOWS}
    WaitForSingleObject(FEventHandler,INFINITE);
    {$ELSE}
    FEventWaiting.WaitFor(INFINITE);
    {$ENDIF}

    if not Terminated and (FState = stSignalled) then
      Synchronize(DoEventSignalled)
  end;
end;



constructor TEventHandler.Create(Owner: TIBEvents);
var
  PSa : PSecurityAttributes;
{$IFDEF WINDOWS}
  Sd : TSecurityDescriptor;
  Sa : TSecurityAttributes;
begin
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
  PSa := @Sa;
{$ELSE}
begin
  PSa:= nil;
{$ENDIF}
  inherited Create(true);
  FOwner := Owner;
  FState := stIdle;
  FCriticalSection := TCriticalSection.Create;
  {$IFDEF WINDOWS}
  FEventHandler := CreateEvent(PSa,false,true,nil);
  {$ELSE}
  FEventWaiting := TEventObject.Create(PSa,false,true,FOwner.Name+'.Events');
  {$ENDIF}
  FEvents := TStringList.Create;
  FreeOnTerminate := true;
  Resume
end;

destructor TEventHandler.Destroy;
begin
  if assigned(FCriticalSection) then FCriticalSection.Free;
  {$IFDEF WINDOWS}
  CloseHandle(FEventHandler);
  {$ELSE}
  if assigned(FEventWaiting) then FEventWaiting.Free;
  {$ENDIF}
  if assigned(FEvents) then FEvents.Free;
  inherited Destroy;
end;

procedure TEventHandler.Terminate;
begin
  inherited Terminate;
  {$IFDEF WINDOWS}
  SetEvent(FEventHandler);
  {$ELSE}
  FEventWaiting.SetEvent;
  {$ENDIF}
  CancelEvents;
end;

procedure TEventHandler.RegisterEvents(Events: TStrings);
var
  i: integer;
  EventNames: array of PChar;
begin
  UnregisterEvents;

  if Events.Count = 0 then
    exit;

  setlength(EventNames,MaxEvents);
  try
    for i := 0 to Events.Count-1 do
      EventNames[i] := PChar(Events[i]);
    FEvents.Assign(Events);
    FEventBufferlen := isc_event_block(@FEventBuffer,@FResultBuffer,
                        Events.Count,
                        EventNames[0],EventNames[1],EventNames[2],
                        EventNames[3],EventNames[4],EventNames[5],
                        EventNames[6],EventNames[7],EventNames[8],
                        EventNames[9],EventNames[10],EventNames[11],
                        EventNames[12],EventNames[13],EventNames[14]
                        );
    FState := stHasEvb;
    FRegisteredState := true;
    QueueEvents
  finally
    SetLength(EventNames,0)
  end;
end;

procedure TEventHandler.UnregisterEvents;
begin
  if FRegisteredState then
  begin
    CancelEvents;
    FRegisteredState := false;
  end;
end;

{ TIBEvents }

procedure TIBEvents.ValidateDatabase( Database: TIBDatabase);
begin
  if not assigned( Database) then
    IBError(ibxeDatabaseNameMissing, [nil]);
  if not Database.Connected then
    IBError(ibxeDatabaseClosed, [nil]);
end;

constructor TIBEvents.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.AfterDatabaseConnect := DoAfterDatabaseConnect;
  FEvents := TStringList.Create;
  with TStringList( FEvents) do
  begin
    OnChange := EventChange;
    Duplicates := dupIgnore;
  end;
  FEventHandler := TEventHandler.Create(self)
end;

destructor TIBEvents.Destroy;
begin
  if FIBLoaded then
  begin
    UnregisterEvents;
    SetDatabase(nil);
    TStringList(FEvents).OnChange := nil;
    FBase.Free;
    FEvents.Free;
  end;
  if assigned(FEventHandler) then
    TEventHandler(FEventHandler).Terminate;
  FEventHandler := nil;
  inherited Destroy;
end;



procedure TIBEvents.EventChange( sender: TObject);
begin
  { check for blank event }
  if TStringList(Events).IndexOf( '') <> -1 then
    IBError(ibxeInvalidEvent, [nil]);
  { check for too many events }
  if Events.Count > MaxEvents then
  begin
    TStringList(Events).OnChange := nil;
    Events.Delete( MaxEvents);
    TStringList(Events).OnChange := EventChange;
    IBError(ibxeMaximumEvents, [nil]);
  end;
  if Registered then
    TEventHandler(FEventHandler).RegisterEvents(Events);
end;

procedure TIBEvents.Notification( AComponent: TComponent;
                                        Operation: TOperation);
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBase.Database) then
  begin
    UnregisterEvents;
    FBase.Database := nil;
  end;
end;

procedure TIBEvents.RegisterEvents;
begin
  ValidateDatabase( Database);
  if csDesigning in ComponentState then FRegistered := true
  else
  begin
    if not FBase.Database.Connected then
      FDeferredRegister := true
    else
    begin
      TEventHandler(FEventHandler).RegisterEvents(Events);
      FRegistered := true;
    end;
  end;
end;

procedure TIBEvents.SetEvents( value: TStrings);
begin
  FEvents.Assign( value);
end;

procedure TIBEvents.SetDatabase( value: TIBDatabase);
begin
  if value <> FBase.Database then
  begin
    if Registered then UnregisterEvents;
    if assigned( value) and value.Connected then ValidateDatabase( value);
    FBase.Database := value;
    if (FBase.Database <> nil) and FBase.Database.Connected then
      DoAfterDatabaseConnect(FBase.Database)
  end;
end;

function TIBEvents.GetDatabase: TIBDatabase;
begin
  Result := FBase.Database
end;

procedure TIBEvents.SetRegistered( value: Boolean);
begin
  FDeferredRegister := false;
  if not assigned(FBase) or (FBase.Database = nil) then
  begin
    FDeferredRegister := value;
    Exit;
  end;

  if value then RegisterEvents else UnregisterEvents;
end;

procedure TIBEvents.UnregisterEvents;
begin
  FDeferredRegister := false;
  if not FRegistered then
    Exit;
  if csDesigning in ComponentState then
    FRegistered := false
  else
  begin
    TEventHandler(FEventHandler).UnRegisterEvents;
    FRegistered := false;
  end;
end;

procedure TIBEvents.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  UnregisterEvents;
end;

procedure TIBEvents.DoAfterDatabaseConnect(Sender: TObject);
begin
  if FDeferredRegister then
    Registered := true
end;

function TIBEvents.GetDatabaseHandle: TISC_DB_HANDLE;
begin
  ValidateDatabase(FBase.Database);
  Result := FBase.Database.Handle;
end;


end.
