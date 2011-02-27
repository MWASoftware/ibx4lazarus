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
{    Contributor: Tony Whyman, MWA Software (http://www.mwasoftare.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBEvents;

{$Mode Delphi}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  Classes, Graphics, Controls,
  Forms, Dialogs, IBHeader, IBExternals, IB, IBDatabase;

const
  MaxEvents = 15;
  EventLength = 64;

type

  TEventAlert = procedure( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean) of object;

  TEventBuffer = array[ 0..MaxEvents-1, 0..EventLength-1] of char;

  { TIBEvents }

  TIBEvents = class(TComponent)
  private
    FIBLoaded: Boolean;
    FBase: TIBBase;
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FEventHandler: TObject;
    FRegistered: boolean;
    procedure EventChange(sender: TObject);
    function GetDatabase: TIBDatabase;
    procedure SetDatabase( value: TIBDatabase);
    procedure ValidateDatabase( Database: TIBDatabase);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    procedure SetEvents( value: TStrings);
    procedure SetRegistered( value: boolean);

  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents;
    procedure UnRegisterEvents;
  published
    property  Database: TIBDatabase read GetDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
  end;

implementation

uses
  IBIntf, syncobjs;

type

  { TEventHandler }

  TEventHandler = class(TThread)
  private
    FOwner: TIBEvents;
    FCriticalSection: TCriticalSection;
    FEventWaiting: TEventObject;
    FEvents: TStringList;
    FEventSignalled: boolean;
    FEventListChanged: boolean;
    FEventWait: boolean;
    FEventBuffer: PChar;
    FEventBufferLen: integer;
    FEventID: ISC_LONG;
    FRegisteredState: Boolean;
    FResultBuffer: PChar;
    FSignalledResultBuffer: string;
    FCancelAlerts: boolean;
    procedure AllocateEventBlock;
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
    procedure EventListChanged(Events: TStrings);
    procedure RegisterEvents(Events: TStrings);
    procedure UnregisterEvents;
  end;

procedure IBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
begin
  { Handle events asynchronously in second thread }
  TEventHandler(ptr).HandleEventSignalled(length,updated);
end;



{ TEventHandler }

procedure TEventHandler.AllocateEventBlock;
var
  i: integer;
  EventNames: array of PChar;
begin
  if FEvents.Count = 0 then exit;
  writeln(FEvents.Count,' Events');
  FCriticalSection.Enter;
  try
    setlength(EventNames,FEvents.Count);
    for i := 0 to FEvents.Count-1 do
    begin
      writeln('Registering for ',FEvents[i]);
      EventNames[i] := PChar(FEvents[i]);
    end;
  finally
    FCriticalSection.Leave
  end;
  writeln('Calling Event Block');
  FEventBufferlen := isc_event_block(@FEventBuffer,@FResultBuffer,
                        FEvents.Count,EventNames);
  writeln('Event Block Returned ',FEventBufferlen);
end;

procedure TEventHandler.QueueEvents;
var
  callback: pointer;
begin
  writeln('Queue Events');
  FEventSignalled := false;
  callback := @IBEventCallback;
  if (isc_que_events( StatusVector, @FOwner.FBase.Database.Handle, @FEventID, FEventBufferLen,
                     FEventBuffer, TISC_CALLBACK(callback), PVoid(Self)) <> 0) then
    IBDatabaseError;
  FEventWait := true;
end;

procedure TEventHandler.CancelEvents;
begin
  if not FEventWait then Exit;
  writeln('Cancel Events');
  FCriticalSection.Enter;
  try
    if (isc_Cancel_events( StatusVector, @FOwner.FBase.Database.Handle, @FEventID) <> 0) then
        IBDatabaseError;
  finally
    FCriticalSection.Leave
  end;
  isc_free( FEventBuffer);
  FEventBuffer := nil;
  isc_free( FResultBuffer);
  FResultBuffer := nil;
  FEventWait := false;
  FEventSignalled := false;
end;

procedure TEventHandler.HandleEventSignalled(length: short; updated: PChar);
begin
  FCriticalSection.Enter;
  try
    SetString(FSignalledResultBuffer,updated,length);
    FEventSignalled := true;
    FEventWaiting.SetEvent
  finally
    FCriticalSection.Leave
  end;
end;

procedure TEventHandler.DoEventSignalled;
var
  Status: PStatusVector;
  i: integer;
begin
writeln('Handle Event');
    { prevent modification of vital data structures while handling events }
    isc_event_counts( StatusVector, FEventBufferLen, FEventBuffer, PChar(FSignalledResultBuffer));
    FCancelAlerts := false;
    if assigned(FOwner.FOnEventAlert)  then
    begin
      for i := 0 to FEvents.Count-1 do
      begin
        try
        Status := StatusVectorArray;
        if (Status[i] <> 0) and not FCancelAlerts then
            FOwner.FOnEventAlert( self, FEvents[FEvents.Count-i-1], Status[i], FCancelAlerts);
        except
          Application.HandleException( nil);
        end;
      end;
    end;
  writeln('Events Handled')
end;

procedure TEventHandler.Execute;
begin
  writeln('Event Handler Started');
  while not Terminated do
  begin
    if not FEventListChanged and not FEventSignalled  and
     ((FRegisteredState = FEventWait)  or (FRegisteredState and (FOwner.Events.Count = 0))) then
     begin
       writeln('Sleeping');
      FEventWaiting.WaitFor(INFINITE);
     end;
      writeln('Awake');

    if FEventSignalled  then
    begin
      writeln('Event Signalled');
      FEventSignalled := false;
      Synchronize(DoEventSignalled);
      if  FCancelAlerts then
        CancelEvents
      else
        QueueEvents
    end;

    if FEventListChanged then
    begin
      writeln('List Changed');
      FEventListChanged := false;
      CancelEvents;
      if  FOwner.Events.Count > 0 then
      begin
        AllocateEventBlock;
        QueueEvents;
      end
    end;

    if FRegisteredState and not FEventWait and (FOwner.Events.Count > 0) then
    begin
      writeln('Do Register');
      AllocateEventBlock;
      QueueEvents
    end
    else
    if not FRegisteredState and FEventWait then
    begin
      writeln('Do Unregister');
      CancelEvents
    end
  end;
  writeln('Loop Exit');
  CancelEvents;
  writeln('Thread Exit')
end;

constructor TEventHandler.Create(Owner: TIBEvents);
var
  PSa : PSecurityAttributes;
{$IFDEF WINDOWS}
  Sd : TSecurityDescriptor;
  Sa := TSecurityAttributes;
begin
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
{$ELSE}
begin
  PSa:= nil;
{$ENDIF}
  inherited Create(true);
  FOwner := Owner;
  FCriticalSection := TCriticalSection.Create;
  FEventWaiting := TEventObject.Create(PSa,false,true,FOwner.Name+'.Events');
  FEvents := TStringList.Create;
  Resume
end;

destructor TEventHandler.Destroy;
begin
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FEventWaiting) then FEventWaiting.Free;
  if assigned(FEvents) then FEvents.Free;
  inherited Destroy;
end;

procedure TEventHandler.Terminate;
begin
  inherited Terminate;
//  writeln('Terminate Called');
  FEventWaiting.SetEvent
end;

procedure TEventHandler.EventListChanged(Events: TStrings);
begin
   FCriticalSection.Enter;
    try
      FEvents.Assign(Events);
      FEventListChanged := true;
    finally
      FCriticalSection.Leave
    end;
  FEventWaiting.SetEvent
end;

procedure TEventHandler.RegisterEvents(Events: TStrings);
begin
  if not FRegisteredState then
  begin
    writeln('Register');
    FCriticalSection.Enter;
    try
      FEvents.Assign(Events);
      FRegisteredState := true;
    finally
      FCriticalSection.Leave
    end;
    FEventWaiting.SetEvent
  end;
end;

procedure TEventHandler.UnregisterEvents;
begin
  if FRegisteredState then
  begin
    writeln('Unregister');
    FRegisteredState := false;
    FEventWaiting.SetEvent
  end;
end;

procedure TIBEvents.ValidateDatabase( Database: TIBDatabase);
begin
  if not assigned( Database) then
    IBError(ibxeDatabaseNameMissing, [nil]);
  if not Database.Connected then
    IBError(ibxeDatabaseClosed, [nil]);
end;

{ TIBEvents }

constructor TIBEvents.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
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
    SetDatabase( nil);
    TStringList(FEvents).OnChange := nil;
    FEvents.Free;
    FBase.Free;
  end;
  if assigned(FEventHandler) then
  begin
    TEventHandler(FEventHandler).Terminate;
    TEventHandler(FEventHandler).WaitFor;
    TEventHandler(FEventHandler).Free
  end;
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
    TEventHandler(FEventHandler).EventListChanged(Events);
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
    TEventHandler(FEventHandler).RegisterEvents(Events);
    FRegistered := true;
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
    UnregisterEvents;
    if assigned( value) and value.Connected then ValidateDatabase( value);
    FBase.Database := value;
  end;
end;

function TIBEvents.GetDatabase: TIBDatabase;
begin
  Result := FBase.Database
end;

procedure TIBEvents.SetRegistered( value: Boolean);
begin
  if (csReading in ComponentState) then
    FRegistered := value
  else if FRegistered <> value  then
    if value then RegisterEvents else UnregisterEvents;
end;

procedure TIBEvents.UnregisterEvents;
begin
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


end.
