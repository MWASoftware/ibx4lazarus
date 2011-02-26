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
{************************************************************************}

unit IBEvents;

{$Mode Delphi}

interface

uses
{$IFDEF LINUX }
  unix,
{$ELSE}
  Windows,
{$ENDIF}  Classes, Graphics, Controls,
  Forms, Dialogs, IBHeader, IBExternals, IB, IBDatabase;

const
  MaxEvents = 15;
  EventLength = 64;

type

  TEventAlert = procedure( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean) of object;

  TEventBuffer = array[ 0..MaxEvents-1, 0..EventLength-1] of char;

  TIBEvents = class(TComponent)
  private
    FIBLoaded: Boolean;
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FQueued: Boolean;
    FRegistered: Boolean;
    Changing: Boolean;
    CS: TRTLCriticalSection;
    EventBuffer: PChar;
    EventBufferLen: integer;
    EventID: ISC_LONG;
    ProcessingEvents: Boolean;
    RegisteredState: Boolean;
    ResultBuffer: PChar;
    FDatabase: TIBDatabase;
    procedure SetDatabase( value: TIBDatabase);
    procedure ValidateDatabase( Database: TIBDatabase);
    procedure DoQueueEvents;
    procedure EventChange( sender: TObject);
    procedure UpdateResultBuffer( length: short; updated: PChar);
  protected
    procedure HandleEvent;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    procedure SetEvents( value: TStrings);
    procedure SetRegistered( value: boolean);
    function  GetNativeHandle: TISC_DB_HANDLE;

  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelEvents;
    procedure QueueEvents;
    procedure RegisterEvents;
    procedure UnRegisterEvents;
    property  Queued: Boolean read FQueued;
  published
    property  Database: TIBDatabase read FDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
  end;

implementation

uses
  IBIntf;

function TIBEvents.GetNativeHandle: TISC_DB_HANDLE;
begin
  if assigned( FDatabase) and FDatabase.Connected then
    Result := FDatabase.Handle
  else result := nil;
end;

procedure TIBEvents.ValidateDatabase( Database: TIBDatabase);
begin
  if not assigned( Database) then
    IBError(ibxeDatabaseNameMissing, [nil]);
  if not Database.Connected then
    IBError(ibxeDatabaseClosed, [nil]);
end;

{ TIBEvents }

function HandleEvent( param: pointer): ptrint;
begin
  { don't let exceptions propogate out of thread }
  try
    TIBEvents( param).HandleEvent;
  except
    Application.HandleException( nil);
  end;
  EndThread;
end;

procedure IBEventCallback( ptr: pointer; length: short; updated: PChar); cdecl;
begin
  { Handle events asynchronously in second thread }
  EnterCriticalSection( TIBEvents( ptr).CS);
  TIBEvents( ptr).UpdateResultBuffer( length, updated);
  if TIBEvents( ptr).Queued then
    BeginThread( @HandleEvent,ptr);
  LeaveCriticalSection( TIBEvents( ptr).CS);
end;

constructor TIBEvents.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  InitCriticalSection( CS);
  FEvents := TStringList.Create;
  with TStringList( FEvents) do
  begin
    OnChange := EventChange;
    Duplicates := dupIgnore;
  end;
end;

destructor TIBEvents.Destroy;
begin
  if FIBLoaded then
  begin
    UnregisterEvents;
    SetDatabase( nil);
    TStringList(FEvents).OnChange := nil;
    FEvents.Free;
    DoneCriticalSection( CS);
  end;
  inherited Destroy;
end;

procedure TIBEvents.CancelEvents;
begin
  if ProcessingEvents then
    IBError(ibxeInvalidCancellation, [nil]);  
  if FQueued then
  begin
    try
      { wait for event handler to finish before cancelling events }
      EnterCriticalSection( CS);
      ValidateDatabase( Database);
      FQueued := false;
      Changing := true;
      if (isc_Cancel_events( StatusVector, @FDatabase.Handle, @EventID) > 0) then
        IBDatabaseError;
    finally
      LeaveCriticalSection( CS);
    end;
  end;
end;

procedure TIBEvents.DoQueueEvents;
var
  callback: pointer;
begin
  ValidateDatabase( DataBase);
  callback := @IBEventCallback;
  if (isc_que_events( StatusVector, @FDatabase.Handle, @EventID, EventBufferLen,
                     EventBuffer, TISC_CALLBACK(callback), PVoid(Self)) > 0) then
    IBDatabaseError;
  FQueued := true;
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
  if Registered then RegisterEvents;
end;

procedure TIBEvents.HandleEvent;
var
  Status: PStatusVector;
  CancelAlerts: Boolean;
  i: integer;
begin
  try
    { prevent modification of vital data structures while handling events }
    EnterCriticalSection( CS);
    ProcessingEvents := true;
    isc_event_counts( StatusVector, EventBufferLen, EventBuffer, ResultBuffer);
    CancelAlerts := false;
    if assigned(FOnEventAlert) and not Changing then
    begin
      for i := 0 to Events.Count-1 do
      begin
        try
        Status := StatusVectorArray;
        if (Status[i] <> 0) and not CancelAlerts then
            FOnEventAlert( self, Events[Events.Count-i-1], Status[i], CancelAlerts);
        except
          Application.HandleException( nil);
        end;
      end;
    end;
    Changing := false;
    if not CancelAlerts and FQueued then DoQueueEvents;
  finally
    ProcessingEvents := false;
    LeaveCriticalSection( CS);
  end;
end;

procedure TIBEvents.Loaded;
begin
  inherited Loaded;
  try
    if RegisteredState then RegisterEvents;
  except
    if csDesigning in ComponentState then
      Application.HandleException( self)
    else raise;
  end;
end;

procedure TIBEvents.Notification( AComponent: TComponent;
                                        Operation: TOperation);
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    UnregisterEvents;
    FDatabase := nil;
  end;
end;

procedure TIBEvents.QueueEvents;
begin
  if not FRegistered then
    IBError(ibxeNoEventsRegistered, [nil]);
  if ProcessingEvents then
    IBError(ibxeInvalidQueueing, [nil]);
  if not FQueued then
  begin
    try
      { wait until current event handler is finished before queuing events }
      EnterCriticalSection( CS);
      DoQueueEvents;
      Changing := true;
    finally
      LeaveCriticalSection( CS);
    end;
  end;
end;

procedure TIBEvents.RegisterEvents;
var
  i: integer;
  EventNames: array of PChar;
begin
  ValidateDatabase( Database);
  if csDesigning in ComponentState then FRegistered := true
  else begin
    UnregisterEvents;
    if Events.Count = 0 then exit;
    setlength(EventNames,Events.Count);
    for i := 0 to Events.Count-1 do
      EventNames[i] := PChar(Events[i]);

    EventBufferlen := isc_event_block(@EventBuffer,@ResultBuffer,
                        Events.Count,EventNames);
    FRegistered := true;
    QueueEvents;
  end;
end;

procedure TIBEvents.SetEvents( value: TStrings);
begin
  FEvents.Assign( value);
end;

procedure TIBEvents.SetDatabase( value: TIBDatabase);
begin
  if value <> FDatabase then
  begin
    UnregisterEvents;
    if assigned( value) and value.Connected then ValidateDatabase( value);
    FDatabase := value;
  end;
end;

procedure TIBEvents.SetRegistered( value: Boolean);
begin
  if (csReading in ComponentState) then
    RegisteredState := value
  else if FRegistered <> value then
    if value then RegisterEvents else UnregisterEvents;
end;

procedure TIBEvents.UnregisterEvents;
begin
  if ProcessingEvents then
    IBError(ibxeInvalidRegistration, [nil]);
  if csDesigning in ComponentState then
    FRegistered := false
  else if not (csLoading in ComponentState) then
  begin
    CancelEvents;
    if FRegistered then
    begin
      isc_free( EventBuffer);
      EventBuffer := nil;
      isc_free( ResultBuffer);
      ResultBuffer := nil;
    end;
    FRegistered := false;
  end;
end;

procedure TIBEvents.UpdateResultBuffer( length: short; updated: PChar);
var
  i: integer;
begin
  for i := 0 to length-1 do
    ResultBuffer[i] := updated[i];
end;

end.
