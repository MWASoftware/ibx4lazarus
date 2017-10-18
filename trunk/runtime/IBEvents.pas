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

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  Classes, IBExternals, IB, IBDatabase;

const
  MaxEvents = 15;

type

  TEventAlert = procedure( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean) of object;

  { TIBEvents }

  TIBEvents = class(TComponent)
  private
    FBase: TIBBase;
    FEventIntf: IEvents;
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FRegistered: boolean;
    FDeferredRegister: boolean;
    FStartEvent: boolean;
    procedure EventHandler(Sender: IEvents);
    procedure ProcessEvents;
    procedure EventChange(sender: TObject);
    function GetDatabase: TIBDatabase;
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
    property DeferredRegister: boolean read FDeferredRegister write FDeferredRegister;
    property EventIntf: IEvents read FEventIntf;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read FRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
  end;


implementation

uses SysUtils, FBMessages;

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
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := @DoBeforeDatabaseDisconnect;
  FBase.AfterDatabaseConnect := @DoAfterDatabaseConnect;
  FEvents := TStringList.Create;
  FStartEvent := true;
  with TStringList( FEvents) do
  begin
    OnChange := @EventChange;
    Duplicates := dupIgnore;
  end;
end;

destructor TIBEvents.Destroy;
begin
  UnregisterEvents;
  SetDatabase(nil);
  TStringList(FEvents).OnChange := nil;
  FBase.Free;
  FEvents.Free;
end;

procedure TIBEvents.EventHandler(Sender: IEvents);
begin
  TThread.Synchronize(nil,@ProcessEvents);
end;

procedure TIBEvents.ProcessEvents;
var EventCounts: TEventCounts;
    CancelAlerts: Boolean;
    i: integer;
begin
  if (csDestroying in ComponentState) or (FEventIntf = nil) then Exit;
  CancelAlerts := false;
  EventCounts := FEventIntf.ExtractEventCounts;
  if FStartEvent then
    FStartEvent := false {ignore the first one}
  else
  if assigned(FOnEventAlert) then
  begin
    CancelAlerts := false;
    for i := 0 to Length(EventCounts) -1 do
    begin
      OnEventAlert(self,EventCounts[i].EventName,EventCounts[i].Count,CancelAlerts);
      if CancelAlerts then break;
    end;
  end;
  if CancelAlerts then
    UnRegisterEvents
  else
    FEventIntf.AsyncWaitForEvent(@EventHandler);
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
    TStringList(Events).OnChange := @EventChange;
    IBError(ibxeMaximumEvents, [nil]);
  end;
  if Registered  and (FEventIntf <> nil) then
  begin
    FEventIntf.SetEvents(Events);
    FEventIntf.AsyncWaitForEvent(@EventHandler);
  end;
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
  if FRegistered then Exit;
  ValidateDatabase( Database);
  if csDesigning in ComponentState then FRegistered := true
  else
  begin
    if not FBase.Database.Connected then
      FDeferredRegister := true
    else
    begin
      FEventIntf := Database.Attachment.GetEventHandler(Events);
      FEventIntf.AsyncWaitForEvent(@EventHandler);
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

procedure TIBEvents.SetRegistered(value: boolean);
begin
  FDeferredRegister := false;
  if not assigned(FBase) or (FBase.Database = nil) then
  begin
    FDeferredRegister := value;
    Exit;
  end;

  if value then RegisterEvents else UnregisterEvents;
end;

procedure TIBEvents.UnRegisterEvents;
begin
  FDeferredRegister := false;
  if not FRegistered then
    Exit;
  if csDesigning in ComponentState then
    FRegistered := false
  else
  begin
    FEventIntf := nil;
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


end.
