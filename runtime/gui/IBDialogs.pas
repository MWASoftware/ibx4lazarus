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

unit IBDialogs;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes,  Controls, Forms, StdCtrls, ExtCtrls, IB, IBInternals;

type

  { TIBLCLInterface }

  TIBLCLInterface = class(TInterfacedObject,IIBGUIInterface)
    private
      FSetCursorDepth: integer;
  public
    function ServerLoginDialog(var AServerName: string;
                               var AUserName, APassword: string): Boolean;  virtual;
    function LoginDialogEx(var ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean; virtual;
    procedure SetCursor;
    procedure RestoreCursor;
    function CreateTimer: IIBTimerInf;
    procedure QueueAsyncCall(const AMethod: TIBDataEvent;  Data: PtrInt);
  end;

implementation

{$R IBDialogs.lfm}

uses CustomTimer;

type
  { TIBXLoginDlg }

  TIBXLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    DatabaseName: TLabel;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TIBTimer }

  TIBTimer = class(TInterfacedObject,IIBTimerInf)
  private
    FTimer: TCustomTimer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnabled: boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);
    function GetOnStartTimer: TNotifyEvent;
    procedure SetOnStartTimer(Value: TNotifyEvent);
    function GetOnStopTimer: TNotifyEvent;
    procedure SetOnStopTimer(Value: TNotifyEvent);
end;

{ TIBTimer }

constructor TIBTimer.Create;
begin
  inherited Create;
  FTimer := TCustomTimer.Create(nil);
end;

destructor TIBTimer.Destroy;
begin
  if FTimer <> nil then FTimer.Free;
  inherited Destroy;
end;

function TIBTimer.GetEnabled: boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TIBTimer.SetEnabled(Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

function TIBTimer.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TIBTimer.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TIBTimer.GetOnTimer: TNotifyEvent;
begin
  Result := FTimer.OnTimer;
end;

procedure TIBTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FTimer.OnTimer := Value;
end;

function TIBTimer.GetOnStartTimer: TNotifyEvent;
begin
  Result := FTimer.OnStartTimer;
end;

procedure TIBTimer.SetOnStartTimer(Value: TNotifyEvent);
begin
  FTimer.OnStartTimer := Value;
end;

function TIBTimer.GetOnStopTimer: TNotifyEvent;
begin
  Result := FTimer.OnStopTimer;
end;

procedure TIBTimer.SetOnStopTimer(Value: TNotifyEvent);
begin
  FTimer.OnStopTimer := Value;
end;

function TIBLCLInterface.ServerLoginDialog(var AServerName: string;
  var AUserName, APassword: string): Boolean;
var ActiveForm: TCustomForm;
begin
  ActiveForm := Screen.ActiveCustomForm;
  with TIBXLoginDlg.Create(nil) do
  try
    Caption := 'Firebird Server Login';
    TargetCaption.Caption := 'Server Name: ';
    DatabaseName.Caption := AServerName;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end;
  finally
    Free;
  end;
  if ActiveForm <> nil then
  begin
    ActiveForm.SetFocus;
    Application.ProcessMessages;
  end;
end;

function TIBLCLInterface.LoginDialogEx(var ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
var ActiveForm: TCustomForm;
begin
  ActiveForm := Screen.ActiveCustomForm;
  with TIBXLoginDlg.Create(Application) do
  try
    DatabaseName.Caption := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if NameReadOnly then
      UserName.Enabled := False
    else
      if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end
  finally
    Free;
  end;
  if (ActiveForm <> nil) and ActiveForm.CanFocus then
  begin
    ActiveForm.SetFocus;
    Application.ProcessMessages;
  end;
end;

procedure TIBLCLInterface.SetCursor;
begin
  if (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault) then
  begin
    if FSetCursorDepth = 0 then
      Screen.Cursor := crHourGlass;
    Inc(FSetCursorDepth);
  end;
end;

procedure TIBLCLInterface.RestoreCursor;
begin
  if FSetCursorDepth > 0 then
  begin
     Dec(FSetCursorDepth);
     if FSetCursorDepth = 0 then
       Screen.Cursor := crDefault
  end;
end;

function TIBLCLInterface.CreateTimer: IIBTimerInf;
begin
  Result := TIBTimer.Create;
end;

procedure TIBLCLInterface.QueueAsyncCall(const AMethod: TIBDataEvent;
  Data: PtrInt);
begin
  Application.QueueAsyncCall(AMethod,Data);
end;

initialization
  IBGUIInterface :=  TIBLCLInterface.Create;

end.
