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
  SysUtils, Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls, IB, IBTypes;

type

  { TIBLCLInterface }

  TIBLCLInterface = class(TInterfacedObject,TIBGUIInterface)
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
  end;

implementation

{$R IBDialogs.lfm}

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

function TIBLCLInterface.ServerLoginDialog(var AServerName: string;
  var AUserName, APassword: string): Boolean;
begin
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
end;

function TIBLCLInterface.LoginDialogEx(var ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
begin
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

initialization
  IBGUIInterface :=  TIBLCLInterface.Create;

end.
