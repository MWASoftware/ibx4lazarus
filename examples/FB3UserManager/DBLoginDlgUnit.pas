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

unit DBLoginDlgUnit;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, StdCtrls, ExtCtrls, Buttons, IB, IBDialogs;

type
  { TDBLoginDlg }

  TDBLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    DatabaseName: TEdit;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(var aDatabaseName, aUserName, aPassword: string
      ): TModalResult;
  end;

var DBLoginDlg: TDBLoginDlg;

implementation

{$R *.lfm}

{ TDBLoginDlg }

procedure TDBLoginDlg.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFileDir(DatabaseName.Text);
  if OpenDialog1.Execute then
    DatabaseName.Text := OpenDialog1.FileName;
end;

function TDBLoginDlg.ShowModal(var aDatabaseName, aUserName, aPassword: string
  ): TModalResult;
begin
  DatabaseName.Text := aDatabaseName;
  UserName.Text := aUserName;
  Password.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDatabaseName := DatabaseName.Text;
    aUserName := UserName.Text;
    aPassword := Password.Text;
  end;
end;


end.
