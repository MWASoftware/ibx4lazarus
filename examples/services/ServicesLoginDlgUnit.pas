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

unit ServicesLoginDlgUnit;

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
  { TSvcLoginDlg }

  TSvcLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    ServiceName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(var aServiceName, aUserName, aPassword: string
      ): TModalResult;
  end;

var SvcLoginDlg: TSvcLoginDlg;

implementation

{$R *.lfm}

{ TSvcLoginDlg }

function TSvcLoginDlg.ShowModal(var aServiceName, aUserName, aPassword: string
  ): TModalResult;
begin
  ServiceName.Text := aServiceName;
  UserName.Text := aUserName;
  Password.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aServiceName := ServiceName.Text;
    aUserName := UserName.Text;
    aPassword := Password.Text;
  end;
end;


end.
