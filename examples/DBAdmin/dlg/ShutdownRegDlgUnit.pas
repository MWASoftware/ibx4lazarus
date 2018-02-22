(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
(*
 * ShutdownRegDlgUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit ShutdownRegDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IBServices;

type

  { TShutdownReqDlg }

  TShutdownReqDlg = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    DatabaseName: TEdit;
    Delay: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OKBtn: TButton;
    ShutdownOptions: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(const aDatabaseName: string; var aShutDownmode: TShutdownMode;
      var aDelay: integer): TModalResult;
  end;

var
  ShutdownReqDlg: TShutdownReqDlg;

implementation

{$R *.lfm}

{ TShutdownReqDlg }

procedure TShutdownReqDlg.FormShow(Sender: TObject);
begin
  Delay.Text := '60';
end;

function TShutdownReqDlg.ShowModal(const aDatabaseName: string;
  var aShutDownmode: TShutdownMode; var aDelay: integer): TModalResult;
begin
  ShutdownOptions.ItemIndex := ord(aShutDownmode);
  DatabaseName.Text := aDatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDelay := StrToInt(Delay.Text);
    aShutDownmode := TShutdownMode(ShutdownOptions.ItemIndex);
  end;
end;

end.

