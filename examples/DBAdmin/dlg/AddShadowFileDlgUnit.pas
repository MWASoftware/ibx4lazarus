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
 * AddShadowFileDlgUnit.pas
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
unit AddShadowFileDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAddShadowFileDlg }

  TAddShadowFileDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    FileLength: TEdit;
    FileName: TEdit;
    InPagesBtn: TRadioButton;
    Label1: TLabel;
    Label3: TLabel;
    RadioButton2: TRadioButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var aFileName: string; var aFileLength: integer;
      var aPages: boolean): TModalResult;
  end;

var
  AddShadowFileDlg: TAddShadowFileDlg;

implementation

{$R *.lfm}

{ TAddShadowFileDlg }

procedure TAddShadowFileDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var i: integer;
begin
  if ModalResult = mrOK then
  begin
    if (FileLength.Text <> '') and not TryStrToInt(FileLength.Text,i) then
    begin
      MessageDlg('File Length must be an integer or empty',mtError,[mbOK],0);
      FileLength.SetFocus;
      CloseAction := caNone;
      Exit;
    end;
    if FileName.Text = '' then
    begin
      MessageDlg('A File name must be given',mtError,[mbOK],0);
      FileName.SetFocus;
      CloseAction := caNone;
      Exit;
    end;
  end;
end;

procedure TAddShadowFileDlg.FormShow(Sender: TObject);
begin
  FileName.SetFocus;
end;

function TAddShadowFileDlg.ShowModal(var aFileName: string;
  var aFileLength: integer; var aPages: boolean): TModalResult;
begin
  FileName.Text := '';
  FileLength.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aFileName := FileName.Text;
    aPages := InPagesBtn.Checked;
    if FileLength.Text <> '' then
      aFileLength := StrToInt(FileLength.Text)
    else
      aFileLength := -1;
  end;
end;

end.

