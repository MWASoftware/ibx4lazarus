(*
 * NewUserDlgUnit.pas
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
unit NewUserDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TNewUserDlg }

  TNewUserDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var UserName, Password: string): TModalResult;
  end;

var
  NewUserDlg: TNewUserDlg;

implementation

{$R *.lfm}


{ TNewUserDlg }

procedure TNewUserDlg.FormShow(Sender: TObject);
begin
  Edit1.SetFocus
end;

procedure TNewUserDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if Edit1.Text = '' then
    begin
      MessageDlg('A User Name must be given',mtError,[mbOK],0);
      CloseAction := caNone;
    end
    else
    if Edit2.Text <> Edit3.Text then
    begin
      MessageDlg('Passwords do not match',mtError,[mbOK],0);
      CloseAction := caNone;
    end;
  end;
end;

function TNewUserDlg.ShowModal(var UserName, Password: string): TModalResult;
begin
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    UserName := Edit1.Text;
    Password := Edit2.Text;
  end;
end;

end.

