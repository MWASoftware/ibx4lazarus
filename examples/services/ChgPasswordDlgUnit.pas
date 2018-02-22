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
            
unit ChgPasswordDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TChgPasswordDlg }

  TChgPasswordDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var Password: string): TModalResult;
  end;

var
  ChgPasswordDlg: TChgPasswordDlg;

implementation

{$R *.lfm}

{ TChgPasswordDlg }

procedure TChgPasswordDlg.FormShow(Sender: TObject);
begin
  Edit2.SetFocus;
end;

procedure TChgPasswordDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if Edit2.Text <> Edit3.Text then
    begin
      MessageDlg('Passwords do not match',mtError,[mbOK],0);
      CloseAction := caNone;
    end;
  end;
end;

function TChgPasswordDlg.ShowModal(var Password: string): TModalResult;
begin
  Edit2.Text := '';
  Edit3.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
    Password := Edit2.Text;
end;

end.

