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
            
unit SelectValidationDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TSelectValidationDlg }

  TSelectValidationDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OnlineValidation: TRadioButton;
    FullValidation: TRadioButton;
  private

  public
    function ShowModal(aServerName: string;
      var DatabaseName: string; var aOnlineValidation: boolean): TModalResult;
  end;

var
  SelectValidationDlg: TSelectValidationDlg;

implementation

{$R *.lfm}

{ TSelectValidationDlg }

function TSelectValidationDlg.ShowModal(aServerName: string;
  var DatabaseName: string; var aOnlineValidation: boolean): TModalResult;
begin
  Edit1.Text := DatabaseName;
  Edit2.text := aServerName;
  OnlineValidation.checked := aOnlineValidation;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    DatabaseName := Edit1.Text;
    aOnlineValidation := OnlineValidation.checked;
  end;
end;

end.

