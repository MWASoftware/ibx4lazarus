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
            
unit SelectDBDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TSelectDBDlg }

  TSelectDBDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
  private

  public
    function ShowModal(var DatabaseName: string): TModalResult;

  end;

var
  SelectDBDlg: TSelectDBDlg;

implementation

{$R *.lfm}

{ TSelectDBDlg }

function TSelectDBDlg.ShowModal(var DatabaseName: string): TModalResult;
begin
  Edit1.Text := DatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
   DatabaseName := Edit1.Text;
  end;

end;

end.

