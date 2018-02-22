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
    function ShowModal(var aDatabaseName: string; var aShutDownmode: TShutdownMode;
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

function TShutdownReqDlg.ShowModal(var aDatabaseName: string;
  var aShutDownmode: TShutdownMode; var aDelay: integer): TModalResult;
begin
  ShutdownOptions.ItemIndex := ord(aShutDownmode);
  DatabaseName.Text := aDatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDelay := StrToInt(Delay.Text);
    aShutDownmode := TShutdownMode(ShutdownOptions.ItemIndex);
    aDatabaseName := DatabaseName.Text;
  end;
end;

end.

