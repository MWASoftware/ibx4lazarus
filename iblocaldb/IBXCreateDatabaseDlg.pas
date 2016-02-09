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
 *  The Original Code is (C) 2014 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBXCreateDatabaseDlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBServices, StdCtrls, ExtCtrls;

type

  { TCreateDatabaseDlg }

  TCreateDatabaseDlg = class(TForm)
    IBRestoreService1: TIBRestoreService;
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoRestore(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  CreateDatabaseDlg: TCreateDatabaseDlg;

implementation

{$R *.lfm}

procedure TCreateDatabaseDlg.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoRestore,0)
end;

procedure TCreateDatabaseDlg.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  if FileExists(IBRestoreService1.DatabaseName[0]) then
  begin
    ModalResult := mrOK
  end
  else
    ModalResult := mrCancel
end;

procedure TCreateDatabaseDlg.DoRestore(Data: PtrInt);
begin
 try
  IBRestoreService1.Active := true;
  IBRestoreService1.ServiceStart;
  try
    while not IBRestoreService1.Eof do
    begin
      Status.Caption := Trim(IBRestoreService1.GetNextLine);
      Application.ProcessMessages
    end;
  finally
    IBRestoreService1.Active := false
  end;
  Timer1.Interval := 500;
 except on E:Exception do
   raise
 end;
end;

end.
