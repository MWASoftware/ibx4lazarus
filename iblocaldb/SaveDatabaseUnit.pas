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
unit SaveDatabaseUnit;

interface

{$mode objfpc}{$H+}

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBServices, StdCtrls, ExtCtrls;

type

  { TSaveDatabase }

  TSaveDatabase = class(TForm)
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    IBBackupService1: TIBBackupService;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure DoBackup(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  SaveDatabase: TSaveDatabase;

implementation

{$R *.lfm}

{ TSaveDatabase }

procedure TSaveDatabase.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoBackup,0);
end;

procedure TSaveDatabase.DoBackup(Data: PtrInt);
begin
 try
  IBBackupService1.Active := true;
  IBBackupService1.ServiceStart;
  try
    while not IBBackupService1.Eof do
    begin
      Status.Caption := IBBackupService1.GetNextLine;
      Application.ProcessMessages
    end;
  finally
    IBBackupService1.Active := false
  end;
  if FileExists(IBBackupService1.BackupFile[0]) { *Converted from FileExists*  } then
    ModalResult := mrOK
  else
    ModalResult := mrCancel
 except
   ModalResult := mrCancel;
   raise
 end;
 Sleep(500)
end;

end.
