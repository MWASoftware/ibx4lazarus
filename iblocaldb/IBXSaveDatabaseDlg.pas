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
unit IBXSaveDatabaseDlg;

interface

{$mode objfpc}{$H+}

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBServices, StdCtrls, ExtCtrls;

type

  { TSaveDatabaseDlg }

  TSaveDatabaseDlg = class(TForm)
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    IBBackupService1: TIBBackupService;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoBackup(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  SaveDatabaseDlg: TSaveDatabaseDlg;

function SaveDatabaseToArchive(DBName: string;  DBParams: TStrings; aFilename: string): boolean;

implementation

uses Registry;

{$IFDEF WINDOWS}
const
  rgShellFolders      = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  rgPersonal          = 'Personal';
{$ENDIF}

function SaveDatabaseToArchive(DBName: string; DBParams: TStrings;
  aFilename: string): boolean;
begin
 with TSaveDatabaseDlg.Create(Application) do
 try
  if aFilename = ''  then
  begin
    SaveDialog1.InitialDir := GetUserDir;
    {$IFDEF WINDOWS}
    with TRegistry.Create do
    try
      if OpenKey(rgShellFolders,false) then
      begin
        SaveDialog1.InitialDir := ReadString(rgPersonal)
      end;
    finally
      Free
    end;
    {$ENDIF}
    if SaveDialog1.Execute then
      aFilename := SaveDialog1.FileName
    else
      Exit;
  end;
  IBBackupService1.SetDBParams(DBParams);
  IBBackupService1.BackupFile.Clear;
  IBBackupService1.DatabaseName := DBName;
  IBBackupService1.BackupFile.Add(aFilename);
  Result := ShowModal = mrOK
 finally
   Free
 end;
end;

{$R *.lfm}

{ TSaveDatabaseDlg }

procedure TSaveDatabaseDlg.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoBackup,0);
end;

procedure TSaveDatabaseDlg.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  if FileExists(IBBackupService1.BackupFile[0])  then
    ModalResult := mrOK
  else
    ModalResult := mrCancel
end;

procedure TSaveDatabaseDlg.DoBackup(Data: PtrInt);
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
 except
   ModalResult := mrCancel;
   raise
 end;
 Timer1.Interval := 500;
end;

end.
