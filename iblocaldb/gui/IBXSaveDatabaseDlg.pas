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
  Dialogs, IBXServices, StdCtrls, ExtCtrls;

type

  { TSaveDatabaseDlg }

  TSaveDatabaseDlg = class(TForm)
    IBBackupService1: TIBXServerSideBackupService;
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure IBBackupService1GetNextLine(Sender: TObject; var Line: string);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoBackup(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  SaveDatabaseDlg: TSaveDatabaseDlg;

function SaveDatabaseToArchive(aBackupService: TIBXServerSideBackupService; aFilename: string): boolean;

implementation

uses Registry;

{$IFDEF WINDOWS}
const
  rgShellFolders      = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  rgPersonal          = 'Personal';
{$ENDIF}

function SaveDatabaseToArchive(aBackupService: TIBXServerSideBackupService;
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
  IBBackupService1.Assign(aBackupService);
  IBBackupService1.BackupFiles.Clear;
  IBBackupService1.BackupFiles.Add(aFileName);
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

procedure TSaveDatabaseDlg.IBBackupService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Status.Caption := Line;
  Application.ProcessMessages;
end;

procedure TSaveDatabaseDlg.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  if FileExists(IBBackupService1.BackupFiles[0])  then
    ModalResult := mrOK
  else
    ModalResult := mrCancel
end;

procedure TSaveDatabaseDlg.DoBackup(Data: PtrInt);
begin
 try
  IBBackupService1.Execute(nil);
 except
   ModalResult := mrCancel;
   raise
 end;
 Timer1.Interval := 500;
end;

end.
