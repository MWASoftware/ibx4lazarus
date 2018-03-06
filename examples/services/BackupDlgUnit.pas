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
            
unit BackupDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IBXServices, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TBackupDlg }

  TBackupDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    CSBackupService1: TIBXClientSideBackupService;
    SSBackupService1: TIBXServerSideBackupService;
    ServerName: TEdit;
    DBName: TEdit;
    BackupFilename: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ServerSideBtn: TRadioButton;
    ClientSideBtn: TRadioButton;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FOutputLog: TStrings;
    procedure RunBackup;
  public
    { public declarations }
    function ShowModal(var aDBName: string; OutputLog: TStrings): TModalResult;
 end;

var
  BackupDlg: TBackupDlg;

implementation

{$R *.lfm}

{ TBackupDlg }

procedure TBackupDlg.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    BackupFilename.Text := SaveDialog1.Filename;
end;

function TBackupDlg.ShowModal(var aDBName: string; OutputLog: TStrings): TModalResult;
begin
  DBName.Text := aDBName;
  FOutputLog := OutputLog;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    RunBackup;
    aDBName := DBName.Text;
  end;
end;

procedure TBackupDlg.RunBackup;
var BackupCount: integer;
begin
  FOutputLog.Add('Starting Backup');
  if ClientSideBtn.Checked then
  begin
    CSBackupService1.BackupToFile(BackupFilename.Text,BackupCount);
    FOutputLog.Add(Format('Backup Completed - File Size = %d bytes',[BackupCount]));
    MessageDlg(Format('Backup Completed - File Size = %d bytes',[BackupCount]),mtInformation,[mbOK],0);
  end
  else
  begin
    SSBackupService1.Execute(FOutputLog);
    FOutputLog.Add('Backup Completed');
    MessageDlg('Backup Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TBackupDlg.FormShow(Sender: TObject);
begin
  ServerName.Text := CSBackupService1.ServicesConnection.ServerName;
  SSBackupService1.BackupFile.Clear;
end;

procedure TBackupDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;
  if DBName.Text = '' then
    raise Exception.Create('A Database Name must be given');
  if BackupFilename.Text = '' then
    raise Exception.Create('A Backup File Name must be given');
  CSBackupService1.DatabaseName := DBName.Text;
  SSBackupService1.DatabaseName := DBName.Text;
  if ServerSideBtn.Checked then
    SSBackupService1.BackupFile.Add(BackupFilename.Text);
end;

end.

