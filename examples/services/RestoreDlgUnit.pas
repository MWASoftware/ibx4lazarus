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
            
unit RestoreDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, IBServices, IBXServices;

type

  { TRestoreDlg }

  TRestoreDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    CSRestoreService1: TIBXClientSideRestoreService;
    SSRestoreService1: TIBXServerSideRestoreService;
    ReplaceExisting: TCheckBox;
    ServerName: TEdit;
    DBName: TEdit;
    BackupFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    ServerSideBtn: TRadioButton;
    ClientSideBtn: TRadioButton;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBRestoreService1GetNextLine(Sender: TObject; var Line: string);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FOutputLog: TStrings;
    procedure RunRestore;
  public
    { public declarations }
    function ShowModal(var aDBName: string; OutputLog: TStrings): TModalResult;
  end;

var
  RestoreDlg: TRestoreDlg;

implementation

{$R *.lfm}

{ TRestoreDlg }

procedure TRestoreDlg.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    BackupFileName.Text := OpenDialog1.Filename;
end;

procedure TRestoreDlg.RunRestore;
begin
  FOutputLog.Add('Restore Started');
  if ClientSideBtn.Checked then
    CSRestoreService1.RestoreFromFile(BackupFilename.Text,FOutputLog)
  else
    SSRestoreService1.Execute(FOutputLog);

  FOutputLog.Add('Restore Completed');
  MessageDlg('Restore Completed',mtInformation,[mbOK],0);
end;

function TRestoreDlg.ShowModal(var aDBName: string; OutputLog: TStrings
  ): TModalResult;
begin
  DBName.Text := aDBName;
  FOutputLog := OutputLog;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    RunRestore;
    aDBName := DBName.Text;
  end;
end;

procedure TRestoreDlg.FormShow(Sender: TObject);
begin
  ServerName.Text := CSRestoreService1.ServicesConnection.ServerName;
end;

procedure TRestoreDlg.IBRestoreService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Application.ProcessMessages;
end;

procedure TRestoreDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;
  if DBName.Text = '' then
    raise Exception.Create('A Database Name must be given');
  if BackupFileName.Text = '' then
    raise Exception.Create('A Backup File Name must be given');
  CSRestoreService1.DatabaseFiles.Clear;
  CSRestoreService1.DatabaseFiles.Add(DBName.Text);
  SSRestoreService1.DatabaseFiles.Clear;
  SSRestoreService1.DatabaseFiles.Add(DBName.Text);
  SSRestoreService1.BackupFiles.Clear;
  SSRestoreService1.BackupFiles.Add(BackupFileName.Text);
  if ReplaceExisting.Checked then
  begin
     CSRestoreService1.Options := CSRestoreService1.Options + [Replace] - [CreateNewDB];
     SSRestoreService1.Options := SSRestoreService1.Options + [Replace] - [CreateNewDB];
  end
  else
  begin
    CSRestoreService1.Options := CSRestoreService1.Options - [Replace] + [CreateNewDB];
    SSRestoreService1.Options := SSRestoreService1.Options - [Replace] + [CreateNewDB];
  end;
end;

end.

