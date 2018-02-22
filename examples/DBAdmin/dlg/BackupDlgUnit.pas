(*
 * BackupDlgUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit BackupDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, IBServices;

type

  { TBackupDlg }

  TBackupDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    NoDBTriggers: TCheckBox;
    NoGarbageCollection: TCheckBox;
    MetadataOnly: TCheckBox;
    IgnoreLimboTransactions: TCheckBox;
    IgnoreChecksums: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    IBBackupService1: TIBBackupService;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Report: TMemo;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SelectTab: TTabSheet;
    ReportTab: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ReportTabShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoBackup(Data: PtrInt);
  public
    { public declarations }
 end;

var
  BackupDlg: TBackupDlg;

implementation

{$R *.lfm}

{ TBackupDlg }

procedure TBackupDlg.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Edit3.Text := SaveDialog1.Filename;
end;

procedure TBackupDlg.DoBackup(Data: PtrInt);
var bakfile: TFileStream;
    BackupCount: integer;
begin
  bakfile := nil;
  with IBBackupService1 do
  begin
  Options := [];
  if IgnoreChecksums.Checked then
    Options := Options + [IBServices.IgnoreChecksums];
  if IgnoreLimboTransactions.Checked then
    Options := Options + [IgnoreLimbo];
  if MetadataOnly.Checked then
    Options := Options + [IBServices.MetadataOnly];
  if NoGarbageCollection.Checked then
    Options := Options + [IBServices.NoGarbageCollection];
  if NoDBTriggers.Checked then
    Options := Options + [IBServices.NoDBTriggers];
  end;

  Report.Lines.Add('Starting Backup');
  if IBBackupService1.BackupFileLocation = flClientSide then
    bakfile := TFileStream.Create(IBBackupService1.BackupFile[0],fmCreate);
  try
    IBBackupService1.ServiceStart;
    while not IBBackupService1.Eof do
    begin
      case IBBackupService1.BackupFileLocation of
      flServerSide:
        Report.Lines.Add(IBBackupService1.GetNextLine);
      flClientSide:
        IBBackupService1.WriteNextChunk(bakfile);
      end;
      Application.ProcessMessages;
    end;
    if bakfile <> nil then
      BackupCount := bakfile.Size;
  finally
    if bakfile <> nil then
      bakfile.Free;
  end;

  while IBBackupService1.IsServiceRunning do; {flush}

  {Report completion}
  case IBBackupService1.BackupFileLocation of
  flServerSide:
    begin
      Report.Lines.Add('Backup Completed');
      MessageDlg('Backup Completed',mtInformation,[mbOK],0);
    end;
  flClientSide:
    begin
      Report.Lines.Add(Format('Backup Completed - File Size = %d bytes',[BackupCount]));
      MessageDlg(Format('Backup Completed - File Size = %d bytes',[BackupCount]),mtInformation,[mbOK],0);
    end;
  end;
  IBBackupService1.Active := false;
end;

procedure TBackupDlg.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := SelectTab;
  Edit1.Text := IBBackupService1.ServerName;
  if IBBackupService1.BackupFileLocation = flServerSide then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  Edit2.Text := IBBackupService1.DatabaseName;
  IBBackupService1.BackupFile.Clear;
end;

procedure TBackupDlg.ReportTabShow(Sender: TObject);
begin
  Report.Lines.Clear;
end;

procedure TBackupDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if Edit3.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
    IBBackupService1.BackupFile.Add(Edit3.Text);
    if RadioButton1.Checked then
       IBBackupService1.BackupFileLocation := flServerSide
    else
      IBBackupService1.BackupFileLocation := flClientSide;
    PageControl1.ActivePage := ReportTab;
    Application.QueueAsyncCall(@DoBackup,0);
  end;
end;

end.

