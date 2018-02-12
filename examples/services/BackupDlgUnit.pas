unit BackupDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, IBServices;

type

  { TBackupDlg }

  TBackupDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    IBBackupService1: TIBBackupService;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
     procedure RunBackup;
 end;

var
  BackupDlg: TBackupDlg;

implementation

{$R *.lfm}

uses MainFormUnit;

{ TBackupDlg }

procedure TBackupDlg.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Edit3.Text := SaveDialog1.Filename;
end;

procedure TBackupDlg.RunBackup;
var bakfile: TFileStream;
    BackupCount: integer;
begin
  bakfile := nil;
  MainForm.Memo1.Lines.Add('Starting Backup');
  if IBBackupService1.BackupFileLocation = flClientSide then
    bakfile := TFileStream.Create(IBBackupService1.BackupFile[0],fmCreate);
  try
    IBBackupService1.ServiceStart;
    while not IBBackupService1.Eof do
    begin
      case IBBackupService1.BackupFileLocation of
      flServerSide:
        MainForm.Memo1.Lines.Add(IBBackupService1.GetNextLine);
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
      MainForm.Memo1.Lines.Add('Backup Completed');
      MessageDlg('Backup Completed',mtInformation,[mbOK],0);
    end;
  flClientSide:
    begin
      MainForm.Memo1.Lines.Add(Format('Backup Completed - File Size = %d bytes',[BackupCount]));
      MessageDlg(Format('Backup Completed - File Size = %d bytes',[BackupCount]),mtInformation,[mbOK],0);
    end;
  end;
end;

procedure TBackupDlg.FormShow(Sender: TObject);
begin
  Edit1.Text := IBBackupService1.ServerName;
  if IBBackupService1.BackupFileLocation = flServerSide then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  Edit2.Text := IBBackupService1.DatabaseName;
  IBBackupService1.BackupFile.Clear;
end;

procedure TBackupDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;
  if Edit2.Text = '' then
    raise Exception.Create('A Database Name must be given');
  if Edit3.Text = '' then
    raise Exception.Create('A Backup File Name must be given');
  IBBackupService1.DatabaseName := Edit2.Text;
  IBBackupService1.BackupFile.Add(Edit3.Text);
  if RadioButton1.Checked then
     IBBackupService1.BackupFileLocation := flServerSide
  else
    IBBackupService1.BackupFileLocation := flClientSide;
end;

end.

