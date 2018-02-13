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
    function ShowModal(aService: TIBCustomService; DBName: string): TModalResult;
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

procedure TBackupDlg.DoBackup(Data: PtrInt);
var bakfile: TFileStream;
    BackupCount: integer;
begin
  bakfile := nil;
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

function TBackupDlg.ShowModal(aService: TIBCustomService; DBName: string
  ): TModalResult;
begin
  IBBackupService1.Assign(aService);
  IBBackupService1.DatabaseName := DBName;
  Result := inherited ShowModal;
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

