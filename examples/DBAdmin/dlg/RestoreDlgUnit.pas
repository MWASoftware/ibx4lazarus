unit RestoreDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, IBServices;

type

  { TRestoreDlg }

  TRestoreDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    IBRestoreService1: TIBRestoreService;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Report: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpeedButton1: TSpeedButton;
    SelectTab: TTabSheet;
    ReportTab: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ReportTabShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoRestore(Data: PtrInt);
  public
    { public declarations }
     function ShowModal(aService: TIBCustomService; DBName: string): TModalResult;
 end;

var
  RestoreDlg: TRestoreDlg;

implementation

{$R *.lfm}

uses MainFormUnit;

{ TRestoreDlg }

procedure TRestoreDlg.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit3.Text := OpenDialog1.Filename;
end;

procedure TRestoreDlg.DoRestore(Data: PtrInt);
var bakfile: TFileStream;
    line: string;
begin
  bakfile := nil;
  if IBRestoreService1.BackupFileLocation = flClientSide then
    bakfile := TFileStream.Create(IBRestoreService1.BackupFile[0],fmOpenRead);
  Report.Lines.Add('Restore Started');
  try
    IBRestoreService1.ServiceStart;
    while not IBRestoreService1.Eof do
    begin
      case IBRestoreService1.BackupFileLocation of
      flServerSide:
        Report.Lines.Add(Trim(IBRestoreService1.GetNextLine));
      flClientSide:
        begin
          IBRestoreService1.SendNextChunk(bakfile,line);
          if line <> '' then
           Report.Lines.Add(line);
        end;
      end;
      Application.ProcessMessages
    end;
  finally
    if bakfile <> nil then
      bakfile.Free;
  end;
  while IBRestoreService1.IsServiceRunning do; {flush}

  Report.Lines.Add('Restore Completed');
  MessageDlg('Restore Completed',mtInformation,[mbOK],0);
end;

function TRestoreDlg.ShowModal(aService: TIBCustomService; DBName: string
  ): TModalResult;
begin
  IBRestoreService1.Assign(aService);
  IBRestoreService1.DatabaseName.Clear;
  IBRestoreService1.DatabaseName.Add(DBName);
  Result := inherited ShowModal;
end;

procedure TRestoreDlg.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := SelectTab;
  Edit1.Text := IBRestoreService1.ServerName;
  if IBRestoreService1.BackupFileLocation = flServerSide then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  Edit2.Text := IBRestoreService1.DatabaseName[0];
  IBRestoreService1.BackupFile.Clear;
end;

procedure TRestoreDlg.ReportTabShow(Sender: TObject);
begin
  Report.Lines.Clear;
end;

procedure TRestoreDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if Edit3.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
    IBRestoreService1.BackupFile.Add(Edit3.Text);
    if RadioButton1.Checked then
       IBRestoreService1.BackupFileLocation := flServerSide
    else
      IBRestoreService1.BackupFileLocation := flClientSide;
    PageControl1.ActivePage := ReportTab;
    Application.QueueAsyncCall(@DoRestore,0);
  end;

end;

end.

