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
    DeActivateIndexes: TCheckBox;
    PageBuffers: TEdit;
    Label6: TLabel;
    NoDBTriggersOnRestore: TCheckBox;
    NoShadow: TCheckBox;
    NoValidityCheck: TCheckBox;
    OneRelationAtATime: TCheckBox;
    RestoreMetaDataOnly: TCheckBox;
    UseAllSpace: TCheckBox;
    Edit1: TEdit;
    DBName: TEdit;
    SourceArchive: TEdit;
    PageSize: TEdit;
    IBRestoreService1: TIBRestoreService;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
     function ShowModal(DefaultPageSize, DefaultNumBuffers: integer): TModalResult;
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
    SourceArchive.Text := OpenDialog1.Filename;
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
  IBRestoreService1.ACtive := false;

  Report.Lines.Add('Restore Completed');
  MessageDlg('Restore Completed',mtInformation,[mbOK],0);
end;

function TRestoreDlg.ShowModal(DefaultPageSize, DefaultNumBuffers: integer
  ): TModalResult;
begin
  PageSize.Text := IntToStr(DefaultPageSize);
  PageBuffers.Text := IntToStr(DefaultNumBuffers);
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
  DBName.Text := IBRestoreService1.DatabaseName[0];
  IBRestoreService1.BackupFile.Clear;
  SourceArchive.SetFocus;
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
    if SourceArchive.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
    with IBRestoreService1 do
    begin
      if RadioButton1.Checked then
         BackupFileLocation := flServerSide
      else
        BackupFileLocation := flClientSide;
      BackupFile.Add(SourceArchive.Text);
      PageSize := StrToInt(self.PageSize.Text);
      PageBuffers := StrToInt(self.PageBuffers.Text);
      Options := [Replace];
      if DeactivateIndexes.Checked then
        Options := Options + [IBServices.DeactivateIndexes];
      if NoDBTriggersOnRestore.Checked then
        Options := Options + [IBServices.NoDBTriggersOnRestore];
      if NoShadow.Checked then
        Options := Options + [IBServices.NoShadow];
      if NoValidityCheck.Checked then
        Options := Options + [IBServices.NoValidityCheck];
      if OneRelationAtATime.Checked then
        Options := Options + [IBServices.OneRelationAtATime];
      if RestoreMetaDataOnly.Checked then
        Options := Options + [IBServices.RestoreMetaDataOnly];
      if UseAllSpace.Checked then
        Options := Options + [IBServices.UseAllSpace];
    end;

    PageControl1.ActivePage := ReportTab;
    Application.QueueAsyncCall(@DoRestore,0);
  end;

end;

end.

