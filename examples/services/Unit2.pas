unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, IBServices;

type

  { TForm2 }

  TForm2 = class(TForm)
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
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Edit3.Text := SaveDialog1.Filename;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Edit1.Text := IBBackupService1.ServerName;
  if IBBackupService1.BackupFileLocation = flServerSide then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  Edit2.Text := IBBackupService1.DatabaseName;
  IBBackupService1.BackupFile.Clear;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

