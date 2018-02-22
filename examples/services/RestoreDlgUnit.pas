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
  StdCtrls, Buttons, IBServices;

type

  { TRestoreDlg }

  TRestoreDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    IBRestoreService1: TIBRestoreService;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure RunRestore;
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

procedure TRestoreDlg.RunRestore;
var bakfile: TFileStream;
    line: string;
begin
  bakfile := nil;
  if IBRestoreService1.BackupFileLocation = flClientSide then
    bakfile := TFileStream.Create(IBRestoreService1.BackupFile[0],fmOpenRead);
  MainForm.Memo1.Lines.Add('Restore Started');
  try
    IBRestoreService1.ServiceStart;
    while not IBRestoreService1.Eof do
    begin
      case IBRestoreService1.BackupFileLocation of
      flServerSide:
        MainForm.Memo1.Lines.Add(Trim(IBRestoreService1.GetNextLine));
      flClientSide:
        begin
          IBRestoreService1.SendNextChunk(bakfile,line);
          if line <> '' then
           MainForm. Memo1.Lines.Add(line);
        end;
      end;
      Application.ProcessMessages
    end;
  finally
    if bakfile <> nil then
      bakfile.Free;
  end;
  while IBRestoreService1.IsServiceRunning do; {flush}

  MainForm.Memo1.Lines.Add('Restore Completed');
  MessageDlg('Restore Completed',mtInformation,[mbOK],0);
end;

procedure TRestoreDlg.FormShow(Sender: TObject);
begin
  Edit1.Text := IBRestoreService1.ServerName;
  if IBRestoreService1.BackupFileLocation = flServerSide then
    RadioButton1.Checked := true
  else
    RadioButton2.Checked := true;
  Edit2.Text := IBRestoreService1.DatabaseName[0];
  IBRestoreService1.BackupFile.Clear;
end;

procedure TRestoreDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;
  if Edit2.Text = '' then
    raise Exception.Create('A Database Name must be given');
  if Edit3.Text = '' then
    raise Exception.Create('A Backup File Name must be given');
  IBRestoreService1.DatabaseName.Clear;
  IBRestoreService1.DatabaseName.Add(Edit2.Text);
  IBRestoreService1.BackupFile.Add(Edit3.Text);
  if RadioButton1.Checked then
     IBRestoreService1.BackupFileLocation := flServerSide
  else
    IBRestoreService1.BackupFileLocation := flClientSide;
  if CheckBox1.Checked then
     IBRestoreService1.Options := IBRestoreService1.Options + [Replace] - [CreateNewDB]
  else
    IBRestoreService1.Options := IBRestoreService1.Options - [Replace] + [CreateNewDB]

end;

end.

