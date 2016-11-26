unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBServices, Unit2, Unit3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    IBServerProperties1: TIBServerProperties;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure DoBackup(Data: PtrInt);
    procedure DoRestore(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Form3.IBRestoreService1.DatabaseName.Clear;
  Form3.IBRestoreService1.DatabaseName.Add(GetTempDir + 'mytest.fdb');
  with IBServerProperties1 do
  begin
    Active := true;
    FetchVersionInfo;
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
  end;
end;

procedure TForm1.DoBackup(Data: PtrInt);
var bakfile: TFileStream;
begin
  bakfile := nil;
  with Form2 do
  begin
    IBBackupService1.ServiceIntf := IBServerProperties1.ServiceIntf;
    IBBackupService1.Active := true;
    Memo1.Lines.Add('Starting Backup');
    IBBackupService1.ServiceStart;
    try
      if IBBackupService1.BackupFileLocation = flClientSide then
        bakfile := TFileStream.Create(IBBackupService1.BackupFile[0],fmCreate);
      while not IBBackupService1.Eof do
      begin
        case IBBackupService1.BackupFileLocation of
        flServerSide:
          Memo1.Lines.Add(IBBackupService1.GetNextLine);
        flClientSide:
          IBBackupService1.WriteNextChunk(bakfile);
        end;
        Application.ProcessMessages
      end;
    finally
      if bakfile <> nil then
        bakfile.Free;
    end;
    Memo1.Lines.Add('Backup Completed');
    MessageDlg('Backup Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TForm1.DoRestore(Data: PtrInt);
var bakfile: TFileStream;
    line: string;
begin
  bakfile := nil;
  with Form3 do
  begin
    IBRestoreService1.ServiceIntf := IBServerProperties1.ServiceIntf;
    IBRestoreService1.Active := true;
    IBRestoreService1.ServiceStart;
    Memo1.Lines.Add('Restore Started');
    try
      if IBRestoreService1.BackupFileLocation = flClientSide then
        bakfile := TFileStream.Create(IBRestoreService1.BackupFile[0],fmOpenRead);
      while not IBRestoreService1.Eof do
      begin
        case IBRestoreService1.BackupFileLocation of
        flServerSide:
          Memo1.Lines.Add(Trim(IBRestoreService1.GetNextLine));
        flClientSide:
          begin
            IBRestoreService1.SendNextChunk(bakfile,line);
            if line <> '' then
              Memo1.Lines.Add(line);
          end;
        end;
        Application.ProcessMessages
      end;
    finally
      if bakfile <> nil then
        bakfile.Free;
    end;
    Memo1.Lines.Add('Restore Completed');
    MessageDlg('Restore Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form2.IBBackupService1.ServerName := IBServerProperties1.ServerName;
  if Form2.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoBackup,0);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Form3.IBRestoreService1.ServerName := IBServerProperties1.ServerName;
  if Form3.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoRestore,0);
end;

end.

