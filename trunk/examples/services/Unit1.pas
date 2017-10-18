unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBServices, IB, Unit2, Unit3,  ListUsersUnit, LimboTransactionsUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    IBLogService1: TIBLogService;
    IBServerProperties1: TIBServerProperties;
    IBStatisticalService1: TIBStatisticalService;
    IBValidationService1: TIBValidationService;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
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
var i: integer;
begin
  Form3.IBRestoreService1.DatabaseName.Clear;
  Form3.IBRestoreService1.DatabaseName.Add(GetTempDir + 'mytest.fdb');
  with IBServerProperties1 do
  begin
    repeat
      try
        Active := true;
      except
       on E:EIBClientError do
        begin
          Close;
          Exit
        end;
       On E:Exception do
         MessageDlg(E.Message,mtError,[mbOK],0);
      end;
    until Active; {Loop until logged in or user cancels}
    FetchVersionInfo;
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    FetchDatabaseInfo;
    Memo1.Lines.Add('No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    Memo1.Lines.Add('No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      Memo1.Lines.Add('DB Name = ' + DatabaseInfo.DbName[i]);
    FetchConfigParams;
    Memo1.Lines.Add('Base Location = ' + ConfigParams.BaseLocation);
    Memo1.Lines.Add('Lock File Location = ' + ConfigParams.LockFileLocation);
    Memo1.Lines.Add('Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
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
    if IBRestoreService1.IsServiceRunning then
      Exception.Create('A Service is still running');
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
  if Form2.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoBackup,0);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Form3.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoRestore,0);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Lines.Add('Server Log');
  IBLogService1.ServiceIntf := IBServerProperties1.ServiceIntf;
  with IBLogService1 do
  begin
    Active := true;
    ServiceStart;
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var DBName: string;
begin
  DBName := IBStatisticalService1.DatabaseName;
  if InputQuery('Select Database','Enter Database Name on ' + IBStatisticalService1.ServerName,
         DBName) then
  begin
    IBStatisticalService1.DatabaseName := DBName;
    Memo1.Lines.Add('Database Statistics for ' + IBStatisticalService1.DatabaseName);
    IBStatisticalService1.ServiceIntf := IBServerProperties1.ServiceIntf;
    with IBStatisticalService1 do
    begin
      Active := true;
      ServiceStart;
      while not Eof do
      begin
        Memo1.Lines.Add(GetNextLine);
        Application.ProcessMessages;
      end;
    end;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ListUsersForm.IBSecurityService1.ServiceIntf := IBServerProperties1.ServiceIntf;
  ListUsersForm.ShowModal;
end;

procedure TForm1.Button7Click(Sender: TObject);
var DBName: string;
begin
  DBName := IBValidationService1.DatabaseName;
  if InputQuery('Select Database','Enter Database Name on ' + IBValidationService1.ServerName,
         DBName) then
  begin
    IBValidationService1.DatabaseName := DBName;
    Memo1.Lines.Add('Database Validation for ' + IBValidationService1.DatabaseName);
    Memo1.Lines.Add('Running...');
    IBValidationService1.ServiceIntf := IBServerProperties1.ServiceIntf;
    Application.ProcessMessages;
    with IBValidationService1 do
    begin
      Active := true;
      ServiceStart;
      while not Eof do
      begin
        Memo1.Lines.Add(GetNextLine);
        Application.ProcessMessages;
      end;
    end;
    Memo1.Lines.Add('Validation Completed');
    MessageDlg('Validation Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var DBName: string;
begin
  with LimboTransactionsForm do
  begin
    DBName := IBValidationService1.DatabaseName;
    if InputQuery('Select Database','Enter Database Name on ' + IBValidationService1.ServerName,
           DBName) then
    begin
      IBValidationService1.ServiceIntf := IBServerProperties1.ServiceIntf;
      IBValidationService1.DatabaseName := DBName;
      ShowModal;
    end;
  end;
end;

end.

