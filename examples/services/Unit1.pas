unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBServices, IB;

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
    IBOnlineValidationService1: TIBOnlineValidationService;
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
    procedure IBServerProperties1Login(Service: TIBCustomService;
      LoginParams: TStrings);
  private
    { private declarations }
    FUseOnlineValidation: boolean;
    procedure DoBackup(secDB: PtrInt);
    procedure DoRestore(secDB: PtrInt);
    procedure DoShowStatistics(secDB: PtrInt);
    procedure DoValidation(secDB: PtrInt);
    procedure DoLimboTransactions(secDB: PtrInt);
    procedure DoListUsers(secDB: PtrInt);
    procedure AttachService(aService: TIBCustomService; Initialise:boolean = false); overload;
    procedure AttachService(aService: TIBCustomService; secDB: integer;
      aDatabaseName: string); overload;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses IBErrorCodes, FBMessages, ServicesLoginDlgUnit, SelectValidationDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit;

const
  use_global_login     = 0; {Login to backup/restore with global sec db}
  use_alt_sec_db_login = 1; {Login to backup/restore with alt sec db}

resourcestring
  sLoginAgain = 'This database appears to use an alternative security database. '+
                'You must now log into the alternative security database using ' +
                'login credentials for the alternative security database';

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var i: integer;
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  RestoreDlg.IBRestoreService1.DatabaseName.Clear;
  RestoreDlg.IBRestoreService1.DatabaseName.Add(GetTempDir + 'mytest.fdb');
  AttachService(IBServerProperties1);
  with IBServerProperties1 do
  begin
    FetchVersionInfo;
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    Memo1.Lines.Add(Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
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

procedure TForm1.IBServerProperties1Login(Service: TIBCustomService;
  LoginParams: TStrings);
var aServiceName: string;
    aUserName: string;
    aPassword: string;
begin
  aServiceName := Service.ServerName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if SvcLoginDlg.ShowModal(aServiceName, aUserName, aPassword) = mrOK then
  begin
    Service.ServerName := aServiceName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TForm1.DoBackup(secDB: PtrInt);
var bakfile: TFileStream;
    NeedsExpectedDB: boolean;
    BackupCount: integer;
begin
  bakfile := nil;
  with BackupDlg do
  try
    AttachService(IBBackupService1,secDB,IBBackupService1.DatabaseName);

    NeedsExpectedDB := false;
    try
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
          Application.ProcessMessages;
        end;
        if bakfile <> nil then
          BackupCount := bakfile.Size;
      finally
        if bakfile <> nil then
          bakfile.Free;
      end;
      case IBBackupService1.BackupFileLocation of
      flServerSide:
        begin
          Memo1.Lines.Add('Backup Completed');
          MessageDlg('Backup Completed',mtInformation,[mbOK],0);
        end;
      flClientSide:
        begin
          Memo1.Lines.Add(Format('Backup Completed - File Size = %d bytes',[BackupCount]));
          MessageDlg(Format('Backup Completed - File Size = %d bytes',[BackupCount]),mtInformation,[mbOK],0);
        end;
      end;
    except
     on E: EIBInterBaseError do
       if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
         NeedsExpectedDB := true
       else
         raise;
    end;
  finally
    IBBackupService1.Active := false;
    if NeedsExpectedDB then {Need expected_db}
    begin
      MessageDlg(sLoginAgain,mtInformation,[mbOK],0);
      Application.QueueAsyncCall(@DoBackup,use_alt_sec_db_login);
    end;
  end;
end;

procedure TForm1.DoRestore(secDB: PtrInt);
var bakfile: TFileStream;
    line: string;
    NeedsExpectedDB: boolean;
begin
  bakfile := nil;
  with RestoreDlg do
  try
    AttachService(IBRestoreService1,secDB,IBRestoreService1.DatabaseName[0]);

    NeedsExpectedDB := false;
    try
      IBRestoreService1.ServiceStart;
      Memo1.Lines.Add('Restore Started');
      if IBRestoreService1.BackupFileLocation = flClientSide then
        bakfile := TFileStream.Create(IBRestoreService1.BackupFile[0],fmOpenRead);
      try
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
    except
         on E: EIBInterBaseError do
           if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
             NeedsExpectedDB := true
           else
             raise;
      end;
  finally
    IBRestoreService1.Active := false;
    if NeedsExpectedDB then {Need expected_db}
    begin
      MessageDlg(sLoginAgain,mtInformation,[mbOK],0);
      Application.QueueAsyncCall(@DoRestore,use_alt_sec_db_login);
    end;
  end;

end;

procedure TForm1.DoShowStatistics(secDB: PtrInt);
var NeedsExpectedDB: boolean;
begin
  AttachService(IBStatisticalService1,secDB,IBStatisticalService1.DatabaseName);
  NeedsExpectedDB := false;
  with IBStatisticalService1 do
  try
    try
      ServiceStart;
      Memo1.Lines.Add('Database Statistics for ' + IBStatisticalService1.DatabaseName);
      while not Eof do
      begin
        Memo1.Lines.Add(GetNextLine);
        Application.ProcessMessages;
      end;
    except
       on E: EIBInterBaseError do
         if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
         begin
           NeedsExpectedDB := true;
           Exit;
         end;
    end;
  finally
    Active := false;
    if NeedsExpectedDB then {Need expected_db}
    begin
      MessageDlg(sLoginAgain,mtInformation,[mbOK],0);
      Application.QueueAsyncCall(@DoShowStatistics,use_alt_sec_db_login);
    end;
  end;
end;

procedure TForm1.DoValidation(secDB: PtrInt);
var NeedsExpectedDB: boolean;
    aValidationService: TIBControlAndQueryService;
    DBName: string;
begin
  if FUseOnlineValidation then
  begin
    aValidationService := IBOnlineValidationService1;
    DBName := IBOnlineValidationService1.DatabaseName;
  end
  else
  begin
    aValidationService := IBValidationService1;
    DBName := IBValidationService1.DatabaseName;
  end;
  NeedsExpectedDB := false;
  AttachService(aValidationService,secDB,DBName);
  Application.ProcessMessages;
  with aValidationService do
  try
    Active := true;
    try
      ServiceStart;
      Memo1.Lines.Add('Running...');
      while not Eof do
      begin
        Memo1.Lines.Add(GetNextLine);
        Application.ProcessMessages;
      end;
    except
       on E: EIBInterBaseError do
         if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
         begin
           NeedsExpectedDB := true;
           Exit;
         end;
    end;
    Memo1.Lines.Add('Validation Completed');
    MessageDlg('Validation Completed',mtInformation,[mbOK],0);
  finally
    if NeedsExpectedDB then {Need expected_db}
    begin
      MessageDlg(sLoginAgain,mtInformation,[mbOK],0);
      Application.QueueAsyncCall(@DoValidation,use_alt_sec_db_login);
    end;
  end;
end;

procedure TForm1.DoLimboTransactions(secDB: PtrInt);
var NeedsExpectedDB: boolean;
begin
  with LimboTransactionsForm do
  try
    AttachService(LimboTransactionValidation,secDB,LimboTransactionValidation.DatabaseName);
    try
      {test access credentials}
      LimboTransactionValidation.ServiceStart;
      LimboTransactionValidation.FetchLimboTransactionInfo;
    except
       on E: EIBInterBaseError do
         if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
         begin
           NeedsExpectedDB := true;
           Exit;
         end;
    end;
    ShowModal;
  finally
    LimboTransactionValidation.Active := false;
    if NeedsExpectedDB then {Need expected_db}
    begin
      MessageDlg(sLoginAgain,mtInformation,[mbOK],0);
      Application.QueueAsyncCall(@DoLimboTransactions,use_alt_sec_db_login);
    end;
  end;
end;

procedure TForm1.DoListUsers(secDB: PtrInt);
begin
  with ListUsersForm do
  begin
    AttachService(IBSecurityService1,SecDB,'');
    ShowModal;
  end;
end;

procedure TForm1.AttachService(aService: TIBCustomService; Initialise: boolean);
begin
  with aService do
  while not Active do
  begin
    if Initialise then
    begin
      Assign(IBServerProperties1);
      LoginPrompt := Params.IndexOfName('password') = -1;
    end;
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
  end; {Loop until logged in or user cancels}
end;

procedure TForm1.AttachService(aService: TIBCustomService; secDB: integer; aDatabaseName: string
  );
var index: integer;
begin
  with aService do
  begin
    if not active then
      Assign(IBServerProperties1);
    index := Params.IndexOfName('password');
    if secDB = use_alt_sec_db_login then
    begin
      ServiceIntf := nil;
      LoginPrompt := true;
      Params.Add('expected_db='+aDatabaseName);
      Params.Delete(index);
    end
    else
      LoginPrompt := index = -1;
  end;
  AttachService(aService);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BackupDlg.IBBackupService1.ServerName := IBServerProperties1.ServerName;
  if BackupDlg.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoBackup,use_global_login);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RestoreDlg.IBRestoreService1.ServerName := IBServerProperties1.ServerName;
  if RestoreDlg.ShowModal = mrOK then
    Application.QueueAsyncCall(@DoRestore,use_global_login);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Lines.Add('Server Log');
  AttachService(IBLogService1,true);
  with IBLogService1 do
  begin
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
  if InputQuery('Select Database','Enter Database Name on ' + IBServerProperties1.ServerName,
         DBName) then
  begin
    IBStatisticalService1.DatabaseName := DBName;
    Application.QueueAsyncCall(@DoShowStatistics,use_global_login);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoListUsers,use_global_login);
end;

procedure TForm1.Button7Click(Sender: TObject);
var DBName: string;
begin
  DBName := IBValidationService1.DatabaseName;
  if SelectValidationDlg.ShowModal(IBServerProperties1.ServerName,DBName,FUseOnlineValidation) = mrOK then
  begin
    IBValidationService1.DatabaseName := DBName;
    Memo1.Lines.Add('Database Validation for ' + IBValidationService1.DatabaseName);
    Application.QueueAsyncCall(@DoValidation,use_global_login);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var DBName: string;
begin
  with LimboTransactionsForm do
  begin
    DBName := LimboTransactionValidation.DatabaseName;
    if InputQuery('Select Database','Enter Database Name on ' + IBServerProperties1.ServerName,
           DBName) then
    begin
      LimboTransactionValidation.DatabaseName := DBName;
      Application.QueueAsyncCall(@DoLimboTransactions,use_global_login);
    end;
  end;
end;

end.

