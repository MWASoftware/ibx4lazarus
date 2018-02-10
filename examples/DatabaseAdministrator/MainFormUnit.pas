unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Menus, IBServices, IB;

type

  TRunServiceProc = procedure of object;

  { TForm1 }

  TForm1 = class(TForm)
    LimboTransactions: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    Validate: TAction;
    Statistics: TAction;
    Properties: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
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
    procedure FormShow(Sender: TObject);
    procedure IBServerProperties1Login(Service: TIBCustomService;
      LoginParams: TStrings);
    procedure IBStatisticalService1Login(Service: TIBCustomService;
      LoginParams: TStrings);
    procedure LimboTransactionsExecute(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure StatisticsExecute(Sender: TObject);
    procedure ValidateExecute(Sender: TObject);
  private
    { private declarations }
    FValidationService: TIBControlAndQueryService;
    FDBName: string;
    FIsExpectedDB: boolean;
    FServerPassword: string;
    function RunService(aService: TIBCustomService; secDB: integer;
      RunProc: TRunServiceProc): integer;
    procedure RunBackup;
    procedure RunRestore;
    procedure RunShowStatistics;
    procedure RunValidation;
    procedure RunLimboTransactions;
    procedure DoBackup(secDB: PtrInt);
    procedure DoRestore(secDB: PtrInt);
    procedure DoShowStatistics(secDB: PtrInt);
    procedure DoValidation(secDB: PtrInt);
    procedure DoLimboTransactions(secDB: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses IBErrorCodes, FBMessages, ServicesLoginDlgUnit, SelectValidationDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit, SelectDBDlgUnit,
  DatabasePropertiesUnit, AltDBSvcLoginDlgUnit;

const
  use_global_login     = 0; {Login to backup/restore with global sec db}
  use_alt_sec_db_login = 1; {Login to backup/restore with alt sec db}
  login_failed         = -1;
  login_successful     = -2;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var i: integer;
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  RestoreDlg.IBRestoreService1.DatabaseName.Clear;
  RestoreDlg.IBRestoreService1.DatabaseName.Add(GetTempDir + 'mytest.fdb');
  FDBName := IBStatisticalService1.DatabaseName;
  with IBServerProperties1 do
  begin
    while not Active do
    begin
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

    {Display the server properties}
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

    if ServerVersionNo[1] < 3 then
    begin
      {Hide References to Alt.Sec. Database if Firebird 2.x or earlier}
      SelectDBDlg.UseAltSecDB.Visible := false;
      SelectValidationDlg.UseAltSecDB.Visible := false;
      RestoreDlg.UseAltSecDB.Visible := false;
      BackupDlg.UseAltSecDB.Visible := false;
    end;
  end;
  {Leave IBServerProperties1 as active and use this as the common service interface}
end;

{This is the initial logon to the default security database on the server}

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
    FServerPassword := aPassword; {Remember this for database login}
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

{This is the login dialog for a alt. security database}

procedure TForm1.IBStatisticalService1Login(Service: TIBCustomService;
  LoginParams: TStrings);
var aServiceName: string;
    aUserName: string;
    aPassword: string;
begin
  aServiceName := Service.ServerName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if AltDBSvcLoginDlg.ShowModal(aServiceName, aUserName, aPassword) = mrOK then
  begin
    Service.ServerName := aServiceName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TForm1.LimboTransactionsExecute(Sender: TObject);
begin
  with LimboTransactionsForm do
  begin
    if SelectDBDlg.ShowModal(FDBName,FIsExpectedDB) = mrOK then
    begin
      LimboTransactionValidation.DatabaseName := FDBName;
      if FIsExpectedDB then
        Application.QueueAsyncCall(@DoLimboTransactions,use_alt_sec_db_login)
      else
        Application.QueueAsyncCall(@DoLimboTransactions,use_global_login);
    end;
  end;
end;

procedure TForm1.PropertiesExecute(Sender: TObject);
begin
  if SelectDBDlg.ShowModal(FDBName,FIsExpectedDB) = mrOK then
  begin
    if FIsExpectedDB then
      DatabaseProperties.ShowModal(IBServerProperties1,FDBName,'')
    else
      DatabaseProperties.ShowModal(IBServerProperties1,FDBName,FServerPassword);
  end;
end;

procedure TForm1.StatisticsExecute(Sender: TObject);
begin
  if SelectDBDlg.ShowModal(FDBName,FIsExpectedDB) = mrOK then
  begin
    IBStatisticalService1.DatabaseName := FDBName;
    if FIsExpectedDB then
      Application.QueueAsyncCall(@DoShowStatistics,use_alt_sec_db_login)
    else
      Application.QueueAsyncCall(@DoShowStatistics,use_global_login);
  end;
end;

procedure TForm1.ValidateExecute(Sender: TObject);
var UseOnlineValidation: boolean;
begin
  UseOnlineValidation := false;
  if SelectValidationDlg.ShowModal(IBServerProperties1.ServerName,FDBName,UseOnlineValidation,FIsExpectedDB) = mrOK then
  begin
    if UseOnlineValidation then
    begin
      FValidationService := IBOnlineValidationService1;
      IBOnlineValidationService1.DatabaseName := FDBName;
    end
    else
    begin
      FValidationService :=  IBValidationService1;
      IBValidationService1.DatabaseName := FDBName;
    end;
    Memo1.Lines.Add('Database Validation for ' + FDBName);
    if FIsExpectedDB then
      Application.QueueAsyncCall(@DoValidation,use_alt_sec_db_login)
    else
      Application.QueueAsyncCall(@DoValidation,use_global_login);
  end;
end;

{Common code for launching a service that might need to use and alt. security database}

function TForm1.RunService(aService: TIBCustomService; secDB: integer;
  RunProc: TRunServiceProc): integer;
var index: integer;
begin
  Result := login_failed;
  with aService do
  begin
    if not active then
      Assign(IBServerProperties1);
    if secDB = use_alt_sec_db_login then
    begin
      ServiceIntf := nil;
      LoginPrompt := true;
      Params.Add('expected_db='+FDBName);
      index := Params.IndexOfName('password');
      if index <> -1 then
        Params.Delete(index);
    end;

    {Now make sure we are logged in}

    while not Active do
    begin
      try
        Active := true;
      except
       on E:EIBClientError do
          Exit;
       On E:Exception do
         MessageDlg(E.Message,mtError,[mbOK],0);
      end;
    end; {Loop until logged in or user cancels}

    {Now run the service and see if we need to log in under an alt. Sec. Database}

    try
      try
        RunProc;
        Result := login_successful;
      except
         on E: EIBInterBaseError do
           if (E.IBErrorCode = isc_sec_context) and (secDB = use_global_login) then {Need expected_db}
             Result := use_alt_sec_db_login
           else
             raise;
      end;
    finally
      aService.Active := false;
    end;
  end;
end;

procedure TForm1.RunBackup;
var bakfile: TFileStream;
    BackupCount: integer;
begin
  bakfile := nil;
  with BackupDlg do
  begin
    Memo1.Lines.Add('Starting Backup');
    if IBBackupService1.BackupFileLocation = flClientSide then
      bakfile := TFileStream.Create(IBBackupService1.BackupFile[0],fmCreate);
    try
      IBBackupService1.ServiceStart;
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

    {Report completion}
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
  end;
end;

procedure TForm1.RunRestore;
var bakfile: TFileStream;
    line: string;
begin
  bakfile := nil;
  with RestoreDlg do
  begin
    if IBRestoreService1.BackupFileLocation = flClientSide then
      bakfile := TFileStream.Create(IBRestoreService1.BackupFile[0],fmOpenRead);
    Memo1.Lines.Add('Restore Started');
    try
      IBRestoreService1.ServiceStart;
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

procedure TForm1.RunShowStatistics;
begin
  with IBStatisticalService1 do
  try
    ServiceStart;
    Memo1.Lines.Add('Database Statistics for ' + IBStatisticalService1.DatabaseName);
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
  except on E:Exception do
    MessageDlg(E.Message,mtError,[mbOK],0);
  end;
end;

procedure TForm1.RunValidation;
begin
  with FValidationService do
  TRY
    ServiceStart;
    Memo1.Lines.Add('Running...');
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
    Memo1.Lines.Add('Validation Completed');
    MessageDlg('Validation Completed',mtInformation,[mbOK],0);
  except on E:Exception do
    MessageDlg(E.Message,mtError,[mbOK],0);
  end;
end;

procedure TForm1.RunLimboTransactions;
begin
  with LimboTransactionsForm do
  try
    {test access credentials}
    LimboTransactionValidation.ServiceStart;
    LimboTransactionValidation.FetchLimboTransactionInfo;
    ShowModal;
  except on E:Exception do
    MessageDlg(E.Message,mtError,[mbOK],0);
  end;
end;

procedure TForm1.DoBackup(secDB: PtrInt);
begin
  with BackupDlg do
    if RunService(IBBackupService1,secDB,@RunBackup) = use_alt_sec_db_login then
       RunService(IBBackupService1,use_alt_sec_db_login,@RunBackup)
end;

procedure TForm1.DoRestore(secDB: PtrInt);
begin
  with RestoreDlg do
    if RunService(IBRestoreService1,secDB,@RunRestore) = use_alt_sec_db_login then
       RunService(IBRestoreService1,use_alt_sec_db_login,@RunRestore)
end;

procedure TForm1.DoShowStatistics(secDB: PtrInt);
begin
  if RunService(IBStatisticalService1,secDB,@RunShowStatistics) = use_alt_sec_db_login then
     RunService(IBStatisticalService1,use_alt_sec_db_login,@RunShowStatistics)
end;

procedure TForm1.DoValidation(secDB: PtrInt);
begin
  if RunService(FValidationService,secDB,@RunValidation) = use_alt_sec_db_login then
     RunService(FValidationService,use_alt_sec_db_login,@RunValidation)
end;

procedure TForm1.DoLimboTransactions(secDB: PtrInt);
begin
  with LimboTransactionsForm do
  if RunService(LimboTransactionValidation,secDB,@RunLimboTransactions) = use_alt_sec_db_login then
     RunService(LimboTransactionValidation,use_alt_sec_db_login,@RunLimboTransactions)
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BackupDlg.IBBackupService1.DatabaseName := FDBName;
  BackupDlg.IBBackupService1.Assign(IBServerProperties1);
  BackupDlg.UseAltSecDB.Checked := FIsExpectedDB;
  if BackupDlg.ShowModal = mrOK then
  begin
    FIsExpectedDB := BackupDlg.UseAltSecDB.Checked;
    FDBName := BackupDlg.IBBackupService1.DatabaseName;
    if FIsExpectedDB then
      Application.QueueAsyncCall(@DoBackup,use_alt_sec_db_login)
    else
      Application.QueueAsyncCall(@DoBackup,use_global_login);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RestoreDlg.IBRestoreService1.DatabaseName[0] := FDBName;
  RestoreDlg.IBRestoreService1.Assign(IBServerProperties1);
  RestoreDlg.UseAltSecDB.Checked := FIsExpectedDB;
  if RestoreDlg.ShowModal = mrOK then
  begin
    FIsExpectedDB := RestoreDlg.UseAltSecDB.Checked;
    FDBName := RestoreDlg.IBRestoreService1.DatabaseName[0];
    if FIsExpectedDB then
      Application.QueueAsyncCall(@DoRestore,use_alt_sec_db_login)
    else
      Application.QueueAsyncCall(@DoRestore,use_global_login);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Lines.Add('Server Log');
  {No chance that we will need an alt. security database - so just assign it the
   server connection}
  IBLogService1.Assign(IBServerProperties1);
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
begin
  PopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  with ListUsersForm do
  begin
    {No chance that we will need an alt. security database - so just assign it the
     server connection}
    IBSecurityService1.Assign(IBServerProperties1);
    ShowModal;
  end;
end;

end.

