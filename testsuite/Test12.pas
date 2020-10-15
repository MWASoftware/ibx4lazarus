unit Test12;

{$mode objfpc}{$H+}

{Test 12 Test use of Services Connection}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB, IBXServices,
  IBDatabaseInfo, IBQuery, IBSQL, IBDatabase;

const
  aTestID    = '12';
  aTestTitle = 'Test use of Services Connection';

type

{ TTest12 }

  TTest12 = class(TIBXTestBase)
  private
    FIBConfigService: TIBXConfigService;
    FIBXServicesConnection: TIBXServicesConnection;
    FIBLogService: TIBXLogService;
    FIBOnlineValidationService: TIBXOnlineValidationService;
    FIBServerProperties: TIBXServerProperties;
    FIBStatisticalService: TIBXStatisticalService;
    FIBValidationService: TIBXValidationService;
    FIBXSecurityService: TIBXSecurityService;
    FUserList: TIBXServicesUserList;
    FBackupService: TIBXClientSideBackupService;
    FRestoreService: TIBXClientSideRestoreService;
    FSSBackupService: TIBXServerSideBackupService;
    FSSRestoreService: TIBXServerSideRestoreService;
    FLimboTransactionsList: TIBXServicesLimboTransactionsList;
    FLimboTransResolutionService: TIBXLimboTransactionResolutionService;
    FIBDatabaseInfo: TIBDatabaseInfo;
    FIBShadowDatabase: TIBDatabase;
    function IsDatabaseOnline (DBName: AnsiString): boolean;
    function IsShadowDatabase(DBName: AnsiString): boolean;
    procedure ShowServerProperties;
    procedure ShowStatistics;
    procedure ShowServerLog;
    procedure ValidateDatabase;
    procedure UserListHandling;
    procedure DBUpDown;
    procedure DatabaseSweepDB;
    procedure BackupRestore;
    procedure SSBackupRestore;
    procedure LimboTransactionResolution;
    procedure DatabasePropertiesTests;
    procedure ShowDatabaseProperties;
    procedure CreateShadow;
    procedure RemoveShadow;
    procedure ShowShadowFiles;
    procedure ActivateShadow;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils;

const First2 = 2;

{ TTest12 }

function TTest12.IsDatabaseOnline(DBName: AnsiString): boolean;
var Lines: TStringList;
    i: integer;
    line: string;
begin
  {Scan header page to see if database is online }
  Result := true;
  with TIBXStatisticalService.Create(nil) do
  try
    ServicesConnection := FIBXServicesConnection;
    DatabaseName := DBName;
    Options := [HeaderPages];
    Lines := TStringList.Create;
    try
      Execute(Lines);
      for i := 0 to Lines.Count - 1 do
      begin
        line := Lines[i];
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
         begin
           Result := false;
           break;
         end;

      end;
    finally
      Lines.Free;
    end
  finally
     Free
  end;
end;

function TTest12.IsShadowDatabase(DBName: AnsiString): boolean;
var Lines: TStringList;
    i: integer;
    line: string;
begin
  {Scan header page to see if database is a shadow}
  Result := false;
  with TIBXStatisticalService.Create(nil) do
  try
    ServicesConnection := FIBXServicesConnection;
    DatabaseName := DBName;
    Options := [HeaderPages];
    Lines := TStringList.Create;
    try
      Execute(Lines);
      for i := 0 to Lines.Count - 1 do
      begin
        line := Lines[i];
         if (Pos('Attributes',Line) <> 0)  and (Pos('shadow',Line) <> 0) then
         begin
           Result := true;
           break;
         end;

      end;
    finally
      Lines.Free;
    end
  finally
     Free
  end;
end;

procedure TTest12.ShowServerProperties;
var i: integer;
begin
  with FIBServerProperties, FIBXServicesConnection do
  begin
    writeln(OutFile,'Firebird Library PathName = ' + FirebirdAPI.GetFBLibrary.GetLibraryFilePath);
    writeln(OutFile,'Connect String = ',FIBXServicesConnection.ConnectString);
    writeln(OutFile,'Server Version = ' + VersionInfo.ServerVersion);
    writeln(OutFile,'Server Implementation = ' + VersionInfo.ServerImplementation);
    writeln(OutFile,'Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    writeln(OutFile,Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
    writeln(OutFile,'No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    writeln(OutFile,'No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      writeln(OutFile,'DB Name = ' + DatabaseInfo.DbName[i]);
    writeln(OutFile,'Base Location = ' + ConfigParams.BaseLocation);
    writeln(OutFile,'Lock File Location = ' + ConfigParams.LockFileLocation);
    writeln(OutFile,'Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
    writeln(OutFile,'Message File Location = ' + ConfigParams.MessageFileLocation);
    for i := Low(ConfigParams.ConfigFileParams) to High(ConfigParams.ConfigFileParams) do
      writeln(OutFile,ConfigParams.ConfigFileParams[i]);
    for i := Low(ConfigParams.ConfigFileData.ConfigFileKey) to High(ConfigParams.ConfigFileData.ConfigFileKey) do
      writeln(OutFile,Format('%d=%s',[ConfigParams.ConfigFileData.ConfigFileKey[i],ConfigParams.ConfigFileData.ConfigFileValue[i]]));
  end;
end;

procedure TTest12.ShowStatistics;
var S: TStringList;
begin
  writeln(OutFile,'Database Statistics for ' + FIBStatisticalService.DatabaseName);
  S := TStringList.Create;
  try
    FIBStatisticalService.Execute(S);
    WriteStrings(S);
  finally
    S.Free;
  end;
end;

procedure TTest12.ShowServerLog;
var S: TStringList;
begin
  writeln(OutFile,'Server Log');
  S := TStringList.Create;
  try
    FIBLogService.Execute(S);
    WriteStrings(S,First2);
  finally
    S.Free;
  end;
end;

procedure TTest12.ValidateDatabase;
var S: TStringList;
begin
  S := TStringList.Create;
  try
    writeln(OutFile,'Online Validation');
    FIBOnlineValidationService.Execute(S);
    WriteStrings(S);
    S.Clear;
    writeln(OutFile,'Normal Validation');
    FIBValidationService.Options := [ValidateFull];
    FIBValidationService.Execute(S);
    WriteStrings(S);
  finally
    S.Free;
  end;
  writeln(OutFile,'Validation Completed');
end;

procedure TTest12.UserListHandling;
begin
  writeln(Outfile,' Current User List');
  FUserList.Active := true;
  PrintDataSet(FUserList);
  writeln(Outfile,'Add user');
  with FUserList do
  begin
    Append;
    FieldByName('SEC$USER_NAME').AsString := 'Test12Tester';
    FieldByName('SEC$PASSWORD').AsString := 'LetMeIn';
    FieldByName('SEC$LAST_NAME').AsString := 'Tester';
    Post;
  end;
  writeln(Outfile,'Updated User List');
  PrintDataSet(FUserList);
  writeln(Outfile,'Close and re-open user list');
  FUserList.Active := false;
  FUserList.Active := true;
  PrintDataSet(FUserList);
  writeln(Outfile,'Modify the new user');
  if FUserList.Locate('SEC$USER_NAME','Test12Tester',[loCaseInsensitive]) then
  with FUserList do
  begin
    Edit;
    FieldByName('SEC$FIRST_NAME').AsString := 'The';
    Post;
    PrintDataSet(FUserList);
    writeln(Outfile,'Close and re-open user list');
    Active := false;
    Active := true;
    PrintDataSet(FUserList);
  end
  else
    writeln(Outfile,'Added user not found');
  writeln(Outfile,'Now delete the new user');
  if FUserList.Locate('SEC$USER_NAME','Test12Tester',[loCaseInsensitive]) then
    FUserList.Delete;
  FUserList.Active := false;
  FUserList.Active := true;
  writeln(Outfile,'Updated User List');
  PrintDataSet(FUserList);
  FUserList.Active := false;
end;

procedure TTest12.DBUpDown;
begin
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline(FIBConfigService.DatabaseName));
  FIBConfigService.ShutDownDatabase(Forced,0);
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline(FIBConfigService.DatabaseName));
  FIBConfigService.BringDatabaseOnline;
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline(FIBConfigService.DatabaseName));
end;

procedure TTest12.DatabaseSweepDB;
var S: TStringList;
begin
  writeln(OutFile,'Database Sweep');
  FIBValidationService.Options := [SweepDB];
  S := TStringList.Create;
  try
    FIBValidationService.Execute(S);
    WriteStrings(S);
  finally
    S.Free;
  end;
  writeln(OutFile,'Database Swept');
end;

procedure TTest12.BackupRestore;
var BackupCount: integer;
    S: TStringList;
begin
  writeln(OutFile);
  writeln(OutFile,'Starting Backup');
  FBackupService.BackupToFile(Owner.GetBackupFileName,BackupCount);
  writeln(OutFile,Format('Backup Completed - File Size = %d bytes',[BackupCount]));
  writeln(OutFile,'Restore Started');
  S := TStringList.Create;
  try
    FRestoreService.RestoreFromFile(Owner.GetBackupFileName,S);
    WriteStrings(S);
  finally
    S.Free;
  end;
  writeln(Outfile,'Restore Completed');
end;

procedure TTest12.SSBackupRestore;
var S: TStringList;
begin
  writeln(OutFile);
  writeln(OutFile,'Starting Server Side Backup');
  S := TStringList.Create;
  try
    FSSBackupService.BackupFiles.Add(Owner.GetBackupFileName);
    FSSBackupService.Verbose := true;
    FSSBackupService.Execute(S);
    WriteStrings(S);
    writeln(OutFile,'Backup Completed');
    writeln(OutFile,'Restore Started');
    FSSRestoreService.BackupFiles.Assign(FSSBackupService.BackupFiles);
    FSSRestoreService.Execute(S);
    WriteStrings(S);
  finally
    S.Free;
  end;
  writeln(Outfile,'Restore Completed');
end;

procedure TTest12.LimboTransactionResolution;
var S: TStrings;
begin
  writeln(Outfile,'Show Limbo Transactions');
  S := TStringList.Create;
  try
    FLimboTransactionsList.Active := true;
    PrintDataSet(FLimboTransactionsList);
    writeln(Outfile,'Call Fix Limbo transactions');
    FLimboTransactionsList.FixErrors(CommitGlobal,S);
    WriteStrings(S);
  finally
    S.Free;
  end;
end;

procedure TTest12.DatabasePropertiesTests;
begin
  writeln(Outfile,'Update properties');
  IBDatabase.Connected := false;
  with FIBConfigService do
  begin
    SetNoLinger;
    SetSweepInterval(10000);
    SetDBSqlDialect(1);
    SetPageBuffers(1024);
    SetAsyncMode(true);
    SetReserveSpace(false);
    SetReadOnly(true);
  end;
  IBDatabase.Connected := true;
  ShowDatabaseProperties;
end;

procedure TTest12.ShowDatabaseProperties;
var Linger,
    ForcedWrites,
    ReserveSpace: TField;
begin
  with FIBDatabaseInfo do
  begin
    writeln(Outfile,'Database Properties for ',DBFileName );
    writeln(OutFile,'ODS Minor Version = ' + IntToStr(ODSMinorVersion));
    writeln(OutFile,'ODS Major Version = ' + IntToStr(ODSMajorVersion));
    writeln(OutFile,'DB SQLDialect = ' + IntToStr(DBSQLDialect));
    writeln(OutFile,'Page Size = ' + IntToStr(PageSize));
    writeln(OutFile,'Number of Buffers = ' + IntToStr(NumBuffers));
    writeln(OutFile,'Version = ' + Version);
    ShowBoolValue(ForcedWrites,'Forced Writes Enabled','Forced Writes Disabled');
    writeln(OutFile,'Sweep Interval = ' + IntToStr(SweepInterval));
    ShowBoolValue(ReadOnly,'Database is Read Only','Database is Read/Write');
    writeln(Outfile,'Database Online = ',IsDatabaseOnline(DBFileName));
    writeln(Outfile,'Database is Shadow = ',IsShadowDatabase(DBFileName));
  end;
  with TIBQuery.Create(nil) do
  try
     Database := FIBDatabaseInfo.Database;
     Transaction := IBTransaction;
     Transaction.Active := true;
     SQL.Text := 'Select * From RDB$Database, MON$Database';
     Active := true;
     Linger := FindField('RDB$LINGER');
     if Linger <> nil then
     begin
       if Linger.IsNull then
         writeln(Outfile,'Linger = 0')
       else
         writeln(Outfile,'Linger = ',Linger.AsString);
     end
     else
       writeln(OutFile,'Linger not found');
     ForcedWrites := FindField('MON$FORCED_WRITES');
     if ForcedWrites <> nil then
       ShowBoolValue(ForcedWrites.AsInteger,'Database in Synchronous Mode','Database in Asynchronous Mode')
     else
       writeln(Outfile,'Sync State unknown');
     ReserveSpace := FieldByName('MON$RESERVE_SPACE');
     if ReserveSpace <> nil then
       writeln(Outfile,'Reserve Space = ',ReserveSpace.AsBoolean)
     else
       writeln(Outfile,'Reserve Space unknown');
  finally
    Free
  end;
end;

procedure TTest12.CreateShadow;
var ShadowFileName: AnsiString;
begin
  ShadowFileName :=  GetTempFileName;
  with TIBSQL.Create(nil) do
  try
    Database := IBDatabase;
    Transaction := IBTransaction;
    ReadWriteTransaction;
    SQL.Text := 'Create Shadow 1 AUTO ''' + ShadowFileName + '''';
    Transaction.Active := true;
    ExecQuery;
    Transaction.Commit;
  finally
    Free
  end;
  FIBShadowDatabase.DatabaseName := MakeConnectString('',ShadowFileName,inet);
end;

procedure TTest12.RemoveShadow;
begin
  with TIBSQL.Create(nil) do
  try
    Database := IBDatabase;
    Transaction := IBTransaction;
    ReadWriteTransaction;
    SQL.Text := 'Drop Shadow 1 PRESERVE FILE';
    Transaction.Active := true;
    ExecQuery;
    Transaction.Commit;
  finally
    Free
  end;
end;

procedure TTest12.ShowShadowFiles;
var Qry: TIBQuery;
begin
  Qry := TIBQuery.Create(nil);
  with Qry do
  try
    Database := IBDatabase;
    Transaction := IBTransaction;
    SQl.Text := 'Select * From RDB$Files Where RDB$Shadow_Number <> 0 '+
                'Order by RDB$Shadow_Number, RDB$FILE_SEQUENCE';
    Transaction.Active := true;
    Active := true;
    writeln(Outfile,'Shadow Files');
    PrintDataSet(Qry);
    Transaction.Commit;
  finally
    Free
  end;
end;

procedure TTest12.ActivateShadow;
begin
  FIBConfigService.DatabaseName := FIBShadowDatabase.DatabaseName;
  writeln(Outfile,'Activating Shadow');
  FIBConfigService.ActivateShadow;
end;

procedure TTest12.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBXServicesConnection := TIBXServicesConnection.Create(Application);
  FIBConfigService := TIBXConfigService.Create(Application);
  FIBConfigService.ServicesConnection := FIBXServicesConnection;
  FIBLogService := TIBXLogService.Create(Application);
  FIBLogService.ServicesConnection := FIBXServicesConnection;
  FIBOnlineValidationService := TIBXOnlineValidationService.Create(Application);
  FIBOnlineValidationService.ServicesConnection := FIBXServicesConnection;
  FIBServerProperties := TIBXServerProperties.Create(Application);
  FIBServerProperties.ServicesConnection := FIBXServicesConnection;
  FIBStatisticalService := TIBXStatisticalService.Create(Application);
  FIBStatisticalService.ServicesConnection := FIBXServicesConnection;
  FIBValidationService := TIBXValidationService.Create(Application);
  FIBValidationService.ServicesConnection := FIBXServicesConnection;
  FIBXSecurityService := TIBXSecurityService.Create(Application);
  FIBXSecurityService.ServicesConnection := FIBXServicesConnection;
  FUserList := TIBXServicesUserList.Create(Application);
  FUserList.Source := FIBXSecurityService;
  FBackupService := TIBXClientSideBackupService.Create(Application);
  FBackupService.ServicesConnection := FIBXServicesConnection;
  FRestoreService := TIBXClientSideRestoreService.Create(Application);
  FRestoreService.ServicesConnection := FIBXServicesConnection;
  FSSBackupService := TIBXServerSideBackupService.Create(Application);
  FSSBackupService.ServicesConnection := FIBXServicesConnection;
  FSSRestoreService := TIBXServerSideRestoreService.Create(Application);
  FSSRestoreService.ServicesConnection := FIBXServicesConnection;
  FLimboTransResolutionService := TIBXLimboTransactionResolutionService.Create(Application);
  FLimboTransResolutionService.ServicesConnection := FIBXServicesConnection;
  FLimboTransactionsList := TIBXServicesLimboTransactionsList.Create(Application);
  FLimboTransactionsList.Source := FLimboTransResolutionService;
  FIBDatabaseInfo := TIBDatabaseInfo.Create(Application);
  FIBShadowDatabase := TIBDatabase.Create(Application);
end;

function TTest12.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest12.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest12.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBQuery.SQL.Text := 'Select * From EMPLOYEE Order by EMP_NO';
  FIBXServicesConnection.ServerName := Owner.Server;
  FIBXServicesConnection.Protocol := TCP;
  FIBXServicesConnection.PortNo := Owner.PortNo;
  FIBXServicesConnection.Params.Values['user_name'] := Owner.GetUserName;
  FIBXServicesConnection.Params.Values['password'] := Owner.GetPassword;
  FIBXServicesConnection.FirebirdLibraryPathName := Owner.ClientLibraryPath;
  FIBStatisticalService.Options := [DataPages];
  FIBStatisticalService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FIBOnlineValidationService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FIBValidationService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FIBConfigService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FBackupService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FBackupService.Options := [IgnoreLimbo];
  FRestoreService.DatabaseFiles.Add(ExtractDBName(Owner.GetNewDatabaseName));
  FRestoreService.StatisticsRequested := [bsTotalTime];
  FSSBackupService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FSSBackupService.Options := [IgnoreLimbo];
  FSSRestoreService.DatabaseFiles.Add(ExtractDBName(Owner.GetNewDatabaseName));
  FSSRestoreService.StatisticsRequested := [bsTotalTime];
  FLimboTransResolutionService.DatabaseName := ExtractDBName(Owner.GetEmployeeDatabaseName);
  FIBDatabaseInfo.Database := IBDatabase;
  ReadOnlyTransaction;
  FIBShadowDatabase.Params.Assign(IBDatabase.Params);
end;

procedure TTest12.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  FIBXServicesConnection.Connected := true;
  ShowServerProperties;
  ShowStatistics;
  ShowServerLog;
  ValidateDatabase;
  DatabaseSweepDB;
  UserListHandling;
  DBUpDown;
  BackupRestore;
  writeln(OutFile,'Show the EMPLOYEE Table from the restored database');
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    IBQuery.Active := true;
    PrintDataset(IBQuery);
  finally
    IBDatabase.DropDatabase;
  end;
  SSBackupRestore;
  writeln(OutFile,'Show the EMPLOYEE Table from the restored database');
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    IBQuery.Active := true;
    PrintDataset(IBQuery);
  finally
    IBDatabase.DropDatabase;
  end;
  LimboTransactionResolution;
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  FIBConfigService.DatabaseName := IBDatabase.DatabaseName;
  FIBXServicesConnection.Connected := false;
  writeln(Outfile,'Creating an empty database ',IBDatabase.DatabaseName);
  IBDatabase.CreateDatabase;
  try
    FIBXServicesConnection.Connected := true;
    ShowDatabaseProperties;
    DatabasePropertiesTests;
  finally
    IBDatabase.DropDatabase;
  end;
  writeln(Outfile,'Create and activate a shadow file');
  IBDatabase.CreateDatabase;
  try
    CreateShadow;
    ShowShadowFiles;
    RemoveShadow;
    IBDatabase.Connected := false;
    writeln(Outfile,FIBShadowDatabase.DatabaseName,' Is Shadow Database = ',IsShadowDatabase(FIBShadowDatabase.DatabaseName));
    ActivateShadow;
    writeln(Outfile,FIBShadowDatabase.DatabaseName,' Is Shadow Database = ',IsShadowDatabase(FIBShadowDatabase.DatabaseName));
  finally
    IBDatabase.DropDatabase;
    FIBShadowDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest12);

end.

