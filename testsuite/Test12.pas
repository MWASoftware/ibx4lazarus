unit Test12;

{$mode objfpc}{$H+}

{Test 1: Titlecursor}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBXServices;

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
    function IsDatabaseOnline: boolean;
    procedure ShowServerProperties;
    procedure ShowStatistics;
    procedure ShowServerLog;
    procedure ValidateDatabase;
    procedure ShowUserList;
    procedure DBUpDown;
    procedure DatabaseSweepDB;
    procedure BackupRestore;
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest12 }

function TTest12.IsDatabaseOnline: boolean;
var Lines: TStringList;
    i: integer;
    line: string;
begin
  {Scan header page to see if database is online - assumes that service is already set up}
  Result := true;
  with FIBStatisticalService do
  begin
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
    WriteStrings(S);
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

procedure TTest12.ShowUserList;
begin
  FUserList.Active := true;
  PrintDataSet(FUserList);
end;

procedure TTest12.DBUpDown;
begin
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline);
  FIBConfigService.ShutDownDatabase(Forced,0);
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline);
  FIBConfigService.BringDatabaseOnline;
  writeln(OutFile,'Employee Database is Online = ',IsDatabaseOnline);
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

procedure TTest12.CreateObjects(Application: TCustomApplication);
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
end;

procedure TTest12.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  FIBXServicesConnection.Connected := true;
  ShowServerProperties;
  ShowStatistics;
  ShowServerLog;
  ValidateDatabase;
  DatabaseSweepDB;
  ShowUserList;
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
end;

initialization
  RegisterTest(TTest12);

end.

