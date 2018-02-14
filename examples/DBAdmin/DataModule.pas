unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, db, memds, IBDatabase, IBSQL, IBQuery,
  IBCustomDataSet, IBUpdate, IBDatabaseInfo, IBServices, IB, Dialogs, Controls,
  Forms;

type

  { TDatabaseData }

  TDatabaseData = class(TDataModule)
    ApplicationProperties1: TApplicationProperties;
    CharSetLookup: TIBQuery;
    CurrentTransaction: TIBTransaction;
    DatabaseQuery: TIBQuery;
    Attachments: TIBDataSet;
    IBSecurityService1: TIBSecurityService;
    LegacyUserList: TMemDataset;
    UserListGROUPID: TLongintField;
    UserListSource: TDataSource;
    DBCharSet: TIBQuery;
    DBSecFiles: TIBQuery;
    ExecDDL: TIBSQL;
    IBConfigService1: TIBConfigService;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    AttmtQuery: TIBQuery;
    IBLogService1: TIBLogService;
    IBServerProperties1: TIBServerProperties;
    IBStatisticalService1: TIBStatisticalService;
    RoleNameList: TIBQuery;
    RoleNameListGRANTED: TBooleanField;
    RoleNameListRDBDESCRIPTION: TIBMemoField;
    RoleNameListRDBOWNER_NAME: TIBStringField;
    RoleNameListRDBPRIVILEGE: TIBStringField;
    RoleNameListRDBROLE_NAME: TIBStringField;
    RoleNameListRDBSECURITY_CLASS: TIBStringField;
    RoleNameListRDBSYSTEM_FLAG: TSmallintField;
    RoleNameListRDBUSER: TIBStringField;
    RoleNameListUSERNAME: TIBStringField;
    TableNameLookup: TIBQuery;
    TagsUpdate: TIBUpdate;
    UpdateCharSet: TIBUpdate;
    SecGlobalAuth: TIBQuery;
    ShadowFiles: TIBQuery;
    ShadowFilesFileMode: TStringField;
    ShadowFilesRDBFILE_FLAGS: TSmallintField;
    ShadowFilesRDBFILE_LENGTH: TIntegerField;
    ShadowFilesRDBFILE_NAME: TIBStringField;
    ShadowFilesRDBFILE_SEQUENCE: TSmallintField;
    ShadowFilesRDBFILE_START: TIntegerField;
    ShadowFilesRDBSHADOW_NUMBER: TSmallintField;
    UpdateUserRoles: TIBUpdate;
    UpdateUsers: TIBUpdate;
    UserList: TIBQuery;
    UserListCURRENT_CONNECTION: TIBLargeIntField;
    UserListDBCREATOR: TBooleanField;
    UserListLOGGEDIN: TBooleanField;
    UserListSECACTIVE: TBooleanField;
    UserListSECADMIN: TBooleanField;
    UserListSECFIRST_NAME: TIBStringField;
    UserListSECLAST_NAME: TIBStringField;
    UserListSECMIDDLE_NAME: TIBStringField;
    UserListSECPLUGIN: TIBStringField;
    UserListUSERID: TLongintField;
    UserListUSERNAME: TIBStringField;
    UserListUSERPASSWORD: TIBStringField;
    UserTags: TIBQuery;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure AttachmentsAfterOpen(DataSet: TDataSet);
    procedure CurrentTransactionAfterTransactionEnd(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeClose(DataSet: TDataSet);
    procedure DBCharSetAfterClose(DataSet: TDataSet);
    procedure DBCharSetBeforeOpen(DataSet: TDataSet);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure IBStatisticalService1Login(Service: TIBCustomService;
      LoginParams: TStrings);
    procedure LegacyUserListAfterOpen(DataSet: TDataSet);
    procedure LegacyUserListBeforeClose(DataSet: TDataSet);
    procedure LegacyUserListBeforeDelete(DataSet: TDataSet);
    procedure LegacyUserListBeforePost(DataSet: TDataSet);
    procedure TagsUpdateApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure UpdateCharSetApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
    procedure UpdateUserRolesApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
    procedure UpdateUsersApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure UserListAfterInsert(DataSet: TDataSet);
    procedure UserListAfterOpen(DataSet: TDataSet);
    procedure UserListAfterPost(DataSet: TDataSet);
    procedure UserListAfterScroll(DataSet: TDataSet);
    procedure UserListBeforeClose(DataSet: TDataSet);
    procedure UserTagsAfterInsert(DataSet: TDataSet);
  private
    FAfterDataReload: TNotifyEvent;
    FAfterDBConnect: TNotifyEvent;
    FDBHeaderScanned: boolean;
    FDisconnecting: boolean;
    FISShadowDatabase: boolean;
    FDBUserName: string;
    FDBPassword: string;
    FServerUserName: string;
    FServerPassword: string;
    FLocalConnect: boolean;
    FUsersLoading: boolean;
    procedure ActivateService(aService: TIBCustomService);
    function GetAutoAdmin: boolean;
    procedure GetDBFlags;
    function GetDBReadOnly: boolean;
    function GetForcedWrites: boolean;
    function GetLingerDelay: string;
    function GetNoReserve: boolean;
    function GetSweepInterval: integer;
    procedure SetAutoAdmin(AValue: boolean);
    procedure SetDBReadOnly(AValue: boolean);
    procedure SetForcedWrites(AValue: boolean);
    procedure SetLingerDelay(AValue: string);
    procedure SetNoReserve(AValue: boolean);
    procedure SetSweepInterval(AValue: integer);
    procedure ReloadData(Data: PtrInt=0);
  public
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure DropDatabase;
    procedure BackupDatabase;
    procedure RestoreDatabase;
    procedure BringDatabaseOnline;
    procedure ShutDown(aShutDownmode: TShutdownMode; aDelay: integer);
    function IsDatabaseOnline: boolean;
    function IsShadowDatabase: boolean;
    procedure ActivateShadow;
    procedure AddSecondaryFile(aFileName: string; StartAt,FileLength: integer);
    procedure AddShadowSet;
    procedure RemoveShadowSet(ShadowSet: integer);
    procedure LoadPerformanceStatistics(Lines: TStrings);
    procedure LoadDatabaseStatistics(OptionID: integer; Lines: TStrings);
    procedure LoadServerProperties(Lines: TStrings);
    procedure LoadServerLog(Lines: TStrings);
    property AutoAdmin: boolean read GetAutoAdmin write SetAutoAdmin;
    property Disconnecting: boolean read FDisconnecting;
    property ForcedWrites: boolean read GetForcedWrites write SetForcedWrites;
    property LingerDelay: string read GetLingerDelay write SetLingerDelay;
    property DBReadOnly: boolean read GetDBReadOnly write SetDBReadOnly;
    property NoReserve: boolean read GetNoReserve write SetNoReserve;
    property SweepInterval: integer read GetSweepInterval write SetSweepInterval;
    property AfterDBConnect: TNotifyEvent read FAfterDBConnect write FAfterDBConnect;
    property AfterDataReload: TNotifyEvent read FAfterDataReload write FAfterDataReload;
  end;

var
  DatabaseData: TDatabaseData;

implementation

{$R *.lfm}

uses DBLoginDlgUnit, IBUtils, FBMessages, IBErrorCodes, ShutdownDatabaseDlgUnit,
  BackupDlgUnit, RestoreDlgUnit, AddShadowSetDlgUnit, ServicesLoginDlgUnit;

const
  sAddSecondarySQL  = 'Alter Database Add File ''%s'' Starting at %d';
  sAddSecondarySQL2 = 'Alter Database Add File ''%s'' Starting at %d Length %d';
  sRemoveShadow     = 'Drop Shadow %d';
  sRemoveShadow12   = 'Drop Shadow %d DELETE FILE';
  sPreserveShadow   = 'Drop Shadow %d PRESERVE FILE';

resourcestring
  sPreserveShadowFiles = 'Preserve Shadow Set Files after drop?';

{ TDatabaseData }

procedure TDatabaseData.UpdateCharSetApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  if UpdateKind = ukModify then
  begin
    ExecDDL.SQL.Text := 'ALTER DATABASE SET DEFAULT CHARACTER SET ' +
                Params.ByName('RDB$CHARACTER_SET_NAME').AsString;
    ExecDDL.ExecQuery;
  end;
end;

procedure TDatabaseData.UpdateUserRolesApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

  procedure Grant(Params: ISQLParams);
  begin
    ExecDDL.SQL.Text := 'Grant ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' to ' + Params.ByName('USERNAME').AsString;
    ExecDDL.ExecQuery;
  end;

  procedure Revoke(Params: ISQLParams);
  begin
    ExecDDL.SQL.Text := 'Revoke ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' from ' + Params.ByName('USERNAME').AsString;
    ExecDDL.ExecQuery;
  end;

begin
  if UpdateKind = ukModify then
  begin
    if Params.ByName('GRANTED').AsInteger = 0 then
      Revoke(Params)
    else
      Grant(Params);
  end;
end;

procedure TDatabaseData.UpdateUsersApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

  function FormatStmtOptions: string;
  var Param: ISQLParam;
  begin
    Result := Trim(Params.ByName('UserName').AsString);
    Param := Params.ByName('USERPASSWORD');
    if (Param <> nil) and not Param.IsNull  then
      Result += ' PASSWORD ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$FIRST_NAME');
    if Param <> nil then
      Result += ' FIRSTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$MIDDLE_NAME');
    if Param <> nil then
      Result += ' MIDDLENAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$LAST_NAME');
    if Param <> nil then
      Result += ' LASTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$ACTIVE');
    if Param <> nil then
    begin
      if Param.AsBoolean then
        Result += ' ACTIVE'
      else
        Result += ' INACTIVE';
    end;
    Param := Params.ByName('SEC$PLUGIN');
    if Param <> nil then
      Result += ' USING PLUGIN ' + QuoteIdentifierIfNeeded((Sender as TIBUpdate).DataSet.Database.SQLDialect,Param.AsString);
  end;

begin
    case UpdateKind of
    ukInsert:
        ExecDDL.SQL.Text := 'CREATE USER ' + FormatStmtOptions;
    ukModify:
        ExecDDL.SQL.Text := 'ALTER USER ' + FormatStmtOptions;
    ukDelete:
      ExecDDL.SQL.Text := 'DROP USER ' + Trim(Params.ByName('UserName').AsString);
    end;
    ExecDDL.ExecQuery;

  if UpdateKind = ukInsert then
  begin
    if Params.ByName('SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + Trim(Params.ByName('UserName').AsString) + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end;
  end
  else
  if UpdateKind = ukModify then
  {Update Admin Role if allowed}
  begin
    if Params.ByName('SEC$ADMIN').AsBoolean and not Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + Trim(Params.ByName('UserName').AsString) + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end
    else
    if not Params.ByName('SEC$ADMIN').AsBoolean and Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + Trim(Params.ByName('UserName').AsString) + ' REVOKE ADMIN ROLE';
      ExecDDL.ExecQuery;
    end
  end;

  {Update DB Creator Role}
  if Params.ByName('DBCreator').AsBoolean and not Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ExecDDL.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + Trim(Params.ByName('UserName').AsString);
    ExecDDL.ExecQuery;
  end
  else
  if not Params.ByName('DBCreator').AsBoolean and Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ExecDDL.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + Trim(Params.ByName('UserName').AsString);
    ExecDDL.ExecQuery;
  end
end;

procedure TDatabaseData.UserListAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('SEC$ADMIN').AsBoolean := false;
  DataSet.FieldByName('SEC$ACTIVE').AsBoolean := false;
  DataSet.FieldByName('DBCreator').AsBoolean := false;
  DataSet.FieldByName('SEC$PLUGIN').AsString := 'Srp';
  DataSet.FieldByName('UserID').AsInteger := 0;
  DataSet.FieldByName('GroupID').AsInteger := 0;
  DataSet.FieldByName('UserPassword').Clear;
end;

procedure TDatabaseData.UserListAfterOpen(DataSet: TDataSet);
begin
  UserListSource.DataSet := UserList;
  RoleNameList.Active := true;
  UserTags.Active := true;
end;

procedure TDatabaseData.UserListAfterPost(DataSet: TDataSet);
begin
  CurrentTransaction.Commit;
end;

procedure TDatabaseData.UserListAfterScroll(DataSet: TDataSet);
begin
  UserList.FieldByName('SEC$PLUGIN').ReadOnly := UserList.State <> dsInsert;
end;

procedure TDatabaseData.UserListBeforeClose(DataSet: TDataSet);
begin
  RoleNameList.Active := false;
  UserTags.Active := false;
end;

procedure TDatabaseData.UserTagsAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('SEC$USER_NAME').AsString := UserList.FieldByName('UserName').AsString;
end;

procedure TDatabaseData.GetDBFlags;
var Line: string;
begin
  if FDBHeaderScanned or not DatabaseQuery.Active or not AttmtQuery.Active then Exit;
  FIsShadowDatabase := false;

  try
    ActivateService(IBStatisticalService1);

    with IBStatisticalService1 do
    begin
      try
        Options := [HeaderPages];
        ServiceStart;
        while not Eof do
        begin
           Line := GetNextLine;
           if (Pos('Attributes',Line) <> 0) and (Pos('shadow',Line) <> 0) then
             FIsShadowDatabase := true;

        end
      finally
        Active := False;
      end
    end;
  except on E: Exception do
    MessageDlg('Error getting DB Header Page: ' + E.Message,mtError,[mbOK],0);
  end;
end;

function TDatabaseData.GetAutoAdmin: boolean;
begin
  Result := false;
  if not CurrentTransaction.Active then Exit;
  SecGlobalAuth.Active := true; {sets AutoAdmin}
  try
    Result := SecGlobalAuth.FieldByName('Mappings').AsInteger > 0;
  finally
    SecGlobalAuth.Active := false;
  end;
end;

function TDatabaseData.GetDBReadOnly: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$READ_ONLY').AsInteger  <> 0);
end;

function TDatabaseData.GetForcedWrites: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$FORCED_WRITES').AsInteger  <> 0);
end;

procedure TDatabaseData.SetLingerDelay(AValue: string);
begin
  if (StrToInt(AValue) =  DatabaseQuery.FieldByName('RDB$LINGER').AsInteger) then Exit;

  if (AValue = '') or (StrToInt(AValue) = 0) then
  begin
    if MessageDlg('Turn off Linger Permanently?',mtConfirmation,[mbYes,mbNo],0) = mrNo then
    begin
      ActivateService(IBConfigService1);
      IBConfigService1.SetNoLinger;
      CurrentTransaction.Commit; {Refresh}
      Exit;
    end;
    ExecDDL.SQL.Text := 'ALTER DATABASE DROP LINGER'
  end
  else
    ExecDDL.SQL.Text := 'ALTER DATABASE SET LINGER TO ' + AValue;
  with ExecDDL do
  begin
    Transaction.Active := true;
    ExecQuery;
    Transaction.Commit;
  end;
end;

procedure TDatabaseData.ActivateService(aService: TIBCustomService);

  procedure AssignDatabase(IBService: TIBCustomService; DBName: string);
  begin
    if IBService is TIBValidationService then
      TIBValidationService(IBService).DatabaseName := DBName
    else
    if IBService is TIBOnlineValidationService then
        TIBOnlineValidationService(IBService).DatabaseName := DBName
    else
    if IBService is TIBStatisticalService then
      TIBStatisticalService(IBService).DatabaseName := DBName
    else
    if IBService is TIBConfigService then
      TIBConfigService(IBService).DatabaseName := DBName;
  end;

  procedure SetupParams(IBService: TIBCustomService; UseDefaultSecDatabase: boolean; DBName: string);
  var index: integer;
  begin
    with IBService do
    begin
      Active := false;
      if FServerPassword <> '' then
      begin
        Params.Values['user_name'] := FServerUserName;
        Params.Values['password'] := FServerPassword;
      end
      else
      begin
        Params.Values['user_name'] := FDBUserName;
        Params.Values['password'] := FDBPassword;
      end;

      Protocol := GetProtocol(IBDatabase1.DatabaseName);
      if Protocol = Local then
      begin
        ServerName := 'Localhost';
        if not FileExists(DBName) or FileIsReadOnly(DBName) then
          Protocol := TCP; {Use loopback if we can't read/write file}
      end
      else
        ServerName := IBDatabaseInfo.DBSiteName;
    end;
    AssignDatabase(IBService,DBName);

    {Are we using a different security database?}

    if not UseDefaultSecDatabase then
      IBService.Params.Values['expected_db'] := DBName
    else
    begin
      index := IBService.Params.IndexOfName('expected_db');
      if index <> -1 then IBService.Params.Delete(index);
    end;
  end;

var SecPlugin: TField;
    UsingDefaultSecDatabase: boolean;
begin
  {Are we using a different security database?}

  SecPlugin := DatabaseQuery.FindField('MON$SEC_DATABASE');
  UsingDefaultSecDatabase := (SecPlugin = nil) or (Trim(SecPlugin.AsString) = 'Default');

  {The server properties service is the base service holding the service interface}
  if not IBServerProperties1.Active then
  begin
    SetupParams(IBServerProperties1,UsingDefaultSecDatabase,AttmtQuery.FieldByName('MON$ATTACHMENT_NAME').AsString);
    with IBServerProperties1 do
    begin
      LoginPrompt := (Protocol <> Local) and (FDBPassword = '') and (FServerPassword = '');
      repeat
        try
          Active := true;
          LoginPrompt := false;
        except
          on E:EIBClientError do {Typically Login cancelled}
            begin
              MessageDlg(E.Message,mtError,[mbOK],0);
              Exit;
            end;
          on E:Exception do
            begin
              MessageDlg(E.Message,mtError,[mbOK],0);
              LoginPrompt := true;
            end;
          end;
      until Active;
    end;
  end;

  if aService = IBServerProperties1 then
    Exit;

  aService.Assign(IBServerProperties1);
  AssignDatabase(aService,AttmtQuery.FieldByName('MON$ATTACHMENT_NAME').AsString);
end;

procedure TDatabaseData.SetNoReserve(AValue: boolean);
begin
  ActivateService(IBConfigService1);
  IBConfigService1.SetReserveSpace(AValue);
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseData.SetSweepInterval(AValue: integer);
begin
  ActivateService(IBConfigService1);
  IBConfigService1.SetSweepInterval(AValue);
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseData.ReloadData(Data: PtrInt);
begin
  if csDestroying in ComponentState then Exit;
  CurrentTransaction.Active := true;
  DataBaseQuery.Active := true;
  AttmtQuery.Active := true;
  if assigned(FAfterDataReload) then
    AfterDataReload(self);
end;

destructor TDatabaseData.Destroy;
begin
  Application.RemoveAsyncCalls(self);
  inherited Destroy;
end;

procedure TDatabaseData.Connect;
begin
  Disconnect;
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Exit
      end;
    On E:Exception do
      begin
       MessageDlg(E.Message,mtError,[mbOK],0);
       FDBPassword := '';
      end;
    end;
  until IBDatabase1.Connected;
end;

procedure TDatabaseData.Disconnect;
begin
  FDBPassword := '';
  FServerPassword := '';
  FLocalConnect := false;
  IBDatabase1.Connected := false;
  IBConfigService1.Active := false;
  IBStatisticalService1.Active := false;
  IBServerProperties1.Active := false;
end;

procedure TDatabaseData.DropDatabase;
begin
  IBDatabase1.DropDatabase;
  Disconnect;
end;

procedure TDatabaseData.BackupDatabase;
begin
  ActivateService(IBConfigService1);
  BackupDlg.ShowModal(IBConfigService1,IBDatabase1.DatabaseName);
end;

procedure TDatabaseData.RestoreDatabase;
var DefaultPageSize: integer;
begin
  ActivateService(IBConfigService1);
  DefaultPageSize := DatabaseQuery.FieldByName('MON$PAGE_SIZE').AsInteger;
  IBDatabase1.Connected := false;
  try
    RestoreDlg.ShowModal(IBConfigService1,IBDatabase1.DatabaseName,DefaultPageSize);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDatabaseData.BringDatabaseOnline;
begin
  if IsDatabaseOnline then
    MessageDlg('Database is already online!',mtInformation,[mbOK],0)
  else
  begin
    ActivateService(IBConfigService1);
    IBDatabase1.Connected := false;
    try
      IBConfigService1.BringDatabaseOnline;
      while IBConfigService1.IsServiceRunning do;
    finally
      IBDatabase1.Connected := true;
    end;
    if IsDatabaseOnline then
      MessageDlg('Database is back online',mtInformation,[mbOK],0)
    else
      MessageDlg('Database is still shutdown!',mtError,[mbOK],0);
  end;
end;

procedure TDatabaseData.ShutDown(aShutDownmode: TShutdownMode; aDelay: integer);
begin
  ActivateService(IBConfigService1);
  IBDatabase1.Connected := false;
  try
    ShutdownDatabaseDlg.Shutdown(IBConfigService1, aShutDownmode, aDelay);
  finally
    IBDatabase1.Connected := true;
  end;
end;

function TDatabaseData.GetLingerDelay: string;
var Linger: TField;
begin
  Result := 'n/a';
  if not  DatabaseQuery.Active then exit;
  Linger := DatabaseQuery.FindField('RDB$LINGER');
  if Linger <> nil then
  begin
    if Linger.IsNull then
      Result := '0'
    else
      Result := Linger.AsString;
  end;
end;

function TDatabaseData.GetNoReserve: boolean;
begin
  Result :=  DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$RESERVE_SPACE').AsInteger <> 0);
end;

function TDatabaseData.GetSweepInterval: integer;
begin
  if DatabaseQuery.Active then
    Result :=  DatabaseQuery.FieldByName('MON$SWEEP_INTERVAL').AsInteger
  else
    Result := 0;
end;

procedure TDatabaseData.SetAutoAdmin(AValue: boolean);
begin
  ActivateService(IBConfigService1);
  IBConfigService1.SetAutoAdmin(AValue);
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseData.SetDBReadOnly(AValue: boolean);
begin
  ActivateService(IBConfigService1);
  IBDatabase1.Connected := false;
  try
    IBConfigService1.SetReadOnly(AValue);
    while IBConfigService1.IsServiceRunning do;
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDatabaseData.SetForcedWrites(AValue: boolean);
begin
  ActivateService(IBConfigService1);
  IBConfigService1.SetAsyncMode(not AValue);
  while IBConfigService1.IsServiceRunning do;
end;

function TDatabaseData.IsDatabaseOnline: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$SHUTDOWN_MODE').AsInteger = 0);
end;

function TDatabaseData.IsShadowDatabase: boolean;
begin
  GetDBFlags;
  Result := FIsShadowDatabase;
end;

procedure TDatabaseData.ActivateShadow;
begin
  ActivateService(IBConfigService1);
  IBConfigService1.ActivateShadow;
  while IBConfigService1.IsServiceRunning do;
  MessageDlg('Shadow Database activated. You should now rename the file or change the database alias name to point to the shadow',
    mtInformation,[mbOK],0);
end;

procedure TDatabaseData.AddSecondaryFile(aFileName: string; StartAt,
  FileLength: integer);
var SQLText: string;
begin
  if FileLength <> -1 then
    SQLText := Format(sAddSecondarySQL2,[aFileName,StartAt,FileLength])
  else
    SQLText := Format(sAddSecondarySQL,[aFileName,StartAt]);
  ExecDDL.SQL.Text := SQLText;
  ExecDDL.ExecQuery;
  CurrentTransaction.Commit;
end;

procedure TDatabaseData.AddShadowSet;
var CurrentLocation: TBookmark;
    ShadowSet: integer;
begin
  if ShadowFiles.RecordCount = 0 then
    ShadowSet := 1
  else
  with ShadowFiles do
  begin
    CurrentLocation := Bookmark;
    DisableControls;
    try
      Last;
      ShadowSet := FieldByName('RDB$Shadow_Number').AsInteger + 1;
    finally
      Bookmark := CurrentLocation;
      EnableControls
    end
  end;
  AddShadowSetDlg.ShowModal(ShadowSet);
  CurrentTransaction.Commit;
end;

procedure TDatabaseData.RemoveShadowSet(ShadowSet: integer);
begin
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    if MessageDlg(Format(sRemoveShadow,[ShadowSet]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
       ExecDDL.SQL.Text := Format(sRemoveShadow,[ShadowSet]);
  end
  else
    case MessageDlg(Format(sPreserveShadowFiles,[ShadowSet]),mtConfirmation,[mbYes,mbNo,mbCancel],0) of
    mrNo:
      ExecDDL.SQL.Text  :=Format(sRemoveShadow12,[ShadowSet]);
    mrYes:
      ExecDDL.SQL.Text := Format(sPreserveShadow,[ShadowSet]);
    mrCancel:
      Exit;
    end;
  ExecDDL.ExecQuery;
  CurrentTransaction.Commit;
end;

procedure TDatabaseData.LoadPerformanceStatistics(Lines: TStrings);

  procedure AddPerfStats(Heading: string; stats: TStrings);
  var i: integer;
  begin
    with Lines do
    begin
      if stats.count = 0 then exit;
      Add('');
      Add(Heading);
      for i := 0 to stats.Count - 1 do
      begin
        if TableNameLookup.Locate('RDB$RELATION_ID',stats.Names[i],[]) then
          Add('  ' + TableNameLookup.FieldByName('RDB$RELATION_NAME').AsString + ' = ' + stats.ValueFromIndex[i]);
      end;
    end;
  end;

begin
  TableNameLookup.Active := true;
  with IBDatabaseInfo, Lines do
  begin
    Add(Format('Number of reads from the memory buffer cache = %d',[Fetches]));
    Add(Format('Number of writes to the memory buffer cache = %d',[Marks]));
    Add(Format('Number of page reads = %d',[Reads]));
    Add(Format('Number of page writes = %d',[Writes]));
    Add('');
    Add('Since Database last attached:');
    AddPerfStats('Number of removals of a version of a record',BackoutCount);
    AddPerfStats('Number of database deletes',DeleteCount);
    AddPerfStats('Number of removals of a committed record',ExpungeCount);
    AddPerfStats('Number of inserts',InsertCount);
    AddPerfStats('Number of removals of old versions of fully mature records',PurgeCount);
    AddPerfStats('Number of reads done via an index',ReadIdxCount);
    AddPerfStats('Number of sequential table scans',ReadSeqCount);
    AddPerfStats('Number of database updates',UpdateCount);
  end;
end;

procedure TDatabaseData.LoadDatabaseStatistics(OptionID: integer; Lines: TStrings);
begin
  if OptionID = 1 then
    LoadPerformanceStatistics(Lines)
  else
  with IBStatisticalService1 do
  begin
    case OptionID of
    0: Options := [HeaderPages];
    2: options := [DataPages];
    3: Options := [IndexPages];
    4: Options := [SystemRelations]
    end;
    Active := true;
    ServiceStart;
    while not Eof do
      Lines.Add(GetNextLine);
  end;
end;

procedure TDatabaseData.LoadServerProperties(Lines: TStrings);
var i: integer;
begin
  Lines.Clear;
  ActivateService(IBServerProperties1);
  with IBServerProperties1 do
  begin
    FetchVersionInfo;
    Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    Lines.Add(Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
    FetchDatabaseInfo;
    Lines.Add('No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    Lines.Add('No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      Lines.Add('DB Name = ' + DatabaseInfo.DbName[i]);
    FetchConfigParams;
    Lines.Add('Base Location = ' + ConfigParams.BaseLocation);
    Lines.Add('Lock File Location = ' + ConfigParams.LockFileLocation);
    Lines.Add('Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
  end;
end;

procedure TDatabaseData.LoadServerLog(Lines: TStrings);
begin
  Lines.Clear;
  ActivateService(IBLogService1);
  if IBLogService1.Protocol = Local then
    Lines.Add('Server Log not available with embedded server')
  else
  with IBLogService1 do
  begin
    ServiceStart;
    while not Eof do
      Lines.Add(GetNextLine);
  end;
end;

procedure TDatabaseData.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
    aCreateIfNotExist: boolean;
begin
  if FLocalConnect or (FDBPassword <> '') {reconnect}  then
  begin
    LoginParams.Values['user_name'] := FDBUserName;
    LoginParams.Values['password'] := FDBPassword;
    exit;
  end;

  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword, aCreateIfNotExist) = mrOK then
  begin
    FDBPassword := aPassword; {remember for reconnect}
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FDBUserName := aUserName;
    FDBPassword := aPassword;
    Database.CreateIfNotExists := aCreateIfNotExist;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TDatabaseData.IBStatisticalService1Login(Service: TIBCustomService;
  LoginParams: TStrings);
var aUserName,
    aPassword: string;
begin
  aUserName := LoginParams.Values['user_name'];
  if SvcLoginDlg.ShowModal(Service.ServerName,aUserName,aPassword) = mrOK then
  begin
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FServerUserName := aUserName;
    FServerPassword := aPassword;
  end;
end;

procedure TDatabaseData.LegacyUserListAfterOpen(DataSet: TDataSet);
var i: integer;
begin
  ActivateService(IBSecurityService1);
  with IBSecurityService1 do
  begin
    DisplayUsers;
    FUsersLoading := true;
    try
      for i := 0 to UserInfoCount - 1 do
      with UserInfo[i],LegacyUserList do
      begin
        Append;
        FieldByName('UserID').AsInteger := UserID;
        FieldByName('GroupID').AsInteger := GroupID;
        FieldByName('UserName').AsString := UserName;
        FieldByName('SEC$FIRST_NAME').AsString := FirstName;
        FieldByName('SEC$MIDDLE_NAME').AsString := MiddleName;
        FieldByName('SEC$LAST_NAME').AsString := LastName;
        FieldByName('UserPassword').Clear;
        FieldByName('SEC$ADMIN').AsBoolean := AdminRole;
        Post;
      end;
    finally
      FUsersLoading := false;
    end;
  end;
  UserListSource.DataSet := LegacyUserList;
  RoleNameList.Active := true;
end;

procedure TDatabaseData.LegacyUserListBeforeClose(DataSet: TDataSet);
begin
  RoleNameList.Active := false;
  with LegacyUserList do
  begin
    if State in [dsEdit,dsInsert] then Post;
    Clear(false);
  end;
end;

procedure TDatabaseData.LegacyUserListBeforeDelete(DataSet: TDataSet);
begin
  ActivateService(IBSecurityService1);
  with IBSecurityService1 do
  begin
    UserName := DataSet.FieldByName('UserName').AsString;
    DeleteUser;
  end;
end;

procedure TDatabaseData.LegacyUserListBeforePost(DataSet: TDataSet);

  procedure SetParams;
  begin
    with LegacyUserList, IBSecurityService1 do
    begin
      UserID := FieldByName('UserID').AsInteger;
      GroupID := FieldByName('GroupID').AsInteger;
      UserName := FieldByName('UserName').AsString;
      FirstName := FieldByName('SEC$FIRST_NAME').AsString;
      MiddleName := FieldByName('SEC$MIDDLE_NAME').AsString;
      LastName := FieldByName('SEC$LAST_NAME').AsString;
      if not FieldByName('UserPassword').IsNull then
        Password := FieldByName('UserPassword').AsString;
      AdminRole := FieldByName('SEC$ADMIN').AsBoolean;
    end;
  end;

 begin
    if FUsersLoading then Exit;
    ActivateService(IBSecurityService1);
    case LegacyUserList.State of
    dsEdit:
      begin
        SetParams;
        IBSecurityService1.ModifyUser;
      end;
    dsInsert:
      begin
        SetParams;
        IBSecurityService1.AddUser;
      end;
    end;

end;

procedure TDatabaseData.TagsUpdateApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
var sql: string;
begin
  sql := '';
  case UpdateKind of
  ukInsert,
  ukModify:
    begin
      sql := 'ALTER USER ' + Trim(Params.ByName('SEC$USER_NAME').AsString)
         + ' TAGS (' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('SEC$KEY').AsString)
         + '=''' + SQLSafeString(Params.ByName('SEC$VALUE').AsString) + '''';
      if Params.ByName('SEC$KEY').AsString <> Params.ByName('OLD_SEC$KEY').AsString then
        sql += ', DROP ' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('OLD_SEC$KEY').AsString);
      sql +=')'
    end;

  ukDelete:
    sql := 'ALTER USER ' + Trim(Params.ByName('SEC$USER_NAME').AsString)
         + ' TAGS (DROP ' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('SEC$KEY').AsString) + ')';
  end;
  ExecDDL.SQL.Text := sql;
  ExecDDL.ExecQuery;
end;

procedure TDatabaseData.IBDatabase1AfterConnect(Sender: TObject);
begin
  {Virtual tables did not exist prior to Firebird 2.1 - so don't bother with old version}
  with IBDatabaseInfo do
    if (ODSMajorVersion < 11) or ((ODSMajorVersion = 11) and (ODSMinorVersion < 1)) then
    begin
      IBDatabase1.Connected := false;
      raise Exception.Create('This application requires Firebird 2.1 or later');
    end;

  FLocalConnect := GetProtocol(IBDatabase1.DatabaseName) = Local;

  if assigned(FAfterDBConnect) then
    AfterDBConnect(self);

  ReloadData;
end;

procedure TDatabaseData.IBDatabase1AfterDisconnect(Sender: TObject);
begin
  FDisconnecting := false;
end;

procedure TDatabaseData.IBDatabase1BeforeDisconnect(Sender: TObject);
begin
  FDisconnecting := true;
end;

procedure TDatabaseData.DatabaseQueryAfterOpen(DataSet: TDataSet);
begin
  DBCharSet.Active := true;
end;

procedure TDatabaseData.CurrentTransactionAfterTransactionEnd(Sender: TObject);
begin
  if not Disconnecting and not (csDestroying in ComponentState) then
    Application.QueueAsyncCall(@ReloadData,0);
end;

procedure TDatabaseData.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  if E is EIBInterBaseError then
  begin
    if RoleNameList.State in [dsInsert,dsEdit] then
      RoleNameList.Cancel;
    if UserList.State in [dsInsert,dsEdit] then
      UserList.Cancel;
  end;
  MessageDlg(E.Message,mtError,[mbOK],0);
  CurrentTransaction.Rollback;
end;

procedure TDatabaseData.AttachmentsAfterOpen(DataSet: TDataSet);
begin
  Attachments.Locate('MON$ATTACHMENT_ID',AttmtQuery.FieldByName('MON$ATTACHMENT_ID').AsInteger,[]);
end;

procedure TDatabaseData.DatabaseQueryBeforeClose(DataSet: TDataSet);
begin
  DBCharSet.Active := false;
end;

procedure TDatabaseData.DBCharSetAfterClose(DataSet: TDataSet);
begin
  CharSetLookup.Active := false;
end;

procedure TDatabaseData.DBCharSetBeforeOpen(DataSet: TDataSet);
begin
  CharSetLookup.Active := true;
end;

end.

