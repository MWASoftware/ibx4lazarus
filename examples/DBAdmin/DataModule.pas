unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, db, IBDatabase, IBSQL, IBQuery, IBCustomDataSet,
  IBUpdate, IBDatabaseInfo, IBServices, IB, Dialogs, Controls, Forms;

type

  { TDatabaseData }

  TDatabaseData = class(TDataModule)
    CharSetLookup: TIBQuery;
    CurrentTransaction: TIBTransaction;
    DatabaseQuery: TIBQuery;
    DBCharSet: TIBQuery;
    DBSecFiles: TIBQuery;
    ExecDDL: TIBSQL;
    IBConfigService1: TIBConfigService;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    AttmtQuery: TIBQuery;
    IBStatisticalService1: TIBStatisticalService;
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
    procedure CurrentTransactionAfterTransactionEnd(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeClose(DataSet: TDataSet);
    procedure DBCharSetAfterClose(DataSet: TDataSet);
    procedure DBCharSetBeforeOpen(DataSet: TDataSet);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure UpdateCharSetApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
  private
    FAfterDataReload: TNotifyEvent;
    FAfterDBConnect: TNotifyEvent;
    FDBHeaderScanned: boolean;
    FDisconnecting: boolean;
    FISShadowDatabase: boolean;
    FDBUserName: string;
    FDBPassword: string;
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
    procedure ActivateService(aService: TIBCustomService);
    procedure SetNoReserve(AValue: boolean);
    procedure SetSweepInterval(AValue: integer);
    procedure ReloadData(Data: PtrInt=0);
  public
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure BackupDatabase;
    procedure RestoreDatabase;
    procedure BringDatabaseOnline;
    procedure ShutDown(aShutDownmode: TShutdownMode; aDelay: integer);
    function IsDatabaseOnline: boolean;
    function IsShadowDatabase: boolean;
    procedure ActivateShadow;
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
  BackupDlgUnit, RestoreDlgUnit;

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

procedure TDatabaseData.GetDBFlags;
var Line: string;
begin
  if FDBHeaderScanned then Exit;
  FIsShadowDatabase := false;

  try
    ActivateService(IBStatisticalService1);

    with IBStatisticalService1 do
    begin
      try
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
  SecGlobalAuth.Active := true; {sets AutoAdmin}
  try
    Result := SecGlobalAuth.FieldByName('Mappings').AsInteger > 0;
  finally
    SecGlobalAuth.Active := false;
  end;
end;

function TDatabaseData.GetDBReadOnly: boolean;
begin
  Result := DatabaseQuery.FieldByName('MON$READ_ONLY').AsInteger  <> 0;
end;

function TDatabaseData.GetForcedWrites: boolean;
begin
  Result := DatabaseQuery.FieldByName('MON$FORCED_WRITES').AsInteger  <> 0;
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
var SecPlugin: TField;
    index: integer;
begin
  {The stats service is the base service holding the service interface}
  with IBStatisticalService1 do
  if not Active then
  begin
    Params.Values['user_name'] := FDBUserName;
    Params.Values['password'] := FDBPassword;
    Protocol := GetProtocol(IBDatabase1.DatabaseName);
    if Protocol = Local then
    begin
      ServerName := 'Localhost';
      if not FileExists(IBDatabase1.DatabaseName) or FileIsReadOnly(IBDatabase1.DatabaseName) then
        Protocol := TCP; {Use loopback if we can't read/write file}
    end
    else
      ServerName := AttmtQuery.FieldByName('MON$REMOTE_HOST').AsString;
    DatabaseName := AttmtQuery.FieldByName('MON$ATTACHMENT_NAME').AsString;
    SecPlugin := DatabaseQuery.FindField('MON$SEC_DATABASE');
    if (SecPlugin <> nil) and (Trim(SecPlugin.AsString) <> 'Default') then
      Params.Values['expected_db'] := AttmtQuery.FieldByName('MON$ATTACHMENT_NAME').AsString
    else
    begin
      index := Params.IndexOfName('expected_db');
      if index <> -1 then Params.Delete(index);
    end;
    LoginPrompt := false;
    repeat
      try
        Active := true;
      except
        on E:EIBClientError do {Typically Login cancelled}
          begin
            MessageDlg(E.Message,mtError,[mbOK],0);
            Exit;
          end;
         on E: EIBInterBaseError do
           if E.IBErrorCode = isc_sec_context then {Need expected_db}
             LoginPrompt := true
           else
             raise;
        end;
    until Active;

  end;

  if aService = IBStatisticalService1 then
    Exit;

  aService.Assign(IBStatisticalService1);
  if aService is TIBValidationService then
    TIBValidationService(aService).DatabaseName := IBDatabase1.DatabaseName
  else
  if aService is TIBOnlineValidationService then
      TIBOnlineValidationService(aService).DatabaseName := IBDatabase1.DatabaseName
  else
  if aService is TIBStatisticalService then
    TIBStatisticalService(aService).DatabaseName := IBDatabase1.DatabaseName
  else
  if aService is TIBConfigService then
    TIBConfigService(aService).DatabaseName := IBDatabase1.DatabaseName;
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
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
end;

procedure TDatabaseData.Disconnect;
begin
  FDBPassword := '';
  IBDatabase1.Connected := false;
end;

procedure TDatabaseData.BackupDatabase;
begin
  ActivateService(IBConfigService1);
  BackupDlg.ShowModal(IBConfigService1,IBDatabase1.DatabaseName);
end;

procedure TDatabaseData.RestoreDatabase;
begin
  ActivateService(IBConfigService1);
  IBDatabase1.Connected := false;
  try
    RestoreDlg.ShowModal(IBConfigService1,IBDatabase1.DatabaseName);
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
  Linger := DatabaseQuery.FindField('RDB$LINGER');
  if Linger <> nil then
  begin
    if Linger.IsNull then
      Result := '0'
    else
      Result := Linger.AsString;
  end
  else
    Result := 'n/a';
end;

function TDatabaseData.GetNoReserve: boolean;
begin
  Result := DatabaseQuery.FieldByName('MON$RESERVE_SPACE').AsInteger <> 0;
end;

function TDatabaseData.GetSweepInterval: integer;
begin
  Result := DatabaseQuery.FieldByName('MON$SWEEP_INTERVAL').AsInteger;
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
  Result := DatabaseQuery.FieldByName('MON$SHUTDOWN_MODE').AsInteger = 0;
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

procedure TDatabaseData.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
begin
  if FDBPassword <> '' then {reconnect}
  begin
    LoginParams.Values['user_name'] := FDBUserName;
    LoginParams.Values['password'] := FDBPassword;
    exit;
  end;

  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword) = mrOK then
  begin
    FDBPassword := aPassword; {remember for reconnect}
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FDBUserName := aUserName;
    FDBPassword := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
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
  if not Disconnecting then
    Application.QueueAsyncCall(@ReloadData,0);
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

