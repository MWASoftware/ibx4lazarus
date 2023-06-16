unit DBADataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, db, memds, DataModule,
  IBXServices, IBDatabase, IBQuery, ibxscript, ServerDataUnit, DatabaseDataUnit,IB;

type

  { TDBADatabaseData }

  TDBADatabaseData = class(TDBDataModule)
    IBXClientSideRestoreService1: TIBXClientSideRestoreService;
    IBXScript1: TIBXScript;
    procedure DataModuleCreate(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure IBXScript1CreateDatabase(Sender: TObject;
      var DatabaseFileName: string);
    procedure IBXServicesConnection1AfterConnect(Sender: TObject);
    procedure IBXServicesConnection1AfterDisconnect(Sender: TObject);
    procedure IBXServicesConnection1Login(Service: TIBXServicesConnection;
      var aServerName: string; LoginParams: TStrings);
    procedure IBXServicesConnection1SecurityContextException(
      Service: TIBXServicesConnection; var aAction: TSecContextAction);
  private
    FDatabaseData: TDatabaseData;
    FSchemaVersion: integer;
    FServicePassword: string;
    FDatabasePassword: string;
    FNewPassword: boolean;
    FNewServicePassword: boolean;
    FServiceConnectCount: integer;
    FDBConnectCount: integer;
    FServerData: TServerData;
    FSecDBException: boolean;
    function IdentifyDatabase: integer;
    function GetSchemaVersion: integer;
    function HasTable(aTableName: string): boolean;
    procedure GetSchemaVersion2(Sender: TObject; var VersionNo: integer);
    procedure SetDatabaseData(AValue: TDatabaseData);
    procedure SetServerData(AValue: TServerData);
  protected
    function GetEmbeddedMode: boolean; override;
    procedure ConnectServicesAPI; override;
    function CallLoginDlg(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult; override;
    procedure Notification(AComponent: TComponent;
          Operation: TOperation); override;
  public
    function Connect: boolean; override;
    procedure Disconnect; override;
    procedure Reconnect;
    procedure PerformUpgrade(aOnUpgradeDone: TNotifyEvent);
    property ServerData: TServerData read FServerData write SetServerData;
    property DatabaseData: TDatabaseData read FDatabaseData write SetDatabaseData;
    property SchemaVersion: integer read FSchemaVersion;
  end;

var
  DBADatabaseData: TDBADatabaseData;

implementation

{$R *.lfm}

uses PasswordCacheUnit, LocalDataModule, IBInternals, IBMessages, IBUtils,
  IBXCreateDatabaseFromSQLDlgUnit, DBACreateDatabaseDlgUnit,
  IBXUpgradeDatabaseDlg, IBXUpgradeConfFile;

{ TDBADatabaseData }

procedure TDBADatabaseData.DataModuleCreate(Sender: TObject);
begin
  DBDataModule := self;
end;

procedure TDBADatabaseData.IBDatabase1AfterConnect(Sender: TObject);
begin
  CurrentTransaction.Active := true;
  DataBaseQuery.Active := true;
  AttmtQuery.Active := true;
  FDatabaseData.Attachment := IBDatabase1.Attachment;
  if FDatabaseData.AppID = 0 then
    FDatabaseData.AppID := IdentifyDatabase;
  FDatabaseData.DefaultUserName := DBUserName;
  if FNewPassword then
    PasswordCache.SavePassword(DBUserName,ExtractDatabaseName(IBDatabase1.DatabaseName),
                               ExtractServerName(IBDatabase1.DatabaseName),
                               FDatabasePassword,
                               SecurityDatabase);
  FNewPassword := false;
  FSchemaVersion := GetSchemaVersion;
  inherited;
end;

procedure TDBADatabaseData.IBDatabase1AfterDisconnect(Sender: TObject);
begin
  inherited;
  FDatabasePassword := '';
end;

procedure TDBADatabaseData.IBDatabase1CreateDatabase(Sender: TObject);
var Schema: string;
begin
  if FNewPassword then
    PasswordCache.SavePassword(DBUserName,ExtractDatabaseName(IBDatabase1.DatabaseName),
                               ExtractServerName(IBDatabase1.DatabaseName),
                               FDatabasePassword,
                               SecurityDatabase);
  if LocalData.AppDatabases.Locate('ID',FDatabaseData.AppID,[]) then
  begin
    Schema := localData.AppDatabases.FieldByName('schema').AsString;
    if IsGBakFile(Schema) then
    begin
      ConnectServicesAPI;
      IBDatabase1.Attachment.Disconnect;
      try
        DBACreateDatabaseDlgUnit.RestoreDatabaseFromArchive(IBXClientSideRestoreService1,Schema)
      finally
        IBDatabase1.Attachment.Connect;
      end;
    end
    else
      IBXCreateDatabaseFromSQLDlgUnit.CreateNewDatabase(IBDatabase1,Schema);
    PerformUpgrade(nil);
  end
  else
    raise Exception.CreateFmt('Unable to locate database ID %d',[FDatabaseData.AppID]);
end;

procedure TDBADatabaseData.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
begin
  inherited;
  FDatabasePassword := LoginParams.Values['password'];
  FNewPassword := true;
end;

procedure TDBADatabaseData.IBXScript1CreateDatabase(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := FDatabaseData.DatabasePath;
end;

procedure TDBADatabaseData.IBXServicesConnection1AfterConnect(Sender: TObject);
begin
  inherited;
  FServerData.ServiceIntf := IBXServicesConnection1.ServiceIntf;
  ServerData.DefaultUserName := FServiceUserName;
  if not IBDatabase1.Connected and (ServerData.DomainName <> '') and FNewServicePassword then
    PasswordCache.SavePassword(FServiceUserName,ServerData.DomainName,FServicePassword);
  FNewServicePassword := false;
end;

procedure TDBADatabaseData.IBXServicesConnection1AfterDisconnect(Sender: TObject
  );
begin
  FServiceConnectCount := 0;
end;

procedure TDBADatabaseData.IBXServicesConnection1Login(
  Service: TIBXServicesConnection; var aServerName: string;
  LoginParams: TStrings);
var prompt: boolean;
  DisplayName: string;
begin
  FServicePassword := '';
  if ServerData.DomainName <> '' then
  begin
    prompt := true;
    if IBDatabase1.Connected and (FServiceConnectCount = 0) then
    begin
      prompt := not PasswordCache.GetPassword(FServiceUserName,ExtractDatabaseName(IBDatabase1.DatabaseName),aServerName,FServicePassword);
      if not prompt then
        LoginParams.Values['expected_db'] := ExtractDatabaseName(IBDatabase1.DatabaseName)
      else
      if not FSecDBException then
        prompt := not PasswordCache.GetPassword(FServiceUserName,aServerName,FServicePassword);
    end
    else
    if (FServiceConnectCount < 2) and not FSecDBException then
      prompt := not PasswordCache.GetPassword(FServiceUserName,aServerName,FServicePassword);

    if prompt then
      begin
        if not assigned(IBGUIInterface) then
          raise Exception.Create('Service Login Interface Missing');

        DisplayName := ServerData.ServerName;
        if not IBGUIInterface.ServerLoginDialog(DisplayName, FServiceUsername, FServicePassword) then
        begin
          ServerData := nil;
          IBError(ibxeOperationCancelled, [nil]);
        end;
      end;
  end;
  LoginParams.Values['user_name'] := FServiceUserName;
  LoginParams.Values['password'] := FServicePassword;
  FNewServicePassword := true;
  Inc(FServiceConnectCount);
  FSecDBException := false;
end;

procedure TDBADatabaseData.IBXServicesConnection1SecurityContextException(
  Service: TIBXServicesConnection; var aAction: TSecContextAction);
begin
  FSecDBException := true;
end;

function TDBADatabaseData.IdentifyDatabase: integer;
begin
  Result := 0;
  with LocalData.AppDatabases do
  begin
    First;
    while not EOF do
    begin
      if IBDatabase1.Attachment.OpenCursorAtStart('Select ' +
                                          FieldByName('Signature').AsString + ' From RDB$Database')[0].AsInteger <> 0  then
      begin
        Result := FieldByName('ID').AsInteger;
        break;
      end;
      Next;
    end;
  end;
end;

function TDBADatabaseData.GetSchemaVersion: integer;
begin
  if (FDatabaseData.DBControlTable <> '') and HasTable(FDatabaseData.DBControlTable) then
    Result := IBDatabase1.Attachment.OpenCursorAtStart('Select VersionNo From ' + FDatabaseData.DBControlTable)[0].AsInteger
  else
    Result := 0;
end;

function TDBADatabaseData.HasTable(aTableName: string): boolean;
begin
  Result := IBDatabase1.Attachment.OpenCursorAtStart(
         Format('Select Case when Exists(Select * From RDB$RELATIONS Where RDB$RELATION_NAME = ''%s'') then 1 else 0 end From RDB$DATABASE',[aTableName]))
         [0].AsInteger = 1;
end;

procedure TDBADatabaseData.GetSchemaVersion2(Sender: TObject;
  var VersionNo: integer);
begin
  VersionNo := GetSchemaVersion;
end;

procedure TDBADatabaseData.SetServerData(AValue: TServerData);
begin
  if FServerData = AValue then Exit;
  if FServerData <> nil then
    FServerData.RemoveFreeNotification(self);
  FServerData := AValue;
  FServiceConnectCount := 0;
  if FServerData = nil then
  begin
    IBXServicesConnection1.Connected := false;
    Exit;
  end;
  FServerData.FreeNotification(self);

  IBXServicesConnection1.ServiceIntf := ServerData.ServiceIntf;
  while not IBXServicesConnection1.Connected do
  begin
    if ServerData.DomainName = '' then
      IBXServicesConnection1.Protocol := Local
    else
       IBXServicesConnection1.Protocol := inet;
    IBXServicesConnection1.ServerName := ServerData.DomainName;
    if ServerData.ConnectAsUser then
      FServiceUserName := ''
    else
      FServiceUserName := ServerData.DefaultUserName;
    try
      IBXServicesConnection1.Connected := true;
    except
      on E:EIBClientError do
        break;
      on E: Exception do
        Application.ShowException(E);
    end;
  end;
  if IBDatabase1.Connected then
  begin
    IBXServicesConnection1.ServiceIntf := nil;
    IBXServicesConnection1.SetServiceIntf(ServerData.ServiceIntf,IBDatabase1);
  end
end;

function TDBADatabaseData.GetEmbeddedMode: boolean;
begin
  if DatabaseData <> nil then
    Result := inherited GetEmbeddedMode
  else
    Result := IBXServicesConnection1.Connected and (ServerData.DomainName = '');
end;

procedure TDBADatabaseData.SetDatabaseData(AValue: TDatabaseData);
begin
  if FDatabaseData = AValue then Exit;
  FServerData := nil;
  FDatabaseData := AValue;
  if FDatabaseData = nil then
     IBDatabase1.Attachment := nil
  else
  begin
    IBDatabase1.Attachment := FDatabaseData.Attachment;
    if not IBDatabase1.Connected then
    begin
      FDBConnectCount := 0;
      IBDatabase1.DatabaseName := 'inet://'+ FDatabaseData.ServerData.DomainName + '/' + FDatabaseData.DatabasePath;
      if FDatabaseData.ConnectAsUser then
         IBDatabase1.Params.Values['user_name'] := ''
      else
        IBDatabase1.Params.Values['user_name'] := FDatabaseData.DefaultUserName;
      FDatabasePassword := '';
      Connect;
    end
    else
      ConnectServicesAPI;
  end;
end;

procedure TDBADatabaseData.ConnectServicesAPI;
begin
  ServerData := nil;
  if SecurityDatabase <> FDatabaseData.ServerData.SecDatabase then
    FDatabaseData.SetSecDatabase(SecurityDatabase);
  ServerData := FDatabaseData.ServerData;
end;

function TDBADatabaseData.CallLoginDlg(var aDatabaseName, aUserName,
  aPassword: string; var aCreateIfNotExist: boolean): TModalResult;
var prompt: boolean;
begin
  prompt := true;
  aCreateIfNotExist := FDatabaseData.CreateIfNotExists;
  if (FDBConnectCount = 0) and assigned(FDatabaseData.ServerData) then
  begin
      prompt := not PasswordCache.GetPassword(aUserName,ExtractDatabaseName(aDatabaseName),
                                               FDatabaseData.ServerData.DomainName,aPassword,
                                               not FDatabaseData.UsesDefaultSecDatabase);
      if prompt and FDatabaseData.UsesDefaultSecDatabase then
        prompt := not PasswordCache.GetPassword(aUserName,FDatabaseData.ServerData.DomainName,aPassword);
  end;

  Inc(FDBConnectCount);
  if prompt then
  begin
    if assigned(IBGUIInterface) and
      IBGUIInterface.LoginDialogEx(aDatabaseName, aUserName, aPassword,false) then
        Result := mrOK
      else
        Result := mrCancel;
  end
  else
    Result := mrOK;
end;

procedure TDBADatabaseData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FServerData) and (Operation = opRemove) then
    FServerData := nil;
end;

function TDBADatabaseData.Connect: boolean;
begin
  if DatabaseData <> nil then
    Result := inherited Connect
  else
    Result := true;
end;

procedure TDBADatabaseData.Disconnect;
begin
  inherited Disconnect;
  if FServerData <> nil then
    IBXServicesConnection1.ServiceIntf := FServerData.ServiceIntf;
end;

procedure TDBADatabaseData.Reconnect;
begin
  Disconnect;
  if DatabaseData <> nil then
    Connect
  else
    IBXServicesConnection1.Connected := true;
end;

procedure TDBADatabaseData.PerformUpgrade(aOnUpgradeDone: TNotifyEvent);
var UpgradeConfFile: TUpgradeConfFile;
begin
  UpgradeConfFile := TUpgradeConfFile.Create(FDatabaseData.UpgradeConfFile);
  try
    RunUpgradeDatabase(IBDatabase1,nil,UpgradeConfFile,'','',FDatabaseData.CurrentVersion,
                        @GetSchemaVersion2,aOnUpgradeDone);
  finally
    UpgradeConfFile.Free;
  end;
  FSchemaVersion := GetSchemaVersion;
end;

end.

