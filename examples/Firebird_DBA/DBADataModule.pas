unit DBADataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, db, memds, DataModule,
  IBXServices, IBDatabase, IBSQL, IBQuery, IBCustomDataSet, IBUpdate,
  IBDatabaseInfo, ibxscript, ServerDataUnit, DatabaseDataUnit,IB;

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
  private
    FDatabaseData: TDatabaseData;
    FServicePassword: string;
    FDatabasePassword: string;
    FServiceConnectCount: integer;
    FDBConnectCount: integer;
    FServerData: TServerData;
    function IdentifyDatabase: integer;
    procedure SetDatabaseData(AValue: TDatabaseData);
    procedure SetServerData(AValue: TServerData);
  protected
    function GetEmbeddedMode: boolean; override;
    procedure ConnectServicesAPI; override;
    function CallLoginDlg(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult; override;
  public
    function Connect: boolean; override;
    procedure Disconnect; override;
    property ServerData: TServerData read FServerData write SetServerData;
    property DatabaseData: TDatabaseData read FDatabaseData write SetDatabaseData;
  end;

var
  DBADatabaseData: TDBADatabaseData;

implementation

{$R *.lfm}

uses PasswordCacheUnit, LocalDataModule, IBTypes, FBMessages, IBUtils;

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
  PasswordCache.SavePassword(DBUserName,ExtractDatabaseName(IBDatabase1.DatabaseName),
                             ExtractServerName(IBDatabase1.DatabaseName),
                             FDatabasePassword,
                             SecurityDatabase);
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
  if LocalData.AppDatabases.Locate('ID',FDatabaseData.AppID,[]) then
  begin
    Schema := localData.AppDatabases.FieldByName('schema').AsString;
    if UpperCase(ExtractFileExt(Schema)) = '.GBK' then
       IBXClientSideRestoreService1.RestoreFromFile(Schema,nil)
    else
      IBXScript1.ExecSQLScript(Schema);
  end;
end;

procedure TDBADatabaseData.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
begin
  inherited;
  FDatabasePassword := LoginParams.Values['password'];
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
  if not IBDatabase1.Connected and (ServerData.DomainName <> '') then
    PasswordCache.SavePassword(FServiceUserName,ServerData.DomainName,FServicePassword)
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
      prompt := not PasswordCache.GetPassword(FServiceUserName,ExtractDatabaseName(IBDatabase1.DatabaseName),aServerName,FServicePassword)
    else
    if FServiceConnectCount < 2 then
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
  Inc(FServiceConnectCount);
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

procedure TDBADatabaseData.SetServerData(AValue: TServerData);
begin
  if FServerData = AValue then Exit;
  FServerData := AValue;
  FServiceConnectCount := 0;
  if FServerData = nil then
  begin
    IBXServicesConnection1.Connected := false;
    Exit;
  end;

  IBXServicesConnection1.ServiceIntf := ServerData.ServiceIntf;
  while not IBXServicesConnection1.Connected do
  begin
    if ServerData.DomainName = '' then
      IBXServicesConnection1.Protocol := Local
    else
       IBXServicesConnection1.Protocol := inet;
    IBXServicesConnection1.ServerName := ServerData.DomainName;
    FServiceUserName := ServerData.DefaultUserName;
    try
      IBXServicesConnection1.Connected := true;
    except on E:EIBClientError do
      break;
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
      IBDatabase1.CreateIfNotExists := FDatabaseData.CreateIfNotExists;
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
  ServerData := FDatabaseData.ServerData;
end;

function TDBADatabaseData.CallLoginDlg(var aDatabaseName, aUserName,
  aPassword: string; var aCreateIfNotExist: boolean): TModalResult;
var prompt: boolean;
begin
  prompt := true;
  if (FDBConnectCount = 0) and assigned(ServerData) then
  begin
      prompt := not (PasswordCache.GetPassword(aUserName,ExtractDatabaseName(aDatabaseName),ServerData.DomainName,aPassword) or
                     PasswordCache.GetPassword(aUserName,ServerData.DomainName,aPassword));
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

end.

