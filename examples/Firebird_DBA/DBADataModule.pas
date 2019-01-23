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
    procedure IBDatabase1CreateDatabase(Sender: TObject);
    procedure IBXScript1CreateDatabase(Sender: TObject;
      var DatabaseFileName: string);
    procedure IBXServicesConnection1AfterConnect(Sender: TObject);
    procedure IBXServicesConnection1AfterDisconnect(Sender: TObject);
    procedure IBXServicesConnection1Login(Service: TIBXServicesConnection;
      var aServerName: string; LoginParams: TStrings);
  private
    FDatabaseData: TDatabaseData;
    FServiceUserName: string;
    FServicePassword: string;
    FDatabaseUserName: string;
    FDatabasePassword: string;
    FServiceConnectCount: integer;
    FServerData: TServerData;
    function IdentifyDatabase: integer;
    procedure SetDatabaseData(AValue: TDatabaseData);
    procedure SetServerData(AValue: TServerData);
  protected
    procedure ConnectServicesAPI; override;
    function CallLoginDlg(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult; override;
  public
    function Connect: boolean; override;
    property ServerData: TServerData read FServerData write SetServerData;
    property DatabaseData: TDatabaseData read FDatabaseData write SetDatabaseData;
  end;

var
  DBADatabaseData: TDBADatabaseData;

implementation

{$R *.lfm}

uses PasswordCacheUnit, LocalDataModule, IBTypes;

{ TDBADatabaseData }

procedure TDBADatabaseData.DataModuleCreate(Sender: TObject);
begin
  DBDataModule := self;
end;

procedure TDBADatabaseData.IBDatabase1AfterConnect(Sender: TObject);
begin
  FDatabaseData.Attachment := IBDatabase1.Attachment;
  FDatabaseData.DefaultUserName := FDatabaseUserName;
  PasswordCache.SavePassword(FDatabaseUserName,IBDatabase1.DatabaseName,FDatabasePassword);
  if FDatabaseData.AppID = 0 then
    FDatabaseData.AppID := IdentifyDatabase;
  ServerData := DatabaseData.ServerData;
  inherited;
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

procedure TDBADatabaseData.IBXScript1CreateDatabase(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := FDatabaseData.DatabasePath;
end;

procedure TDBADatabaseData.IBXServicesConnection1AfterConnect(Sender: TObject);
begin
  FServerData.ServiceIntf := IBXServicesConnection1.ServiceIntf;
  ServerData.DefaultUserName := FServiceUserName;
  if IBDatabase1.Connected then
  begin
    DatabaseQuery.Active := true;
    PasswordCache.SavePassword(FServiceUserName,FDatabaseData.DatabasePath,ServerData.DomainName,FServicePassword,
      Trim(DatabaseQuery.FieldByName('MON$SEC_DATABASE').AsString))
  end
  else
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
begin
  FServicePassword := '';
  prompt := true;
  if IBDatabase1.Connected and (FServiceConnectCount = 0) then
      prompt := not PasswordCache.GetPassword(FServiceUserName,IBDatabase1.DatabaseName,aServerName,FServicePassword)
  else
  if FServiceConnectCount < 2 then
    prompt := not PasswordCache.GetPassword(FServiceUserName,aServerName,FServicePassword);

  if prompt then
    begin
      if not assigned(IBGUIInterface) then
        raise Exception.Create('Service Login Interface Missing');

      aServerName := ServerData.ServerName;
      if not IBGUIInterface.ServerLoginDialog(aServerName, FServiceUsername, FServicePassword) then
        Exit;
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
  if IBDatabase1.Connected then
    IBXServicesConnection1.SetServiceIntf(ServerData.ServiceIntf,IBDatabase1)
  else
    IBXServicesConnection1.ServiceIntf := ServerData.ServiceIntf;
  if not IBXServicesConnection1.Connected then
  begin
    if ServerData.DomainName = '' then
      IBXServicesConnection1.Protocol := Local
    else
       IBXServicesConnection1.Protocol := inet;
    IBXServicesConnection1.ServerName := ServerData.DomainName;
    FServiceUserName := ServerData.DefaultUserName;
    IBXServicesConnection1.Connected := true;
  end;
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
      IBDatabase1.DatabaseName := FDatabaseData.DatabasePath;
      IBDatabase1.CreateIfNotExists := FDatabaseData.CreateIfNotExists;
      FDatabaseUserName := FDatabaseData.DefaultUserName;
      FDatabasePassword := '';
      Connect;
    end;
  end;
end;

procedure TDBADatabaseData.ConnectServicesAPI;
begin
  //
end;

function TDBADatabaseData.CallLoginDlg(var aDatabaseName, aUserName,
  aPassword: string; var aCreateIfNotExist: boolean): TModalResult;
begin
  Result := inherited CallLoginDlg(aDatabaseName, aUserName, aPassword, aCreateIfNotExist);
  FDatabaseUserName := aUserName;
  FDatabasePassword := aPassword;
end;

function TDBADatabaseData.Connect: boolean;
begin
  if DatabaseData <> nil then
    Result := inherited Connect
  else
    Result := true;
end;

end.

