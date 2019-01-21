unit DBADataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DataModule, IBXServices,
  ServerDataUnit,DatabaseDataUnit;

type

  { TDBADatabaseData }

  TDBADatabaseData = class(TDatabaseData)
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBXServicesConnection1AfterConnect(Sender: TObject);
    procedure IBXServicesConnection1AfterDisconnect(Sender: TObject);
    procedure IBXServicesConnection1Login(Service: TIBXServicesConnection;
      var aServerName: string; LoginParams: TStrings);
  private
    FDatabaseData: TDatabaseData;
    FServiceUserName: string;
    FServicePassword: string;
    FServiceConnectCount: integer;
    FServerData: TServerData;
    procedure SetDatabaseData(AValue: TDatabaseData);
    procedure SetServerData(AValue: TServerData);
  protected
    procedure ConnectServicesAPI; override;
    function CallLoginDlg(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult; override;
  public
    property ServerData: TServerData read FServerData write SetServerData;
    property DatabaseData: TDatabaseData read FDatabaseData write SetDatabaseData;
  end;

var
  DBADatabaseData: TDBADatabaseData;

implementation

{$R *.lfm}

uses PasswordCacheUnit;

{ TDBADatabaseData }

procedure TDBADatabaseData.IBDatabase1AfterConnect(Sender: TObject);
begin
  inherited;
  FDatabaseData.Attachment := IBDatabase1.Attachment;
  FDatabaseData.DefaultUserName := FDatabaseUserName;
  PasswordCache.SavePassword(FDatabaseUserName,IBDatabase1.DatabaseName,FDatabasePassword);
end;

procedure TDBADatabaseData.IBXServicesConnection1AfterConnect(Sender: TObject);
begin
  FServerData.ServiceIntf := IBXServicesConnection1.ServiceIntf;
  ServerData.DefaultUserName := FServiceUserName;
  if IBDatabase1.Connected then
    PasswordCache.SavePassword(FServiceUserName,IBDatabase1.DatabaseName,FServicePassword)
  else
    PasswordCache.SavePassword(FServiceUserName,FServicePassword)
end;

procedure TDBADatabaseData.IBXServicesConnection1AfterDisconnect(Sender: TObject
  );
begin
  FConnectCount := 0;
end;

procedure TDBADatabaseData.IBXServicesConnection1Login(
  Service: TIBXServicesConnection; var aServerName: string;
  LoginParams: TStrings);
var prompt: boolean;
begin
  FServicePassword := '';
  prompt := true;
  if IBDatabase1.Connected and (FConnectCount = 0) then
      prompt := not PasswordCache.GetPassword(FServiceUserName,IBDatabase1.DatabaseName,aServerName,FServicePassword)
  else
  if FConnectCount < 2 then
    prompt := not PasswordCache.GetPassword(FServiceUserName,aServerName,FServicePassword);

  if prompt then
    begin
      if not assigned(IBGUIInterface) then
        raise Exception.Create('Service Login Interface Missing');

      if not IBGUIInterface.ServerLoginDialog(ServerName, FServiceUsername, FServicePassword) then
        break;
    end;
  LoginParams.Values['user_name'] := FServiceUserName;
  LoginParams.Values['password'] := FServicePassword;
  Inc(FServiceConnectCount);
end;

procedure TDBADatabaseData.SetServerData(AValue: TServerData);
begin
  if FServerData = AValue then Exit;
  FServerData := AValue;
  FServiceConnectCount := 0;
  IBXServicesConnection1.ServiceIntf := ServerData.ServiceIntf;
  if not IBXServicesConnection1.Connected then
  begin
    IBXServicesConnection1.ServerName := ServerData.DomainName;
    FServiceUserName := ServerData.DefaultUserName;
    IBXServicesConnection1.Connected := true;
  end;
end;

procedure TDBADatabaseData.SetDatabaseData(AValue: TDatabaseData);
begin
  if FDatabaseData = AValue then Exit;
  FDatabaseData := AValue;
  IBDatabase1.Attachment := FDatabaseData.Attachment;
  if not IBDatabase1.Connected then
  begin
    IBDatabase1.DatabaseName := FDatabaseData.DatabasePath;
    FDatabaseUserName := FDatabaseData.DefaultUserName;
    FDatabasePassword := '';
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBADatabaseData.ConnectServicesAPI;
begin
  ServerData := DatabaseData.ServerData;
end;

function TDBADatabaseData.CallLoginDlg(var aDatabaseName, aUserName,
  aPassword: string; var aCreateIfNotExist: boolean): TModalResult;
begin
  Result := inherited CallLoginDlg(aDatabaseName, aUserName, aPassword, aCreateIfNotExist);
  FDatabaseUserName := aUserName;
  FDatabasePassword := aPassword;
end;

end.

