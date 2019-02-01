unit ServerDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBXServices, IB, Forms, Dialogs, Controls, RegisterServerDlgUnit;

type
  TServerDataList = class;

  { TServerData }

  TServerData = class
  private
    const
      sqlInsert = 'Insert into SERVERS(ServerId,ServerName,DomainName,DefaultUserName) ' +
                  'Values(Gen_ID(UniqueID,1),:ServerName,:DomainName,:DefaultUserName) '+
                  'Returning(ServerID)';
      sqlRefresh = 'Select * From SERVERS A Where A.SERVERID = ?';
      sqlUpdateServerName = 'Update SERVERS Set ServerName = ? Where ServerID = ?';
      sqlUpdateUserName = 'Update SERVERS Set DefaultUserName = ? Where ServerID = ?';
      sqlUpdateDomainName = 'Update SERVERS Set DomainName = ? Where ServerID = ?';
  private
    FConnectAsUser: boolean;
   FDomainName: string;
   FOwner: TServerDataList;
   FSecDatabase: string;
   FServerID: integer;
   FServerName: string;
   FDefaultUserName: string;
   FServiceIntf: IServiceManager;
   procedure SetDomainName(AValue: string);
   procedure SetDefaultUserName(AValue: string);
   procedure SetServerName(AValue: string);
  public
   constructor Create(aOwner: TServerDataList; aServerName,
     aDomainName, aDefaultUserName: string); overload;
   constructor Create(aOwner: TServerDataList; aServerID: integer; aSecDatabase: string='Default'); overload;
   procedure Refresh;
   procedure Disconnect;
   function Reconnect: boolean;
   function ConnectAs: boolean;
   function Select(Reselect: boolean=false): boolean;
   property Owner: TServerDataList read FOwner;
   property ServerID: integer read FServerID;
   property ServerName: string read FServerName write SetServerName;
   property DomainName: string read FDomainName write SetDomainName;
   property DefaultUserName: string read FDefaultUserName write SetDefaultUserName;
   property ConnectAsUser: boolean read FConnectAsUser;
   property ServiceIntf: IServiceManager read FServiceIntf write FServiceIntf;
   property SecDatabase: string read FSecDatabase;
  end;

  { TServerDataList }

  TServerDataList = class
   private
      const
        sqlListServers = 'Select A.SERVERID, A.SERVERNAME, A.DOMAINNAME, A.DefaultUserName From SERVERS A order by 1';
  private
    FServerData: array of TServerData;
    FServerDataLoaded: boolean;
    FLoading: boolean;
    function FindServerData(aServerID: integer): integer;
    function GetServerData(aServerID: integer): TServerData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadServerData;
    procedure Clear;
    function Add(aServerName: string): integer; overload;
    function Add(aServerID: integer): integer; overload;
    procedure Refresh;
    procedure Remove(aServerID: integer);
    function Update(aServerID: integer; dlg: TRegisterServerDlg): TServerData;
    property ServerData[ServerID: integer]: TServerData read GetServerData; default;
  end;

var
    ServerDataList: TServerDataList;

implementation

uses IBTypes, PasswordCacheUnit, LocalDataModule, IBSQL, DBADataModule;

{ TServerDataList }

function TServerDataList.FindServerData(aServerID: integer): integer;
var i: integer;
begin
  LoadServerData;
  Result := -1;
  for i := 0 to Length(FServerData) - 1 do
    if FServerData[i].ServerID = aServerID then
    begin
      Result := i;
      break;
    end;
end;

function TServerDataList.GetServerData(aServerID: integer): TServerData;
var index: integer;
begin
  index := FindServerData(aServerID);
  if index <> -1 then
    Result := FServerData[index]
  else
    Result := nil;
end;

procedure TServerDataList.LoadServerData;
var index: integer;
begin
  if FServerDataLoaded then Exit;

  FLoading := true;
  try
    SetLength(FServerData,0);
    with TIBSQL.Create(nil) do
    try
      SQL.Text := sqlListServers;
      Database := LocalData.LocalDatabase;
      Transaction := LocalData.IBTransaction;
      Transaction.Active := true;
      ExecQuery;
      try
        while not EOF do
        begin
          SetLength(FServerData,Length(FServerData)+1);
          index := Length(FServerData)-1;
          FServerData[index] := TServerData.Create(self,FieldByName('ServerID').AsInteger);
          with FServerData[index] do
          begin
            ServerName := FieldByName('ServerName').AsString;
            DomainName := FieldByName('DomainName').AsString;
            DefaultUserName := FieldByName('DefaultUserName').AsString;
          end;
          Next;
        end;
      finally
        Close;
      end;
    finally
      Free
    end;
  finally
    FLoading := false;
  end;
  FServerDataLoaded := true;
end;

procedure TServerDataList.Clear;
var i: integer;
begin
  for i := 0 to Length(FServerData) - 1 do
    FServerData[i].Free;
  SetLength(FServerData,0);
  FServerDataLoaded := false;
end;

function TServerDataList.Add(aServerName: string): integer;
begin
  SetLength(FServerData,Length(FServerData)+1);
  Result := Length(FServerData)-1;
//    writeln('Add Server ', aServerName, ', index = ', index);
  FServerData[Result] := TServerData.Create(self,aServerName,aServerName,'');
end;

function TServerDataList.Add(aServerID: integer): integer;
begin
  SetLength(FServerData,Length(FServerData)+1);
  Result := Length(FServerData)-1;
//    writeln('Add Server ',aServerID, ', index = ', index);
  FServerData[Result] := TServerData.Create(self,aServerID);
end;

procedure TServerDataList.Refresh;
begin
  Clear;
  LoadServerData;
end;

procedure TServerDataList.Remove(aServerID: integer);
var index, j: integer;
begin
  index := FindServerData(aServerID);
  if index <> -1 then
  begin
    FServerData[index].Free;
    for j := index to Length(FServerData) - 2 do
      FServerData[j] := FServerData[j + 1];
    SetLength(FServerData,Length(FServerData)-1);
  end;
end;

function TServerDataList.Update(aServerID: integer; dlg: TRegisterServerDlg
  ): TServerData;
var index, j: integer;
begin
  index := FindServerData(aServerID);
  if index <> -1 then
  begin
    Result := FServerData[index];
    Result.Refresh;
  end
  else
  begin
    index := Add(aServerID);
    Result := FServerData[index];
  end;
  if dlg <> nil then
  with Result do
  begin
    ServerName := dlg.ServerName.Text;
    DomainName := dlg.DomainName.Text;
    DefaultUserName := dlg.DefaultUserName.Text;
  end;
end;

constructor TServerDataList.Create;
begin
  inherited;
  SetLength(FServerData,0);
end;

destructor TServerDataList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TServerData }

procedure TServerData.SetDomainName(AValue: string);
begin
  if FDomainName = AValue then Exit;
  FDomainName := AValue;
  if not FOwner.FLoading then
  begin
    Disconnect;
    with LocalData do
      LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateDomainName,[FDomainName,FServerID]);
    Select(true);
  end;
end;

procedure TServerData.SetDefaultUserName(AValue: string);
begin
  if FDefaultUserName = AValue then Exit;
  FDefaultUserName := AValue;
  if not FOwner.FLoading then
  with LocalData do
    LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateUserName,[FDefaultUserName,FServerID]);
end;

procedure TServerData.SetServerName(AValue: string);
begin
  if FServerName = AValue then Exit;
  FServerName := AValue;
  {if not FOwner.FLoading then
  with LocalData do
    LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateServerName,[FServerName,FServerID]);}
end;

constructor TServerData.Create(aOwner: TServerDataList; aServerName,
  aDomainName, aDefaultUserName: string);
begin
  inherited Create;
  FOwner := aOwner;
  FSecDatabase := 'Default';
  FServerName := aServerName;
  FDefaultUserName := aDefaultUserName;
  if aDomainName = '' then
    FDomainName := aServerName
  else
    FDomainName := aDomainName;
  FServerID := LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlInsert,
                [FServerName,FDomainName,FDefaultUserName])[0].AsInteger;
  FServiceIntf := nil;
end;

constructor TServerData.Create(aOwner: TServerDataList; aServerID: integer;
  aSecDatabase: string);
begin
  inherited Create;
  FOwner := aOwner;
  FSecDatabase := aSecDatabase;
  FServerID := aServerID;
  Refresh;
  FServiceIntf := nil;
end;

procedure TServerData.Refresh;
var QueryResults: IResultSet;
begin
  QueryResults := LocalData.LocalDatabase.Attachment.OpenCursorAtStart(sqlRefresh,[FServerID]);
  with QueryResults do
  if not IsEOF then
  begin
    FServerName := ByName('ServerName').AsString;
    FDomainName := ByName('DomainName').AsString;
    FDefaultUserName := ByName('DefaultUserName').AsString;
  end;
end;

procedure TServerData.Disconnect;
begin
  if DBADatabaseData.ServerData = self then
  begin
     DBADatabaseData.DatabaseData := nil;
     DBADatabaseData.ServerData := nil;
  end;
  FServiceIntf := nil;
end;

function TServerData.Reconnect: boolean;
begin
  Result := Select(true);
end;

function TServerData.ConnectAs: boolean;
begin
  FConnectAsUser := true;
  try
    Result := Reconnect;
  finally
    FConnectAsUser := false;
  end;
end;

function TServerData.Select(Reselect: boolean): boolean;
begin
  DBADatabaseData.DatabaseData := nil;
  if Reselect then
    DBADatabaseData.ServerData := nil;
  DBADatabaseData.ServerData := self;
  Result := DBADatabaseData.IBXServicesConnection1.Connected;
end;

initialization
  ServerDataList := TServerDataList.Create;

finalization
  if assigned(ServerDataList) then ServerDataList.Free;



end.

