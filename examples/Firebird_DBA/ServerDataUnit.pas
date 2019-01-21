unit ServerDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBXServices, IB, Forms, Dialogs, Controls;

type
  TServerDataList = class;

  { TServerData }

  TServerData = class
  private
    const
      sqlInsert = 'Insert into SERVERS(ServerId,ServerName,DomainName,DefaultUserName) ' +
                  'Values(Gen_ID(UniqueID,1),:ServerName,:DomainName,:DefaultUserName) '+
                  'Returning(ServerID)';
      sqlRefresh = 'Select * From SERVERS A Where A.SERVERID = :ServerID';
      sqlUpdateServerName = 'Update SERVERS Set ServerName = ? Where ServerID = ?';
      sqlUpdateUserName = 'Update SERVERS Set DefaultUserName = ? Where ServerID = ?';
      sqlUpdateDomainName = 'Update SERVERS Set DomainName = ? Where ServerID = ?';
  private
   FDomainName: string;
   FOwner: TServerDataList;
   FServerID: integer;
   FServerName: string;
   FDefaultUserName: string;
   FServiceIntf: IServiceManager;
   procedure SetDomainName(AValue: string);
   procedure SetFDefaultUserName(AValue: string);
   procedure SetServerName(AValue: string);
  public
   constructor Create(aOwner: TServerDataList; aServerName,
     aDomainName, aDefaultUserName: string);
   procedure Refresh;
   procedure Select;
   property ServerID: integer read FServerID;
   property ServerName: string read FServerName write SetServerName;
   property DomainName: string read FDomainName write SetDomainName;
   property DefaultUserName: string read FDefaultUserName write SetFDefaultUserName;
   property ServiceIntf: IServiceManager read FServiceIntf write FServiceIntf;
  end;

  { TServerDataList }

  TServerDataList = class
   private
      const
        sqlListServers = 'Select A.SERVERID, A.SERVERNAME, A.DOMAINNAME, A.DefaultUserName From SERVERS A order by 1';
  private
    FServerData: array of TServerData;
    FServerDataLoaded: boolean;
    function FindServerData(aServerID: integer): integer;
    function GetServerData(aServerID: integer): TServerData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadServerData;
    procedure Clear;
    procedure Add(aServerID: integer; aServerName: string);
    procedure Refresh;
    procedure Remove(aServerID: integer);
    property ServerData[ServerID: integer]: TServerData read GetServerData; default;
  end;

var
    ServerDataList: TServerDataList;

implementation

uses IBTypes, IBXForms, IBSQL, PasswordCacheUnit, LocalDataModule;

{ TServerDataList }

function TServerDataList.FindServerData(aServerID: integer): integer;
var i: integer;
begin
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
        FServerData[index] := TServerData.Create(self,FieldByName('ServerID').AsInteger,
                                                      FieldByName('ServerName').AsString,
                                                      FieldByName('DomainName').AsString,
                                                      FieldByName('DefaultUserName').AsString);
        Next;
      end;
    finally
      Close;
    end;
  finally
    Free
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

procedure TServerDataList.Add(aServerID: integer; aServerName: string);
var index: integer;
begin
  index := FindServerData(aServerID);
  if index = -1 then
  begin
    SetLength(FServerData,Length(FServerData)+1);
    index := Length(FServerData)-1;
//    writeln('Add Server ',aServerID, ',', aServerName, ', index = ', index);
    FServerData[index] := TServerData.Create(self,aServerID,aServerName,'','');                                                                                               ,11
  end;
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

constructor TServerDataList.Create;
begin
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
  FServiceIntf := nil;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateDomainName,[FDomainName,FServerID]);
end;

procedure TServerData.SetFDefaultUserName(AValue: string);
begin
  if FDefaultUserName = AValue then Exit;
  FDefaultUserName := AValue;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateUserName,[FDefaultUserName,FServerID]);
end;

procedure TServerData.SetServerName(AValue: string);
begin
  if FServerName = AValue then Exit;
  FServerName := AValue;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateServerName,[FServerName,FServerID]);
end;

constructor TServerData.Create(aOwner: TServerDataList; aServerName,
  aDomainName, aDefaultUserName: string);
begin
  FOwner := aOwner;
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

procedure TServerData.Select;
begin
  DBADataModule.IBDatabase1.Connected := false;
  DBADataModule.ServerData := self;
end;

initialization
  ServerDataList := TServerDataList.Create;

finalization
  if assigned(ServerDataList) then ServerDataList.Free;



end.

