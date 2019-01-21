unit DatabaseDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IB, ServerDataUnit;

type
  TDatabaseDataList = class;

  { TDatabaseData }

  TDatabaseData = class
  private
    const
      sqlInsert = 'Insert into Databases(DatabaseID,DatabaseName,Server) '+
                  'Values(Gen_id(UniqueID,1),:DatabaseName,:Server) '+
                  'Returning(DatabaseID)';
      sqlRefresh    = 'Select * from Databases Where DatabaseID = ?';
      sqlUpdatePath = 'Update Databases Set DatabasePath = ? Where DatabaseID = ?';
      sqlUpdateName = 'Update Databases Set DatabaseName = ? Where DatabaseID = ?';
      sqlUpdateUser = 'Update Databases Set DefaultUserName = ? Where DatabaseID = ?';
  private
    FOwner: TDatabaseDataList;
    FAttachment: IAttachment;
    FDatabaseID: integer;
    FDatabaseName: string;
    FDatabasePath: string;
    FDefaultUserName: string;
    FServerData: TServerData;
    procedure SetDatabaseName(AValue: string);
    procedure SetDatabasePath(AValue: string);
    procedure SetDefaultUserName(AValue: string);
  public
    constructor Create(aOwner: TDatabaseDataList;
      aDatabaseName: string; aServer: TServerData); overload;
    constructor Create(aOwner: TDatabaseDataList; aDatabaseID: integer; aServer: TServerData); overload;
    procedure Refresh;
    procedure Select;
    property DatabaseID: integer read FDatabaseID;
    property DatabasePath: string read FDatabasePath write SetDatabasePath;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property DefaultUserName: string read FDefaultUserName write SetDefaultUserName;
    property Attachment: IAttachment read FAttachment write FAttachment;
    property ServerData: TServerData read FServerData;
    property Owner: TDatabaseDataList read FOwner;
  end;

  { TDatabaseList }

  { TDatabaseDataList }

  TDatabaseDataList = class
  private
    FDatabaseDataList: array of TDatabaseData;
    function FindDatabaseData(aDatabaseID: integer): integer;
    function GetRemoteDatabase(aDatabaseID: integer): TDatabaseData;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(aDatabaseName: string; aServer: TServerData): TDatabaseData; overload;
    function Add(aDatabaseID: integer; aServer: TServerData): TDatabaseData; overload;
    function ByIndex(index: integer): IAttachment;
    procedure Clear;
    procedure Refresh;
    procedure Remove(aDatabaseID: integer);
    procedure Select(aDatabaseID: integer);
    procedure Update(aDatabaseID: integer);
    property Attachment[aDatabaseID: integer]: IAttachment read GetRemoteDatabase; default;
  end;

var
  DatabaseDataList: TDatabaseDataList;

implementation

uses DBADataModule;

{ TDatabaseData }

procedure TDatabaseData.SetDatabaseName(AValue: string);
begin
  if FDatabaseName = AValue then Exit;
  FDatabaseName := AValue;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateName,[FDatabaseName,FDatabaseID]);
end;

procedure TDatabaseData.SetDatabasePath(AValue: string);
begin
  if FDatabasePath = AValue then Exit;
  FDatabasePath := AValue;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdatePath,[FDatabasePath,FDatabaseID]);
end;

procedure TDatabaseData.SetDefaultUserName(AValue: string);
begin
  if FDefaultUserName = AValue then Exit;
  FDefaultUserName := AValue;
  LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateUser,[FDefaultUserName,FDatabaseID]);
end;

constructor TDatabaseData.Create(aOwner: TDatabaseDataList;
  aDatabaseName: string; aServer: TServerData);
begin
  inherited;
  FOwner := aOwner;
  FDatabaseName := aDatabaseName;
  FServerData := aServer;
  FDatabaseID := LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlInsert,
                   [aDatabaseName,aServer.ServerID])[0].AsInteger;
end;

constructor TDatabaseData.Create(aOwner: TDatabaseDataList;
  aDatabaseID: integer; aServer: TServerData);
begin
  inherited Create;
  FOwner := aOwner;
  FDatabaseID := aDatabaseID;
  FServerData := aServer;
  Refresh;
end;

procedure TDatabaseData.Refresh;
var QueryResults: IResultSet;
    aServerID: integer;
begin
  QueryResults := LocalData.LocalDatabase.Attachment.OpenCursorAtStart(sqlRefresh,[FDatabaseID]);
  with QueryResults do
  if not IsEOF then
  begin
    FDatabaseName := ByName('DatabaseName').AsString;
    FDatabasePath := ByName('DatabasePath').AsString;
    DefaultUserName := ByName('DefaultUserName').AsString;
    aServerID := ByName('Server').AsInteger;
    if aServerID <> ServerData.ServerID then
      ServerData := ServerDataList.ServerData[aServerID];
    if ServerData = nil then
      raise Exception.CreateFmt('Unknown Server for DatabaseName = %s',[DatabaseName]);
  end;
end;

procedure TDatabaseData.Select;
begin
  DBADatabaseData.DatabaseData := self;
end;

{ TDatabaseDataList }

function TDatabaseDataList.FindDatabaseData(aDatabaseID: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(FDatabaseDataList) - 1 do
    if FDatabaseDataList[i].DatabaseID = aDatabaseID then
    begin
      Result := i;
      Exit;
    end;
end;

function TDatabaseDataList.GetRemoteDatabase(aDatabaseID: integer
  ): TDatabaseData;
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
  begin
    Result := FDatabaseDataList[index];
    Result.Connect;
  end
  else
    Result := nil;
end;

constructor TDatabaseDataList.Create;
begin
  inherited Create;
  SetLength(FDatabaseDataList,0);
end;

destructor TDatabaseDataList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDatabaseDataList.Add(aDatabaseName: string; aServer: TServerData): TDatabaseData;
var index: integer;
begin
  SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
  index := Length(FDatabaseDataList)-1;
  FDatabaseDataList[index] := TDatabaseData.Create(self,aDatabaseName,aServer);
  Result := FDatabaseDataList[index];
end;

function TDatabaseDataList.Add(aDatabaseID: integer; aServer: TServerData): TDatabaseData;
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index = -1 then
  begin
    SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
    index := Length(FDatabaseDataList)-1;
    FDatabaseDataList[index] := TDatabaseData.Create(self,aDatabaseIO,aServer);
  end;
  Result := FDatabaseDataList[index];
end;

function TDatabaseDataList.ByIndex(index: integer): IAttachment;
begin
  if (index < 0) or (index >= Length(FDatabaseDataList)) then
    raise Exception.CreateFmt('Database Index (%d) out of range',[index]);
  Result := FDatabaseDataList[index];
end;

procedure TDatabaseDataList.Clear;
var i: integer;
begin
  for i := 0 to Length(FDatabaseDataList) - 1 do
     FDatabaseDataList[i].Free;
  SetLength(FDatabaseDataList,0);
end;

procedure TDatabaseDataList.Refresh;
var i: integer;
begin
  Clear;

end;

procedure TDatabaseDataList.Remove(aDatabaseID: integer);
var index, j: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
  begin
    FDatabaseDataList[index].Free;
    for j := index to Length(FDatabaseDataList) - 2 do
      FDatabaseDataList[j] := FDatabaseDataList[j + 1];
    SetLength(FDatabaseDataList,Length(FDatabaseDataList)-1);
  end;
end;

procedure TDatabaseDataList.Select(aDatabaseID: integer);
begin
  with DBADatabaseData do
  begin
    IBDatabase1.Attachment := Attachment[aDatabaseID];
  end;
end;

procedure TDatabaseDataList.Update(aDatabaseID: integer);
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
    FDatabaseDataList[index].Refresh
  else
    Add(aDatabaseID);
end;

initialization
  DatabaseDataList := TDatabaseDataList.Create;

finalization
  if assigned(DatabaseDataList) then DatabaseDataList.Free;



end.

