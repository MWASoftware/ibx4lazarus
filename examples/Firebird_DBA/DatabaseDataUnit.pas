unit DatabaseDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IB, ServerDataUnit, RegisterExistingDBDlgUnit;

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
    FAppID: integer;
    FCreateIfNotExists: boolean;
    FDBControlTable: string;
    FOwner: TDatabaseDataList;
    FAttachment: IAttachment;
    FDatabaseID: integer;
    FDatabaseName: string;
    FDatabasePath: string;
    FDefaultUserName: string;
    FServerData: TServerData;
    FShutDownProc: string;
    FTitle: string;
    procedure SetAppID(AValue: integer);
    procedure SetDatabaseName(AValue: string);
    procedure SetDatabasePath(AValue: string);
    procedure SetDefaultUserName(AValue: string);
  public
    constructor Create(aOwner: TDatabaseDataList;
      aDatabaseName: string; aServer: TServerData); overload;
    constructor Create(aOwner: TDatabaseDataList; aDatabaseID: integer; aServer: TServerData); overload;
    procedure Refresh;
    function Select: boolean;
    property DatabaseID: integer read FDatabaseID;
    property DatabasePath: string read FDatabasePath write SetDatabasePath;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property DefaultUserName: string read FDefaultUserName write SetDefaultUserName;
    property Attachment: IAttachment read FAttachment write FAttachment;
    property CreateIfNotExists: boolean read FCreateIfNotExists write FCreateIfNotExists;
    property AppID: integer read FAppID write SetAppID;
    property Title: string read FTitle;
    property DBControlTable: string read FDBControlTable;
    property ShutDownProc: string read FShutDownProc;
    property ServerData: TServerData read FServerData;
    property Owner: TDatabaseDataList read FOwner;
  end;

  { TDatabaseDataList }

  TDatabaseDataList = class
  private
    const
      sqlSelectAll = 'Select * from Databases order by DatabaseID';
  private
    FDatabaseDataList: array of TDatabaseData;
    FDatabaseListLoaded: boolean;
    FLoading: boolean;
    function FindDatabaseData(aDatabaseID: integer): integer;
    function GetDatabaseData(aDatabaseID: integer): TDatabaseData;
    procedure LoadDatabaseList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(aDatabaseName: string; aServer: TServerData): integer; overload;
    function Add(aDatabaseID: integer; aServer: TServerData): integer; overload;
    procedure Clear;
    procedure Refresh;
    procedure Remove(aDatabaseID: integer);
    procedure Select(aDatabaseID: integer);
    function Update(aDatabaseID: integer; dlg: TRegisterExistingDBDlg): TDatabaseData;
    property DatabaseData[index: integer]: TDatabaseData read GetDatabaseData;
  end;

var
  DatabaseDataList: TDatabaseDataList;

implementation

uses DBADataModule, LocalDataModule, IBSQL, CreateNewDBDlgUnit;

{ TDatabaseData }

procedure TDatabaseData.SetDatabaseName(AValue: string);
begin
  if FDatabaseName = AValue then Exit;
  FDatabaseName := AValue;
  if not Owner.FLoading then
    LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateName,[FDatabaseName,FDatabaseID]);
end;

procedure TDatabaseData.SetAppID(AValue: integer);
begin
  if FAppID = AValue then Exit;
  FAppID := AValue;
  if not Owner.FLoading and LocalData.AppDatabases.Locate('ID',FAppID,[]) then
  begin
    FTitle := LocalData.AppDatabases.FieldByName('Title').AsString;
    FDBControlTable := LocalData.AppDatabases.FieldByName('DBControlTable').AsString;
    FShutDownProc := LocalData.AppDatabases.FieldByName('ShutDownProc').AsString;
  end;
end;

procedure TDatabaseData.SetDatabasePath(AValue: string);
begin
  if FDatabasePath = AValue then Exit;
  FDatabasePath := AValue;
  if not Owner.FLoading then
    LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdatePath,[FDatabasePath,FDatabaseID]);
end;

procedure TDatabaseData.SetDefaultUserName(AValue: string);
begin
  if FDefaultUserName = AValue then Exit;
  FDefaultUserName := AValue;
  if not Owner.FLoading then
    LocalData.LocalDatabase.Attachment.ExecuteSQL([isc_tpb_write],sqlUpdateUser,[FDefaultUserName,FDatabaseID]);
end;

constructor TDatabaseData.Create(aOwner: TDatabaseDataList;
  aDatabaseName: string; aServer: TServerData);
begin
  inherited Create;
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
      FServerData := ServerDataList.ServerData[aServerID];
    if FServerData = nil then
      raise Exception.CreateFmt('Unknown Server for DatabaseName = %s',[DatabaseName]);
  end;
end;

function TDatabaseData.Select: boolean;
begin
  DBADatabaseData.DatabaseData := self;
  Result := DBADatabaseData.IBDatabase1.Connected;
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

function TDatabaseDataList.Add(aDatabaseName: string; aServer: TServerData): integer;
begin
  SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
  Result := Length(FDatabaseDataList)-1;
  FDatabaseDataList[Result] := TDatabaseData.Create(self,aDatabaseName,aServer);
end;

function TDatabaseDataList.Add(aDatabaseID: integer; aServer: TServerData): integer;
begin
  SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
  Result := Length(FDatabaseDataList)-1;
  FDatabaseDataList[Result] := TDatabaseData.Create(self,aDatabaseID,aServer);
end;

function TDatabaseDataList.GetDatabaseData(aDatabaseID: integer): TDatabaseData;
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
    Result := FDatabaseDataList[index]
  else
    Result := nil;
end;

procedure TDatabaseDataList.LoadDatabaseList;
var index: integer;
begin
  if FDatabaseListLoaded then Exit;

  FLoading := true;
  try
    SetLength(FDatabaseDataList,0);
    with TIBSQL.Create(nil) do
    try
      SQL.Text := sqlSelectAll;
      Database := LocalData.LocalDatabase;
      Transaction := LocalData.IBTransaction;
      Transaction.Active := true;
      ExecQuery;
      try
        while not EOF do
        begin
          SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
          index := Length(FDatabaseDataList)-1;
          FDatabaseDataList[index] := TDatabaseData.Create(self,
                                                           FieldByName('DatabaseID').AsInteger,
                                                           ServerDataList[FieldByName('Server').AsInteger]);
          with FDatabaseDataList[index] do
          begin
            DatabaseName := FieldByName('DatabaseName').AsString;
            DatabasePath := FieldByName('DatabasePath').AsString;
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

    FDatabaseListLoaded := true;
  finally
    FLoading := false
  end;
end;

procedure TDatabaseDataList.Clear;
var i: integer;
begin
  for i := 0 to Length(FDatabaseDataList) - 1 do
     FDatabaseDataList[i].Free;
  SetLength(FDatabaseDataList,0);
  FDatabaseListLoaded := false;
end;

procedure TDatabaseDataList.Refresh;
var i: integer;
begin
  Clear;
  LoadDatabaseList;
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
  DBADatabaseData.IBDatabase1.Attachment := DatabaseData[aDatabaseID].Attachment;
end;

function TDatabaseDataList.Update(aDatabaseID: integer;
  dlg: TRegisterExistingDBDlg): TDatabaseData;
var index: integer;
begin
  Result := nil;
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
  begin
    Result := FDatabaseDataList[index];
    Result.Refresh;
  end
  else
  if dlg <> nil then
  begin
    index := Add(aDatabaseID,dlg.ServerData);
    Result := FDatabaseDataList[index];
  end;

  if dlg <> nil then
  with FDatabaseDataList[index] do
  begin
    DatabaseName := dlg.DatabaseName.Text;
    DatabasePath := dlg.DatabasePath.Text;
    DefaultUserName := dlg.DefaultUserName.Text;
    if dlg is TCreateNewDBDlg then
    begin
      CreateIfNotExists := true;
      AppID := TCreateNewDBDlg(dlg).AppDBLookup.KeyValue;
    end;
  end;
end;

initialization
  DatabaseDataList := TDatabaseDataList.Create;

finalization
  if assigned(DatabaseDataList) then DatabaseDataList.Free;



end.

