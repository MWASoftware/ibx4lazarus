unit DatabaseListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IB;

type

  { TDatabaseList }

  TDatabaseDataList = class
  private
    FAttachments: array of IAttachment;
    function FindDatabaseData(aDatabaseID: integer): integer;
    function GetRemoteDatabase(aDatabaseID: integer): TDBADatabaseData;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(aDatabaseID: integer; aDatabaseName: string): IAttachment;
    function ByIndex(index: integer): IAttachment;
    procedure Clear;
    procedure Refresh;
    procedure Remove(aDatabaseID: integer);
    procedure Select(aDatabaseID: integer);
    procedure Update(aDatabaseID: integer; aDatabaseName: string);
    property Attachment[aDatabaseID: integer]: IAttachment read GetRemoteDatabase; default;
  end;

var
  DatabaseDataList: TDatabaseDataList;

implementation

uses DBADataModule;

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
  ): TDBADatabaseData;
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

function TDatabaseDataList.Add(aDatabaseID: integer; aDatabaseName: string
  ): IAttachment;
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index = -1 then
  begin
    SetLength(FDatabaseDataList,Length(FDatabaseDataList)+1);
    index := Length(FDatabaseDataList)-1;
    FDatabaseDataList[index] := TRemoteDatabase.Create(nil);
    FDatabaseDataList[index].DatabaseID := aDatabaseID;
    FDatabaseDataList[index].DatabaseName := aDatabaseName;
    FDatabaseDataList[index].SetOwner(self);
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
  for i := 0 to Length(FDatabaseDataList) - 1 do
     FDatabaseDataList[i].Refresh;
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

procedure TDatabaseDataList.Update(aDatabaseID: integer; aDatabaseName: string);
var index: integer;
begin
  index := FindDatabaseData(aDatabaseID);
  if index <> -1 then
    FDatabaseDataList[index].DatabaseName := aDatabaseName;
end;

initialization
  DatabaseDataList := TDatabaseDataList.Create;

finalization
  if assigned(DatabaseDataList) then DatabaseDataList.Free;



end.

