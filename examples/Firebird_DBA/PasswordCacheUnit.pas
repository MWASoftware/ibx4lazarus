unit PasswordCacheUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, IB;

type

  { TPasswordCache }

  TPasswordCache = class
  private
    FPasswordList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetPassword(aUserName, aServerName: string; var password: string): boolean; overload;
    function GetPassword(aUserName, aDatabaseName, aServerName: string; var password: string): boolean; overload;
    procedure SavePassword(aUserName, aServerName, aPassword: string); overload;
    procedure SavePassword(aUserName, aDatabaseName, aServerName, aPassword, aSecDatabase: string); overload;
    procedure RemovePassword(aUserName, aServerName: string); overload;
    procedure RemovePassword(aUserName, aDatabaseName, aServerName: string); overload;
  end;

function PasswordCache: TPasswordCache;
function ExtractDatabaseName(ConnectString: string): string;
function ExtractServerName(ConnectString: string): string;

implementation

uses LocalDataModule, Variants, IBUtils;

var FPasswordCache: TPasswordCache;

function PasswordCache: TPasswordCache;
begin
  Result := FPasswordCache;
end;

function ExtractDatabaseName(ConnectString: string): string;
var aServerName: AnsiString;
    aDatabaseName: AnsiString;
    aProtocol: TProtocolAll;
    aPortNo: AnsiString;
begin
  if ParseConnectString(ConnectString,aServerName, aDatabaseName, aProtocol, aPortNo) then
    Result := aDatabaseName
  else
    Result := '';
end;

function ExtractServerName(ConnectString: string): string;
var aServerName: AnsiString;
    aDatabaseName: AnsiString;
    aProtocol: TProtocolAll;
    aPortNo: AnsiString;
begin
  if ParseConnectString(ConnectString,aServerName, aDatabaseName, aProtocol, aPortNo) then
    Result := aServerName
  else
    Result := '';
end;

{ TPasswordCache }

constructor TPasswordCache.Create;
begin
  FPasswordList := TStringList.Create;
end;

destructor TPasswordCache.Destroy;
begin
  if assigned(FPasswordList) then FPasswordList.Free;
  inherited Destroy;
end;

function TPasswordCache.GetPassword(aUserName, aServerName: string;
  var password: string): boolean;
var Results: IResultSet;
begin
  with LocalData do
    Results := LocalDatabase.Attachment.OpenCursorAtStart(
                 PasswordCacheTable.Transaction.TransactionIntf,
                 'Select DBAPasswordIndex From PasswordCache Where ServerName = ? '+
                 'and SecurityDatabase is NULL and DBAUSER = ? '+
                 'Order by DatabaseName NULLS FIRST',  {NULLS are first}
                 [aServerName,aUserName]);
  Result := not Results.IsEof;
  if Result then
  begin
    password := FPasswordList[Results[0].AsInteger];
//    writeln('Get PW ', aUserName, ',', aServerName,',',password, ',', Results[0].AsInteger);
  end;
end;

function TPasswordCache.GetPassword(aUserName, aDatabaseName,
  aServerName: string; var password: string): boolean;
var Results: IResultSet;
begin
  with LocalData do
    Results := LocalDatabase.Attachment.OpenCursorAtStart(
                 PasswordCacheTable.Transaction.TransactionIntf,
                 'Select DBAPasswordIndex From PasswordCache Where ServerName = ? '+
                 'and DBAUSER = ? and (DatabaseName IS NULL OR DatabaseName = ?) '+
                 'ORDER BY DatabaseName NULLS LAST', {ensure NULLs are last}
                 [aServerName,aUserName,aDatabaseName]);
  Result := not Results.IsEof;
  if Result then
    password := FPasswordList[Results[0].AsInteger];
//  writeln('Get PW ', aUserName, ',', aDatabaseName, ',', aServerName,',',password);
end;

procedure TPasswordCache.SavePassword(aUserName, aServerName, aPassword: string
  );
var index: integer;
    Results: IResultSet;
begin
//  writeln('Add PW ', aUserName, ',',aServerName,',', aPassword);
  with LocalData do
    Results := LocalDatabase.Attachment.OpenCursorAtStart(
                 PasswordCacheTable.Transaction.TransactionIntf,
                 'Select DBAPasswordIndex From PasswordCache Where ServerName = ? '+
                 'and DBAUSER = ? and DatabaseName is NULL',
                 [aServerName,aUserName]);
  if not Results.IsEof then
    FPasswordList[Results[0].AsInteger] := aPassword
  else
  with LocalData.PasswordCacheTable do
  begin
    Active := true;
    Append;
    try
      FieldByName('ServerName').AsString := aServerName;
      FieldByName('DBAUser').AsString := aUserName;
      FieldByName('DatabaseName').Clear;
      FieldByName('DBAPASSWORDINDEX').AsInteger := FPasswordList.Add(aPassword);
      Post;
    except
      Cancel;
      raise;
    end;
  end;
end;

procedure TPasswordCache.SavePassword(aUserName, aDatabaseName, aServerName,
  aPassword, aSecDatabase: string);
var index: integer;
    Results: IResultSet;
begin
//  writeln('Add PW ', aUserName, ',',aDatabaseName,',',aServerName,',', aPassword,',',aSecDatabase);
  with LocalData do
    Results := LocalDatabase.Attachment.OpenCursorAtStart(
                 PasswordCacheTable.Transaction.TransactionIntf,
                 'Select DBAPasswordIndex From PasswordCache Where ServerName = ? '+
                 'and DBAUSER = ? and DatabaseName = ?',
                 [aServerName,aUserName,aDatabaseName]);
  if not Results.IsEof then
    FPasswordList[Results[0].AsInteger] := aPassword
  else
  with LocalData.PasswordCacheTable do
  begin
    Active := true;
    Append;
    try
      FieldByName('ServerName').AsString := aServerName;
      FieldByName('DatabaseName').AsString := aDatabaseName;
      FieldByName('DBAUser').AsString := aUserName;
      FieldByName('DBAPASSWORDINDEX').AsInteger := FPasswordList.Add(aPassword);
      if aSecDatabase = 'Default' then
        FieldByName('SecurityDatabase').Clear
      else
        FieldByName('SecurityDatabase').AsString := aSecDatabase;
      Post;
    except
      Cancel;
      raise;
    end;
  end;
end;

procedure TPasswordCache.RemovePassword(aUserName, aServerName: string);
begin
//  writeln('Remove PW ', aUserName, ',', aServerName);
  with LocalData.PasswordCacheTable do
  begin
    if Locate('SERVERNAME;DATABASENAME;DBAUSER',VarArrayOf([aServerName, NULL,aUserName]),[]) then
    begin
      FPasswordList[FieldByName('DBAPASSWORDINDEX').AsInteger] := '';
      Delete;
    end;
  end;
end;

procedure TPasswordCache.RemovePassword(aUserName, aDatabaseName,
  aServerName: string);
begin
  //  writeln('Remove PW ', aUserName, ',', aDatabaseName, ',',aServerName);
    with LocalData.PasswordCacheTable do
    begin
      if Locate('SERVERNAME;DATABASENAME;DBAUSER',VarArrayOf([aServerName, aDatabaseName, aUserName]),[]) then
      begin
        FPasswordList[FieldByName('DBAPASSWORDINDEX').AsInteger] := '';
        Delete;
      end;
    end;
end;

initialization
   FPasswordCache := TPasswordCache.Create;

finalization
  FPasswordCache.Free;

end.

