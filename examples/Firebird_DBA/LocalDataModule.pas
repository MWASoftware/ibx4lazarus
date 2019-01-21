unit LocalDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, IBEvents, IBQuery, IBDatabase, IBCustomDataSet,
  IBSQL, IBLocalDBSupport, db, IBXCustomIBLocalDBSupport;

const
  MaxHistory = 20;

type
  { TSQLSaveList }

  TSQLSaveList = class
  private
    FList: TList;
    FHeadPtr: integer;
    FTailPtr: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function CanGetNext: boolean;
    function CanGetPrior: boolean;
    function GetNext: TStrings;
    function GetPrior: TStrings;
    procedure Add(sql: TStrings);
    procedure SaveSQLHistory;
    procedure LoadSQLHistory;
  end;

  { TLocalData }

  TLocalData = class(TDataModule)
    AppDatabases: TMemDataset;
    UpdateKey: TIBSQL;
    LookupKey: TIBSQL;
    LocalDatabase: TIBDatabase;
    DBControl: TIBQuery;
    IBLocalDBSupport: TIBLocalDBSupport;
    IBTransaction: TIBTransaction;
    LoadSQLHistory: TIBSQL;
    PasswordCacheTable: TIBDataSet;
    SaveSQLHistory: TIBSQL;
    ServersAndDatabasesHASCHILD: TIntegerField;
    ServersAndDatabasesID: TIntegerField;
    ServersAndDatabasesITEMNAME: TIBStringField;
    ServersAndDatabasesITEMTYPE: TIntegerField;
    ServersAndDatabasesPARENT: TIntegerField;
    ServersQuery: TIBQuery;
    SetUUID: TIBSQL;
    procedure AppDatabasesAfterClose(DataSet: TDataSet);
    procedure AppDatabasesAfterOpen(DataSet: TDataSet);
    procedure IBLocalDBSupportGetSharedDataDir(Sender: TObject;
      var SharedDataDir: string);
    procedure LocalDatabaseAfterConnect(Sender: TObject);
    procedure LocalDatabaseAfterDisconnect(Sender: TObject);
  private
    FMinVersionNo: integer;
    { private declarations }
    FSQLHistory: TSQLSaveList;
    procedure CheckDBVersion;
    function GetControlTableName: string;
    function GetCurrentDBVersionNo: integer;
    function GetRequiredDBVersionNo: integer;
    function GetUserConfig(KeyName: string): string;
    function GetUUID: string;
    procedure LoadAppDatabases;
    function GetDatabaseDirs: TStrings;
    function HasTable(TableName: string): boolean;
    procedure SetNewUUID;
    procedure SetUserConfig(KeyName: string; AValue: string);
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property UserConfig[KeyName: string]: string read GetUserConfig write SetUserConfig;
    property SQLHistory: TSQLSaveList read FSQLHistory;
    property CurrentDBVersionNo: integer read GetCurrentDBVersionNo;
    property RequiredDBVersionNo: integer read GetRequiredDBVersionNo;
    property MinVersionNo: integer read FMinVersionNo;
    property UUID: string read GetUUID;
  end;

  { EDBVersionProblem }

  EDBVersionProblem = class(Exception)
  private
    FVersionFound: integer;
    FVersionWanted: integer;
  public
    constructor Create(aVersionFound, aVersionWanted: integer);
    property VersionFound: integer read FVersionFound;
    property VersionWanted: integer read FVersionWanted;
  end;

const
  rgCurServerDB = 'CurrentServerDB';
  rgSQLGroup    = 'SQLGroup';

var
  LocalData: TLocalData;

implementation

{$R *.lfm}

{ EDBVersionProblem }

constructor EDBVersionProblem.Create(aVersionFound, aVersionWanted: integer);
begin
  CreateFmt(sBadDBVersion,[aVersionWanted,aVersionFound]);
  FVersionWanted := aVersionWanted;
  FVersionFound := aVersionFound;
end;

{ TLocalData }

procedure TLocalData.AppDatabasesAfterOpen(DataSet: TDataSet);
begin
  LoadAppDatabases;
end;

procedure TLocalData.IBLocalDBSupportGetSharedDataDir(Sender: TObject;
  var SharedDataDir: string);
begin
  if DirectoryExists('data')
    SharedDataDir := 'data' + DirectorySeparator + 'regdatabase'
  {$IFDEF UNIX}
  else
    SharedDataDir := SharedDataDir + 'data' + DirectorySeparator + 'regdatabase';
  {$ENDIF}
end;

procedure TLocalData.LocalDatabaseAfterConnect(Sender: TObject);
begin
  CheckDBVersion;
  FSQLHistory.LoadSQLHistory;
  PasswordCacheTable.Active := true;
end;

procedure TLocalData.LocalDatabaseAfterDisconnect(Sender: TObject);
begin
  FSQLHistory.SaveSQLHistory;
end;

procedure TLocalData.CheckDBVersion;
var CurrentVersion: integer;
begin
  CurrentVersion := CurrentDBVersionNo;
  if (MinVersionNo = -1) and (CurrentVersion <> RequiredDBVersionNo) then
    raise EDBVersionProblem.Create(CurrentVersion,RequiredDBVersionNo);

  if (CurrentVersion > RequiredDBVersionNo) or (RequiredDBVersionNo < MinVersionNo) then
    raise EDBVersionProblem.Create(CurrentVersion,RequiredDBVersionNo);
end;

function TLocalData.GetControlTableName: string;
begin
  Result := 'DBCONTROL';
end;

function TLocalData.GetCurrentDBVersionNo: integer;
var bin_uuid: string;
    I: integer;
    MinVersionField: TField;
begin
  Result := 0;
  if (FUUID = '') or not HasTable(GetControlTableName) then
    Exit;

  DBControl.SQL.Text := 'Select * From ' + GetControlTableName;
  DBControl.Active := true;
   try
     Result := DBControl.FieldByName('VersionNo').AsInteger;
     MinVersionField :=  DBControl.FindField('MinVersionNo');
     if MinVersionField <> nil then
       FMinVersionNo := MinVersionField.AsInteger
     else
       FMinVersionNo := -1;
     bin_uuid := DBControl.FieldByName('UUID').AsString;
   finally
     DBControl.Active := false;
   end;

   for I := 1 to Length(bin_uuid) do
   begin
     if I in [5,7,9] then
       FUUID := FUUID + '-';
     FUUID := FUUID + Format('%x',[ord(bin_uuid[I])])
   end;
end;

function TLocalData.GetRequiredDBVersionNo: integer;
begin
  Result := IBLocalDBSupport.RequiredVersionNo;
end;

function TLocalData.GetUserConfig(KeyName: string): string;
begin
  Result := '';
  with LookupKey do
  begin
    Transaction.Active := true;
    ParamByName('KeyName').AsString := KeyName;
    ExecQuery;
    try
      if not EOF then
        Result := FieldByName('KeyValue').AsString;
    finally
      Close
    end;
  end;
end;

function TLocalData.GetUUID: string;
begin
  GetCurrentDBVersionNo;
  Result := FUUID;
end;

procedure TLocalData.AppDatabasesAfterClose(DataSet: TDataSet);
begin
  AppDatabases.Clear(false);
end;

procedure TLocalData.LoadAppDatabases;
var
    DatabaseDirs: TStrings;
    i: integer;
    ini: TIniFile;
    AppDatabaseDir: string;
begin
  DatabaseDirs := GetDatabaseDirs;
  AppDatabases.Clear(false);
  for i := 0 to DatabaseDirs.Count - 1 do
  begin
    AppDatabases.Append;
    try
      AppDatabases.FieldByName('ID').AsInteger := i + 1;
      AppDatabaseDir := DatabaseDirs[i];
      AppDatabases.FieldByName('Path').AsString := AppDatabaseDir;

      if FileExists(AppDatabaseDir + DirectorySeparator + 'schema.sql') then
         AppDatabases.FieldByName('schema').AsString := AppDatabaseDir + DirectorySeparator + 'schema.sql'
      else
      if FileExists(AppDatabaseDir + DirectorySeparator + 'schema.gbk') then
         AppDatabases.FieldByName('schema').AsString := AppDatabaseDir + DirectorySeparator + 'schema.gbk';

      if FileExists(AppDatabaseDir + DirectorySeparator + 'database.conf') then
      begin
        ini := TIniFile.Create(AppDatabaseDir + DirectorySeparator + 'database.conf');
        try
          AppDatabases.FieldByName('Title').AsString := ini.ReadString('Database','Title','<no title>');
          AppDatabases.FieldByName('DBControlTable').AsString := ini.ReadString('Database','DBControlTable','');
          AppDatabases.FieldByName('Signature').AsString := ini.ReadString('Database','Signature','');
          AppDatabases.FieldByName('ShutdownProc').AsString := ini.ReadString('Database','ShutdownProcedure','');
        finally
          ini.Free;
        end;

        if FileExists(AppDatabaseDir + DirectorySeparator + 'upgrade.conf') then
        begin
          ini := TIniFile.Create(AppDatabaseDir + DirectorySeparator + 'upgrade.conf');
          try
            AppDatabases.FieldByName('CurrentVersion').AsInteger := ini.ReadInteger('status','current',0);
          finally
            ini.Free;
          end;
        end;
      end;
    finally
      AppDatabases.Post;
    end;
  end;
  DatabaseDirs.Free;
end;

{$IFDEF WINDOWS}
function TLocalData.GetDatabaseDirs: TStrings;
var KeyNames: TStrings;
    i: integer;
begin
  Result := TStringList.Create;
  KeyNames := TStringList.Create;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(rgRootRegistryKey + '\databases') then
    begin
      GetValueNames(KeyNames);
      for i := 0 to KeyNames.Count - 1 do
        Result.Add(ReadString(KeyNames[i]));
    end
  finally
    KeyNames.Free;
    Free
  end;
end;
{$ELSE}
function TLocalData.GetDatabaseDirs: TStrings;
var RootDir: string;
begin
  RootDir := ExtractFileDir(ExtractFileDir(IBLocalDBSupport.SharedDataDir)) + DirectorySeparator + 'databases';
  Result := FindAllDirectories(RootDir,false);
end;

function TLocalData.HasTable(TableName: string): boolean;
begin
  Result := LocalDatabase.Attachment.OpenCursorAtStart(
         'Select count(*) From RDB$RELATIONS Where RDB$RELATION_NAME = ?',
            [TableName])[0].AsInteger > 0;
end;

procedure TLocalData.SetNewUUID;
begin
  if HasTable(GetControlTableName) then
    with SetUUID do
    begin
      SQL.Text := Format('Update %s Set UUID = GEN_UUID()',[GetControlTableName]);
      Transaction.Active := true;
      ExecQuery
    end;
  FUUID := '';
  GetCurrentDBVersionNo;
end;

procedure TLocalData.SetUserConfig(KeyName: string; AValue: string);
begin
  with UpdateKey do
  begin
    Transaction.Active := true;
    ParamByName('KeyName').AsString := KeyName;
    ParamByName('KeyValue').AsString := KeyValue;
    ExecQuery;
    Transction.Commit;
  end;
end;

procedure TLocalData.Loaded;
begin
  inherited Loaded;
  AppDatabases.Active := true;
end;

{$ENDIF}

constructor TLocalData.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSQLHistory := TSQLSaveList.Create;
end;

destructor TLocalData.Destroy;
begin
  if assigned(FSQLHistory) then FSQLHistory.Free;
  inherited Destroy;
end;

{ TSQLSaveList }

constructor TSQLSaveList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FHeadPtr := -1;
  FTailPtr := -1;
end;

destructor TSQLSaveList.Destroy;
begin
  if FList <> nil then
  begin
    Clear;
    FList.Free;
  end;
  inherited Destroy;
end;

procedure TSQLSaveList.Clear;
var i: integer;
begin
  for i := 0 to FList.Count -1 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TSQLSaveList.CanGetNext: boolean;
begin
  Result := (FHeadPtr >= 0) and (FHeadPtr < FList.Count - 1);
end;

function TSQLSaveList.CanGetPrior: boolean;
begin
  Result := FHeadPtr > 0;
end;

function TSQLSaveList.GetNext: TStrings;
begin
  if FTailPtr < FHeadPtr then
  begin
    Inc(FHeadPtr);
    Result := TStrings(FList[FHeadptr]);
  end
  else {reversal}
  begin
    FHeadPtr := FTailPtr;
    Result := TStrings(FList[FHeadptr]);
  end;
  FTailPtr := FHeadPtr - 1;
//  writeln(FHeadPtr,',',FTailPtr);
end;

function TSQLSaveList.GetPrior: TStrings;
begin
  if FTailPtr > FHeadPtr then
  begin
    Dec(FHeadPtr);
    Result := TStrings(FList[FHeadptr]);
  end
  else {reversal}
  begin
    FHeadPtr := FTailPtr;
    Result := TStrings(FList[FHeadptr]);
  end;
  FTailPtr := FHeadPtr + 1;
//  writeln(FHeadPtr,',',FTailPtr);
end;

procedure TSQLSaveList.Add(sql: TStrings);
var s: TStringList;
begin
  s := TStringList.Create;
  s.Assign(sql);
  FHeadPtr := FList.Add(s) + 1;
  FTailPtr := FHeadPtr - 1;
//  writeln(FHeadPtr,',',FTailPtr);
end;

procedure TSQLSaveList.SaveSQLHistory;
var LowerBound, i, j: integer;
begin
  with LocalData.SaveSQLHistory do
  begin
    Transaction.Active := true;
    LowerBound := 0;
    if FList.Count > MaxHistory then
      LowerBound := FList.Count - MaxHistory;
    j := 0;
    for i := FList.Count - 1 downto LowerBound do
    begin
      ParamByName('SEQNO').AsInteger := j;
      ParamByName('SQLText').AsString := TStrings(FList[i]).Text;
      ExecQuery;
      Inc(j);
    end;
    Transaction.Commit;
  end;
end;

procedure TSQLSaveList.LoadSQLHistory;
var s: TStringList;
begin
  with LocalData.LoadSQLHistory do
  begin
    Transaction.Active := true;
    ExecQuery;
    try
      while not EOF do
      begin
        s := TStringList.Create;
        s.Text := FieldByName('SQLText').AsString;
        FHeadPtr := FList.Add(s) + 1;
        FTailPtr := FHeadPtr - 1;
        Next;
      end;
    finally
      Close
    end;
  end;
end;

end.

