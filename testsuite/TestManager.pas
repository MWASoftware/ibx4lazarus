unit TestManager;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

interface

uses
  Classes, SysUtils, CustApp, IB, IBUtils, FmtBCD;

{$IF not defined(LineEnding)}
const
  LineEnding = #$0D#$0A;
{$IFEND}

type
  TTestManager = class;

  { TTestBase }

  TTestBase = class
  private
    FOwner: TTestManager;
    function GetFirebirdAPI: IFirebirdAPI;
  protected
    FHexStrings: boolean;
    function ExtractDBName(ConnectString: AnsiString): AnsiString;
    function GetTestID: AnsiString; virtual; abstract;
    function GetTestTitle: AnsiString; virtual; abstract;
    procedure PrintHexString(s: AnsiString);
    procedure PrintDPB(DPB: IDPB);
    procedure PrintMetaData(meta: IMetaData);
    procedure ParamInfo(SQLParams: ISQLParams);
    function ReportResults(Statement: IStatement): IResultSet;
    procedure ReportResult(aValue: IResults);
    function StringToHex(octetString: string; MaxLineLength: integer=0): string;
    procedure WriteArray(ar: IArray);
    procedure WriteAffectedRows(Statement: IStatement);
    procedure WriteDBInfo(DBInfo: IDBInformation);
    procedure WriteBytes(Bytes: TByteArray);
    procedure WriteOperationCounts(Category: AnsiString; ops: TDBOperationCounts);
    procedure WritePerfStats(stats: TPerfCounters);
    procedure CheckActivity(Attachment: IAttachment); overload;
    procedure CheckActivity(Transaction: ITransaction); overload;
  public
    constructor Create(aOwner: TTestManager);  virtual;
    function TestTitle: AnsiString;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); virtual; abstract;
    property Owner: TTestManager read FOwner;
  end;

  TTest = class of TTestBase;

  { TTestManager }

  TTestManager = class
  private
    FApplication: TCustomApplication;
    FClientLibraryPath: string;
    FServer: AnsiString;
    FTests: TStringList;
    FEmployeeDatabaseName: AnsiString;
    FNewDatabaseName: AnsiString;
    FSecondNewDatabaseName: AnsiString;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FBackupFileName: AnsiString;
    FShowStatistics: boolean;
    FFirebirdAPI: IFirebirdAPI;
    FPortNo: AnsiString;
    procedure CleanUp;
    function GetFirebirdAPI: IFirebirdAPI;
    function GetIndexByTestID(aTestID: AnsiString): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetUserName: AnsiString;
    function GetPassword: AnsiString;
    function GetEmployeeDatabaseName: AnsiString;
    function GetNewDatabaseName: AnsiString;
    function GetSecondNewDatabaseName: AnsiString;
    function GetBackupFileName: AnsiString;
    procedure RunAll;
    procedure Run(TestID: AnsiString);
    procedure SetClientLibraryPath(aLibName: string);
    procedure SetUserName(aValue: AnsiString);
    procedure SetPassword(aValue: AnsiString);
    procedure SetEmployeeDatabaseName(aValue: AnsiString);
    procedure SetNewDatabaseName(aValue: AnsiString);
    procedure SetSecondNewDatabaseName(aValue: AnsiString);
    procedure SetBackupFileName(aValue: AnsiString);
    procedure SetServerName(AValue: AnsiString);
    procedure SetPortNum(aValue: AnsiString);
    property ShowStatistics: boolean read FShowStatistics write FShowStatistics;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    property Server: AnsiString read FServer;
    property Application: TCustomApplication read FApplication write FApplication;
    property ClientLibraryPath: string read FClientLibraryPath;
  end;

var
  TestMgr: TTestManager = nil;

var OutFile: text;

procedure RegisterTest(aTest: TTest);

implementation

{$IFDEF MSWINDOWS}
uses windows;

function GetTempDir: AnsiString;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;
{$ENDIF}

procedure RegisterTest(aTest: TTest);
var test: TTestBase;
begin
  if TestMgr = nil then
    TestMgr := TTestManager.Create;
  test := aTest.Create(TestMgr);
  TestMgr.FTests.AddObject(test.GetTestID,test);
end;

{ TTestBase }

constructor TTestBase.Create(aOwner: TTestManager);
begin
  inherited Create;
  FOwner := aOwner;
end;

function TTestBase.TestTitle: AnsiString;
begin
  Result := 'Test ' + GetTestID + ': ' + GetTestTitle;
end;

function TTestBase.GetFirebirdAPI: IFirebirdAPI;
begin
  Result := FOwner.FirebirdAPI;
end;

function TTestBase.ExtractDBName(ConnectString: AnsiString): AnsiString;
var ServerName: AnsiString;
    Protocol: TProtocolAll;
    PortNo: AnsiString;
    i: integer;
begin
  if not ParseConnectString(ConnectString, ServerName, Result, Protocol,PortNo) then
  begin
    {assume either inet format (remote) or localhost}
    Result := ConnectString;
    if Pos('inet',Result) = 1 then
    begin
      system.Delete(Result,1,7);
      i := Pos('/',Result);
      if i > 0 then
        system.delete(Result,1,i);
    end
    else
    if Pos('localhost:',Result) = 1 then
      system.Delete(Result,1,10)
  end;
end;

procedure TTestBase.PrintHexString(s: AnsiString);
var i: integer;
begin
  for i := 1 to length(s) do
    write(OutFile,Format('%x ',[byte(s[i])]));
end;

procedure TTestBase.PrintDPB(DPB: IDPB);
var i: integer;
begin
  writeln(OutFile,'DPB');
  writeln(OutFile,'Count = ', DPB.getCount);
  for i := 0 to DPB.getCount - 1 do
    writeln(OutFile,DPB[i].getParamType,' = ', DPB[i].AsString);
  writeln(OutFile);
end;

procedure TTestBase.PrintMetaData(meta: IMetaData);
var i, j: integer;
    ar: IArrayMetaData;
    bm: IBlobMetaData;
    Bounds: TArrayBounds;
begin
  writeln(OutFile,'Metadata');
  for i := 0 to meta.GetCount - 1 do
  with meta[i] do
  begin
    writeln(OutFile,'SQLType =',GetSQLTypeName);
    writeln(OutFile,'sub type = ',getSubType);
    writeln(OutFile,'Table = ',getRelationName);
    writeln(OutFile,'Owner = ',getOwnerName);
    writeln(OutFile,'Column Name = ',getSQLName);
    writeln(OutFile,'Alias Name = ',getAliasName);
    writeln(OutFile,'Field Name = ',getName);
    writeln(OutFile,'Scale = ',getScale);
    writeln(OutFile,'Charset id = ',getCharSetID);
    if getIsNullable then writeln(OutFile,'Nullable') else writeln(OutFile,'Not Null');
    writeln(OutFile,'Size = ',GetSize);
    case getSQLType of
      SQL_ARRAY:
        begin
          writeln(OutFile,'Array Meta Data:');
          ar := GetArrayMetaData;
          writeln(OutFile,'SQLType =',ar.GetSQLTypeName);
          writeln(OutFile,'Scale = ',ar.getScale);
          writeln(OutFile,'Charset id = ',ar.getCharSetID);
          writeln(OutFile,'Size = ',ar.GetSize);
          writeln(OutFile,'Table = ',ar.GetTableName);
          writeln(OutFile,'Column = ',ar.GetColumnName);
          writeln(OutFile,'Dimensions = ',ar.GetDimensions);
          write(OutFile,'Bounds: ');
          Bounds := ar.GetBounds;
          for j := 0 to Length(Bounds) - 1 do
            write(OutFile,'(',Bounds[j].LowerBound,':',Bounds[j].UpperBound,') ');
          writeln(OutFile);
        end;
      SQL_BLOB:
        begin
          writeln(OutFile);
          writeln(OutFile,'Blob Meta Data');
          bm := GetBlobMetaData;
          writeln(OutFile,'SQL SubType =',bm.GetSubType);
          writeln(OutFile,'Table = ',bm.GetRelationName);
          writeln(OutFile,'Column = ',bm.GetColumnName);
          writeln(OutFile,'CharSetID = ',bm.GetCharSetID);
          writeln(OutFile,'Segment Size = ',bm.GetSegmentSize);
          writeln(OutFile);
        end;
    end;
    writeln(OutFile);
  end;
end;

procedure TTestBase.ParamInfo(SQLParams: ISQLParams);
var i: integer;
begin
  writeln(OutFile,'SQL Params');
  for i := 0 to SQLParams.Count - 1 do
  with SQLParams[i] do
  begin
    writeln(OutFile,'SQLType =',GetSQLTypeName);
    writeln(OutFile,'sub type = ',getSubType);
    writeln(OutFile,'Field Name = ',getName);
    writeln(OutFile,'Scale = ',getScale);
    writeln(OutFile,'Charset id = ',getCharSetID);
    if getIsNullable then writeln(OutFile,'Nullable') else writeln(OutFile,'Not Null');
    writeln(OutFile,'Size = ',GetSize);
    writeln(OutFile);
  end;
end;

function TTestBase.ReportResults(Statement: IStatement): IResultSet;
begin
  Result := Statement.OpenCursor;
  try
    while Result.FetchNext do
      ReportResult(Result);
  finally
    Result.Close;
  end;
  writeln(OutFile);
end;

procedure TTestBase.ReportResult(aValue: IResults);
var i: integer;
    s: AnsiString;
    dt: TDateTime;
begin
  for i := 0 to aValue.getCount - 1 do
  begin
    if aValue[i].IsNull then
      writeln(OutFile,aValue[i].Name,' = NULL')
    else
    case aValue[i].SQLType of
    SQL_ARRAY:
      begin
        if not aValue[i].IsNull then
          WriteArray(aValue[i].AsArray);
      end;
    SQL_FLOAT,SQL_DOUBLE,
    SQL_D_FLOAT:
      writeln(OutFile, aValue[i].Name,' = ',FormatFloat('#,##0.00',aValue[i].AsFloat));

    SQL_INT64:
      if aValue[i].Scale <> 0 then
        writeln(OutFile, aValue[i].Name,' = ',FormatFloat('#,##0.00',aValue[i].AsFloat))
      else
        writeln(OutFile,aValue[i].Name,' = ',aValue[i].AsString);

    SQL_BLOB:
      if aValue[i].IsNull then
        writeln(OutFile,aValue[i].Name,' = (null blob)')
      else
      if aValue[i].SQLSubType = 1 then
      begin
        s := aValue[i].AsString;
        if FHexStrings then
        begin
          write(OutFile,aValue[i].Name,' = ');
          PrintHexString(s);
          writeln(OutFile,' (Charset Id = ',aValue[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')');
        end
        else
        begin
          writeln(OutFile,aValue[i].Name,' (Charset Id = ',aValue[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')');
          writeln(OutFile);
          writeln(OutFile,s);
        end
      end
      else
        writeln(OutFile,aValue[i].Name,' = (blob), Length = ',aValue[i].AsBlob.GetBlobSize);

    SQL_TEXT,SQL_VARYING:
    begin
      s := aValue[i].AsString;
      if FHexStrings then
      begin
        write(OutFile,aValue[i].Name,' = ');
        PrintHexString(s);
        writeln(OutFile,' (Charset Id = ',aValue[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')');
      end
      else
      if aValue[i].GetCharSetID > 0 then
        writeln(OutFile,aValue[i].Name,' = ',s,' (Charset Id = ',aValue[i].GetCharSetID, ' Codepage = ',StringCodePage(s),')')
      else
        writeln(OutFile,aValue[i].Name,' = ',s);
    end;

    SQL_TIMESTAMP:
      writeln(OutFile,aValue[i].Name,' = ',FBFormatDateTime('yyyy/mm/dd hh:nn:ss.zzzz',aValue[i].AsDateTime));
    SQL_TYPE_DATE:
      writeln(OutFile,aValue[i].Name,' = ',FBFormatDateTime('yyyy/mm/dd',aValue[i].AsDate));
    SQL_TYPE_TIME:
      writeln(OutFile,aValue[i].Name,' = ',FBFormatDateTime('hh:nn:ss.zzzz',aValue[i].AsTime));
    SQL_TIMESTAMP_TZ:
    begin
      aValue[i].GetAsDateTime(dt,s);
      writeln(OutFile,aValue[i].Name,' = ',FBFormatDateTime('yyyy/mm/dd hh:nn:ss.zzzz',dt) + ' ' + s);
    end;
    SQL_TIME_TZ:
    begin
      aValue[i].GetAsDateTime(dt,s);
      writeln(OutFile,aValue[i].Name,' = ',FBFormatDateTime('hh:nn:ss.zzzz',dt) + ' ' + s);
    end;

    else
      writeln(OutFile,aValue[i].Name,' = ',aValue[i].AsString);
    end;
  end;
end;

function TTestBase.StringToHex(octetString: string; MaxLineLength: integer
  ): string;

  function ToHex(aValue: byte): string;
  const
    HexChars: array [0..15] of char = '0123456789ABCDEF';
  begin
    Result := HexChars[aValue shr 4] +
               HexChars[(aValue and $0F)];
  end;

var i, j: integer;
begin
  i := 1;
  Result := '';
  if MaxLineLength = 0 then
  while i <= Length(octetString) do
  begin
    Result += ToHex(byte(octetString[i]));
    Inc(i);
  end
  else
  while i <= Length(octetString) do
  begin
      for j := 1 to MaxLineLength do
      begin
        if i > Length(octetString) then
          Exit
        else
          Result += ToHex(byte(octetString[i]));
        inc(i);
      end;
      Result += LineEnding;
  end;
end;

procedure TTestBase.WriteArray(ar: IArray);
var Bounds: TArrayBounds;
    i,j: integer;
begin
  write(OutFile,'Array: ');
  Bounds := ar.GetBounds;
  case ar.GetDimensions of
  1:
    begin
      for i := Bounds[0].LowerBound to Bounds[0].UpperBound do
        write(OutFile,'(',i,': ',ar.GetAsVariant([i]),') ');
    end;

  2:
    begin
      for i := Bounds[0].LowerBound to Bounds[0].UpperBound do
        for j := Bounds[1].LowerBound to Bounds[1].UpperBound do
          write(OutFile,'(',i,',',j,': ',ar.GetAsVariant([i,j]),') ');
    end;
  end;
  writeln(OutFile);
end;

procedure TTestBase.WriteAffectedRows(Statement: IStatement);
var  SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
begin
  Statement.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount);
  writeln(OutFile,'Select Count = ', SelectCount,' InsertCount = ',InsertCount,' UpdateCount = ', UpdateCount, ' DeleteCount = ',DeleteCount);
end;

procedure TTestBase.WriteDBInfo(DBInfo: IDBInformation);
var i, j: integer;
    bytes: TByteArray;
    ConType: integer;
    DBFileName: AnsiString;
    DBSiteName: AnsiString;
    Version: byte;
    VersionString: AnsiString;
    Users: TStringList;
begin
  for i := 0 to DBInfo.GetCount - 1 do
  with DBInfo[i] do
  case getItemType of
  isc_info_db_read_only:
     if getAsInteger <> 0 then
       writeln(OutFile,'Database is Read Only')
     else
       writeln(OutFile,'Database is Read/Write');
  isc_info_allocation:
    writeln(OutFile,'Pages =',getAsInteger);
  isc_info_base_level:
    begin
      bytes := getAsBytes;
      write(OutFile,'Base Level = ');
      WriteBytes(Bytes);
    end;
   isc_info_db_id:
     begin
       DecodeIDCluster(ConType,DBFileName,DBSiteName);
       writeln(OutFile,'Database ID = ', ConType,' FB = ', DBFileName, ' SN = ',DBSiteName);
     end;
   isc_info_implementation:
     begin
       bytes := getAsBytes;
       write(OutFile,'Implementation = ');
       WriteBytes(Bytes);
     end;
   isc_info_no_reserve:
     writeln(OutFile,'Reserved = ',getAsInteger);
   isc_info_ods_minor_version:
     writeln(OutFile,'ODS minor = ',getAsInteger);
   isc_info_ods_version:
     writeln(OutFile,'ODS major = ',getAsInteger);
   isc_info_page_size:
     writeln(OutFile,'Page Size = ',getAsInteger);
   isc_info_version:
     begin
       DecodeVersionString(Version,VersionString);
       writeln(OutFile,'Version = ',Version,': ',VersionString);
     end;
   isc_info_current_memory:
     writeln(OutFile,'Server Memory = ',getAsInteger);
   isc_info_forced_writes:
     writeln(OutFile,'Forced Writes  = ',getAsInteger);
   isc_info_max_memory:
     writeln(OutFile,'Max Memory  = ',getAsInteger);
   isc_info_num_buffers:
     writeln(OutFile,'Num Buffers  = ',getAsInteger);
   isc_info_sweep_interval:
     writeln(OutFile,'Sweep Interval  = ',getAsInteger);
   isc_info_user_names:
     begin
       Users := TStringList.Create;
       try
        write(OutFile,'Logged in Users: ');
        DecodeUserNames(Users);
        for j := 0 to Users.Count - 1 do
          write(OutFile,Users[j],',');

       finally
         Users.Free;
       end;
       writeln(OutFile);
     end;
   isc_info_fetches:
     writeln(OutFile,'Fetches  = ',getAsInteger);
   isc_info_marks:
     writeln(OutFile,'Writes  = ',getAsInteger);
   isc_info_reads:
     writeln(OutFile,'Reads  = ',getAsInteger);
   isc_info_writes:
     writeln(OutFile,'Page Writes  = ',getAsInteger);
   isc_info_backout_count:
     WriteOperationCounts('Record Version Removals',getOperationCounts);
   isc_info_delete_count:
     WriteOperationCounts('Deletes',getOperationCounts);
   isc_info_expunge_count:
     WriteOperationCounts('Expunge Count',getOperationCounts);
   isc_info_insert_count:
     WriteOperationCounts('Insert Count',getOperationCounts);
   isc_info_purge_count:
     WriteOperationCounts('Purge Count Countites',getOperationCounts);
   isc_info_read_idx_count:
     WriteOperationCounts('Indexed Reads Count',getOperationCounts);
   isc_info_read_seq_count:
     WriteOperationCounts('Sequential Table Scans',getOperationCounts);
   isc_info_update_count:
     WriteOperationCounts('Update Count',getOperationCounts);
   isc_info_db_SQL_Dialect:
     writeln(OutFile,'SQL Dialect = ',getAsInteger);
   isc_info_creation_date:
     writeln(OutFile,'Database Created: ',DateTimeToStr(getAsDateTime));
   isc_info_active_tran_count:
     writeln(OutFile,'Active Transaction Count = ',getAsInteger);
   fb_info_page_contents:
     begin
       writeln('Database Page');
       PrintHexString(getAsString);
       writeln;
     end;
   fb_info_pages_used:
     writeln(OutFile,'Pages Used = ',getAsInteger);
   fb_info_pages_free:
   writeln(OutFile,'Pages Free = ',getAsInteger);

   isc_info_truncated:
     writeln(OutFile,'Results Truncated');
   else
     writeln(OutFile,'Unknown Response ',getItemType);
  end;
end;

procedure TTestBase.WriteBytes(Bytes: TByteArray);
var i: integer;
begin
  for i := 0 to length(Bytes) - 1 do
    write(OutFile,Bytes[i],',');
  writeln(OutFile);
end;

procedure TTestBase.WriteOperationCounts(Category: AnsiString;
  ops: TDBOperationCounts);
var i: integer;
begin
  writeln(OutFile,Category,' Operation Counts');
  for i := 0 to Length(ops) - 1 do
  begin
    writeln(OutFile,'Table ID = ',ops[i].TableID);
    writeln(OutFile,'Count = ',ops[i].Count);
  end;
  writeln(OutFile);
end;

procedure TTestBase.WritePerfStats(stats: TPerfCounters);
var LargeCompFormat: string;
    ThreeSigPlacesFormat: string;
begin
  LargeCompFormat := '#' + DefaultFormatSettings.ThousandSeparator + '##0';
  ThreeSigPlacesFormat := '#0' + DefaultFormatSettings.DecimalSeparator + '000';
  writeln(OutFile,'Current memory = ', FormatFloat(LargeCompFormat,stats[psCurrentMemory]));
  writeln(OutFile,'Delta memory = ', FormatFloat(LargeCompFormat,stats[psDeltaMemory]));
  writeln(OutFile,'Max memory = ', FormatFloat(LargeCompFormat,stats[psMaxMemory]));
  writeln(OutFile,'Elapsed time= ', FormatFloat(ThreeSigPlacesFormat,stats[psRealTime]/1000),' sec');
  writeln(OutFile,'Cpu = ', FormatFloat(ThreeSigPlacesFormat,stats[psUserTime]/1000),' sec');
  writeln(OutFile,'Buffers = ', FormatFloat('#0',stats[psBuffers]));
  writeln(OutFile,'Reads = ', FormatFloat('#0',stats[psReads]));
  writeln(OutFile,'Writes = ', FormatFloat('#0',stats[psWrites]));
  writeln(OutFile,'Fetches = ', FormatFloat('#0',stats[psFetches]));
end;

procedure TTestBase.CheckActivity(Attachment: IAttachment);
begin
    writeln(OutFile,'Database Activity = ',Attachment.HasActivity)
end;

procedure TTestBase.CheckActivity(Transaction: ITransaction);
begin
  writeln(OutFile,'Transaction Activity = ',Transaction.HasActivity)
end;

{ TTestManager }

procedure TTestManager.CleanUp;
var DPB: IDPB;
    Attachment: IAttachment;
begin
  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).setAsString(GetUserName);
  DPB.Add(isc_dpb_password).setAsString(GetPassword);
  Attachment := FirebirdAPI.OpenDatabase(GetNewDatabaseName,DPB,false);
  if Attachment <> nil then
    Attachment.DropDatabase;
  Attachment := FirebirdAPI.OpenDatabase(GetSecondNewDatabaseName,DPB,false);
  if Attachment <> nil then
    Attachment.DropDatabase;
end;

function TTestManager.GetFirebirdAPI: IFirebirdAPI;
begin
  if FFirebirdAPI = nil then
    FFirebirdAPI := IB.FirebirdAPI;
  Result := FFirebirdAPI;
end;

function TTestManager.GetIndexByTestID(aTestID: AnsiString): integer;
begin
  try
    Result := FTests.IndexOf(aTestID);
  except
    raise Exception.CreateFmt('Invalid Test ID - %s',[aTestID]);
  end;
  if Result = -1 then
    raise Exception. CreateFmt('Invalid Test ID - %s',[aTestID]);
end;

constructor TTestManager.Create;
begin
  inherited Create;
  FTests := TStringList.Create;
  FTests.Sorted := true;
  FTests.Duplicates := dupError;
  FNewDatabaseName :=  GetTempDir + 'fbtestsuite.fdb';
  FSecondNewDatabaseName :=  GetTempDir + 'fbtestsuite2.fdb';
  FUserName := 'SYSDBA';
  FPassword := 'masterkey';
  FEmployeeDatabaseName := 'employee';
  FBackupFileName := GetTempDir + 'testbackup.gbk';
  FServer := 'localhost';
end;

destructor TTestManager.Destroy;
var i: integer;
begin
  if assigned(FTests) then
  begin
    for i := 0 to FTests.Count - 1 do
      FTests.Objects[i].Free;
    FTests.Free;
  end;
  inherited Destroy;
end;

function TTestManager.GetUserName: AnsiString;
begin
  Result := FUserName;
end;

function TTestManager.GetPassword: AnsiString;
begin
  Result := FPassword;
end;

function TTestManager.GetEmployeeDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FEmployeeDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FEmployeeDatabaseName, inet,FPortNo);
end;

function TTestManager.GetNewDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FNewDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FNewDatabaseName, inet,FPortNo);
end;

function TTestManager.GetSecondNewDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FSecondNewDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FSecondNewDatabaseName, inet,FPortNo);
end;

function TTestManager.GetBackupFileName: AnsiString;
begin
  Result := FBackupFileName;
end;

procedure TTestManager.RunAll;
var i: integer;
begin
  CleanUP;
  for i := 0 to FTests.Count - 1 do
    with TTestBase(FTests.Objects[i]) do
  begin
    writeln(OutFile,'Running ' + TestTitle);
    writeln(ErrOutput,'Running ' + TestTitle);
    try
      RunTest('UTF8',3);
    except on E:Exception do
      begin
        writeln(OutFile,'Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
    writeln(OutFile);
    writeln(OutFile);
  end;
end;

procedure TTestManager.Run(TestID: AnsiString);
begin
  CleanUp;
  with TTestBase(FTests.Objects[GetIndexByTestID(TestID)]) do
  begin
    writeln(OutFile,'Running ' + TestTitle);
    writeln(ErrOutput,'Running ' + TestTitle);
    try
      RunTest('UTF8',3);
    except on E:Exception do
      begin
        writeln(OutFile,'Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
    writeln(OutFile);
    writeln(OutFile);
  end;
end;

procedure TTestManager.SetClientLibraryPath(aLibName: string);
begin
  FFirebirdAPI := LoadFBLibrary(aLibName).GetFirebirdAPI;
  FClientLibraryPath := aLibName;
end;

procedure TTestManager.SetUserName(aValue: AnsiString);
begin
  FUserName := aValue;
end;

procedure TTestManager.SetPassword(aValue: AnsiString);
begin
  FPassword := aValue;
end;

procedure TTestManager.SetEmployeeDatabaseName(aValue: AnsiString);
begin
  FEmployeeDatabaseName := aValue;
end;

procedure TTestManager.SetNewDatabaseName(aValue: AnsiString);
begin
  FNewDatabaseName := aValue;
end;

procedure TTestManager.SetSecondNewDatabaseName(aValue: AnsiString);
begin
  FSecondNewDatabaseName := aValue;
end;

procedure TTestManager.SetBackupFileName(aValue: AnsiString);
begin
  FBackupFileName := aValue;
end;

procedure TTestManager.SetServerName(AValue: AnsiString);
begin
  if FServer = AValue then Exit;
  FServer := AValue;
end;

procedure TTestManager.SetPortNum(aValue: AnsiString);
begin
  FPortNo := aValue;
end;

end.

