unit TestApplication;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

interface

uses
  Classes, SysUtils, CustApp, Firebird, IB, IBUtils, FmtBCD;

{$IF not defined(LineEnding)}
const
  LineEnding = #$0D#$0A;
{$IFEND}

const
  Copyright = 'Copyright MWA Software 2016-2020';

type
  TTestApplication = class;

  { TTestBase }

  TTestBase = class
  private
    FOwner: TTestApplication;
    function GetFirebirdAPI: IFirebirdAPI;
    procedure SetOwner(AOwner: TTestApplication);
  protected
    FHexStrings: boolean;
    procedure DumpBCD(bcd: tBCD);
    procedure ClientLibraryPathChanged; virtual;
    procedure CreateObjects(Application: TCustomApplication); virtual;
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
    procedure WriteSQLData(aValue: ISQLData);
    procedure CheckActivity(Attachment: IAttachment); overload;
    procedure CheckActivity(Transaction: ITransaction); overload;
    procedure InitTest; virtual;
    function SkipTest: boolean; virtual;
  public
    constructor Create(aOwner: TTestApplication);  virtual;
    function TestTitle: AnsiString;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); virtual; abstract;
    property Owner: TTestApplication read FOwner;
    property TestID: string read GetTestID;
  end;

  TTest = class of TTestBase;

  { TTestApplication }

  TTestApplication = class(TCustomApplication)
  private
    class var FTests: TStringList;
  private
    class procedure CreateTestList;
    class procedure DestroyTestList;
  private
    FClientLibraryPath: string;
    FServer: AnsiString;
    FEmployeeDatabaseName: AnsiString;
    FNewDatabaseName: AnsiString;
    FSecondNewDatabaseName: AnsiString;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FBackupFileName: AnsiString;
    FShowStatistics: boolean;
    FFirebirdAPI: IFirebirdAPI;
    FPortNo: AnsiString;
    FCreateObjectsDone: boolean;
    procedure CleanUp;
    function GetFirebirdAPI: IFirebirdAPI;
    function GetIndexByTestID(aTestID: AnsiString): integer;
    procedure SetClientLibraryPath(aLibName: string);
    procedure SetUserName(aValue: AnsiString);
    procedure SetPassword(aValue: AnsiString);
    procedure SetEmployeeDatabaseName(aValue: AnsiString);
    procedure SetNewDatabaseName(aValue: AnsiString);
    procedure SetSecondNewDatabaseName(aValue: AnsiString);
    procedure SetBackupFileName(aValue: AnsiString);
    procedure SetServerName(AValue: AnsiString);
    procedure SetPortNum(aValue: AnsiString);
  protected
    function GetShortOptions: AnsiString; virtual;
    function GetLongOptions: AnsiString; virtual;
    procedure GetParams(var DoPrompt: boolean; var TestID: AnsiString); virtual;
    procedure DoRun; override;
    procedure WriteHelp; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetUserName: AnsiString;
    function GetPassword: AnsiString;
    function GetEmployeeDatabaseName: AnsiString;
    function GetNewDatabaseName: AnsiString;
    function GetSecondNewDatabaseName: AnsiString;
    function GetBackupFileName: AnsiString;
    procedure RunAll;
    procedure RunTest(TestID: AnsiString);
    property ShowStatistics: boolean read FShowStatistics write FShowStatistics;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    property Server: AnsiString read FServer;
    property PortNo: AnsiString read FPortNo;
    property ClientLibraryPath: string read FClientLibraryPath;
  end;

  ESkipException = class(Exception);

var
  TestApp: TTestApplication = nil;

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
  TTestApplication.CreateTestList;
  test := aTest.Create(TestApp);
  TTestApplication.FTests.AddObject(test.GetTestID,test);
  if TestApp <> nil then
    test.CreateObjects(TestApp);
end;

{ TTestBase }

constructor TTestBase.Create(aOwner: TTestApplication);
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

procedure TTestBase.SetOwner(AOwner: TTestApplication);
begin
  FOwner := AOwner;
end;

procedure TTestBase.DumpBCD(bcd: tBCD);
var i,l: integer;
begin
  with bcd do
  begin
    writeln(OutFile,'  Precision = ',bcd.Precision);
    writeln(OutFile,'  Sign = ',(SignSpecialPlaces and $80) shr 7);
    writeln(OutFile,'  Special = ', (SignSpecialPlaces and $40) shl 6);
    writeln(OutFile,'  Places = ', SignSpecialPlaces and $7F);
    write(OutFile,'  Digits = ');
    l := Precision div 2;
    if not odd(Precision) then l := l - 1;
    for i := 0 to l do
      write(OutFile,Format('%.2x',[Fraction[i]]),' ');
    writeln(OutFile);
  end;
end;

procedure TTestBase.ClientLibraryPathChanged;
begin
  //Do nothing yet
end;

procedure TTestBase.CreateObjects(Application: TCustomApplication);
begin
  //Do nothing yet
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
begin
  for i := 0 to aValue.getCount - 1 do
    WriteSQLData(aValue[i]);
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

procedure TTestBase.WriteSQLData(aValue: ISQLData);
var s: AnsiString;
    dt: TDateTime;
    dstOffset: SmallInt;
    aTimeZone: AnsiString;
begin
  if aValue.IsNull then
    writeln(OutFile,aValue.Name,' = NULL')
  else
  case aValue.SQLType of
  SQL_ARRAY:
    begin
      write(OutFile, aValue.Name,' = ');
      if not aValue.IsNull then
        WriteArray(aValue.AsArray)
      else
        writeln(OutFile,'NULL');
    end;
  SQL_FLOAT,SQL_DOUBLE,
  SQL_D_FLOAT:
    writeln(OutFile, aValue.Name,' = ',FormatFloat('#,##0.00',aValue.AsFloat));

  SQL_INT64:
    if aValue.Scale <> 0 then
      writeln(OutFile, aValue.Name,' = ',FormatFloat('#,##0.00',aValue.AsFloat))
    else
      writeln(OutFile,aValue.Name,' = ',aValue.AsString);

  SQL_BLOB:
    if aValue.IsNull then
      writeln(OutFile,aValue.Name,' = (null blob)')
    else
    if aValue.SQLSubType = 1 then
    begin
      s := aValue.AsString;
      if FHexStrings then
      begin
        write(OutFile,aValue.Name,' = ');
        PrintHexString(s);
        writeln(OutFile,' (Charset Id = ',aValue.GetCharSetID, ' Codepage = ',StringCodePage(s),')');
      end
      else
      begin
        writeln(OutFile,aValue.Name,' (Charset Id = ',aValue.GetCharSetID, ' Codepage = ',StringCodePage(s),')');
        writeln(OutFile);
        writeln(OutFile,s);
      end
    end
    else
      writeln(OutFile,aValue.Name,' = (blob), Length = ',aValue.AsBlob.GetBlobSize);

  SQL_TEXT,SQL_VARYING:
  begin
    s := aValue.AsString;
    if FHexStrings then
    begin
      write(OutFile,aValue.Name,' = ');
      PrintHexString(s);
      writeln(OutFile,' (Charset Id = ',aValue.GetCharSetID, ' Codepage = ',StringCodePage(s),')');
    end
    else
    if aValue.GetCharSetID > 0 then
      writeln(OutFile,aValue.Name,' = ',s,' (Charset Id = ',aValue.GetCharSetID, ' Codepage = ',StringCodePage(s),')')
    else
      writeln(OutFile,aValue.Name,' = ',s);
  end;

  SQL_TIMESTAMP:
    writeln(OutFile,aValue.Name,' = ',FBFormatDateTime('yyyy/mm/dd hh:nn:ss.zzzz',aValue.AsDateTime));
  SQL_TYPE_DATE:
    writeln(OutFile,aValue.Name,' = ',FBFormatDateTime('yyyy/mm/dd',aValue.AsDate));
  SQL_TYPE_TIME:
    writeln(OutFile,aValue.Name,' = ',FBFormatDateTime('hh:nn:ss.zzzz',aValue.AsTime));
  SQL_TIMESTAMP_TZ,
  SQL_TIMESTAMP_TZ_EX:
  begin
    aValue.GetAsDateTime(dt,dstOffset,aTimeZone);
    writeln(OutFile,aValue.Name,' =');
    writeln(OutFile,'  AsString  = ',aValue.GetAsString);
    writeln(OutFile,'  Formatted = ',FBFormatDateTime('yyyy/mm/dd hh:nn:ss.zzzz',dt),' ',aTimeZone);
    writeln(OutFile,'  TimeZoneID = ',aValue.GetStatement.GetAttachment.GetTimeZoneServices.TimeZoneName2TimeZoneID(aTimeZone));
    writeln(OutFile,'  Time Zone Name = ',aTimeZone);
    writeln(OutFile,'  UTC Time = ',DateTimeToStr( aValue.GetAsUTCDateTime));
    writeln(OutFile,'  DST Offset = ',dstOffset);
  end;
  SQL_TIME_TZ,
  SQL_TIME_TZ_EX:
  begin
    aValue.GetAsDateTime(dt,dstOffset,aTimeZone);
    writeln(OutFile,aValue.Name,' =');
    writeln(OutFile,'  AsString =  ',aValue.GetAsString);
    writeln(OutFile,'  Formatted = ',FBFormatDateTime('hh:nn:ss.zzzz',dt),' ',aTimeZone);
    writeln(OutFile,'  TimeZoneID = ',aValue.GetStatement.GetAttachment.GetTimeZoneServices.TimeZoneName2TimeZoneID(aTimeZone));
    writeln(OutFile,'  Time Zone Name = ',aTimeZone);
    writeln(OutFile,'  UTC Time = ',TimeToStr( aValue.GetAsUTCDateTime));
    writeln(OutFile,'  DST Offset = ',dstOffset);
  end;

  else
    writeln(OutFile,aValue.Name,' = ',aValue.AsString);
  end;
end;

procedure TTestBase.CheckActivity(Attachment: IAttachment);
begin
    writeln(OutFile,'Database Activity = ',Attachment.HasActivity)
end;

procedure TTestBase.CheckActivity(Transaction: ITransaction);
begin
  writeln(OutFile,'Transaction Activity = ',Transaction.HasActivity)
end;

procedure TTestBase.InitTest;
begin
  //Do nothing yet
end;

function TTestBase.SkipTest: boolean;
begin
  Result := false;
end;

{ TTestApplication }

class procedure TTestApplication.CreateTestList;
begin
  if FTests = nil then
  begin
    FTests := TStringList.Create;
    FTests.Sorted := true;
    FTests.Duplicates := dupError;
  end;
end;

class procedure TTestApplication.DestroyTestList;
var i: integer;
begin
  if assigned(FTests) then
  begin
    for i := 0 to FTests.Count - 1 do
      FTests.Objects[i].Free;
    FreeAndNil(FTests);
  end;
end;

procedure TTestApplication.CleanUp;
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

function TTestApplication.GetFirebirdAPI: IFirebirdAPI;
begin
  if FFirebirdAPI = nil then
    FFirebirdAPI := IB.FirebirdAPI;
  Result := FFirebirdAPI;
end;

function TTestApplication.GetIndexByTestID(aTestID: AnsiString): integer;
begin
  try
    Result := FTests.IndexOf(aTestID);
  except
    raise Exception.CreateFmt('Invalid Test ID - %s',[aTestID]);
  end;
  if Result = -1 then
    raise Exception. CreateFmt('Invalid Test ID - %s',[aTestID]);
end;

constructor TTestApplication.Create(AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);
  TestApp := self;
  CreateTestList;
  FNewDatabaseName :=  GetTempDir + 'fbtestsuite.fdb';
  FSecondNewDatabaseName :=  GetTempDir + 'fbtestsuite2.fdb';
  FUserName := 'SYSDBA';
  FPassword := 'masterkey';
  FEmployeeDatabaseName := 'employee';
  FBackupFileName := GetTempDir + 'testbackup.gbk';
  FServer := 'localhost';
  for i := 0 to FTests.Count - 1 do
  begin
    TTestBase(FTests.Objects[i]).SetOwner(self);
    TTestBase(FTests.Objects[i]).CreateObjects(self);
  end;
end;

destructor TTestApplication.Destroy;
begin
  TestApp := nil;
  inherited Destroy;
end;

function TTestApplication.GetUserName: AnsiString;
begin
  Result := FUserName;
end;

function TTestApplication.GetPassword: AnsiString;
begin
  Result := FPassword;
end;

function TTestApplication.GetEmployeeDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FEmployeeDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FEmployeeDatabaseName, inet,FPortNo);
end;

function TTestApplication.GetNewDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FNewDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FNewDatabaseName, inet,FPortNo);
end;

function TTestApplication.GetSecondNewDatabaseName: AnsiString;
begin
  if FirebirdAPI.GetClientMajor < 3 then
    Result := MakeConnectString(FServer,  FSecondNewDatabaseName, TCP,FPortNo)
  else
    Result := MakeConnectString(FServer,  FSecondNewDatabaseName, inet,FPortNo);
end;

function TTestApplication.GetBackupFileName: AnsiString;
begin
  Result := FBackupFileName;
end;

procedure TTestApplication.RunAll;
var i: integer;
begin
  CleanUP;
  for i := 0 to FTests.Count - 1 do
   with TTestBase(FTests.Objects[i]) do
  if SkipTest then
    writeln(OutFile,' Skipping ' + TestID)
  else
  begin
    writeln(OutFile,'Running ' + TestTitle);
    writeln(ErrOutput,'Running ' + TestTitle);
    try
      RunTest('UTF8',3);
    except
      on E:ESkipException do
        writeln(OutFile,'Skipping Test: ' + E.Message);
      on E:Exception do
      begin
        writeln(OutFile,'Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
    writeln(OutFile);
    writeln(OutFile);
  end;
end;

procedure TTestApplication.RunTest(TestID: AnsiString);
begin
  CleanUp;
  with TTestBase(FTests.Objects[GetIndexByTestID(TestID)]) do
  if not SkipTest then
  begin
    writeln(OutFile,'Running ' + TestTitle);
    writeln(ErrOutput,'Running ' + TestTitle);
    try
      InitTest;
      RunTest('UTF8',3);
    except
      on E:ESkipException do
        writeln(OutFile,'Skipping Test: ' + E.Message);
      on E:Exception do
      begin
        writeln(OutFile,'Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
    writeln(OutFile);
    writeln(OutFile);
  end;
end;

procedure TTestApplication.SetClientLibraryPath(aLibName: string);
var i: integer;
begin
  FFirebirdAPI := LoadFBLibrary(aLibName).GetFirebirdAPI;
  FClientLibraryPath := aLibName;
  for i := 0 to FTests.Count - 1 do
    TTestBase(FTests.Objects[i]).ClientLibraryPathChanged;

end;

procedure TTestApplication.SetUserName(aValue: AnsiString);
begin
  FUserName := aValue;
end;

procedure TTestApplication.SetPassword(aValue: AnsiString);
begin
  FPassword := aValue;
end;

procedure TTestApplication.SetEmployeeDatabaseName(aValue: AnsiString);
begin
  FEmployeeDatabaseName := aValue;
end;

procedure TTestApplication.SetNewDatabaseName(aValue: AnsiString);
begin
  FNewDatabaseName := aValue;
end;

procedure TTestApplication.SetSecondNewDatabaseName(aValue: AnsiString);
begin
  FSecondNewDatabaseName := aValue;
end;

procedure TTestApplication.SetBackupFileName(aValue: AnsiString);
begin
  FBackupFileName := aValue;
end;

procedure TTestApplication.SetServerName(AValue: AnsiString);
begin
  if FServer = AValue then Exit;
  FServer := AValue;
end;

procedure TTestApplication.SetPortNum(aValue: AnsiString);
begin
  FPortNo := aValue;
end;

function TTestApplication.GetShortOptions: AnsiString;
begin
  Result := 'htupensbolrSPX';
end;

function TTestApplication.GetLongOptions: AnsiString;
begin
  Result := 'help test user passwd employeedb newdbname secondnewdbname backupfile '+
            'outfile fbclientlibrary server stats port prompt';
end;

procedure TTestApplication.GetParams(var DoPrompt: boolean; var TestID: AnsiString);
var ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions(GetShortOptions,GetLongOptions);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('t') then
    TestID := GetOptionValue('t');

  DoPrompt := HasOption('X','prompt');

  if HasOption('u','user') then
    SetUserName(GetOptionValue('u'));

  if HasOption('p','passwd') then
    SetPassword(GetOptionValue('p'));

  if HasOption('e','employeedb') then
    SetEmployeeDatabaseName(GetOptionValue('e'));

  if HasOption('n','newdbname') then
    SetNewDatabaseName(GetOptionValue('n'));

  if HasOption('s','secondnewdbname') then
    SetSecondNewDatabaseName(GetOptionValue('s'));

  if HasOption('b','backupfile') then
    SetBackupFileName(GetOptionValue('b'));

  if HasOption('l','fbclientlibrary') then
    SetClientLibraryPath(GetOptionValue('l'));

  if HasOption('r','server') then
    SetServerName(GetOptionValue('r'));

  if HasOption('o','outfile') then
  begin
    system.Assign(outFile,GetOptionValue('o'));
    ReWrite(outFile);
  end;

  if HasOption('P','port') then
    SetPortNum(GetOptionValue('P'));

  ShowStatistics := HasOption('S','stats');
end;

procedure TTestApplication.DoRun;
var
  DoPrompt: boolean;
  TestID: AnsiString;
begin
  OutFile := stdout;
  GetParams(DoPrompt,TestID);
  {$IF declared(SetTextCodePage)}
  {Ensure consistent UTF-8 output}
  SetTextCodePage(OutFile,cp_utf8);
  {$IFEND}

  {Ensure consistent date reporting across platforms}
  DefaultFormatSettings.ShortDateFormat := 'yyyy/m/d';
  DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS';
  DefaultFormatSettings.DateSeparator := '/';

  writeln(OutFile,Title);
  writeln(OutFile,Copyright);
  writeln(OutFile);
  writeln(OutFile,'Starting Tests');
  writeln(OutFile,'Client API Version = ',FirebirdAPI.GetImplementationVersion);
  writeln(OutFile,'Firebird Environment Variable = ',GetEnvironmentVariable('FIREBIRD'));
  if FirebirdAPI.GetClientMajor >= 3 then
  begin
    writeln(OutFile,'Firebird Bin Directory = ', IMaster(FirebirdAPI.GetIMaster).getConfigManager.getDirectory(IConfigManager.DIR_BIN));
    writeln(OutFile,'Firebird Conf Directory = ', IMaster(FirebirdAPI.GetIMaster).getConfigManager.getDirectory(IConfigManager.DIR_CONF));
  end;
  writeln(OutFile,'Firebird Client Library Path = ',FirebirdAPI.GetFBLibrary.GetLibraryFilePath);

  try
    if TestID = '' then
      RunAll
    else
      RunTest(TestID);
  except on E: Exception do
    writeln('Exception: ',E.Message);
  end;

  writeln(OutFile,'Test Suite Ends');
  Flush(OutFile);
  {$IFDEF WINDOWS}
  if DoPrompt then
  begin
    write('Press Entry to continue');
    readln; {uncomment if running from IDE and console window closes before you can view results}
  end;
  {$ENDIF}

  // stop program loop
  Terminate;
end;

procedure TTestApplication.WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ExeName, ' -h');
end;

finalization
  TTestApplication.DestroyTestList;

end.

