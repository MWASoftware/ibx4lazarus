(*
 *  MWA Software Test suite. This unit provides common
 *  code for all Firebird Database tests.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016-2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit TestApplication;
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{$IF not defined (DCC) and not defined (FPC)}
{$DEFINE DCC}
{$IFEND}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}CustApp,{$ENDIF} FirebirdOOAPI, IB, IBUtils, FmtBCD, FBClientLib;

{$IF not defined(LineEnding)}
const
  {$IFDEF WINDOWS}
  LineEnding = #$0D#$0A;
  {$ELSE}
  LineEnding = #$0A;
  {$ENDIF}
{$IFEND}

const
  Copyright = 'Copyright MWA Software 2016-2021';

type
  {$IFDEF DCC}
  TCustomApplication = class(TComponent)
  private
    FTitle: string;
  protected
    procedure DoRun; virtual; abstract;
  public
    function Exename: string;
    procedure Run;
    procedure Terminate;
    property Title: string read FTitle write FTitle;
  end;
  {$ENDIF}

  TTestApplication = class;

  { TTestBase }

  TTestBase = class
  private
    FOwner: TTestApplication;
    FFloatTpl: AnsiString;
    FCursorSeq: integer;
    function GetFirebirdAPI: IFirebirdAPI;
    procedure SetOwner(AOwner: TTestApplication);
    {$if declared(TJnlEntry) }
    procedure HandleOnJnlEntry(JnlEntry: TJnlEntry);
    {$endif}
  protected
    FHexStrings: boolean;
    FShowBinaryBlob: boolean;
    procedure DumpBCD(bcd: tBCD);
    procedure ClientLibraryPathChanged; virtual;
    procedure CreateObjects(Application: TTestApplication); virtual;
    function ExtractDBName(ConnectString: AnsiString): AnsiString;
    function GetTestID: AnsiString; virtual; abstract;
    function GetTestTitle: AnsiString; virtual; abstract;
    procedure PrintHexString(s: AnsiString);
    procedure PrintDPB(DPB: IDPB);
    procedure PrintTPB(TPB: ITPB);
    procedure PrintSPB(SPB: ISPB);
    procedure PrintMetaData(meta: IMetaData);
    procedure ParamInfo(SQLParams: ISQLParams);
    {$if declared(TJnlEntry) }
    procedure PrintJournalFile(aFileName: AnsiString);
    procedure PrintJournalTable(Attachment: IAttachment);
    {$ifend}
    function ReportResults(Statement: IStatement; ShowCursorName: boolean=false): IResultSet;
    procedure ReportResult(aValue: IResults);
    function StringToHex(octetString: string; MaxLineLength: integer=0): string;
    procedure WriteAttachmentInfo(Attachment: IAttachment);
    procedure WriteArray(ar: IArray);
    procedure WriteAffectedRows(Statement: IStatement);
    procedure WriteDBInfo(DBInfo: IDBInformation);
    procedure WriteTRInfo(TrInfo: ITrInformation);
    procedure WriteBytes(Bytes: TByteArray);
    procedure WriteOperationCounts(Category: AnsiString; ops: TDBOperationCounts);
    procedure WritePerfStats(stats: TPerfCounters);
    procedure WriteSQLData(aValue: ISQLData);
    procedure CheckActivity(Attachment: IAttachment); overload;
    procedure CheckActivity(Transaction: ITransaction); overload;
    procedure InitTest; virtual;
    function SkipTest: boolean; virtual;
    procedure ProcessResults; virtual;
    procedure SetFloatTemplate(tpl: Ansistring);
  public
    constructor Create(aOwner: TTestApplication);  virtual;
    function ChildProcess: boolean; virtual;
    function TestTitle: AnsiString; virtual;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); virtual; abstract;
    property Owner: TTestApplication read FOwner;
    property TestID: AnsiString read GetTestID;
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
    FTestOption: AnsiString;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FBackupFileName: AnsiString;
    FShowStatistics: boolean;
    FFirebirdAPI: IFirebirdAPI;
    FPortNo: AnsiString;
    FCreateObjectsDone: boolean;
    FQuiet: boolean;
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
    procedure SetTestOption(aValue: AnsiString);
  protected
    {$IFDEF FPC}
    function GetShortOptions: AnsiString; virtual;
    function GetLongOptions: AnsiString; virtual;
    {$ENDIF}
    procedure GetParams(var DoPrompt: boolean; var TestID: string); virtual;
    procedure DoRun; override;
    procedure DoTest(index: integer);
    procedure SetFormatSettings; virtual;
    procedure WriteHelp; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetUserName: AnsiString;
    function GetPassword: AnsiString;
    function GetEmployeeDatabaseName: AnsiString;
    function GetNewDatabaseName: AnsiString;
    function GetTempDatabaseName: AnsiString;
    function GetSecondNewDatabaseName: AnsiString;
    function GetBackupFileName: AnsiString;
    procedure RunAll;
    procedure RunTest(TestID: AnsiString);
    property ShowStatistics: boolean read FShowStatistics write FShowStatistics;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    property Server: AnsiString read FServer;
    property PortNo: AnsiString read FPortNo;
    property ClientLibraryPath: string read FClientLibraryPath;
    property TestOption: AnsiString read FTestOption write SetTestOption;
    property Quiet: boolean read FQuiet;
  end;

  { TMsgHash }

  TMsgHash = class
  private
    FFinalised: boolean;
  protected
    procedure CheckNotFinalised;
  public
    procedure AddText(aText: AnsiString); virtual; abstract;
    procedure Finalise; virtual;
    function SameHash(otherHash: TMsgHash): boolean;
    function Digest: AnsiString; virtual; abstract;
    class function CreateMsgHash: TMsgHash;
    property Finalised: boolean read FFinalised;
  end;

  ESkipException = class(Exception);

var
  TestApp: TTestApplication = nil;

var OutFile: text;

procedure RegisterTest(aTest: TTest);

implementation

{$IFNDEF MSWINDOWS}
uses MD5;
{$ENDIF}

{$IFDEF MSWINDOWS}
uses {$IFDEF FPC}MD5,{$ENDIF} windows;

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
//  if TestApp <> nil then
//    test.CreateObjects(TestApp);
end;

{$IFDEF FPC}
type

  { TMD5MsgHash }

  TMD5MsgHash = class(TMsgHash)
  private
    FMD5Context: TMDContext;
    FDigest: TMDDigest;
  public
    constructor Create;
    procedure AddText(aText: AnsiString); override;
    procedure Finalise; override;
    function Digest: AnsiString; override;
  end;

{$DEFINE MSG_HASH_AVAILABLE}

{ TMD5MsgHash }

constructor TMD5MsgHash.Create;
begin
  inherited Create;
  MDInit(FMD5Context,MD_VERSION_5);
end;

procedure TMD5MsgHash.AddText(aText: AnsiString);
begin
  CheckNotFinalised;
  MDUpdate(FMD5Context,PAnsiChar(aText)^,Length(aText));
end;

procedure TMD5MsgHash.Finalise;
begin
  CheckNotFinalised;
  MDFinal(FMD5Context,FDigest);
  inherited Finalise;
end;

function TMD5MsgHash.Digest: AnsiString;
begin
  if not FFinalised then
    Finalise;
  Result :=  MD5Print(FDigest);
end;

class function TMsgHash.CreateMsgHash: TMsgHash;
begin
  Result := TMD5MsgHash.Create;
end;
{$ENDIF}

{$IFNDEF MSG_HASH_AVAILABLE}
type

  { TSimpleMsgHash }

  TSimpleMsgHash = class(TMsgHash)
  private
    FDigest: Int64;
    Finalised: boolean;
  public
    procedure AddText(aText: AnsiString); override;
    function Digest: AnsiString; override;
  end;

{ TSimpleMsgHash }

procedure TSimpleMsgHash.AddText(aText: AnsiString);
const
  modulus = high(Int64) div 100;
var i: integer;
begin
  CheckNotFinalised;
  for i := 1 to length(aText) do
    FDigest := (FDigest * 7 + ord(aText[i])) mod modulus;
end;

function TSimpleMsgHash.Digest: AnsiString;
begin
  if not Finalised then
    Finalise;
  Result := IntToStr(FDigest);
end;

class function TMsgHash.CreateMsgHash: TMsgHash;
begin
  Result := TSimpleMsgHash.Create;
end;

{$ENDIF}

procedure TMsgHash.CheckNotFinalised;
begin
  if FFinalised then
    raise Exception.Create('Digest has been finalised');
end;

procedure TMsgHash.Finalise;
begin
  FFinalised := true;
end;

function TMsgHash.SameHash(otherHash: TMsgHash): boolean;
begin
  Result := Digest = OtherHash.Digest;
end;

{ TTestBase }

constructor TTestBase.Create(aOwner: TTestApplication);
begin
  inherited Create;
  FOwner := aOwner;
  FFloatTpl := '#,###.00';
end;

function TTestBase.ChildProcess: boolean;
begin
  Result := false;
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

{$if declared(TJnlEntry)}
procedure TTestBase.HandleOnJnlEntry(JnlEntry: TJnlEntry);
begin
 with JnlEntry do
 begin
   {$IFNDEF FPC}
   writeln(OutFile,'Journal Entry = ',ord(JnlEntryType),'(', TJournalProcessor.JnlEntryText(JnlEntryType),')');
   {$ELSE}
   writeln(OutFile,'Journal Entry = ',JnlEntryType,'(', TJournalProcessor.JnlEntryText(JnlEntryType),')');
   {$ENDIF}
   writeln(OutFIle,'Timestamp = ',FBFormatDateTime('yyyy/mm/dd hh:nn:ss.zzzz',Timestamp));
   writeln(Outfile,'Attachment ID = ',AttachmentID);
   writeln(OutFile,'Session ID = ',SessionID);
   writeln(OutFile,'Transaction ID = ',TransactionID);
   case JnlEntry.JnlEntryType of
   jeTransStart:
     begin
       writeln(OutFile,'Transaction Name = "',TransactionName,'"');
       PrintTPB(TPB);
       {$IFNDEF FPC}
       writeln(OutFile,'Default Completion = ',ord(DefaultCompletion));
       {$ELSE}
       writeln(OutFile,'Default Completion = ',DefaultCompletion);
       {$ENDIF}
     end;

   jeQuery:
     begin
       writeln(OutFile,'Query = ',QueryText);
     end;

   jeTransCommitRet,
   jeTransRollbackRet:
     writeln(Outfile,'Old TransactionID = ',OldTransactionID);
   end;
 end;
 writeln(OutFile);
end;
{$ifend}

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

procedure TTestBase.CreateObjects(Application: TTestApplication);
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
  writeln(OutFile,'DPB: Item Count = ', DPB.getCount);
  for i := 0 to DPB.getCount - 1 do
  begin
    write(OutFile,'  ',DPB[i].getParamTypeName);
    if DPB[i].AsString <> '' then
      writeln(Outfile,' = ', DPB[i].AsString)
    else
      writeln(OutFile);
  end;
  writeln(OutFile);
end;

procedure TTestBase.PrintTPB(TPB: ITPB);
var i: integer;
begin
  writeln(OutFile,'TPB: Item Count = ', TPB.getCount);
  for i := 0 to TPB.getCount - 1 do
  begin
    write(OutFile,'  ',TPB[i].getParamTypeName);
    if TPB[i].AsString <> '' then
      writeln(Outfile,' = ', TPB[i].AsString)
    else
      writeln(OutFile);
  end;
  writeln(OutFile);
end;

procedure TTestBase.PrintSPB(SPB: ISPB);
var i: integer;
begin
  writeln(OutFile,'SPB: Item Count = ', SPB.getCount);
  for i := 0 to SPB.getCount - 1 do
  begin
    write(OutFile,'  ',SPB[i].getParamTypeName);
    if SPB[i].AsString <> '' then
      writeln(Outfile,' = ', SPB[i].AsString)
    else
      writeln(OutFile);
  end;
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
    if not IsNull then
    begin
      if getCharSetID = 1 then
      begin
        PrintHexString(getAsString);
        writeln(Outfile);
      end
      else
        writeln(Outfile,'Value = ',getAsString);
    end;
    writeln(OutFile);
  end;
end;

{$if declared(TJnlEntry) }
procedure TTestBase.PrintJournalFile(aFileName: AnsiString);
begin
 writeln(OutFile,'Journal Entries');
 TJournalProcessor.Execute(aFileName,FirebirdAPI,HandleOnJnlEntry);
end;

procedure TTestBase.PrintJournalTable(Attachment: IAttachment);
var Results: IResultSet;
begin
  writeln(OutFile,'Journal Table');
  Results := Attachment.OpenCursorAtStart('Select * From IBX$JOURNALS');
  while not Results.IsEof do
  begin
    ReportResult(Results);
    Results.Fetchnext;
  end;
end;
{$ifend}

function TTestBase.ReportResults(Statement: IStatement; ShowCursorName: boolean): IResultSet;
begin
  Result := Statement.OpenCursor;
  try
   if ShowCursorName then
      writeln(Outfile,'Results for Cursor: ',Result.GetCursorName);
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
    Result := Result + ToHex(byte(octetString[i]));
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
          Result := Result + ToHex(byte(octetString[i]));
        inc(i);
      end;
      Result := Result + LineEnding;
  end;
end;

procedure TTestBase.WriteAttachmentInfo(Attachment: IAttachment);
begin
 writeln(outfile,'DB Connect String = ',Attachment.GetConnectString);
 writeln(outfile,'DB Charset ID = ',Attachment.GetDefaultCharSetID);
 writeln(outfile,'DB SQL Dialect = ',Attachment.GetSQLDialect);
 writeln(outfile,'DB Remote Protocol = ', Attachment.GetRemoteProtocol);
 writeln(outfile,'DB ODS Major Version = ',Attachment.GetODSMajorVersion);
 writeln(outfile,'DB ODS Minor Version = ',Attachment.GetODSMinorVersion);
 writeln(outfile,'User Authentication Method = ',Attachment.GetAuthenticationMethod);
 writeln(outfile,'Firebird Library Path = ',Attachment.getFirebirdAPI.GetFBLibrary.GetLibraryFilePath);
 writeln(outfile,'DB Client Implementation Version = ',Attachment.getFirebirdAPI.GetImplementationVersion);
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
       writeln(OutFile);
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

procedure TTestBase.WriteTRInfo(TrInfo: ITrInformation);
var IsolationType, RecVersion: byte;
    i: integer;
    access: integer;
begin
 for i := 0 to TrInfo.GetCount - 1 do
 with TrInfo[i] do
 case getItemType of
   isc_info_tra_id:
     writeln(OutFile,'Transaction ID = ',getAsInteger);
   isc_info_tra_oldest_interesting:
     writeln(OutFile,'Oldest Interesting = ',getAsInteger);
   isc_info_tra_oldest_active:
     writeln(OutFile,'Oldest Action = ',getAsInteger);
   isc_info_tra_oldest_snapshot:
     writeln(OutFile,'Oldest Snapshot = ',getAsInteger);
   fb_info_tra_snapshot_number:
     writeln(OutFile,'Oldest Snapshot Number = ',getAsInteger);
   isc_info_tra_lock_timeout:
     writeln(OutFile,'Lock Timeout = ',getAsInteger);
   isc_info_tra_isolation:
     begin
       DecodeTraIsolation(IsolationType, RecVersion);
       write(OutFile,'Isolation Type = ');
       case IsolationType of
       isc_info_tra_consistency:
         write(OutFile,'isc_info_tra_consistency');
       isc_info_tra_concurrency:
         write(OutFile,'isc_info_tra_concurrency');
       isc_info_tra_read_committed:
         begin
          write(OutFile,'isc_info_tra_read_committed, Options =');
          case RecVersion of
          isc_info_tra_no_rec_version:
            write(OutFile,'isc_info_tra_no_rec_version');
          isc_info_tra_rec_version:
            write(OutFile,'isc_info_tra_rec_version');
          isc_info_tra_read_consistency:
            write(OutFile,'isc_info_tra_read_consistency');
          end;
         end;
       end;
       writeln(OutFile);
     end;
   isc_info_tra_access:
     begin
       write(OutFile,'Transaction Access = ');
       access :=  getAsInteger;
       case access of
       isc_info_tra_readonly:
         writeln(OutFile,'isc_info_tra_readonly');
       isc_info_tra_readwrite:
         writeln(OutFile,'isc_info_tra_readwrite');
       end;
     end;
   fb_info_tra_dbpath:
     writeln(OutFile,'Transaction Database Path = ',getAsString);
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
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}
  {$IFEND}
  begin
    LargeCompFormat := '#' + ThousandSeparator + '##0';
    ThreeSigPlacesFormat := '#0' + DecimalSeparator + '000';
  end;
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
    writeln(OutFile, aValue.Name,' = ',FormatFloat(FFloatTpl,aValue.AsDouble));

  SQL_INT64:
    if aValue.Scale <> 0 then
      writeln(OutFile, aValue.Name,' = ',FormatFloat(FFloatTpl,aValue.AsDouble))
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
    begin
      writeln(OutFile,aValue.Name,' = (blob), Length = ',aValue.AsBlob.GetBlobSize);
      if FShowBinaryBlob then
        PrintHexString(aValue.AsString);
    end;

  SQL_TEXT,SQL_VARYING:
  begin
    if aValue.GetCharSetID = 1 then
      s := aValue.AsString
    else
      s := TrimRight(aValue.AsString);
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

procedure TTestBase.ProcessResults;
begin
  //Do nothing
end;

procedure TTestBase.SetFloatTemplate(tpl: Ansistring);
begin
  FFloatTpl := tpl;
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
    TestID: Ansistring;
begin
  if assigned(FTests) then
  begin
    for i := 0 to FTests.Count - 1 do
    if FTests.Objects[i] <> nil then
    try
      TestID := TTestBase(FTests.Objects[i]).TestID;
      FTests.Objects[i].Free;
    except on E: Exception do
      writeln('Error Freeing Test ',TestID,' Error message = ',E.Message);
    end;
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
  begin
    FFirebirdAPI := IB.FirebirdAPI;
    FFirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);;
  end;
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
//    TTestBase(FTests.Objects[i]).CreateObjects(self);
  end;
end;

destructor TTestApplication.Destroy;
begin
  DestroyTestList;
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

function TTestApplication.GetTempDatabaseName: AnsiString;
begin
  Result := GetTempDir + 'fbtest.fbd';
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
  CleanUp;
  for i := 0 to FTests.Count - 1 do
  begin
    DoTest(i);
    if not Quiet then
      writeln(Outfile,'------------------------------------------------------');
    Sleep(500);
  end;
end;

procedure TTestApplication.RunTest(TestID: AnsiString);
begin
  CleanUp;
  DoTest(GetIndexByTestID(TestID));
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

procedure TTestApplication.SetTestOption(aValue: AnsiString);
begin
  FTestOption := AValue;
end;

{$IFDEF FPC}
function TTestApplication.GetShortOptions: AnsiString;
begin
  Result := 'htupensbolrSPXOq';
end;

function TTestApplication.GetLongOptions: AnsiString;
begin
  Result := 'help test user passwd employeedb newdbname secondnewdbname backupfile '+
            'outfile fbclientlibrary server stats port prompt TestOption quiet';
end;

procedure TTestApplication.GetParams(var DoPrompt: boolean; var TestID: string);
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
  if Length(TestID) = 1 then
    TestID := '0' + TestID;

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

  if HasOption('O','TestOption') then
    SetTestOption(GetOptionValue('O'));

  FQuiet := HasOption('q','quiet')
end;
{$ENDIF}

{$IFDEF DCC}
procedure TTestApplication.GetParams(var DoPrompt: boolean; var TestID: string);

  function GetCmdLineValue(const Switch: string; var aValue: string): boolean;
  var i: integer;
  begin
    aValue := '';
    Result := FindCmdLineSwitch(Switch,false);
    if Result then
    begin
      for i := 0 to ParamCount do
        if (ParamStr(i) = '-' + Switch) and (i <= ParamCount) then
        begin
          aValue := ParamStr(i+1);
          exit;
        end;
      Result := false;
    end;
  end;

var aValue: string;

begin
  // parse parameters
  if FindCmdLineSwitch('h') or FindCmdLineSwitch('help') then
  begin
    WriteHelp;
    Exit;
  end;

  if GetCmdLineValue('t',aValue) then
    TestID := aValue;

  DoPrompt := GetCmdLineValue('X',aValue);

  if GetCmdLineValue('u',aValue) or GetCmdLineValue('user',aValue) then
      SetUserName(aValue);

  if GetCmdLineValue('p',aValue) or GetCmdLineValue('passwd',aValue) then
      SetPassword(aValue);

  if GetCmdLineValue('e',aValue) or GetCmdLineValue('employeedb',aValue) then
      SetEmployeeDatabaseName(aValue);

  if GetCmdLineValue('n',aValue) or GetCmdLineValue('newdbname',aValue) then
      SetNewDatabaseName(aValue);

  if GetCmdLineValue('s',aValue) or GetCmdLineValue('secondnewdbname',aValue) then
      SetSecondNewDatabaseName(aValue);

  if GetCmdLineValue('b',aValue) or GetCmdLineValue('backupfile',aValue) then
      SetBackupFileName(aValue);

  if GetCmdLineValue('r',aValue) or GetCmdLineValue('server',aValue) then
      SetServerName(aValue);

  if GetCmdLineValue('P',aValue) or GetCmdLineValue('port',aValue) then
      SetPortNum(aValue);

  if GetCmdLineValue('l',aValue) or GetCmdLineValue('fbclientlibrary',aValue) then
      SetClientLibraryPath(aValue);

  if GetCmdLineValue('o',aValue) or GetCmdLineValue('outfile',aValue) then
  begin
    system.Assign(outFile,aValue);
    ReWrite(outFile);
  end;

    ShowStatistics := FindCmdLineSwitch('S',false) or FindCmdLineSwitch('stats');

  if GetCmdLineValue('O',aValue) or GetCmdLineValue('TestOption',aValue) then
    SetTestOption(aValue);

  FQuiet :=  FindCmdLineSwitch('q',false)  or FindCmdLineSwitch('quiet');
end;
{$ENDIF}

procedure TTestApplication.DoRun;
var
  DoPrompt: boolean;
  TestID: string;
  MasterProvider: IFBIMasterProvider;
begin
  {$IFDEF FPC}
  OutFile := stdout;
  {$ELSE}
  AssignFile(OutFile,'');
  ReWrite(outFile);
  {$ENDIF}

  GetParams(DoPrompt,TestID);
  if length(TestID) = 1 then
    TestID := '0' + TestID;
  {$ifdef DCC}
  {$IF declared(SetConsoleOutputCP)}
  SetConsoleOutputCP(cp_utf8);
  {$IFEND}
  {$IF declared(SetTextCodePage)}
  {Ensure consistent UTF-8 output}
  SetTextCodePage(OutFile,cp_utf8);
  {$IFEND}
  {$endif}

  {Ensure consistent date reporting across platforms}
  SetFormatSettings;

  if not Quiet then
  begin
    writeln(OutFile,Title);
    writeln(OutFile,Copyright);
    writeln(OutFile);
    writeln(OutFile,'Starting Tests');
    writeln(OutFile,'Client API Version = ',FirebirdAPI.GetImplementationVersion);
    writeln(OutFile,'Firebird Environment Variable = ',sysutils.GetEnvironmentVariable('FIREBIRD'));
    if FirebirdAPI.HasMasterIntf and (FirebirdAPI.QueryInterface(IFBIMasterProvider,MasterProvider) = S_OK) then
    with MasterProvider.GetIMasterIntf.getConfigManager^ do
    begin
      writeln(OutFile,'Firebird Bin Directory = ', getDirectory(DIR_BIN));
      writeln(OutFile,'Firebird Conf Directory = ', getDirectory(DIR_CONF));
    end;
    writeln(OutFile,'Firebird Client Library Path = ',FirebirdAPI.GetFBLibrary.GetLibraryFilePath);
  end;

  try
    if TestID = '' then
      RunAll
    else
      RunTest(TestID);
    CleanUp;
  except on E: Exception do
    begin
      writeln('Exception: ',E.Message);
      writeln(OutFile,'Exception: ',E.Message);
    end;
  end;

  writeln(OutFile,'Test Suite Ends');
  Flush(OutFile);
  {$IFDEF WINDOWS}
  if DoPrompt then
  begin
    write('Press Entry to continue');
    readln; {when running from IDE and console window closes before you can view results}
  end;
  {$ENDIF}

  // stop program loop
  Terminate;
end;

procedure TTestApplication.DoTest(index: integer);
begin
 if FTests.Objects[index] = nil then Exit;
 try
  with TTestBase(FTests.Objects[index]) do
  if SkipTest then
    writeln(OutFile,' Skipping ' + TestID)
  else
  begin
    if not Quiet then
      writeln(OutFile,'Running ' + TestTitle);
    if not ChildProcess then
      writeln(ErrOutput,'Running ' + TestTitle);
    try
      CreateObjects(self);
      InitTest;
      RunTest('UTF8',3);
      ProcessResults;
    except
      on E:ESkipException do
        writeln(OutFile,'Skipping Test: ' + E.Message);
      on E:Exception do
      begin
        writeln(OutFile,'Test Completed with Error: ' + E.Message);
        Exit;
      end;
    end;
    if not Quiet then
    begin
      writeln(OutFile);
      writeln(OutFile);
    end;
  end;
 finally
   FTests.Objects[index].Free;
   FTests.Objects[index] := nil;
   DestroyComponents;
 end;
end;

procedure TTestApplication.SetFormatSettings;
begin
   {$ifdef FPC}
   SetMultiByteConversionCodePage(cp_utf8);
   {$endif}
  {$IF declared(DefaultFormatSettings)}
  with DefaultFormatSettings do
  {$ELSE}
  {$IF declared(FormatSettings)}
  with FormatSettings do
  {$IFEND}{$IFEND}
  begin
    ShortDateFormat := 'dd/m/yyyy';
    LongTimeFormat := 'HH:MM:SS';
    DateSeparator := '/';
  end;
end;

procedure TTestApplication.WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ExeName, ' -h');
end;

{$IFNDEF FPC}
function TCustomApplication.Exename: string;
begin
  Result := ParamStr(0);
end;

procedure TCustomApplication.Run;
begin
  try
    DoRun;
  except on E: Exception do
    writeln(OutFile,E.Message);
  end;
end;

procedure TCustomApplication.Terminate;
begin

end;
{$ENDIF}

end.

