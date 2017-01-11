(*
 *  IBX For Lazarus (Firebird Express)
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
 *  The Original Code is (C) 2014-2017 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit ibxscript;

{$mode objfpc}{$H+}

{$codepage UTF8}

interface

uses Classes, IBDatabase,  IBSQL, IB, IBDataOutput;

const
  ibx_blob = 'IBX_BLOB';
  ibx_array = 'IBX_ARRAY';

  BlobLineLength = 40;

  {Non-character symbols}
  sqNone                 = #0;
  sqEnd                  = #1;
  sqBegin                = #2;
  sqString               = #3;
  sqComment              = #4;
  sqCase                 = #5;
  sqDeclare              = #6;
  sqCommentLine          = #7;
  sqEOL                  = #8;
  sqTab                  = #9;
  sqTerminator           = #10;
  sqEOF                  = #11;
  sqTag                  = #12;
  sqEndTag               = #13;
  sqQuotedString         = #14;
  sqDoubleQuotedString   = #15;

type
  TSQLSymbol = char;

  TSQLStates =  (stInit, stError, stInSQL, stNested,  stInDeclaration);

  TXMLStates =  (stInTag,stAttribute,stAttributeValue,stQuotedAttributeValue,
                 stTagged,stEndTag);

  TXMLTag    =   (xtNone,xtBlob,xtArray,xtElt);

  TOnNextLine = procedure(Sender: TObject; Line: string) of object;
  TOnProgressEvent = procedure (Sender: TObject; Reset: boolean; value: integer) of object;

  TXMLTagDef = record
    XMLTag: TXMLTag;
    TagValue: string;
  end;

const
  XMLTagDefs: array [0..2] of TXMLTagDef = (
    (XMLTag: xtBlob;   TagValue: 'blob'),
    (XMLTag: xtArray;  TagValue: 'array'),
    (XMLTag: xtElt;    TagValue: 'elt')
    );

type

  { TSymbolStream }

  {A simple lookahead one parser to process a text stream as a stream of symbols.
   This is an abstract object, subclassed for different sources.}

  TSymbolStream = class
  private
    FNextSymbol: TSQLSymbol;
    FOnNextLine: TOnNextLine;
    FOnProgressEvent: TOnProgressEvent;
    FTerminator: char;
    FLastChar: char;
    FIndex: integer;
    FLine: string;
    FString: string;
    FXMLTag: TXMLTag;
    FXMLMode: integer;
  protected
    FNextStatement: boolean;
    function GetErrorPrefix: string; virtual; abstract;
    function GetNextSymbol(C: char): TSQLSymbol;
    function FindTag(tag: string; var xmlTag: TXMLTag): boolean;
    function GetNextLine(var Line: string):boolean; virtual; abstract;
  public
    constructor Create;
    procedure ShowError(msg: string; params: array of const);
    function GetSymbol: TSQLSymbol; virtual;
    procedure NextStatement;
    property SymbolValue: string read FString;
    property Terminator: char read FTerminator write FTerminator;
    property XMLTag: TXMLTag read FXMLTag;
    property OnNextLine: TOnNextLine read FOnNextLine write FOnNextLine;
    property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
  end;

  { TBatchSymbolStream }

  {This symbol stream supports non-interactive parsing of a text file, stream or
   lines of text.}

  TBatchSymbolStream = class(TSymbolStream)
  private
    FLines: TStrings;
    FLineIndex: integer;
  protected
    function GetErrorPrefix: string; override;
    function GetNextLine(var Line: string):boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetStreamSource(Lines: TStrings); overload;
    procedure SetStreamSource(S: TStream); overload;
    procedure SetStreamSource(FileName: string); overload;
  end;

  { TInteractiveSymbolStream }

  {This symbol stream supports interactive parsing of commands and
   SQL statements entered at a console}

  TInteractiveSymbolStream = class(TSymbolStream)
  private
    FPrompt: string;
    FContinuePrompt: string;
    FTerminated: boolean;
  protected
    function GetErrorPrefix: string; override;
    function GetNextLine(var Line: string):boolean; override;
  public
    constructor Create(aPrompt: string='SQL>'; aContinue: string = 'CON>');
    function GetSymbol: TSQLSymbol; override;
    property Terminated: boolean read FTerminated write FTerminated;
  end;

  TBlobData = record
    BlobIntf: IBlob;
    SubType: cardinal;
  end;

  TArrayData = record
    ArrayIntf: IArray;
    SQLType: cardinal;
    relationName: string;
    columnName: string;
    dim: cardinal;
    Size: cardinal;
    Scale: integer;
    CharSet: string;
    bounds: TArrayBounds;
    CurrentRow: integer;
    Index: array of integer;
  end;

  { TIBXMLProcessor }

  {This is a simple XML parser that parses the output of a symbol stream as XML
   structured data, recognising tags, attributes and data. The tags are given in
   the table XMLTagDefs. The BlobData and ArrayData properties return blob and
   array data decoded from the XML stream.}

  TIBXMLProcessor = class
  private
    FDatabase: TIBDatabase;
    FSymbolStream: TSymbolStream;
    FState: TXMLStates;
    FTransaction: TIBTransaction;
    FXMLTagStack: array [1..20] of TXMLTag;
    FXMLTagIndex: integer;
    FAttributeName: string;
    FBlobData: array of TBlobData;
    FCurrentBlob: integer;
    FArrayData: array of TArrayData;
    FCurrentArray: integer;
    FBlobBuffer: PChar;
    procedure EndXMLTag(xmltag: TXMLTag);
    procedure EnterTag;
    function GetArrayData(index: integer): TArrayData;
    function GetArrayDataCount: integer;
    function GetBlobData(index: integer): TBlobData;
    function GetBlobDataCount: integer;
    procedure ProcessTagValue(tagValue: string);
    procedure StartXMLTag(xmltag: TXMLTag);
    procedure ProcessAttributeValue(attrValue: string);
    procedure ProcessBoundsList(boundsList: string);
  public
    constructor Create;
    destructor Destroy; override;
    function AnalyseXML(SymbolStream: TSymbolStream): string;
    procedure NextStatement;
    class function FormatBlob(Field: ISQLData): string;
    class function FormatArray(ar: IArray): string;
    property BlobData[index: integer]: TBlobData read GetBlobData;
    property BlobDataCount: integer read GetBlobDataCount;
    property ArrayData[index: integer]: TArrayData read GetArrayData;
    property ArrayDataCount: integer read GetArrayDataCount;
    property Database: TIBDatabase read FDatabase write FDatabase;
    property Transaction: TIBTransaction read FTransaction write FTransaction;
  end;

  { TIBSQLProcessor }

  {This parses a symbol stream into SQL statements. If embedded XML is found then
   this is processed by the supplied XMLProcessor. The HasBegin property provides
   a simple way to recognised stored procedure DDL, and "Execute As" statements.}

  TIBSQLProcessor = class
  private
    FSQLText: string;
    FState: TSQLStates;
    FStack: array [0..16] of TSQLStates;
    FStackindex: integer;
    FHasBegin: boolean;
    FInCase: boolean;
    FNested: integer;
    FXMLProcessor: TIBXMLProcessor;
    FSymbolStream: TSymbolStream;
    procedure AddToSQL(const Symbol: string);
    procedure SetState(AState: TSQLStates);
    function PopState: TSQLStates;
  public
    constructor Create(XMLProcessor: TIBXMLProcessor);
    function GetNextStatement(SymbolStream: TSymbolStream; var stmt: string) : boolean;
    property HasBegin: boolean read FHasBegin;
  end;

  TGetParamValue = procedure(Sender: TObject; ParamName: string; var BlobID: TISC_QUAD) of object;
  TLogEvent = procedure(Sender: TObject; Msg: string) of Object;
  TOnSelectSQL = procedure (Sender: TObject; SQLText: string) of object;
  TOnSetStatement = procedure(Sender: TObject; command, aValue, stmt: string; var Done: boolean) of object;

  { TCustomIBXScript }

  {This is the main script processing engine and can be customised by subclassing
   and defining the symbol stream appropriate for use.

   The RunScript function is used to invoke the processing of a symbol stream. Each
   SQL statement is extracted one by one. If it is recognised as a built in command
   by "ProcessStatement" then it is actioned directly. Otherwise, it is executed
   using the TIBSQL component. Note that SQL validation by this class is only partial
   and is sufficient only to parse the SQL into statements. The Firebird engine does
   the rest when the statement is executed.}

  TCustomIBXScript = class(TComponent)
  private
    FEcho: boolean;
    FIBXMLProcessor: TIBXMLProcessor;
    FIBSQLProcessor: TIBSQLProcessor;
    FDatabase: TIBDatabase;
    FDataOutputFormatter: TIBCustomDataOutput;
    FIgnoreGrants: boolean;
    FOnErrorLog: TLogEvent;
    FOnSelectSQL: TOnSelectSQL;
    FOnSetStatement: TOnSetStatement;
    FShowAffectedRows: boolean;
    FShowPerformanceStats: boolean;
    FStopOnFirstError: boolean;
    FTransaction: TIBTransaction;
    FInternalTransaction: TIBTransaction;
    FISQL: TIBSQL;
    FGetParamValue: TGetParamValue;
    FOnOutputLog: TLogEvent;
    FAutoDDL: boolean;
    procedure DoCommit;
    procedure DoReconnect;
    procedure ExecSQL(stmt: string);
    function GetOnProgressEvent: TOnProgressEvent;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetDataOutputFormatter(AValue: TIBCustomDataOutput);
    procedure SetOnProgressEvent(AValue: TOnProgressEvent);
    procedure SetParamValue(SQLVar: ISQLParam);
    procedure SetShowPerformanceStats(AValue: boolean);
    procedure SetTransaction(AValue: TIBTransaction);
  protected
    FSymbolStream: TSymbolStream;
    procedure Add2Log(const Msg: string; IsError: boolean=true); virtual;
    procedure EchoNextLine(Sender: TObject; Line: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ProcessStatement(stmt: string): boolean; virtual;
    function ProcessStream: boolean;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultSelectSQLHandler(aSQLText: string);
  published
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property DataOutputFormatter: TIBCustomDataOutput read FDataOutputFormatter
                                  write SetDataOutputFormatter;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL default true;
    property Echo: boolean read FEcho write FEcho default true;  {Echo Input to Log}
    property IgnoreGrants: boolean read FIgnoreGrants write FIgnoreGrants;
    property Transaction: TIBTransaction read FTransaction write SetTransaction;
    property ShowAffectedRows: boolean read FShowAffectedRows write FShowAffectedRows;
    property ShowPerformanceStats: boolean read FShowPerformanceStats write SetShowPerformanceStats;
    property StopOnFirstError: boolean read FStopOnFirstError write FStopOnFirstError default true;
    property GetParamValue: TGetParamValue read FGetParamValue write FGetParamValue; {resolve parameterized queries}
    property OnOutputLog: TLogEvent read FOnOutputLog write FOnOutputLog; {Log handler}
    property OnErrorLog: TLogEvent read FOnErrorLog write FOnErrorLog;
    property OnProgressEvent: TOnProgressEvent read GetOnProgressEvent write SetOnProgressEvent; {Progress Bar Support}
    property OnSelectSQL: TOnSelectSQL read FOnSelectSQL write FOnSelectSQL; {Handle Select SQL Statements}
    property OnSetStatement: TOnSetStatement read FOnSetStatement write FOnSetStatement;
  end;

  {
  TIBXScript: runs an SQL script in the specified file or stream. The text is parsed
  into SQL statements which are executed in turn. The intention is to be ISQL
  compatible but with extensions:

  * All DML and DDL Statements are supported.

  * CREATE DATABASE, DROP DATABASE, CONNECT and COMMIT are supported.

  * The following SET statements are supported:
    SET SQL DIALECT
    SET TERM
    SET AUTODDL
    SET BAIL
    SET ECHO
    SET COUNT
    SET STATS
    SET NAMES <character set>

  * New Command: RECONNECT. Performs a commit followed by disconnecting and
    reconnecting to the database.

  * Procedure Bodies (BEGIN .. END blocks) are self-delimiting and do not need
    an extra terminator. If a terminator is present, this is treated as an
    empty statement. The result is ISQL compatible, but does not require the
    use of SET TERM.

  * DML statements may have arguments in IBX format (e.g UPDATE MYTABLE Set data = :mydata).
    Arguments are valid only for BLOB columns and are resolved using the GetParamValue
    event. This returns the blobid to be used. A typical use of the event is to
    read binary data from a file, save it in a blob stream and return the blob id.

  Select SQL statements are not directly supported but can be handled by an external
  handler (OnSelectSQL event). If the handler is not present then an exception
  is raised if a Select SQL statement is found.

  Properties:

  * Database: Link to TIBDatabase component
  * Transaction: Link to Transaction. Defaults to internaltransaction (concurrency, wait)
  * AutoDDL: When true DDL statements are automatically committed after execution
  * Echo: boolean. When true, all SQL statements are echoed to log
  * StopOnFirstError: boolean. When true the script engine terminates on the first
    SQL Error.
  * IgnoreGrants: When true, grant statements are silently discarded. This can be
    useful when applying a script using the Embedded Server.
  * ShowPerformanceStats: When true, performance statistics (in ISQL format) are
    written to the log after a DML statement is executed
  * DataOutputFormatter: Identifies a Data Output Formatter component used to format
    the results of executing a Select Statement


  Events:

  * GetParamValue: called when an SQL parameter is found (in PSQL :name format).
    This is only called for blob fields. Handler should return the BlobID to be
    used as the parameter value.  If not present an exception is raised when a
    parameter is found.
  * OnOutputLog: Called to write SQL Statements to the log (stdout)
  * OnErrorLog: Called to write all other messages to the log (stderr)
  * OnProgressEvent: Progress bar support. If Reset is true the value is maximum
    value of progress bar. Otherwise called to step progress bar.
  * OnSelectSQL: handler for select SQL statements. If not present, select SQL
    statements result in an exception.
  * OnSetStatement: called to process a SET command that has not already been
    handled by TIBXScript.

  The RunScript function is used to execute an SQL Script and may be called
  multiple times.
  }

  { TIBXScript }

  TIBXScript = class(TCustomIBXScript)
  public
    constructor Create(aOwner: TComponent); override;
    {use RunScript instead of PerformUpdate}
    function PerformUpdate(SQLFile: string; aAutoDDL: boolean): boolean; overload; deprecated;
    function PerformUpdate(SQLStream: TStream;   aAutoDDL: boolean): boolean; overload; deprecated;
    function RunScript(SQLFile: string): boolean; overload;
    function RunScript(SQLStream: TStream): boolean; overload;
    function RunScript(SQLLines: TStrings): boolean; overload;
    function ExecSQLScript(sql: string): boolean;
  end;

function StringToHex(octetString: string; MaxLineLength: integer=0): string; overload;
procedure StringToHex(octetString: string; TextOut: TStrings; MaxLineLength: integer=0); overload;


resourcestring
  sInvalidSetStatement = 'Invalid %s Statement - %s';

implementation

uses Sysutils, RegExpr;

resourcestring
  sTerminatorUnknownState = 'Statement Terminator in unexpected state (%d)';
  sUnterminatedString = 'Unterminated string';
  sUnknownSymbol = 'Unknown Symbol %d';
  sNoSelectSQL = 'Select SQL Statements are not supported';
  sStackUnderflow = 'Stack Underflow';
  sNoParamQueries =  'Parameterised Queries are not supported';
  sStackOverFlow = 'Stack Overflow';
  sResolveQueryParam =  'Resolving Query Parameter: %s';
  sNoCommit =  'Commit not allowed here';
  sNoReconnect = 'Reconnect not allowed here';
  sXMLStackUnderflow = 'XML Stack Underflow';
  sInvalidEndTag = 'XML End Tag Mismatch - %s';
  sXMLStackOverFlow = 'XML Stack Overflow';
  sErrorState = 'Entered Error State';
  sXMLError = 'Invalid XML (%c)';
  sXMLAttributeError = 'Unexpected attribute - "%s" = "%s"';
  sInvalidBoundsList = 'Invalid array bounds list - "%s"';
  sBinaryBlockMustbeEven = 'Binary block must have an even number of characters';
  sInvalidCharacterSet = 'Unrecognised character set name - "%s"';
  sOnLineError = 'On Line %d Character %d: ';
  sArrayIndexError = 'Array Index Error (%d)';
  sBlobIndexError = 'Blob Index Error (%d)';
  sStatementError = 'Error processing SQL statement: %s %s - for statement "%s"';

function StringToHex(octetString: string; MaxLineLength: integer): string; overload;

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

procedure StringToHex(octetString: string; TextOut: TStrings; MaxLineLength: integer); overload;
begin
    TextOut.Add(StringToHex(octetString,MaxLineLength));
end;



{ TIBXScript }

constructor TIBXScript.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSymbolStream := TBatchSymbolStream.Create;
  FSymbolStream.OnNextLine := @EchoNextLine;
end;

function TIBXScript.PerformUpdate(SQLFile: string; aAutoDDL: boolean): boolean;
begin
  FAutoDDL := aAutoDDL;
  Result := RunScript( SQLFile);
end;

function TIBXScript.PerformUpdate(SQLStream: TStream; aAutoDDL: boolean
  ): boolean;
begin
  FAutoDDL := aAutoDDL;
  Result := RunScript(SQLStream);
end;

function TIBXScript.RunScript(SQLFile: string): boolean;
begin
  TBatchSymbolStream(FSymbolStream).SetStreamSource(SQLFile);
  Result := ProcessStream;
end;

function TIBXScript.RunScript(SQLStream: TStream): boolean;
begin
  TBatchSymbolStream(FSymbolStream).SetStreamSource(SQLStream);
  Result := ProcessStream;
end;

function TIBXScript.RunScript(SQLLines: TStrings): boolean;
begin
  TBatchSymbolStream(FSymbolStream).SetStreamSource(SQLLines);
  Result := ProcessStream;
end;

function TIBXScript.ExecSQLScript(sql: string): boolean;
var s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := sql;
    TBatchSymbolStream(FSymbolStream).SetStreamSource(s);
    Result := ProcessStream;
  finally
    s.Free;
  end;
end;

{ TCustomIBXScript }

procedure TCustomIBXScript.Add2Log(const Msg: string; IsError: boolean);
begin
  if IsError then
  begin
    if assigned(OnErrorLog) then OnErrorLog(self,Msg)
  end
  else
  if assigned(FOnOutputLog) then FOnOutputLog(self,Msg)
end;

procedure TCustomIBXScript.DoCommit;
begin
  with GetTransaction do
    if InTransaction then Commit;
  GetTransaction.Active := true;
end;

procedure TCustomIBXScript.DoReconnect;
begin
  with GetTransaction do
    if InTransaction then Commit;
  Database.Connected := false;
  Database.Connected := true;
  GetTransaction.Active := true;
end;

procedure TCustomIBXScript.ExecSQL(stmt: string);
var DDL: boolean;
    I: integer;
    stats: TPerfCounters;
begin
   Database.Connected := true;
   FISQL.SQL.Text := stmt;
   FISQL.Transaction := GetTransaction;
   FISQL.Transaction.Active := true;
   FISQL.ParamCheck := not FIBSQLProcessor.HasBegin; {Probably PSQL}
   FISQL.Prepare;
   FISQL.Statement.EnableStatistics(ShowPerformanceStats);

   if FISQL.SQLStatementType in [SQLInsert, SQLUpdate, SQLDelete] then
   begin
     {Interpret parameters}
     for I := 0 to FISQL.Params.Count - 1 do
       SetParamValue(FISQL.Params[I]);
   end;

   if FISQL.SQLStatementType = SQLSelect then
   begin
     if assigned(OnSelectSQL) then
       OnSelectSQL(self,stmt)
     else
       DefaultSelectSQLHandler(stmt);
   end
   else
   begin
     DDL := FISQL.SQLStatementType = SQLDDL;
     if not DDL or not FIgnoreGrants or (Pos('GRANT',AnsiUpperCase(Trim(stmt))) <> 1) then
     begin
       FISQL.ExecQuery;
       if ShowAffectedRows and not DDL then
         Add2Log('Rows Affected: ' + IntToStr(FISQL.RowsAffected));
       if not DDL then
         TIBCustomDataOutput.ShowPerfStats(FISQL.Statement,@Add2Log);
     end;

     if FAutoDDL and DDL then
       FISQL.Transaction.Commit;
     FISQL.Close;
   end;
   FISQL.SQL.Clear;
end;

function TCustomIBXScript.GetOnProgressEvent: TOnProgressEvent;
begin
  Result := FSymbolStream.OnProgressEvent;
end;

function TCustomIBXScript.GetTransaction: TIBTransaction;
begin
 if FTransaction = nil then
   Result := FInternalTransaction
 else
   Result := FTransaction;
end;

procedure TCustomIBXScript.EchoNextLine(Sender: TObject; Line: string);
begin
  if Echo then Add2Log(Line);
end;

procedure TCustomIBXScript.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase) and (Operation = opRemove) then
    FDatabase := nil;
  if (AComponent = FTransaction) and (Operation = opRemove) then
    FTransaction := nil;
  if (AComponent = DataOutputFormatter) and (Operation = opRemove) then
    FDataOutputFormatter := nil;
end;

procedure TCustomIBXScript.SetDatabase(AValue: TIBDatabase);
begin
 if FDatabase = AValue then Exit;
 FDatabase := AValue;
 FISQL.Database := AValue;
 FIBXMLProcessor.Database := AValue;
 FInternalTransaction.DefaultDatabase := AValue;
end;

procedure TCustomIBXScript.SetDataOutputFormatter(AValue: TIBCustomDataOutput);
begin
 if FDataOutputFormatter = AValue then Exit;
 if (FDataOutputFormatter <> nil) and (AValue <> nil) then
   AValue.Assign(FDataOutputFormatter);
 FDataOutputFormatter := AValue;
 if FDataOutputFormatter <> nil then
   FDataOutputFormatter.Database := Database;
end;

procedure TCustomIBXScript.SetOnProgressEvent(AValue: TOnProgressEvent);
begin
  FSymbolStream.OnProgressEvent := AValue;
end;

procedure TCustomIBXScript.SetParamValue(SQLVar: ISQLParam);
var BlobID: TISC_QUAD;
    ix: integer;
begin
  if (SQLVar.SQLType = SQL_BLOB) and (Pos(ibx_blob,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(ibx_blob)+1,length(SQLVar.Name)-length(ibx_blob)));
    SQLVar.AsBlob := FIBXMLProcessor.BlobData[ix].BlobIntf;
    Exit;
  end
  else
  if (SQLVar.SQLType = SQL_ARRAY) and (Pos(ibx_array,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(ibx_array)+1,length(SQLVar.Name)-length(ibx_array)));
    SQLVar.AsArray := FIBXMLProcessor.ArrayData[ix].ArrayIntf;
    Exit;
  end;

  if assigned(FGetParamValue) and (SQLVar.SQLType = SQL_BLOB) then
  begin
    Add2Log(Format(sResolveQueryParam,[SQLVar.Name]));
    GetParamValue(self,SQLVar.Name,BlobID);
    if (BlobID.gds_quad_high = 0) and (BlobID.gds_quad_low = 0) then
      SQLVar.Clear
    else
      SQLVar.AsQuad := BlobID
  end
  else
    raise Exception.Create(sNoParamQueries);
end;

procedure TCustomIBXScript.SetShowPerformanceStats(AValue: boolean);
begin
  if FShowPerformanceStats = AValue then Exit;
  FShowPerformanceStats := AValue;
  if assigned(DataOutputFormatter) then
    DataOutputFormatter.ShowPerformanceStats := AValue;
end;

function TCustomIBXScript.ProcessStream: boolean;
var stmt: string;
begin
  Result := false;
  while FIBSQLProcessor.GetNextStatement(FSymbolStream,stmt) do
  try
//    writeln('stmt = ',stmt);
    if trim(stmt) = '' then continue;
    if not ProcessStatement(stmt) then
      ExecSQL(stmt);

  except on E:Exception do
      begin
        Add2Log(Format(sStatementError,[FSymbolStream.GetErrorPrefix,
                           E.Message,stmt]),true);
        if StopOnFirstError then Exit;
      end
  end;
  Result := true;
end;

function TCustomIBXScript.ProcessStatement(stmt: string): boolean;
var command: string;
    ucStmt: string;

  function Toggle(aValue: string): boolean;
  begin
    aValue := AnsiUpperCase(aValue);
    if aValue = 'ON' then
      Result := true
    else
    if aValue = 'OFF' then
      Result := false
    else
      raise Exception.CreateFmt(sInvalidSetStatement, [command,stmt]);
  end;

  procedure ExtractUserInfo;
  var  RegexObj: TRegExpr;
  begin
    RegexObj := TRegExpr.Create;
    try
      RegexObj.ModifierG := false; {turn off greedy matches}
      RegexObj.Expression := ' +USER +''(.+)''';
      if RegexObj.Exec(ucStmt) then
        FDatabase.Params.Values['user_name'] := RegexObj.Match[1];

      RegexObj.Expression := ' +PASSWORD +''(.+)''';
      if RegexObj.Exec(ucStmt) then
        FDatabase.Params.Values['password'] :=
                    system.copy(stmt,RegexObj.MatchPos[1],RegexObj.MatchLen[1]);
    finally
      RegexObj.Free;
    end;
  end;

  procedure ExtractConnectInfo;
  var  RegexObj: TRegExpr;
  begin
    ExtractUserInfo;
    RegexObj := TRegExpr.Create;
    try
      RegexObj.ModifierG := false; {turn off greedy matches}
      RegexObj.Expression := '^ *CONNECT +''(.*)''';
      if RegexObj.Exec(ucStmt) then
      begin
        FDatabase.DatabaseName := system.copy(stmt,RegexObj.MatchPos[1],RegexObj.MatchLen[1]);
      end;

      RegexObj.Expression := ' +ROLE +''(.+)''';
      if RegexObj.Exec(ucStmt) then
        FDatabase.Params.Values['sql_role_name'] := RegexObj.Match[1]
      else
      with FDatabase.Params do
      if IndexOfName('sql_role_name') <> -1 then
        Delete(IndexOfName('sql_role_name'));

      RegexObj.Expression := ' +CACHE +([0-9]+)';
      if RegexObj.Exec(ucStmt) then
        FDatabase.Params.Values['cache_manager'] := RegexObj.Match[1]
      else
      with FDatabase.Params do
      if IndexOfName('cache_manager') <> -1 then
        Delete(IndexOfName('cache_manager'));
    finally
      RegexObj.Free;
    end;
  end;

  procedure UpdateUserPassword;
  var  RegexObj: TRegExpr;
  begin
    RegexObj := TRegExpr.Create;
    try
      RegexObj.ModifierG := false; {turn off greedy matches}
      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(''.*'') +USER +''(.+)''';
      if not RegexObj.Exec(ucStmt) and (FDatabase.Params.IndexOfName('user_name') <> -1) then
      begin
        RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(''.*'')';
        if RegexObj.Exec(ucStmt) then
        begin
          system.Insert(' USER ''' + FDatabase.Params.Values['user_name'] +'''',stmt,
                 RegexObj.MatchPos[2] + RegexObj.MatchLen[2]);
          ucStmt := AnsiUpperCase(stmt);
        end;
      end;

      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''.*'' +USER +''.+'' PASSWORD +''(.+)''';
      if not RegexObj.Exec(ucStmt) and (FDatabase.Params.IndexOfName('password') <> -1) then
      begin
        RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''.*'' +(USER +''.+'')';
        if RegexObj.Exec(ucStmt) then
        begin
          system.Insert(' PASSWORD ''' + FDatabase.Params.Values['password'] +'''',stmt,
                 RegexObj.MatchPos[2] + RegexObj.MatchLen[2]);
          ucStmt := AnsiUpperCase(stmt);
        end;
      end;
    finally
      RegexObj.Free;
    end;
  end;

var  RegexObj: TRegExpr;
     n: integer;
     charsetid: integer;
     param: string;
     Terminator: char;
begin
  Result := false;
  ucStmt := AnsiUpperCase(stmt);
  Terminator := FSymbolStream.Terminator;
  RegexObj := TRegExpr.Create;
  try
    {process create database}
    RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +.*(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      UpdateUserPassword;
      FDatabase.Connected := false;
      FDatabase.CreateDatabase(stmt);
      FDatabase.Connected := false;
      ExtractUserInfo;
      DoReconnect;
      Result := true;
      Exit;
    end;

    {process connect statement}
    RegexObj.Expression := '^ *CONNECT +.*(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      ExtractConnectInfo;
      DoReconnect;
      Result := true;
      Exit;
    end;

    {Process Drop Database}
    RegexObj.Expression := '^ *DROP +DATABASE *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      FDatabase.DropDatabase;
      Result := true;
      Exit;
    end;

    {process commit statement}
    RegexObj.Expression := '^ *COMMIT *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      DoCommit;
      Result := true;
      Exit;
    end;

    {process Reconnect statement}
    RegexObj.Expression := '^ *RECONNECT *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      DoReconnect;
      Result := true;
      Exit;
    end;


    {Process Set Term}
    RegexObj.Expression := '^ *SET +TERM +(.) *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
       FSymbolStream.Terminator := RegexObj.Match[1][1];
       Result := true;
       Exit;
    end;

    {process Set SQL Dialect}
    RegexObj.Expression := '^ *SET +SQL +DIALECT +([0-9]) *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      n := StrToInt(RegexObj.Match[1]);
      if Database.SQLDialect <> n then
      begin
        Database.SQLDialect := n;
        if Database.Connected then
          DoReconnect;
      end;
      Result := true;
      Exit;
    end;

    {Process Remaining Set statements}
    RegexObj.Expression := '^ *SET +([A-Z]+)( +[A-Z0-9]+|) *(\' + Terminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      command := AnsiUpperCase(RegexObj.Match[1]);
      param := trim(RegexObj.Match[2]);
      if command = 'AUTODDL' then
        AutoDDL := ((RegexObj.MatchLen[2] = 0) and not AutoDDL) or
                   (RegexObj.MatchLen[2] > 0) and Toggle(param)
      else
      if command = 'BAIL' then
        StopOnFirstError := ((RegexObj.MatchLen[2] = 0) and not StopOnFirstError) or
                   (RegexObj.MatchLen[2] > 0) and Toggle(param)
      else
      if command = 'ECHO' then
        Echo := ((RegexObj.MatchLen[2] = 0) and not Echo) or
                   (RegexObj.MatchLen[2] > 0) and Toggle(param)
      else
      if command = 'COUNT' then
        ShowAffectedRows := ((RegexObj.MatchLen[2] = 0) and not ShowAffectedRows) or
                   (RegexObj.MatchLen[2] > 0) and Toggle(param)
      else
      if command = 'STATS' then
        ShowPerformanceStats := ((RegexObj.MatchLen[2] = 0) and not FShowPerformanceStats) or
                   (RegexObj.MatchLen[2] > 0) and Toggle(param)
      else
      if command = 'NAMES' then
      begin
        if FirebirdAPI.CharSetName2CharSetID(param,charsetid) then
        begin
          Database.Params.Values['lc_ctype'] := param;
          if Database.Connected then
            DoReconnect;
        end
        else
          raise Exception.CreateFmt(sInvalidCharacterSet, [param,stmt]);
      end
      else
      begin
        if assigned(DataOutputFormatter) then
          DataOutputFormatter.SetCommand(command,param,stmt,Result);
        if not Result and assigned(OnSetStatement) then
          OnSetStatement(self,command,param,stmt,Result)
        else
          raise Exception.CreateFmt(sInvalidSetStatement, [command,stmt]);
        Exit;
      end;
      Result := true;
      Exit;
    end;

  finally
    RegexObj.Free;
  end;
end;

procedure TCustomIBXScript.SetTransaction(AValue: TIBTransaction);
begin
  if FTransaction = AValue then Exit;
  FTransaction := AValue;
  FIBXMLProcessor.Transaction := AValue;
end;

constructor TCustomIBXScript.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FStopOnFirstError := true;
  FEcho := true;
  FAutoDDL := true;
  FISQL := TIBSQL.Create(self);
  FISQL.ParamCheck := true;
  FIBXMLProcessor := TIBXMLProcessor.Create;
  FIBSQLProcessor := TIBSQLProcessor.Create(FIBXMLProcessor);
  FInternalTransaction := TIBTransaction.Create(self);
  FInternalTransaction.Params.Clear;
  FInternalTransaction.Params.Add('concurrency');
  FInternalTransaction.Params.Add('wait');
end;

destructor TCustomIBXScript.Destroy;
begin
  if FIBSQLProcessor <> nil then FIBSQLProcessor.Free;
  if FIBXMLProcessor <> nil then FIBXMLProcessor.Free;
  if FSymbolStream <> nil then FSymbolStream.Free;
  if FISQL <> nil then FISQL.Free;
  if FInternalTransaction <> nil then FInternalTransaction.Free;
  inherited Destroy;
end;

procedure TCustomIBXScript.DefaultSelectSQLHandler(aSQLText: string);
begin
  if assigned(DataOutputFormatter) then
    DataOutputFormatter.DataOut(aSQLText,@Add2Log)
  else
    FSymbolStream.ShowError(sNoSelectSQL,[nil]);
end;

{ TIBSQLProcessor }

procedure TIBSQLProcessor.AddToSQL(const Symbol: string);
begin
  FSQLText := FSQLText +  Symbol;
//  writeln('SQL = ',FSQLText);
end;

procedure TIBSQLProcessor.SetState(AState: TSQLStates);
begin
  if FStackIndex > 16 then
    FSymbolStream.ShowError(sStackOverFlow,[nil]);
  FStack[FStackIndex] := FState;
  Inc(FStackIndex);
  FState := AState
end;

function TIBSQLProcessor.PopState: TSQLStates;
begin
  if FStackIndex = 0 then
    FSymbolStream.ShowError(sStackUnderflow,[nil]);
  Dec(FStackIndex);
  Result := FStack[FStackIndex]
end;

constructor TIBSQLProcessor.Create(XMLProcessor: TIBXMLProcessor);
begin
  inherited Create;
  FXMLProcessor := XMLProcessor;
end;

function TIBSQLProcessor.GetNextStatement(SymbolStream: TSymbolStream;
  var stmt: string): boolean;
var Symbol: TSQLSymbol;
    NonSpace: boolean;
    Done: boolean;
begin
  FSQLText := '';
  FState := stInit;
  FHasBegin := false;
  FSymbolStream := SymbolStream;
  FXMLProcessor.NextStatement;
  SymbolStream.NextStatement;

  Result := true;
  Done := false;
  NonSpace := false;
  while not Done do
  with SymbolStream do
  begin
    if FState = stError then
      ShowError(sErrorState,[nil]);
    Symbol := GetSymbol;
//    writeln('Symbol = ',Symbol,' Value = ',SymbolValue);
    if not (Symbol in [' ',sqEOL]) then
      NonSpace := true;

    case Symbol of
    sqTag:
      begin
        if FState in [stInSQL,stNested] then
          AddToSQL(FXMLProcessor.AnalyseXML(SymbolStream));
      end;

    sqTerminator:
        case FState of
        stInit: {ignore empty statement};

        stInSQL:
            Done := true;

       stNested:
         AddToSQL(Terminator);

       stInDeclaration:
         begin
           FState := PopState;
           AddToSQL(Terminator);
         end;

       else
         ShowError(sTerminatorUnknownState,[FState]);
       end;

    ';':
        begin
          if FState = stInDeclaration then
            FState := PopState;
          AddToSQL(';');
        end;

    '*':
      begin
       AddToSQL('*');
       if FState =  stInit then
          FState := stInSQL
      end;

    '/':
      begin
       AddToSQL('/');
       if FState =  stInit then
          FState := stInSQL
      end;

    sqComment,
    sqQuotedString,
    sqDoubleQuotedString:
      if FState <> stInit then
        AddToSQL(SymbolValue);

    sqCommentLine:
      if FState <> stInit then
      AddToSQL(SymbolValue + LineEnding);

    sqEnd:
      begin
        AddToSQL(SymbolValue);
        case FState of
        stNested:
          begin
            if FNested = 0 then
            begin
              FState := PopState;
              if not FInCase then
              begin
                FState := stInit;
                Done := true;
              end
              else
                FInCase := false;
            end
           else
              Dec(FNested)
          end;
          {Otherwise ignore}
        end
      end;

    sqBegin:
      begin
        FHasBegin := true;
        AddToSQL(SymbolValue);
        case FState of
        stNested:
          Inc(FNested);

        stInSQL,
        stInit:
          SetState(stNested);
        end
      end;

    sqCase:
    begin
      AddToSQL(SymbolValue);
      case FState of
      stNested:
        Inc(FNested);

      stInSQL,
      stInit:
        begin
          FInCase := true;
          SetState(stNested);
        end;
      end
    end;

    sqDeclare:
      begin
        AddToSQL(SymbolValue);
        if FState in [stInit,stInSQL] then
          SetState(stInDeclaration)
      end;

    sqString:
      begin
        AddToSQL(SymbolValue);
        if FState = stInit then
          FState := stInSQL
      end;

    sqEOL:
      begin
        case FState of
        stInit:
          {Do nothing};
        else
          if NonSpace then AddToSQL(LineEnding);
        end;
      end;

    sqEOF:
      begin
        Done := true;
        Result := trim(FSQLText) <> '';
      end
    else
    if FState <> stInit then
      AddToSQL(Symbol);
    end
  end;
  stmt := FSQLText;
//  writeln('stmt = ',stmt);
end;

{ TIBXMLProcessor }

procedure TIBXMLProcessor.EndXMLTag(xmltag: TXMLTag);
begin
  if FXMLTagIndex = 0 then
    FSymbolStream.ShowError(sXMLStackUnderflow,[nil]);
  if xmltag <> FXMLTagStack[FXMLTagIndex] then
    FSymbolStream.ShowError(sInvalidEndTag,[FSymbolStream.SymbolValue]);

  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    FBlobData[FCurrentBlob].BlobIntf.Close;

  xtArray:
    FArrayData[FCurrentArray].ArrayIntf.SaveChanges;

  xtElt:
    Dec(FArrayData[FCurrentArray].CurrentRow);
  end;
  Dec(FXMLTagIndex);
end;

procedure TIBXMLProcessor.EnterTag;
var aCharSetID: integer;
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    begin
      Database.Connected := true;
      Transaction.Active := true;
      FBlobData[FCurrentBlob].BlobIntf := Database.Attachment.CreateBlob(
        Transaction.TransactionIntf,FBlobData[FCurrentBlob].SubType);
    end;

  xtArray:
    with FArrayData[FCurrentArray] do
    begin
      Database.Connected := true;
      Transaction.Active := true;
      FirebirdAPI.CharSetName2CharSetID(CharSet,aCharSetID);
      SetLength(Index,dim);
      ArrayIntf := Database.Attachment.CreateArray(
                     Transaction.TransactionIntf,
                     Database.Attachment.CreateArrayMetaData(SQLType,
                       relationName,columnName,Scale,Size,
                       aCharSetID,dim,bounds)
                     );
    end;
  end;
end;

function TIBXMLProcessor.GetArrayData(index: integer): TArrayData;
begin
  if (index < 0) or (index > ArrayDataCount) then
    FSymbolStream.ShowError(sArrayIndexError,[index]);
  Result := FArrayData[index];
end;

function TIBXMLProcessor.GetArrayDataCount: integer;
begin
  Result := Length(FArrayData);
end;

function TIBXMLProcessor.GetBlobData(index: integer): TBlobData;
begin
  if (index < 0) or (index > BlobDataCount) then
    FSymbolStream.ShowError(sBlobIndexError,[index]);
  Result := FBlobData[index];
end;

function TIBXMLProcessor.GetBlobDataCount: integer;
begin
  Result := Length(FBlobData);
end;

procedure TIBXMLProcessor.ProcessTagValue(tagValue: string);

  function nibble(hex: char): byte;
  begin
    case hex of
    '0': Result := 0;
    '1': Result := 1;
    '2': Result := 2;
    '3': Result := 3;
    '4': Result := 4;
    '5': Result := 5;
    '6': Result := 6;
    '7': Result := 7;
    '8': Result := 8;
    '9': Result := 9;
    'a','A': Result := 10;
    'b','B': Result := 11;
    'c','C': Result := 12;
    'd','D': Result := 13;
    'e','E': Result := 14;
    'f','F': Result := 15;
    end;
  end;

  procedure RemoveWhiteSpace(var hexData: string);
  var i: integer;
  begin
    {Remove White Space}
    i := 1;
    while i <= length(hexData) do
    begin
      case hexData[i] of
      ' ',#9,#10,#13:
        begin
          if i < Length(hexData) then
            Move(hexData[i+1],hexData[i],Length(hexData)-i);
          SetLength(hexData,Length(hexData)-1);
        end;
      else
        Inc(i);
      end;
    end;
  end;

  procedure WriteToBlob(hexData: string);
  var i,j : integer;
      blength: integer;
      P: PChar;
  begin
    RemoveWhiteSpace(hexData);
    if odd(length(hexData)) then
      FSymbolStream.ShowError(sBinaryBlockMustbeEven,[nil]);
    blength := Length(hexData) div 2;
    IBAlloc(FBlobBuffer,0,blength);
    j := 1;
    P := FBlobBuffer;
    for i := 1 to blength do
    begin
      P^ := char((nibble(hexData[j]) shl 4) or nibble(hexdata[j+1]));
      Inc(j,2);
      Inc(P);
    end;
    FBlobData[FCurrentBlob].BlobIntf.Write(FBlobBuffer^,blength);
  end;

begin
  if tagValue = '' then Exit;
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    WriteToBlob(tagValue);

  xtElt:
    with FArrayData[FCurrentArray] do
      ArrayIntf.SetAsString(index,tagValue);

  end;
end;

procedure TIBXMLProcessor.StartXMLTag(xmltag: TXMLTag);
begin
  if FXMLTagIndex > 19 then
    FSymbolStream.ShowError(sXMLStackOverFlow,[nil]);
  Inc(FXMLTagIndex);
  FXMLTagStack[FXMLTagIndex] := xmltag;
  case xmltag of
  xtBlob:
    begin
      Inc(FCurrentBlob);
      SetLength(FBlobData,FCurrentBlob+1);
      FBlobData[FCurrentBlob].BlobIntf := nil;
      FBlobData[FCurrentBlob].SubType := 0;
    end;

  xtArray:
    begin
      Inc(FCurrentArray);
      SetLength(FArrayData,FCurrentArray+1);
      with FArrayData[FCurrentArray] do
      begin
        ArrayIntf := nil;
        SQLType := 0;
        dim := 0;
        Size := 0;
        Scale := 0;
        CharSet := 'NONE';
        SetLength(Index,0);
        CurrentRow := -1;
      end;
    end;

  xtElt:
    with FArrayData[FCurrentArray] do
      Inc(CurrentRow);

  end;
end;

procedure TIBXMLProcessor.ProcessAttributeValue(attrValue: string);
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    if FAttributeName = 'subtype' then
      FBlobData[FCurrentBlob].SubType := StrToInt(attrValue)
    else
      FSymbolStream.ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

  xtArray:
    if FAttributeName = 'sqltype' then
      FArrayData[FCurrentArray].SQLType := StrToInt(attrValue)
    else
    if FAttributeName = 'relation_name' then
      FArrayData[FCurrentArray].relationName := attrValue
    else
    if FAttributeName = 'column_name' then
      FArrayData[FCurrentArray].columnName := attrValue
    else
    if FAttributeName = 'dim' then
      FArrayData[FCurrentArray].Dim := StrToInt(attrValue)
    else
    if FAttributeName = 'length' then
      FArrayData[FCurrentArray].Size := StrToInt(attrValue)
    else
    if FAttributeName = 'scale' then
      FArrayData[FCurrentArray].Scale := StrToInt(attrValue)
    else
    if FAttributeName = 'charset' then
      FArrayData[FCurrentArray].CharSet := attrValue
    else
    if FAttributeName = 'bounds' then
      ProcessBoundsList(attrValue)
    else
      FSymbolStream.ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

  xtElt:
    if FAttributeName = 'ix' then
      with FArrayData[FCurrentArray] do
        Index[CurrentRow] :=  StrToInt(attrValue)
     else
        FSymbolStream.ShowError(sXMLAttributeError,[FAttributeName,attrValue]);
  end;
end;

procedure TIBXMLProcessor.ProcessBoundsList(boundsList: string);
var list: TStringList;
    i,j: integer;
begin
  list := TStringList.Create;
  try
    list.Delimiter := ',';
    list.DelimitedText := boundsList;
    with FArrayData[FCurrentArray] do
    begin
      if dim <> list.Count then
        FSymbolStream.ShowError(sInvalidBoundsList,[boundsList]);
      SetLength(bounds,dim);
      for i := 0 to list.Count - 1 do
      begin
        j := Pos(':',list[i]);
        if j = 0 then
          raise Exception.CreateFmt(sInvalidBoundsList,[boundsList]);
        bounds[i].LowerBound := StrToInt(system.copy(list[i],1,j-1));
        bounds[i].UpperBound := StrToInt(system.copy(list[i],j+1,length(list[i])-j));
      end;
    end;
  finally
    list.Free;
  end;
end;

constructor TIBXMLProcessor.Create;
begin
  inherited Create;
  NextStatement;
end;

destructor TIBXMLProcessor.Destroy;
begin
  FreeMem(FBlobBuffer);
  inherited Destroy;
end;

function TIBXMLProcessor.AnalyseXML(SymbolStream: TSymbolStream): string;
var Symbol: TSQLSymbol;
    Done: boolean;
    XMLString: string;
begin
  Result := '';
  XMLString := '';
  Done := false;
  FState := stInTag;
  FSymbolStream := SymbolStream;
  with SymbolStream do
  begin
    StartXMLTag(XMLTag);
    while not Done do
    with SymbolStream do
    begin
      Symbol := GetSymbol;

      case Symbol of
      sqEOL:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         XMLString += LineEnding;
      end;

      ' ',sqTab:
        case FState of
        stQuotedAttributeValue,
        stTagged:
           XMLString += ' ';
        end;

      ';':
        case FState of
        stQuotedAttributeValue,
        stTagged:
           XMLString += ';';
        else
          ShowError(sXMLError,[Symbol]);
        end;

      '''':
        case FState of
        stQuotedAttributeValue,
        stTagged:
           XMLString += '''';
        else
          ShowError(sXMLError,[Symbol]);
        end;

      '*':
        case FState of
        stQuotedAttributeValue,
        stTagged:
           XMLString += '*';
        else
          ShowError(sXMLError,[Symbol]);
        end;

      '/':
        case FState of
        stQuotedAttributeValue,
        stTagged:
           XMLString += '/';
        else
          ShowError(sXMLError,[Symbol]);
        end;

      '>':
        case FState of
        stEndTag:
            case XMLTag of
            xtBlob:
              begin
                Result := ':' + Format(ibx_blob+'%d',[FCurrentBlob]);
                Done := true;
              end;
            xtArray:
              begin
                Result := ':' + Format(ibx_array+'%d',[FCurrentArray]);
                Done := true;
              end;
            else
              FState := stTagged;
          end;

        stInTag:
          begin
            XMLString := '';
            FState := stTagged;
            EnterTag;
          end;

        stQuotedAttributeValue,
        stTagged:
          XMLString += '>';

        else
          ShowError(sXMLError,[Symbol]);
        end;

      sqTag:
        if FState = stTagged then
        begin
          FState := stInTag;
          StartXMLTag(XMLTag)
        end
        else
          ShowError(sXMLError,[Symbol]);

      sqEndTag:
        if FState = stTagged then
        begin
          ProcessTagValue(XMLString);
          EndXMLTag(XMLTag);
          FState := stEndTag;
        end
        else
          ShowError(sXMLError,[Symbol]);

      '=':
        case FState of
        stAttribute:
          FState := stAttributeValue;

        stQuotedAttributeValue,
        stTagged:
          XMLString += '=';

        else
          ShowError(sXMLError,[Symbol]);
        end;

      '"':
        case FState of
        stAttributeValue:
          begin
            XMLString := '';
            FState := stQuotedAttributeValue;
          end;

        stQuotedAttributeValue:
          begin
            ProcessAttributeValue(XMLString);
            FState := stInTag;
          end;

        stTagged:
          XMLString += '"';

        else
          ShowError(sXMLError,[Symbol]);
        end;

      sqString:
        case FState of
        stInTag: {attribute name}
          begin
            FAttributeName := SymbolValue;
            FState := stAttribute;
          end;

        stAttributeValue:
          begin
            ProcessAttributeValue(FString);
            FState := stInTag;
          end;

        stQuotedAttributeValue,
        stTagged:
           XMLString += SymbolValue;

        else
          ShowError(sXMLError,[Symbol]);
        end;
      else
        ShowError(sXMLError,[Symbol]);
      end
    end;
  end;
end;

procedure TIBXMLProcessor.NextStatement;
begin
  FXMLTagIndex := 0;
  SetLength(FBlobData,0);
  FCurrentBlob := -1;
  SetLength(FArrayData,0);
  FCurrentArray := -1;
end;

class function TIBXMLProcessor.FormatBlob(Field: ISQLData): string;
var TextOut: TStrings;
begin
  TextOut := TStringList.Create;
  try
    TextOut.Add(Format('<blob subtype="%d">',[Field.getSubtype]));
    StringToHex(Field.AsString,TextOut,BlobLineLength);
    TextOut.Add('</blob>');
    Result := TextOut.Text;
  finally
    TextOut.Free;
  end;
end;

class function TIBXMLProcessor.FormatArray(ar: IArray): string;
var index: array of integer;
    TextOut: TStrings;

    procedure AddElements(dim: integer; indent:string = ' ');
    var i: integer;
        recurse: boolean;
    begin
      SetLength(index,dim+1);
      recurse := dim < ar.GetDimensions - 1;
      with ar.GetBounds[dim] do
      for i := LowerBound to UpperBound do
      begin
        index[dim] := i;
        if recurse then
        begin
          TextOut.Add(Format('%s<elt id="%d">',[indent,i]));
          AddElements(dim+1,indent + ' ');
          TextOut.Add('</elt>');
        end
        else
        if ((ar.GetSQLType = SQL_TEXT) or (ar.GetSQLType = SQL_VARYING)) and
           (ar.GetCharSetID = 1) then
           TextOut.Add(Format('%s<elt ix="%d">%s</elt>',[indent,i,StringToHex(ar.GetAsString(index))]))
        else
          TextOut.Add(Format('%s<elt ix="%d">%s</elt>',[indent,i,ar.GetAsString(index)]));
      end;
    end;

var
    s: string;
    bounds: TArrayBounds;
    i: integer;
    boundsList: string;
begin
  TextOut := TStringList.Create;
  try
    s := Format('<array dim = "%d" sqltype = "%d" length = "%d" relation_name = "%s" column_name = "%s"',
                                [ar.GetDimensions,ar.GetSQLType,ar.GetSize,
                                 ar.GetTableName,ar.GetColumnName]);
    case ar.GetSQLType of
    SQL_DOUBLE, SQL_FLOAT, SQL_LONG, SQL_SHORT, SQL_D_FLOAT, SQL_INT64:
       s += Format(' scale = "%d"',[ ar.GetScale]);
    SQL_TEXT,
    SQL_VARYING:
      s += Format(' charset = "%s"',[FirebirdAPI.GetCharsetName(ar.GetCharSetID)]);
    end;
    bounds := ar.GetBounds;
    boundsList := '';
    for i := 0 to length(bounds) - 1 do
    begin
      if i <> 0 then boundsList += ',';
      boundsList += Format('%d:%d',[bounds[i].LowerBound,bounds[i].UpperBound]);
    end;
    s += Format(' bounds="%s"',[boundsList]);
    s += '>';
    TextOut.Add(s);

    SetLength(index,0);
    AddElements(0);
    TextOut.Add('</array>');
    Result := TextOut.Text;
  finally
    TextOut.Free;
  end;
end;

{ TInteractiveSymbolStream }

function TInteractiveSymbolStream.GetErrorPrefix: string;
begin
  Result := '';
end;

function TInteractiveSymbolStream.GetNextLine(var Line: string): boolean;
begin
  if FNextStatement then
    write(FPrompt)
  else
    write(FContinuePrompt);
  Result := not EOF;
  if Result then
    readln(Line);
end;

constructor TInteractiveSymbolStream.Create(aPrompt: string; aContinue: string);
begin
  inherited Create;
  FPrompt := aPrompt;
  FContinuePrompt := aContinue;
end;

function TInteractiveSymbolStream.GetSymbol: TSQLSymbol;
begin
  if Terminated then
    Result := sqEOF
  else
    Result := inherited GetSymbol;
end;

{ TBatchSymbolStream }

function TBatchSymbolStream.GetErrorPrefix: string;
begin
  Result := Format(sOnLineError,[FLineIndex,FIndex]);
end;

function TBatchSymbolStream.GetNextLine(var Line: string): boolean;
begin
  Result := FLineIndex < FLines.Count;
  if Result then
  begin
    Line := FLines[FLineIndex];
//    writeln('Next Line = ',Line);
    Inc(FLineIndex);
    if assigned(OnProgressEvent) then
      OnProgressEvent(self,false,1);
  end;
end;

constructor TBatchSymbolStream.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TBatchSymbolStream.Destroy;
begin
  if assigned(FLines) then FLines.Free;
  inherited Destroy;
end;

procedure TBatchSymbolStream.SetStreamSource(Lines: TStrings);
begin
  FLineIndex := 0;
  FLines.Assign(Lines);
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FLines.Count);
end;

procedure TBatchSymbolStream.SetStreamSource(S: TStream);
begin
  FLineIndex := 0;
  FLines.LoadFromStream(S);
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FLines.Count);
end;

procedure TBatchSymbolStream.SetStreamSource(FileName: string);
begin
  FLineIndex := 0;
  FLines.LoadFromFile(FileName);
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FLines.Count);
end;

{ TSymbolStream }

function TSymbolStream.GetNextSymbol(C: char): TSQLSymbol;
begin
  Result := sqNone;
  if C = FTerminator then
    Result := sqTerminator
  else
  case C of
  #0..#8,#10..#31,' ':
    Result := ' ';

  #9,';','"','''','/',
  '*','=','>','<',',':
    Result := C;
  else
    begin
      Result := sqString;
      FLastChar := C
    end
  end;
end;

function TSymbolStream.FindTag(tag: string; var xmlTag: TXMLTag): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(XMLTagDefs) - 1 do
    if XMLTagDefs[i].TagValue = tag then
    begin
      xmlTag := XMLTagDefs[i].XMLTag;
      Result := true;
      break;
    end;
end;

constructor TSymbolStream.Create;
begin
  inherited;
  FTerminator := ';';
  NextStatement;
end;

procedure TSymbolStream.ShowError(msg: string; params: array of const);
begin
  raise EIBClientError.CreateFmt(GetErrorPrefix + msg,params);
end;

function TSymbolStream.GetSymbol: TSQLSymbol;
var
    DelimitedText: string;
    CurState: (gsNone,gsInComment,gsInSingleQuotes,gsInDoubleQuotes);
begin
  Result := sqNone;
  CurState := gsNone;
  DelimitedText := '';
  if FNextSymbol <> sqNone then
  begin
    Result := FNextSymbol;
    if Result = sqString then
      FString := FLastChar
    else
      FString := '';
    FNextSymbol := sqNone
  end;

  while FNextSymbol = sqNone do {find the next symbol}
  begin
    if FIndex > Length(FLine) then
    begin
      FNextSymbol := sqEOL;
      FIndex := 0;
    end
    else
    begin
      if FIndex = 0 then
      begin
        if not GetNextLine(FLine) then
        begin
          Result := sqEOF;
          FNextSymbol := sqNone;
          Exit;
        end;
        FIndex := 1;
        FNextStatement := false;
        if assigned(OnNextLine) then
          OnNextLine(self,FLine);
        if CurState <> gsNone then
          DelimitedText += LineEnding;
        if Length(FLine) = 0 then
          continue;
      end;
      if CurState <> gsNone then
        DelimitedText += FLine[FIndex];
      FNextSymbol := GetNextSymbol(FLine[FIndex]);
      Inc(FIndex);
    end;

    case CurState of
    gsNone:
      begin
        {combine if possible}
        case Result of
        sqNone:
          begin
            Result := FNextSymbol;
            if FNextSymbol = sqString then
              FString := FLastChar;
            FNextSymbol := sqNone
          end;

        '/':
          if FXMLMode > 0 then
            break
          else
          if FNextSymbol = '*' then
          begin
            CurState := gsInComment;
            DelimitedText := '/*';
            Result := sqNone;
            FNextSymbol := sqNone
          end
          else
          if FNextSymbol = '/' then
          begin
            FString := '/*' + system.copy(FLine,FIndex,length(FLine)- FIndex + 1) + ' */';
            Result := sqCommentLine;
            FIndex := 0;
            FNextSymbol := sqNone
          end;

        '<':
          if (FXMLMode > 0) and (FNextSymbol = '/') then
          begin
            Result := sqEndTag;
            FString := '';
            FNextSymbol := sqNone
          end
          else
          if FNextSymbol = sqString then
          begin
            Result := sqTag;
            FString := FLastChar;
            FNextSymbol := sqNone
          end;

        '''':
        if FXMLMode > 0 then
          break
        else
        if FNextSymbol = '''' then
        begin
          Result := sqQuotedString;
          FString := '''''';
          FNextSymbol := sqNone
        end
        else
        begin
          CurState := gsInSingleQuotes;
          DelimitedText := '''';
          if FNextSymbol = sqEOL then
            DelimitedText += LineEnding
          else
            DelimitedText += FLine[FIndex-1];
          Result := sqNone;
          FNextSymbol := sqNone
        end;

        '"':
        if FXMLMode > 0 then
          break
        else
        begin
          CurState := gsInDoubleQuotes;
          DelimitedText := '"';
          if FNextSymbol = sqEOL then
            DelimitedText += LineEnding
          else
            DelimitedText += FLine[FIndex-1];
          Result := sqNone;
          FNextSymbol := sqNone
        end;

        sqTag,
        sqEndTag,
        sqString:
          if FNextSymbol = sqString then
          begin
            FString := FString + FLastChar;
            FNextSymbol := sqNone
          end;
        end
      end;

    {Check for state exit condition}
    gsInSingleQuotes:
      if Result = '''' then
      begin
         CurState := gsNone;
         if FNextSymbol = sqEOL then
           FString := DelimitedText
         else
           FString := system.copy(DelimitedText,1,Length(DelimitedText)-1);
         Result := sqQuotedString;
       end;

    gsInDoubleQuotes:
      if Result = '"' then
      begin
         CurState := gsNone;
         if FNextSymbol = sqEOL then
           FString := DelimitedText
         else
           FString := system.copy(DelimitedText,1,Length(DelimitedText)-1);
         Result := sqDoubleQuotedString;
       end;

    gsInComment:
    if (Result = '*') and (FNextSymbol = '/') then
      begin
        CurState := gsNone;
        FString := DelimitedText;
        Result := sqComment;
        FNextSymbol := sqNone
      end;

    end;

    if (CurState <> gsNone) and (FNextSymbol <> sqNone) then
    begin
      Result := FNextSymbol;
      FNextSymbol := sqNone;
    end;
  end;

  if (Result = sqTag) and (FNextSymbol <> sqNone) then
  begin
    if FindTag(FString,FXMLTag) then
      Inc(FXMLMode)
    else
      Result := sqString;
  end
  else
  if (Result = sqEndTag) and (FNextSymbol <> sqNone) then
  begin
    if FindTag(FString,FXMLTag) then
      Dec(FXMLMode)
    else
      Result := sqString;
  end;

  if (FXMLMode = 0) and (Result = sqString) and (FString <> '') then
  begin
       if CompareText(FString,'begin') = 0 then
         Result := sqBegin
       else
       if CompareText(FString,'end') = 0 then
         Result := sqEnd
       else
       if CompareText(FString,'declare') = 0 then
         Result := sqDeclare
       else
       if CompareText(FString,'case') = 0 then
         Result := sqCase
  end;
//  writeln(Result,',',FString);
end;

procedure TSymbolStream.NextStatement;
begin
  FXMLTag := xtNone;
  FNextStatement := true;
end;

end.

