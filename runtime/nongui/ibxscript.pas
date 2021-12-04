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

uses Classes, IBDatabase,  IBSQL, IB, IBDataOutput, IBUtils;

type

  TOnNextLine = procedure(Sender: TObject; Line: string) of object;

  { TSQLStatementReader }

  TSQLStatementReader = class(TSQLXMLReader)
  private
    type
      TSQLState = (stDefault, stInStmt, stInBlock, stInArrayDim, stInDeclare);
  private
    FHasBegin: boolean;
    FOnNextLine: TOnNextLine;
    FTerminator: char;
  protected
    procedure EchoNextLine(aLine: string);
  public
    constructor Create;
    function GetNextStatement(var stmt: string) : boolean; virtual;
    property HasBegin: boolean read FHasBegin;
    property Terminator: char read FTerminator write FTerminator default DefaultTerminator;
    property OnNextLine: TOnNextLine read FOnNextLine write FOnNextLine;
  end;


  { TBatchSQLStatementReader }

  {This SQL Reader supports non-interactive parsing of a text file, stream or
   lines of text.}

  TBatchSQLStatementReader = class(TSQLStatementReader)
  private
    FInStream: TStream;
    FOwnsInStream: boolean;
    FLineIndex: integer;
    FIndex: integer;
    FCurLine: string;
  protected
    function GetChar: char; override;
    function GetErrorPrefix: string; override;
  public
    procedure Reset; override;
    procedure SetStreamSource(Lines: TStrings); overload;
    procedure SetStreamSource(S: TStream); overload;
    procedure SetStreamSource(FileName: string); overload;
    procedure SetStringStreamSource(S: string);
  end;

  { TInteractiveSQLStatementReader }

  {This SQL reader supports interactive parsing of commands and
   SQL statements entered at a console}

  TInteractiveSQLStatementReader = class(TSQLStatementReader)
  private
    FPrompt: string;
    FContinuePrompt: string;
    FTerminated: boolean;
    FLine: string;
    FLineIndex: integer;
    FNextStatement: boolean;
    function GetNextLine(var Line: string):boolean;
  protected
    function GetChar: char; override;
    function GetErrorPrefix: string; override;
  public
    constructor Create(aPrompt: string='SQL>'; aContinue: string = 'CON>');
    function GetNextStatement(var stmt: string) : boolean; override;
    property Terminated: boolean read FTerminated write FTerminated;
  end;

  TGetParamValue = procedure(Sender: TObject; ParamName: string; var BlobID: TISC_QUAD) of object;
  TLogEvent = procedure(Sender: TObject; Msg: string) of Object;
  TOnSelectSQL = procedure (Sender: TObject; SQLText: string) of object;
  TOnSetStatement = procedure(Sender: TObject; command, aValue, stmt: string; var Done: boolean) of object;
  TOnCreateDatabase = procedure (Sender: TObject; var DatabaseFileName: string) of object;

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
    FSQLReader: TSQLStatementReader;
    FDatabase: TIBDatabase;
    FDataOutputFormatter: TIBCustomDataOutput;
    FIgnoreCreateDatabase: boolean;
    FIgnoreGrants: boolean;
    FOnCreateDatabase: TOnCreateDatabase;
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
    function GetOnProgressEvent: TOnProgressEvent;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetDataOutputFormatter(AValue: TIBCustomDataOutput);
    procedure SetOnProgressEvent(AValue: TOnProgressEvent);
    procedure SetParamValue(SQLVar: ISQLParam);
    procedure SetShowPerformanceStats(AValue: boolean);
    procedure SetTransaction(AValue: TIBTransaction);
  protected
    procedure Add2Log(const Msg: string; IsError: boolean=true); virtual;
    procedure ExecSQL(stmt: string);
    procedure EchoNextLine(Sender: TObject; Line: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ProcessStatement(stmt: string): boolean; virtual;
    function ProcessStream: boolean;
    procedure SetSQLStatementReader(SQLStatementReader: TSQLStatementReader);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultSelectSQLHandler(aSQLText: string);
    property SQLStatementReader: TSQLStatementReader read FSQLReader;
  published
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property DataOutputFormatter: TIBCustomDataOutput read FDataOutputFormatter
                                  write SetDataOutputFormatter;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL default true;
    property Echo: boolean read FEcho write FEcho default true;  {Echo Input to Log}
    property IgnoreGrants: boolean read FIgnoreGrants write FIgnoreGrants;
    property IgnoreCreateDatabase: boolean read FIgnoreCreateDatabase write FIgnoreCreateDatabase;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
    property ShowAffectedRows: boolean read FShowAffectedRows write FShowAffectedRows;
    property ShowPerformanceStats: boolean read FShowPerformanceStats write SetShowPerformanceStats;
    property StopOnFirstError: boolean read FStopOnFirstError write FStopOnFirstError default true;
    property GetParamValue: TGetParamValue read FGetParamValue write FGetParamValue; {resolve parameterized queries}
    property OnOutputLog: TLogEvent read FOnOutputLog write FOnOutputLog; {Log handler}
    property OnErrorLog: TLogEvent read FOnErrorLog write FOnErrorLog;
    property OnProgressEvent: TOnProgressEvent read GetOnProgressEvent write SetOnProgressEvent; {Progress Bar Support}
    property OnSelectSQL: TOnSelectSQL read FOnSelectSQL write FOnSelectSQL; {Handle Select SQL Statements}
    property OnSetStatement: TOnSetStatement read FOnSetStatement write FOnSetStatement;
    property OnCreateDatabase: TOnCreateDatabase read FOnCreateDatabase write FOnCreateDatabase;
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


resourcestring
  sInvalidSetStatement = 'Invalid %s Statement - %s';
  sInvalidCharacterSet = 'Unrecognised character set name - "%s"';
  sOnLineError = 'On Line %d Character %d: ';

implementation

uses Sysutils, RegExpr;

resourcestring
  sNoSelectSQL = 'Select SQL Statements are not supported';
  sNoParamQueries =  'Parameterised Queries are not supported';
  sResolveQueryParam =  'Resolving Query Parameter: %s';
  sStatementError = 'Error processing SQL statement: %s %s - for statement "%s"';

{ TSQLStatementReader }

procedure TSQLStatementReader.EchoNextLine(aLine: string);
begin
  if assigned(FOnNextLine) then
    OnNextLine(self,aLine);
end;

constructor TSQLStatementReader.Create;
begin
  inherited Create;
  Terminator := DefaultTerminator;
end;

function TSQLStatementReader.GetNextStatement(var stmt: string): boolean;
var State: TSQLState;
    Nested: integer;
    token: TSQLTokens;
    EndOfStatement: boolean;
begin
  FHasBegin := false;
  EndOfStatement := false;
  Nested := 0;
  stmt := '';
  State := stDefault;
  while not EOF and not EndOfStatement do
  begin
    token := GetNextToken;
//    writeln(token,' ',TokenText,' ',Terminator);
    case State of
    stDefault:
      {ignore everything before a reserved word}
      if (token <= high(TSQLReservedWords)) or (token = sqltIdentifier) then
        begin
          State := stInStmt;
          stmt += TokenText;
        end;

    stInStmt:
       begin
        case token of
          sqltBegin:
          begin
            FHasBegin := true;
            State := stInBlock;
            Nested := 1;
            stmt += TokenText;
          end;

          sqltDeclare:
            begin
              State := stInDeclare;
              stmt += TokenText;
            end;

          sqltOpenSquareBracket:
             begin
               State := stInArrayDim;
               stmt += TokenText;
             end;

          sqltComment:
            stmt += '/*' + TokenText + '*/';

          sqltCommentLine:
            stmt += '/*' + TokenText + ' */' + LineEnding;

          sqltQuotedString:
            stmt += '''' + SQLSafeString(TokenText) + '''';

          sqltIdentifierInDoubleQuotes:
            stmt += '"' + TokenText + '"';

          sqltEOL:
            stmt += LineEnding;

          else
            begin
              if (tokentext = Terminator) and (Nested = 0) then
              begin
                EndOfStatement := true;
                State := stDefault;
              end
              else
                stmt += TokenText;
            end;
          end;
        end;

    {ignore begin..end blocks for Terminator detection }

    stInBlock:
      begin
        case token of
        sqltBegin:
          begin
            Inc(Nested);
            stmt += TokenText;
          end;

        sqltEnd:
          begin
            Dec(Nested);
            stmt += TokenText;
            if Nested = 0 then
            begin
              State := stDefault;
              EndOfStatement := true;
            end;
          end;

        sqltCase:
          {case constructs can appear within select statement in nested blocks.
           We need to match the case constructs END token in order to parse the
           block correctly. This is a simple parser and the only objective is
           to determine the correct end of block. We therefore do not check to
           ensure that the next end properly matches the case. The CASE is thus
           treated the same as BEGIN. The Firebird SQL Parser will flag any errors
           due to mismatched CASE/BEGIN END}
          begin
            Inc(Nested);
            stmt += TokenText;
          end;

        sqltComment:
          stmt += '/*' + TokenText + '*/';

        sqltCommentLine:
          stmt += '/* ' + TokenText + ' */' + LineEnding;

        sqltQuotedString:
          stmt += '''' + SQLSafeString(TokenText) + '''';

        sqltIdentifierInDoubleQuotes:
          stmt += '"' + TokenText + '"';

        sqltEOL:
          stmt += LineEnding;

        else
          stmt += TokenText;
        end;
      end;

      {ignore array dimensions for Terminator detection }

    stInArrayDim:
      begin
        case token of

        sqltComment:
          stmt += '/*' + TokenText + '*/';

        sqltCommentLine:
          stmt += '/* ' + TokenText + ' */' + LineEnding;

        sqltCloseSquareBracket:
        begin
          stmt += TokenText;
          State := stInStmt;
        end;

        sqltEOL:
          stmt += LineEnding;

        else
          stmt += TokenText;
        end;
      end;

    {ignore Declare statement for terminator - semi-colon terminates declaration}

    stInDeclare:
      begin
        case token of

        sqltComment:
          stmt += '/*' + TokenText + '*/';

        sqltCommentLine:
          stmt += '/* ' + TokenText + ' */' + LineEnding;

        sqltQuotedString:
          stmt += '''' + SQLSafeString(TokenText) + '''';  {exists some DECLARE with cursor having SELECT ...\... rc.rdb$constraint_type = 'PRIMARY KEY');}

        sqltSemiColon:
          begin
            State := stInStmt;
            stmt += TokenText;
          end;

        sqltEOL:
          stmt += LineEnding;

        else
          stmt += TokenText;
        end;
      end;
    end;
  end;
  Result := stmt <> '';
end;



{ TIBXScript }

constructor TIBXScript.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SetSQLStatementReader(TBatchSQLStatementReader.Create);
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
  TBatchSQLStatementReader(FSQLReader).SetStreamSource(SQLFile);
  Result := ProcessStream;
end;

function TIBXScript.RunScript(SQLStream: TStream): boolean;
begin
  TBatchSQLStatementReader(FSQLReader).SetStreamSource(SQLStream);
  Result := ProcessStream;
end;

function TIBXScript.RunScript(SQLLines: TStrings): boolean;
begin
  TBatchSQLStatementReader(FSQLReader).SetStreamSource(SQLLines);
  Result := ProcessStream;
end;

function TIBXScript.ExecSQLScript(sql: string): boolean;
begin
  TBatchSQLStatementReader(FSQLReader).SetStringStreamSource(sql);
  Result := ProcessStream;
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
end;

procedure TCustomIBXScript.DoReconnect;
begin
  with GetTransaction do
    if InTransaction then Commit;
  Database.Reconnect;
end;

procedure TCustomIBXScript.ExecSQL(stmt: string);
var DDL: boolean;
    I: integer;
begin
   Database.Connected := true;
   FISQL.SQL.Text := stmt;
   FISQL.Transaction := GetTransaction;
   FISQL.Transaction.Active := true;
//   FISQL.ParamCheck := not FSQLReader.HasBegin; {Probably PSQL}
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
  Result := FSQLReader.OnProgressEvent;
end;

function TCustomIBXScript.GetTransaction: TIBTransaction;
begin
 if not (csDesigning in ComponentState) and (FTransaction = nil) then
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
 if not (csLoading in ComponentState) and (FDatabase = AValue) then Exit;
 FDatabase := AValue;
 FISQL.Database := AValue;
 FInternalTransaction.Active := false;
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
  FSQLReader.OnProgressEvent := AValue;
end;

procedure TCustomIBXScript.SetParamValue(SQLVar: ISQLParam);
var BlobID: TISC_QUAD;
    ix: integer;
begin
  if (SQLVar.SQLType = SQL_BLOB) and (Pos(TSQLXMLReader.ibx_blob,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(TSQLXMLReader.ibx_blob)+1,length(SQLVar.Name)-length(TSQLXMLReader.ibx_blob)));
    SQLVar.AsBlob := FSQLReader.BlobData[ix].BlobIntf;
    Exit;
  end
  else
  if (SQLVar.SQLType = SQL_ARRAY) and (Pos(TSQLXMLReader.ibx_array,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(TSQLXMLReader.ibx_array)+1,length(SQLVar.Name)-length(TSQLXMLReader.ibx_array)));
    SQLVar.AsArray := FSQLReader.ArrayData[ix].ArrayIntf;
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
  FSQLReader.Attachment := Database.Attachment;
  if FTransaction = nil then
    FSQLReader.Transaction := FInternalTransaction.TransactionIntf
  else
    FSQLReader.Transaction := FTransaction.TransactionIntf;
  try
    while FSQLReader.GetNextStatement(stmt) do
    try
      stmt := trim(stmt);
  //    writeln('stmt = "',stmt,'"');
      if stmt = '' then continue;
      if not ProcessStatement(stmt) then
        ExecSQL(stmt);

    except on E:Exception do
        begin
          with GetTransaction do
            if InTransaction then Rollback;
          FSQLReader.Terminator := TSQLStatementReader.DefaultTerminator;
          if assigned(OnErrorLog) then
          begin
            Add2Log(Format(sStatementError,[FSQLReader.GetErrorPrefix,
                               E.Message,stmt]),true);
                               if StopOnFirstError then Exit;
          end
          else
            raise;
        end
    end;
  finally
    FSQLReader.Attachment := nil;
    FSQLReader.Transaction := nil;
  end;
  Result := true;
end;

procedure TCustomIBXScript.SetSQLStatementReader(
  SQLStatementReader: TSQLStatementReader);
begin
  FSQLReader := SQLStatementReader;
  FSQLReader.OnNextLine := @EchoNextLine;
end;

function TCustomIBXScript.ProcessStatement(stmt: string): boolean;
var command: string;

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
      if RegexObj.Exec(stmt) then
        FDatabase.Params.Values['user_name'] := RegexObj.Match[1];

      RegexObj.Expression := ' +PASSWORD +''(.+)''';
      if RegexObj.Exec(stmt) then
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
      RegexObj.ModifierI := true; {case insensitive}
      RegexObj.Expression := '^ *CONNECT +''(.*)''';
      if RegexObj.Exec(stmt) then
      begin
        FDatabase.DatabaseName := system.copy(stmt,RegexObj.MatchPos[1],RegexObj.MatchLen[1]);
      end;

      RegexObj.Expression := ' +ROLE +''(.+)''';
      if RegexObj.Exec(stmt) then
        FDatabase.Params.Values['sql_role_name'] := RegexObj.Match[1]
      else
      with FDatabase.Params do
      if IndexOfName('sql_role_name') <> -1 then
        Delete(IndexOfName('sql_role_name'));

      RegexObj.Expression := ' +CACHE +([0-9]+)';
      if RegexObj.Exec(stmt) then
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
      RegexObj.ModifierI := true; {case insensitive}
      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(''.*'') +USER +''(.+)''';
      if not RegexObj.Exec(stmt) and (FDatabase.Params.IndexOfName('user_name') <> -1) then
      begin
        RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(''.*'')';
        if RegexObj.Exec(stmt) then
        begin
          system.Insert(' USER ''' + FDatabase.Params.Values['user_name'] +'''',stmt,
                 RegexObj.MatchPos[2] + RegexObj.MatchLen[2]);
        end;
      end;

      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''.*'' +USER +''.+'' PASSWORD +''(.+)''';
      if not RegexObj.Exec(stmt) and (FDatabase.Params.IndexOfName('password') <> -1) then
      begin
        RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''.*'' +(USER +''.+'')';
        if RegexObj.Exec(stmt) then
        begin
          system.Insert(' PASSWORD ''' + FDatabase.Params.Values['password'] +'''',stmt,
                 RegexObj.MatchPos[2] + RegexObj.MatchLen[2]);
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
     FileName: string;
     DBConnected: boolean;
     LoginPrompt: boolean;
begin
  Result := false;
  Terminator := FSQLReader.Terminator;
  RegexObj := TRegExpr.Create;
  try
    {process create database}
    RegexObj.ModifierI := true; {case insensitive}
    RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +''(.*)''(.*)(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
      if IgnoreCreateDatabase then
      begin
        Result := true;
        Exit;
      end;
      FileName := system.copy(stmt,RegexObj.MatchPos[2], RegexObj.MatchLen[2]);
      if assigned(FOnCreateDatabase) then
        OnCreateDatabase(self,FileName);
      stmt := 'CREATE DATABASE ''' + FileName + '''' + system.copy(stmt,RegexObj.MatchPos[3], RegexObj.MatchLen[3]);
      UpdateUserPassword;
      if FDatabase.Connected then
        FDatabase.Dropdatabase;
      FDatabase.CreateDatabase(stmt);
      Result := true;
      Exit;
    end;

    {process connect statement}
    RegexObj.Expression := '^ *CONNECT +.*(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
      ExtractConnectInfo;
      FDatabase.Connected := false;
      FDatabase.Connected := true;
      Result := true;
      Exit;
    end;

    {Process Drop Database}
    RegexObj.Expression := '^ *DROP +DATABASE *(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
      FDatabase.DropDatabase;
      Result := true;
      Exit;
    end;

    {process commit statement}
    RegexObj.Expression := '^ *COMMIT *(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
      DoCommit;
      Result := true;
      Exit;
    end;

    {process Reconnect statement}
    RegexObj.Expression := '^ *RECONNECT *(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
      DoReconnect;
      Result := true;
      Exit;
    end;


    {Process Set Term}
    RegexObj.Expression := '^ *SET +TERM +(.) *(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
    begin
       FSQLReader.Terminator := RegexObj.Match[1][1];
       Result := true;
       Exit;
    end;

    {process Set SQL Dialect}
    RegexObj.Expression := '^ *SET +SQL +DIALECT +([0-9]) *(\' + Terminator + '|)';
    if RegexObj.Exec(stmt) then
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
    if RegexObj.Exec(stmt) then
    begin
      command := AnsiUpperCase(RegexObj.Match[1]);
      param := trim(RegexObj.Match[2]);
      if command = 'GENERATOR' then
      begin
        Result := false;
        Exit;
      end;
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
        if Database.Attachment.CharSetName2CharSetID(param,charsetid) then
        begin
          DBConnected := Database.Connected;
          LoginPrompt := Database.LoginPrompt;
          Database.LoginPrompt := false;
          Database.Connected := false;
          Database.Params.Values['lc_ctype'] := param;
          Database.Connected := DBConnected;
          Database.LoginPrompt := LoginPrompt;
        end
        else
          raise Exception.CreateFmt(sInvalidCharacterSet, [param,stmt]);
      end
      else
      begin
        if assigned(DataOutputFormatter) then
          DataOutputFormatter.SetCommand(command,param,stmt,Result);
        if not Result then
        begin
          if assigned(OnSetStatement) then
            OnSetStatement(self,command,param,stmt,Result)
          else
            raise Exception.CreateFmt(sInvalidSetStatement, [command,stmt]);
        end;
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
end;

constructor TCustomIBXScript.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FStopOnFirstError := true;
  FEcho := true;
  FAutoDDL := true;
  FISQL := TIBSQL.Create(self);
  FISQL.ParamCheck := true;
  FInternalTransaction := TIBTransaction.Create(self);
  FInternalTransaction.Params.Clear;
  FInternalTransaction.Params.Add('concurrency');
  FInternalTransaction.Params.Add('wait');
end;

destructor TCustomIBXScript.Destroy;
begin
  if FSQLReader <> nil then FSQLReader.Free;
  if FISQL <> nil then FISQL.Free;
  if FInternalTransaction <> nil then FInternalTransaction.Free;
  inherited Destroy;
end;

procedure TCustomIBXScript.DefaultSelectSQLHandler(aSQLText: string);
begin
  if assigned(DataOutputFormatter) then
    DataOutputFormatter.DataOut(aSQLText,@Add2Log)
  else
    FSQLReader.ShowError(sNoSelectSQL);
end;

{ TInteractiveSQLStatementReader }

function TInteractiveSQLStatementReader.GetErrorPrefix: string;
begin
  Result := '';
end;

function TInteractiveSQLStatementReader.GetNextLine(var Line: string): boolean;
begin
  if FNextStatement then
    write(FPrompt)
  else
    write(FContinuePrompt);
  Result := not system.EOF;
  if Result then
  begin
    readln(Line);
    EchoNextLine(Line);
  end;
end;

function TInteractiveSQLStatementReader.GetChar: char;
begin
  if Terminated then
    Result := #0
  else
  if FLineIndex > Length(FLine) then
  begin
    Result := LF;
    FLineIndex := 0;
  end
  else
  if FLineIndex = 0 then
  begin
    if not GetNextLine(FLine) then
      Result := #0
    else
    if Length(FLine) = 0 then
      Result := LF
    else
    begin
      Result := FLine[1];
      FLineIndex := 2;
    end
  end
  else
  begin
    Result := FLine[FLineIndex];
    Inc(FLineIndex);
  end;
end;

constructor TInteractiveSQLStatementReader.Create(aPrompt: string; aContinue: string);
begin
  inherited Create;
  FPrompt := aPrompt;
  FLineIndex := 0;
  FNextStatement := true;
  FContinuePrompt := aContinue;
end;

function TInteractiveSQLStatementReader.GetNextStatement(var stmt: string
  ): boolean;
begin
  Result := inherited GetNextStatement(stmt);
  FNextStatement := Result;
end;

{ TBatchSQLStatementReader }

function TBatchSQLStatementReader.GetChar: char;
begin
  if not EOF and assigned(FInStream) and not (FInStream.Position = FInStream.Size) then
  begin
    Result := char(FInStream.ReadByte);
    if Result = LF then
    begin
      EchoNextLine(FCurLine);
      FCurLine := '';
      if assigned(OnProgressEvent) then
        OnProgressEvent(self,false,FIndex+1);
      Inc(FLineIndex);
      FIndex := 1;
    end
    else
    if Result <> CR then
    begin
      FCurLine += Result;
      Inc(FIndex);
    end;
  end
  else
    Result := #0;
end;

function TBatchSQLStatementReader.GetErrorPrefix: string;
begin
  Result := Format(sOnLineError,[FLineIndex,FIndex]);
end;

procedure TBatchSQLStatementReader.Reset;
begin
  inherited Reset;
  if FOwnsInStream and assigned(FInStream) then
    FInStream.Free;
  FInStream := nil;
  FOwnsInStream := false;
  FLineIndex := 1;
  FIndex := 1;
  FCurLine := '';
end;

procedure TBatchSQLStatementReader.SetStreamSource(Lines: TStrings);
begin
  Reset;
  FInStream := TMemoryStream.Create;
  FOwnsInStream := true;
  Lines.SaveToStream(FInStream);
  FInStream.Position := 0;
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FInStream.Size);
end;

procedure TBatchSQLStatementReader.SetStreamSource(S: TStream);
begin
  Reset;
  FInStream := S;
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,S.Size - S.Position);
end;

procedure TBatchSQLStatementReader.SetStreamSource(FileName: string);
begin
  Reset;
  FInStream := TFileStream.Create(FileName,fmShareCompat);
  FOwnsInStream := true;
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FInStream.Size);
end;

procedure TBatchSQLStatementReader.SetStringStreamSource(S: string);
begin
  Reset;
  FInStream := TStringStream.Create(S);
  FOwnsInStream := true;
  if assigned(OnProgressEvent) then
    OnProgressEvent(self,true,FInStream.Size);
end;

end.

