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
  TOnProgressEvent = procedure (Sender: TObject; Reset: boolean; value: integer) of object;

  { TSQLXMLReader }

  TSQLXMLReader = class(TSQLTokeniser)
  private
    type
      TXMLStates =  (stNoXML, stInTag,stInTagBody,
                     stAttribute,stAttributeValue,stQuotedAttributeValue,
                     stInEndTag, stInEndTagBody,
                     stXMLData);

      TXMLTag    =   (xtNone,xtBlob,xtArray,xtElt);

      TXMLTagDef = record
        XMLTag: TXMLTag;
        TagValue: string;
      end;

    const
      XMLTagDefs: array [xtBlob..xtElt] of TXMLTagDef = (
        (XMLTag: xtBlob;   TagValue: 'blob'),
        (XMLTag: xtArray;  TagValue: 'array'),
        (XMLTag: xtElt;    TagValue: 'elt')
        );
      MaxXMLTags = 20;
      BlobLineLength = 40;

  public
    const
      ibx_blob = 'IBX_BLOB';
      ibx_array = 'IBX_ARRAY';

    type
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

   private
     FDatabase: TIBDatabase;
     FOnProgressEvent: TOnProgressEvent;
     FTransaction: TIBTransaction;
     FXMLState: TXMLStates;
     FXMLTagStack: array [1..MaxXMLTags] of TXMLTag;
     FXMLTagIndex: integer;
     FAttributeName: string;
     FXMLData: string;
     FBlobData: array of TBlobData;
     FCurrentBlob: integer;
     FBlobBuffer: PChar;
     FArrayData: array of TArrayData;
     FCurrentArray: integer;
     FXMLString: string;
     function FindTag(tag: string; var xmlTag: TXMLTag): boolean;
     function GetArrayData(index: integer): TArrayData;
     function GetArrayDataCount: integer;
     function GetBlobData(index: integer): TBlobData;
     function GetBlobDataCount: integer;
     function GetTagName(xmltag: TXMLTag): string;
     procedure ProcessAttributeValue(attrValue: string);
     procedure ProcessBoundsList(boundsList: string);
     procedure ProcessTagValue(tagValue: string);
     procedure XMLTagInit(xmltag: TXMLTag);
     function XMLTagEnd(var xmltag: TXMLTag): boolean;
     procedure XMLTagEnter;
   protected
     function GetErrorPrefix: string; virtual; abstract;
     function TokenFound(var token: TSQLTokens): boolean; override;
     procedure Reset; override;
     procedure ShowError(msg: string; params: array of const); virtual; overload;
     procedure ShowError(msg: string); overload;
   public
     constructor Create;
     procedure FreeDataObjects;
     class function FormatBlob(Field: ISQLData): string;
     class function FormatArray(Database: TIBDatabase; ar: IArray): string;
     property BlobData[index: integer]: TBlobData read GetBlobData;
     property BlobDataCount: integer read GetBlobDataCount;
     property ArrayData[index: integer]: TArrayData read GetArrayData;
     property ArrayDataCount: integer read GetArrayDataCount;
     property Database: TIBDatabase read FDatabase write FDatabase;
     property Transaction: TIBTransaction read FTransaction write FTransaction;
     property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
  end;

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

function StringToHex(octetString: string; MaxLineLength: integer=0): string; overload;
procedure StringToHex(octetString: string; TextOut: TStrings; MaxLineLength: integer=0); overload;


resourcestring
  sInvalidSetStatement = 'Invalid %s Statement - %s';

implementation

uses Sysutils, RegExpr;

resourcestring
  sNoSelectSQL = 'Select SQL Statements are not supported';
  sNoParamQueries =  'Parameterised Queries are not supported';
  sResolveQueryParam =  'Resolving Query Parameter: %s';
  sXMLStackUnderflow = 'XML Stack Underflow';
  sInvalidEndTag = 'XML End Tag Mismatch - %s';
  sBadEndTagClosing = 'XML End Tag incorrectly closed';
  sXMLStackOverFlow = 'XML Stack Overflow';
  sXMLAttributeError = 'Unexpected attribute - "%s" = "%s"';
  sInvalidBoundsList = 'Invalid array bounds list - "%s"';
  sBinaryBlockMustbeEven = 'Binary block must have an even number of characters';
  sInvalidCharacterSet = 'Unrecognised character set name - "%s"';
  sOnLineError = 'On Line %d Character %d: ';
  sArrayIndexError = 'Array Index Error (%d)';
  sBlobIndexError = 'Blob Index Error (%d)';
  sStatementError = 'Error processing SQL statement: %s %s - for statement "%s"';
  sNotInArray = 'elt tag found but not in an XML array tag';
  sNoDatabase = 'Missing database for xml tag import';
  sNoTransaction = 'Missing transaction for xml tag import';

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
  Result := false;
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

          sqltCR: {ignore};

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

        sqltComment:
          stmt += '/*' + TokenText + '*/';

        sqltCommentLine:
          stmt += '/*' + TokenText + ' */' + LineEnding;

        sqltQuotedString:
          stmt += '''' + SQLSafeString(TokenText) + '''';

        sqltIdentifierInDoubleQuotes:
          stmt += '"' + TokenText + '"';

        sqltCR: {ignore};

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
          stmt += '/*' + TokenText + ' */' + LineEnding;

        sqltCloseSquareBracket:
        begin
          stmt += TokenText;
          State := stInStmt;
        end;

        sqltCR: {ignore};

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
          stmt += '/*' + TokenText + ' */' + LineEnding;

        sqltSemiColon:
          begin
            State := stInStmt;
            stmt += TokenText;
          end;

        sqltCR: {ignore};

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

{ TSQLXMLReader }

function TSQLXMLReader.FindTag(tag: string; var xmlTag: TXMLTag): boolean;
var i: TXMLTag;
begin
  Result := false;
  for i := xtBlob to xtElt do
    if XMLTagDefs[i].TagValue = tag then
    begin
      xmlTag := XMLTagDefs[i].XMLTag;
      Result := true;
      break;
    end;
end;

function TSQLXMLReader.GetArrayData(index: integer): TArrayData;
begin
  if (index < 0) or (index > ArrayDataCount) then
    ShowError(sArrayIndexError,[index]);
  Result := FArrayData[index];
end;

function TSQLXMLReader.GetArrayDataCount: integer;
begin
  Result := Length(FArrayData);
end;

function TSQLXMLReader.GetBlobData(index: integer): TBlobData;
begin
  if (index < 0) or (index > BlobDataCount) then
    ShowError(sBlobIndexError,[index]);
  Result := FBlobData[index];
end;

function TSQLXMLReader.GetBlobDataCount: integer;
begin
  Result := Length(FBlobData);
end;

function TSQLXMLReader.GetTagName(xmltag: TXMLTag): string;
var i: TXMLTag;
begin
  Result := 'unknown';
  for i := xtBlob to xtElt do
    if XMLTagDefs[i].XMLTag = xmltag then
    begin
      Result := XMLTagDefs[i].TagValue;
      Exit;
    end;
end;

procedure TSQLXMLReader.ProcessAttributeValue(attrValue: string);
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    if FAttributeName = 'subtype' then
      FBlobData[FCurrentBlob].SubType := StrToInt(attrValue)
    else
      ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

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
      ShowError(sXMLAttributeError,[FAttributeName,attrValue]);

  xtElt:
    if FAttributeName = 'ix' then
      with FArrayData[FCurrentArray] do
        Index[CurrentRow] :=  StrToInt(attrValue)
     else
        ShowError(sXMLAttributeError,[FAttributeName,attrValue]);
  end;
end;

procedure TSQLXMLReader.ProcessBoundsList(boundsList: string);
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
        ShowError(sInvalidBoundsList,[boundsList]);
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

procedure TSQLXMLReader.ProcessTagValue(tagValue: string);

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
      ShowError(sBinaryBlockMustbeEven,[nil]);
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

procedure TSQLXMLReader.XMLTagInit(xmltag: TXMLTag);
begin
  if FXMLTagIndex > MaxXMLTags then
    ShowError(sXMLStackOverFlow,[nil]);
  Inc(FXMLTagIndex);
  FXMLTagStack[FXMLTagIndex] := xmltag;
  FXMLString := '';

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
        Inc(CurrentRow)
  end;
end;

function TSQLXMLReader.XMLTagEnd(var xmltag: TXMLTag): boolean;
begin
  if FXMLTagIndex = 0 then
    ShowError(sXMLStackUnderflow,[nil]);

  xmlTag := FXMLTagStack[FXMLTagIndex];
  case FXMLTagStack[FXMLTagIndex] of
  xtBlob:
    FBlobData[FCurrentBlob].BlobIntf.Close;

  xtArray:
    FArrayData[FCurrentArray].ArrayIntf.SaveChanges;

  xtElt:
    Dec(FArrayData[FCurrentArray].CurrentRow);
  end;
  Dec(FXMLTagIndex);
  Result := FXMLTagIndex = 0;
end;

procedure TSQLXMLReader.XMLTagEnter;
var aCharSetID: integer;
begin
  if Database = nil then
    ShowError(sNoDatabase);
  if Transaction = nil then
    ShowError(sNoTransaction);
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
      Database.Attachment.CharSetName2CharSetID(CharSet,aCharSetID);
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

{This is where the XML tags are identified and the token stream modified in
 consequence}

function TSQLXMLReader.TokenFound(var token: TSQLTokens): boolean;

 procedure NotAnXMLTag;
 begin
   begin
     if FXMLTagIndex = 0 then
     {nothing to do with XML so go back to processing SQL}
     begin
       QueueToken(token);
       ReleaseQueue(token);
       FXMLState := stNoXML
     end
     else
     begin
       {Not an XML tag, so just push back to XML Data}
       FXMLState := stXMLData;
       FXMLString += GetQueuedText;
       ResetQueue;
     end;
   end;
 end;

var XMLTag: TXMLTag;
begin
  Result := inherited TokenFound(token);
  if not Result then Exit;

  case FXMLState of
  stNoXML:
    if token = sqltLT then
    begin
      ResetQueue;
      QueueToken(token); {save in case this is not XML}
      FXMLState := stInTag;
    end;

  stInTag:
    {Opening '<' found, now looking for tag name or end tag marker}
    case token of
    sqltIdentifier:
      begin
        if FindTag(TokenText,XMLTag) then
        begin
          XMLTagInit(XMLTag);
          QueueToken(token);
          FXMLState := stInTagBody;
        end
        else
          NotAnXMLTag;
      end;

    sqltForwardSlash:
      FXMLState := stInEndTag;

    else
      NotAnXMLTag;
    end {case token};

  stInTagBody:
    {Tag name found. Now looking for attribute or closing '>'}
    case token of
    sqltIdentifier:
      begin
        FAttributeName := TokenText;
        QueueToken(token);
        FXMLState := stAttribute;
      end;

    sqltGT:
      begin
        ResetQueue;
        XMLTagEnter;
        FXMLState := stXMLData;
      end;

    sqltSpace,
    sqltCR, sqltEOL:
      QueueToken(token);

    else
      NotAnXMLTag;
    end {case token};

  stAttribute:
    {Attribute name found. Must be followed by an '=', a '>' or another tag name}
    case token of
      sqltEquals:
      begin
        QueueToken(token);
        FXMLState := stAttributeValue;
      end;

      sqltSpace,
      sqltCR, sqltEOL:
        QueueToken(token);

      sqltIdentifier:
        begin
          ProcessAttributeValue('');
          FAttributeName := TokenText;
          QueueToken(token);
          FXMLState := stAttribute;
        end;

      sqltGT:
        begin
          ProcessAttributeValue('');
          ResetQueue;
          XMLTagEnter;
          FXMLState := stXMLData;
        end;

      else
        NotAnXMLTag;
    end; {case token}

  stAttributeValue:
    {Looking for attribute value as a single identifier or a double quoted value}
    case token of
    sqltIdentifier,sqltIdentifierInDoubleQuotes:
      begin
        ProcessAttributeValue(TokenText);
        QueueToken(token);
        FXMLState := stInTagBody;
      end;

    sqltSpace,
    sqltCR, sqltEOL:
      QueueToken(token);

    else
      NotAnXMLTag;
    end; {case token}

  stXMLData:
    if token = sqltLT then
    begin
      QueueToken(token); {save in case this is not XML}
      FXMLState := stInTag;
    end
    else
      FXMLString += TokenText;

  stInEndTag:
    {Opening '</' found, now looking for tag name}
    case token of
    sqltIdentifier:
      begin
        if FindTag(TokenText,XMLTag) and (XMLTag = FXMLTagStack[FXMLTagIndex]) then
        begin
          QueueToken(token);
          FXMLState := stInEndTagBody;
        end
        else
          ShowError(sInvalidEndTag,[TokenText]);
      end;
    else
      NotAnXMLTag;
    end {case token};

  stInEndTagBody:
  {End tag name found, now looping for closing '>'}
    case Token of
    sqltGT:
      begin
        ProcessTagValue(FXMLString);
        if XMLTagEnd(XMLTag) then
        begin
          ResetQueue;
          QueueToken(sqltColon,':');
          case XMLTag of
            xtBlob:
              QueueToken(sqltIdentifier,Format(ibx_blob+'%d',[FCurrentBlob]));

            xtArray:
              QueueToken(sqltIdentifier, Format(ibx_array+'%d',[FCurrentArray]));
          end;
          ReleaseQueue(token);
          FXMLState := stNoXML;
       end
       else
         FXMLState := stXMLData;
      end;

    sqltSpace,
    sqltCR, sqltEOL:
      QueueToken(token);

    else
      ShowError(sBadEndTagClosing);
    end; {case token}

  end {Case FState};

  {Only allow token to be returned if not processing an XML tag}

  Result := FXMLState = stNoXML;
end;

procedure TSQLXMLReader.ShowError(msg: string; params: array of const);
begin
  raise EIBClientError.CreateFmt(GetErrorPrefix + msg,params);
end;

procedure TSQLXMLReader.ShowError(msg: string);
begin
  ShowError(msg,[nil]);
end;

constructor TSQLXMLReader.Create;
begin
  inherited;
  FXMLState := stNoXML;
end;

procedure TSQLXMLReader.FreeDataObjects;
begin
  FXMLTagIndex := 0;
  SetLength(FBlobData,0);
  FCurrentBlob := -1;
  SetLength(FArrayData,0);
  FCurrentArray := -1;
end;

class function TSQLXMLReader.FormatBlob(Field: ISQLData): string;
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

class function TSQLXMLReader.FormatArray(Database: TIBDatabase; ar: IArray
  ): string;
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
      s += Format(' charset = "%s"',[Database.Attachment.GetCharsetName(ar.GetCharSetID)]);
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
  end;       end;

procedure TSQLXMLReader.Reset;
begin
  inherited Reset;
  FreeDataObjects;
  FXMLString := '';
  FreeMem(FBlobBuffer);
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
 FSQLReader.Database := AValue;
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
  while FSQLReader.GetNextStatement(stmt) do
  try
    stmt := trim(stmt);
//    writeln('stmt = ',stmt);
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
      FDatabase.Connected := false;
      FDatabase.CreateDatabase(stmt);
      FDatabase.Connected := false;
      ExtractUserInfo;
      FDatabase.Connected := true;
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
  FSQLReader.Transaction := AValue;
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

