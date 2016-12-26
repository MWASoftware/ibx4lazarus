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
 *  The Original Code is (C) 2014 Tony Whyman, MWA Software
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
  ibx_blob = ':IBX_BLOB';
  ibx_array = ':IBX_ARRAY';

type
  TSQLSymbol = (sqNone,sqSpace,sqSemiColon,sqSingleQuotes,sqDoubleQuotes,
                sqEnd,sqBegin,sqCommit,sqRollback,sqString,sqCommentStart,
                sqCommentEnd,sqCommentLine,sqAsterisk,sqForwardSlash,
                sqDeclare,sqEOL,sqTerminator, sqReconnect,sqCase,
                sqEquals,sqGT,sqTag,sqEndTag);

  TSQLStates =  (stInit, stError, stInSQL, stNested, stInSingleQuotes,
                 stInDoubleQuotes, stInComment, stInCommentLine,
                 stInDeclaration, stInCommit, stInReconnect,
                 {XML Analysis states}
                 stInTag,stAttribute,stAttributeValue,stQuotedAttributeValue,
                 stTagged,stEndTag);

  TXMLTag    =   (xtNone,xtBinary,xtArray,xtElt);

  TGetParamValue = procedure(Sender: TObject; ParamName: string; var BlobID: TISC_QUAD) of object;
  TLogEvent = procedure(Sender: TObject; Msg: string) of Object;
  TOnProgressEvent = procedure (Sender: TObject; Reset: boolean; value: integer) of object;
  TOnSelectSQL = procedure (Sender: TObject; SQLText: string) of object;

  {
  TIBXScript: runs an SQL script in the specified file or stream. The text is parsed
  into SQL statements which are executed in turn. The intention is to be ISQL
  compatible but with extensions:

  * SET TERM and Set AutoDDL are both supported

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
  * Echo: boolean. When true, all SQL statements are echoed to log
  * StopOnFirstError: boolean. When true the script engine terminates on the first
    SQL Error.
  * IgnoreGrants: When true, grant statements are silently discarded. This can be
    useful when applying a script using the Embedded Server.


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

  The PerformUpdate function is used to execute an SQL Script and may be called
  multiple times.
  }

  TBlobData = record
    BlobIntf: IBlob;
    SubType: cardinal;
  end;

  TArrayData = record
    ArrayIntf: IArray;
    SQLType: cardinal;
    dim: cardinal;
    Size: cardinal;
    Scale: integer;
    CharSet: string;
    bounds: TArrayBounds;
    CurrentRow: integer;
    Index: array of integer;
  end;

  TOnSetStatement = procedure(Sender: TObject; command, aValue, stmt: string; var Done: boolean) of object;


  { TIBXScript }

  TIBXScript = class(TComponent)
  private
    FDatabase: TIBDatabase;
    FDataOutputFormatter: TIBCustomDataOutput;
    FEcho: boolean;
    FIgnoreGrants: boolean;
    FOnErrorLog: TLogEvent;
    FOnProgressEvent: TOnProgressEvent;
    FOnSelectSQL: TOnSelectSQL;
    FOnSetStatement: TOnSetStatement;
    FShowAffectedRows: boolean;
    FShowPerformanceStats: boolean;
    FStopOnFirstError: boolean;
    FTransaction: TIBTransaction;
    FInternalTransaction: TIBTransaction;
    FState: TSQLStates;
    FString: string;
    FISQL: TIBSQL;
    FLastSymbol: TSQLSymbol;
    FNested: integer;
    FLastChar: char;
    FSQLText: string;
    FHasBegin: boolean;
    FInCase: boolean;
    FStack: array [0..16] of TSQLStates;
    FStackindex: integer;
    FGetParamValue: TGetParamValue;
    FOnOutputLog: TLogEvent;
    FTerminator: char;
    FAutoDDL: boolean;
    {XML Control variables}
    FXMLTag: TXMLTag;
    FXMLTagStack: array [1..20] of TXMLTag;
    FXMLTagIndex: integer;
    FAttributeName: string;
    FXMLString: string;
    FBlobData: array of TBlobData;
    FCurrentBlob: integer;
    FArrayData: array of TArrayData;
    FCurrentArray: integer;
    FBlobBuffer: PChar;
    procedure Add2Log(const Msg: string; IsError: boolean=true);
    procedure AddToSQL(const Symbol: string);
    function AnalyseSQL(Lines: TStringList): boolean;
    procedure AnalyseLine(const Line: string);
    procedure DoCommit;
    procedure DoReconnect;
    procedure ExecSQL;
    function GetNextSymbol(C: char): TSQLSymbol;
    function GetSymbol(const Line: string; var index: integer): TSQLSymbol;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetDataOutputFormatter(AValue: TIBCustomDataOutput);
    procedure SetParamValue(SQLVar: ISQLParam);
    procedure SetShowPerformanceStats(AValue: boolean);
    procedure SetState(AState: TSQLStates);
    procedure ClearStatement;
    function PopState: TSQLStates;
    function ProcessStatement(stmt: string): boolean;
    {XML Handling}
    procedure EndXMLTag(xmltag: TXMLTag);
    procedure EnterTag;
    procedure ProcessTagValue(tagValue: string);
    procedure StartXMLTag(xmltag: TXMLTag);
    procedure ProcessAttributeValue(attrValue: string);
    procedure ProcessBoundsList(boundsList: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    {use RunScript instead of PerformUpdate}
    function PerformUpdate(const SQLFile: string;  AutoDDL: boolean): boolean; overload; deprecated;
    function PerformUpdate(const SQLStream: TStream;   AutoDDL: boolean): boolean; overload; deprecated;
    function RunScript(const SQLFile: string;  AutoDDL: boolean): boolean; overload;
    function RunScript(const SQLStream: TStream;   AutoDDL: boolean): boolean; overload;
  published
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property DataOutputFormatter: TIBCustomDataOutput read FDataOutputFormatter
                                  write SetDataOutputFormatter;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL;
    property Echo: boolean read FEcho write FEcho default true;  {Echo Input to Log}
    property IgnoreGrants: boolean read FIgnoreGrants write FIgnoreGrants;
    property Transaction: TIBTransaction read FTransaction write FTransaction;
    property ShowAffectedRows: boolean read FShowAffectedRows write FShowAffectedRows;
    property ShowPerformanceStats: boolean read FShowPerformanceStats write SetShowPerformanceStats;
    property StopOnFirstError: boolean read FStopOnFirstError write FStopOnFirstError default true;
    property GetParamValue: TGetParamValue read FGetParamValue write FGetParamValue; {resolve parameterized queries}
    property OnOutputLog: TLogEvent read FOnOutputLog write FOnOutputLog; {Log handler}
    property OnErrorLog: TLogEvent read FOnErrorLog write FOnErrorLog;
    property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
    property OnSelectSQL: TOnSelectSQL read FOnSelectSQL write FOnSelectSQL; {Handle Select SQL Statements}
    property OnSetStatement: TOnSetStatement read FOnSetStatement write FOnSetStatement;
  end;

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
  sXMLError = 'Invalid XML on Line "%s"';
  sXMLAttributeError = 'Unexpected attribute - "%s"';
  sInvalidBoundsList = 'Invalid array bounds list - "%s"';
  sBlobBlobMustbeEven = 'Binary block must have an even number of characters';
  sInvalidCharacterSet = 'Unrecognised character set name - "%s"';

{ TIBXScript }

procedure TIBXScript.Add2Log(const Msg: string; IsError: boolean);
begin
  if IsError then
  begin
    if assigned(OnErrorLog) then OnErrorLog(self,Msg)
  end
  else
  if assigned(FOnOutputLog) then FOnOutputLog(self,Msg)
end;

procedure TIBXScript.AddToSQL(const Symbol: string);
begin
  FSQLText := FSQLText +  Symbol
end;

procedure TIBXScript.AnalyseLine(const Line: string);
var index: integer;
    Symbol: TSQLSymbol;
    NonSpace: boolean;
begin
  index := 1;
  NonSpace := false;
  while true do
  begin
    if FState = stError then
      raise Exception.Create(sErrorState);
    Symbol := GetSymbol(Line,index);
    if not (Symbol in [sqSpace,sqEOL]) then
      NonSpace := true;

    if FState in [stInTag..stEndTag] then
    {XML State Processing}
    case Symbol of
    sqSpace:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += ' ';
      end;

    sqCommentStart,
    sqCommentEnd,sqCommentLine,
    sqEnd,sqBegin,sqCommit,sqRollback,
    sqDeclare,sqReconnect,sqCase:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += FString;
      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqSemiColon:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += ';';
      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqSingleQuotes:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += '''';
      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqAsterisk:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += '*';
      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqForwardSlash:
      case FState of
      stQuotedAttributeValue,
      stTagged:
         FXMLString += '/';
      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqGT:
      case FState of
      stEndTag:
        begin
          case FXMLTag of
          xtBinary:
            AddToSQL(Format(ibx_blob+'%d',[FCurrentBlob]));
          xtArray:
            AddToSQL(Format(ibx_array+'%d',[FCurrentArray]));
          end;
          PopState;
        end;

      stInTag:
        begin
          FXMLString := '';
          FState := stTagged;
          EnterTag;
        end;

      stQuotedAttributeValue,
      stTagged:
        FXMLString += '>';

      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqTag:
      if FState = stTagged then
      begin
        StartXMLTag(FXMLTag);
        SetState(stInTag)
      end
      else
        raise Exception.CreateFmt(sXMLError,[Line]);

    sqEndTag:
      if FState = stTagged then
      begin
        ProcessTagValue(FXMLString);
        EndXMLTag(FXMLTag);
        FState := stEndTag;
      end
      else
        raise Exception.CreateFmt(sXMLError,[Line]);

    sqEquals:
      case FState of
      stAttribute:
        FState := stAttributeValue;

      stQuotedAttributeValue,
      stTagged:
        FXMLString += '=';

      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqDoubleQuotes:
      case FState of
      stAttributeValue:
        begin
          FXMLString := '';
          FState := stQuotedAttributeValue;
        end;

      stQuotedAttributeValue:
        begin
          ProcessAttributeValue(FXMLString);
          FState := stInTag;
        end;

      stTagged:
        FXMLString += '"';

      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqString:
      case FState of
      stInTag: {attribute name}
        begin
          FAttributeName := FString;
          FState := stAttribute;
        end;

      stAttributeValue:
        begin
          ProcessAttributeValue(FString);
          FState := stInTag;
        end;

      stQuotedAttributeValue:
         FXMLString += FString;

      stTagged:
          FXMLString += FString;

      else
        raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    end
    else
    {SQL State Processing}
    case Symbol of
    sqSpace:
      if not (FState in [stInComment,stInCommentLine]) then
        AddToSQL(' ');

    sqEquals:
      if not (FState in [stInComment,stInCommentLine]) then
        AddToSQL('=');

    sqGT:
      if not (FState in [stInComment,stInCommentLine]) then
        AddToSQL('>');

    sqTag:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState in [stInSQL,stNested] then
        begin
          StartXMLTag(FXMLTag);
          SetState(stInTag);
        end
        else
          raise Exception.CreateFmt(sXMLError,[Line]);
      end;

    sqTerminator:
      if not (FState in [stInComment,stInCommentLine]) then
        case FState of
        stInit: {ignore empty statement};

        stInSQL:
            ExecSQL;

       stInCommit:
            DoCommit;

       stInReconnect:
           DoReconnect;

       stNested, stInSingleQuotes, stInDoubleQuotes:
         AddToSQL(FTerminator);

       stInDeclaration:
         begin
           FState := PopState;
           AddToSQL(FTerminator);
         end;

       else
         raise Exception.CreateFmt(sTerminatorUnknownState,[FState]);
       end;

    sqSemiColon:
        begin
          if FState = stInDeclaration then
            FState := PopState;
          AddToSQL(';');
        end;

    sqAsterisk:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
       AddToSQL('*');
       if FState =  stInit then
          FState := stInSQL
      end;

    sqForwardSlash:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
       AddToSQL('/');
       if FState =  stInit then
          FState := stInSQL
      end;

    sqCommentStart:
      if not (FState in [stInComment,stInCommentLine]) then
        SetState(stInComment);

    sqCommentEnd:
      if FState = stInComment then
      begin
        AddToSQL('/* ' + Trim(FString) + ' */');
        FState := PopState
      end
      else
        FState := stError;

    sqCommentLine:
      if not (FState in [stInComment,stInCommentLine]) then
        SetState(stInCommentLine);

    sqSingleQuotes:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        case FState of
        stInSingleQuotes:
          FState := PopState;
        stInDoubleQuotes:
          {Ignore};
        else
          SetState(stInSingleQuotes)
        end;
        AddToSQL('''')
      end;

    sqDoubleQuotes:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        case FState of
        stInSingleQuotes:
          {Ignore};
        stInDoubleQuotes:
          FState := PopState;
        else
          SetState(stInDoubleQuotes)
        end;
        AddToSQL('"')
      end;

    sqEnd:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        AddToSQL(FString);
        case FState of
        stInSingleQuotes,
        stInDoubleQuotes:
          {Ignore};
        stNested:
          begin
            if FNested = 0 then
            begin
              FState := PopState;
              if not FInCase then
              begin
                FState := stInit;
                ExecSQL
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
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        FHasBegin := true;
        AddToSQL(FString);
        case FState of
        stInSingleQuotes,
        stInDoubleQuotes:
          {Ignore};
        stNested:
          Inc(FNested);

        stInSQL,
        stInit:
          SetState(stNested);
        end
      end;

    sqCase:
    if not (FState in [stInComment,stInCommentLine]) then
    begin
      AddToSQL(FString);
      case FState of
      stInSingleQuotes,
      stInDoubleQuotes:
        {Ignore};
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
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        AddToSQL(FString);
        if FState in [stInit,stInSQL] then
          SetState(stInDeclaration)
      end;

    sqCommit:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState = stInit then
          FState := stInCommit
        else
          AddToSQL(FString);
      end;

    sqReconnect:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState = stInit then
          FState := stInReconnect
        else
          raise Exception.Create(sNoReconnect)
      end;

    sqString:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        AddToSQL(FString);
        if FState = stInit then
          FState := stInSQL
      end;

    sqEOL:
      begin
        case FState of
        stInCommentLine:
        begin
          AddToSQL('/* ' + Trim(FString) + ' */');
          FState := PopState;
        end;
        stInDoubleQuotes,
        stInSingleQuotes:
          raise Exception.Create(sUnterminatedString);
        stInit:
          Exit;
        end;
        if NonSpace then AddToSQL(LineEnding);
        Exit;
      end;
    else
      raise Exception.CreateFmt(sUnknownSymbol,[Symbol]);
    end
  end
end;

function TIBXScript.AnalyseSQL(Lines: TStringList): boolean;
var I: integer;
begin
  Result := true;
  ClearStatement;
  FLastSymbol := sqNone;
  for I := 0 to Lines.Count - 1 do
  begin
    if Echo then Add2Log(Lines[I],false);
    if assigned(OnProgressEvent) then
      OnProgressEvent(self,false,1);
    try
      AnalyseLine(Lines[I]);
    except on E:Exception do
      begin
        Add2Log(E.Message);
        Result := false;
        if StopOnFirstError then Exit;
        ClearStatement;
        FLastSymbol := sqNone;
      end
    end;
  end;
  if FState <> stInit then
    AnalyseLine(';');
  Result := (FStackIndex = 0) and (FState = stInit)
end;

constructor TIBXScript.Create(aOwner: TComponent);
begin
  inherited;
  FStopOnFirstError := true;
  FEcho := true;
  FState := stInit;
  FISQL := TIBSQL.Create(self);
  FISQL.ParamCheck := true;
  FInternalTransaction := TIBTransaction.Create(self);
  FInternalTransaction.Params.Clear;
  FInternalTransaction.Params.Add('concurrency');
  FInternalTransaction.Params.Add('wait');
  ClearStatement;
end;

destructor TIBXScript.Destroy;
begin
  if FISQL <> nil then FISQL.Free;
  FreeMem(FBlobBuffer);
  if FInternalTransaction <> nil then FInternalTransaction.Free;
  inherited;
end;

function TIBXScript.PerformUpdate(const SQLFile: string; AutoDDL: boolean
  ): boolean;
begin
  Result := RunScript( SQLFile,AutoDDL);
end;

function TIBXScript.PerformUpdate(const SQLStream: TStream; AutoDDL: boolean
  ): boolean;
begin
  Result := RunScript(SQLStream,AutoDDL);
end;

procedure TIBXScript.DoCommit;
begin
  with GetTransaction do
    if InTransaction then Commit;
  if not GetTransaction.InTransaction then
    GetTransaction.StartTransaction;
  ClearStatement;
end;

procedure TIBXScript.DoReconnect;
begin
  with GetTransaction do
    if InTransaction then Commit;
  Database.Connected := false;
  Database.Connected := true;
  if not GetTransaction.InTransaction then
    GetTransaction.StartTransaction;
  ClearStatement;
end;

procedure TIBXScript.EndXMLTag(xmltag: TXMLTag);
begin
  if FXMLTagIndex = 0 then
    raise Exception.Create(sXMLStackUnderflow);
  if xmltag <> FXMLTagStack[FXMLTagIndex] then
    raise Exception.CreateFmt(sInvalidEndTag,[FString]);

  case FXMLTagStack[FXMLTagIndex] of
  xtBinary:
    FBlobData[FCurrentBlob].BlobIntf.Close;

  xtArray:
    FArrayData[FCurrentArray].ArrayIntf.SaveChanges;
  end;
  Dec(FXMLTagIndex);
end;

procedure TIBXScript.EnterTag;
var aCharSetID: integer;
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBinary:
    FBlobData[FCurrentBlob].BlobIntf := Database.Attachment.CreateBlob(
      Transaction.TransactionIntf,FBlobData[FCurrentBlob].SubType);

  xtArray:
    with FArrayData[FCurrentArray] do
    begin
      FirebirdAPI.CharSetName2CharSetID(CharSet,aCharSetID);
      ArrayIntf := Database.Attachment.CreateArray(
                     Transaction.TransactionIntf,
                     Database.Attachment.CreateArrayMetaData(SQLType,Scale,Size,
                     aCharSetID,dim,bounds)
                     );
    end;

  xtElt:
    Inc(FArrayData[FCurrentArray].CurrentRow);
  end;
end;

procedure TIBXScript.ProcessTagValue(tagValue: string);

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

  procedure WriteToBlob(hexData: string);
  var i,j : integer;
      blength: integer;
  begin
    if odd(length(hexData)) then
      raise Exception.Create(sBlobBlobMustbeEven);
    blength := Length(hexData) div 2;
    IBAlloc(FBlobBuffer,0,blength);
    j := 1;
    for i := 1 to blength do
    begin
      FBlobBuffer^ := char((nibble(hexData[j]) shl 4) or nibble(hexdata[j+1]));
      Inc(j,2);
    end;
    FBlobData[FCurrentBlob].BlobIntf.Write(FBlobBuffer^,blength);
  end;

begin
  if tagValue = '' then Exit;
  case FXMLTagStack[FXMLTagIndex] of
  xtBinary:
      WriteToBlob(tagValue);

  xtElt:
    with FArrayData[FCurrentArray] do
      ArrayIntf.SetAsString(index,tagValue);

  end;
end;

procedure TIBXScript.ExecSQL;
var DDL: boolean;
    I: integer;
    stats: TPerfCounters;
begin
 if FSQLText <> '' then
 begin
   if ProcessStatement(FSQLText) then {Handle Set Statement}
   begin
     ClearStatement;
     Exit;
   end;

   Database.Connected := true;
   FISQL.SQL.Text := FSQLText;
   FISQL.Transaction := GetTransaction;
   FISQL.Transaction.Active := true;
   FISQL.ParamCheck := not FHasBegin; {Probably PSQL}
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
     if assigned(DataOutputFormatter) then
       DataOutputFormatter.DataOut(FSQLText,@Add2Log)
     else
     if assigned(OnSelectSQL) then
       OnSelectSQL(self,FSQLText)
     else
       raise Exception.Create(sNoSelectSQL);
   end
   else
   begin
     DDL := FISQL.SQLStatementType = SQLDDL;
     if not DDL or not FIgnoreGrants or (Pos('GRANT',AnsiUpperCase(Trim(FSQLText))) <> 1) then
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
   ClearStatement;
 end
end;



function TIBXScript.GetNextSymbol(C: char): TSQLSymbol;
begin
    if C = FTerminator then
      Result := sqTerminator
    else
    case C of
    ' ',#9,#10,#13:
      Result := sqSpace;
    ';':
      Result := sqSemiColon;
    '"':
      Result := sqDoubleQuotes;
    '''':
      Result := sqSingleQuotes;
    '/':
      Result := sqForwardSlash;
    '*':
      Result := sqAsterisk;
    '=':
      Result := sqEquals;
    '>':
      Result := sqGT;
    else
      begin
        Result := sqString;
        FLastChar := C
      end
    end;
end;

function TIBXScript.GetSymbol(const Line: string; var index: integer): TSQLSymbol;
begin
  Result := sqNone;
  if FLastSymbol <> sqNone then
  begin
    Result := FLastSymbol;
    if Result = sqString then
      FString := FLastChar;
    FLastSymbol := sqNone
  end;

  while (index <= Length(Line)) and (FLastSymbol = sqNone) do
  begin
    FLastSymbol := GetNextSymbol(Line[index]);
    {combine if possible}
    case Result of
    sqNone:
      begin
        Result := FLastSymbol;
        if FLastSymbol = sqString then
          FString := FLastChar;
        FLastSymbol := sqNone
      end;

    sqForwardSlash:
      if FLastSymbol = sqAsterisk then
      begin
        Result := sqCommentStart;
        FString := '/*';
        FLastSymbol := sqNone
      end
      else
      if FLastSymbol = sqForwardSlash then
      begin
        FString := '//';
        Result := sqCommentLine;
        FLastSymbol := sqNone
      end;

    sqAsterisk:
      if FLastSymbol = sqForwardSlash then
      begin
        FString := '*/';
        Result := sqCommentEnd;
        FLastSymbol := sqNone
      end;

    sqString:
      if FLastSymbol = sqString then
      begin
        FString := FString + FLastChar;
        FLastSymbol := sqNone
      end;
    end;
    Inc(index)
  end;

  if (index > Length(Line)) then
    if Result = sqNone then
      Result := sqEOL
    else
    if (FLastSymbol = sqNone) and (Result <> sqEOL) then
      FLastSymbol := sqEOL;

  if Result = sqString then
  begin
    if FString <> '' then
      if CompareText(FString,'begin') = 0 then
        Result := sqBegin
      else
      if CompareText(FString,'end') = 0 then
        Result := sqEnd
      else
      if CompareText(FString,'declare') = 0 then
        Result := sqDeclare
      else
      if CompareText(FString,'commit') = 0 then
        Result := sqCommit
      else
      if CompareText(FString,'reconnect') = 0 then
        Result := sqReconnect
      else
      if CompareText(FString,'case') = 0 then
        Result := sqCase
      else
      if CompareText(FString,'<binary') = 0 then
      begin
        FXMLTag := xtBinary;
        Result := sqTag
      end
      else
      if CompareText(FString,'<array') = 0 then
      begin
        FXMLTag := xtArray;
        Result := sqTag
      end
      else
      if CompareText(FString,'<elt') = 0 then
      begin
        FXMLTag := xtElt;
        Result := sqTag
      end
      else
      if CompareText(FString,'</binary') = 0 then
      begin
        FXMLTag := xtBinary;
        Result := sqEndTag
      end
      else
      if CompareText(FString,'</array') = 0 then
      begin
        FXMLTag := xtArray;
        Result := sqEndTag
      end
      else
      if CompareText(FString,'</elt') = 0 then
      begin
        FXMLTag := xtElt;
        Result := sqEndTag
      end
  end
end;

function TIBXScript.GetTransaction: TIBTransaction;
begin
  if FTransaction = nil then
    Result := FInternalTransaction
  else
    Result := FTransaction;
end;

procedure TIBXScript.SetDatabase(AValue: TIBDatabase);
begin
  if FDatabase = AValue then Exit;
  FDatabase := AValue;
  FISQL.Database := AValue;
  FInternalTransaction.DefaultDatabase := AValue;
end;

procedure TIBXScript.SetDataOutputFormatter(AValue: TIBCustomDataOutput);
begin
  if FDataOutputFormatter = AValue then Exit;
  if (FDataOutputFormatter <> nil) and (AValue <> nil) then
    AValue.Assign(FDataOutputFormatter);
  FDataOutputFormatter := AValue;
  if FDataOutputFormatter <> nil then
    FDataOutputFormatter.Database := Database;
end;

function TIBXScript.RunScript(const SQLFile: string;
                                     AutoDDL: boolean): boolean;
var F: TFileStream;
begin
  F := TFileStream.Create(SQLFile,fmOpenRead or fmShareDenyNone);
  try
    Result := RunScript(F,AutoDDL)
  finally
    F.Free
  end;
end;

function TIBXScript.RunScript(const SQLStream: TStream; AutoDDL: boolean): boolean;
var Lines: TStringList;
begin
  FTerminator := ';';
  FLastChar := ' ';
  FLastSymbol := sqNone;
  FAutoDDL := AutoDDL;
  try
    Lines := TStringList.Create;
    Lines.LoadFromStream(SQLStream);
    try
      if assigned(OnProgressEvent) then
        OnProgressEvent(self,true,Lines.Count);

      Result := AnalyseSQL(Lines)
    finally
      Lines.Free
    end;
  except on E:Exception do
    begin
      Add2Log(E.Message);
      with GetTransaction do
        if InTransaction then Rollback;
      Result := false
    end
  end;
  with GetTransaction do
    if InTransaction then Commit;
end;

function TIBXScript.PopState: TSQLStates;
begin
  if FStackIndex = 0 then
    raise Exception.Create(sStackUnderflow);
  Dec(FStackIndex);
  Result := FStack[FStackIndex]
end;

function TIBXScript.ProcessStatement(stmt: string): boolean;
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

      RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +USER +''.+'' PASSWORD +''(.+)''';
      if not RegexObj.Exec(ucStmt) and (FDatabase.Params.IndexOfName('password') <> -1) then
      begin
        RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(USER +''.+'')';
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
begin
  Result := false;
  ucStmt := AnsiUpperCase(stmt);
  RegexObj := TRegExpr.Create;
  try
    {process create database}
    RegexObj.Expression := '^ *CREATE +(DATABASE|SCHEMA) +(.*) *(\' + FTerminator + '|)';
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
    RegexObj.Expression := '^ *CONNECT +(.*) *(\' + FTerminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
      ExtractConnectInfo;
      DoReconnect;
      Result := true;
      Exit;
    end;


    {Process Set Term}
    RegexObj.Expression := '^ *SET +TERM +(.) *(\' + FTerminator + '|)';
    if RegexObj.Exec(ucStmt) then
    begin
       FTerminator := RegexObj.Match[1][1];
       Result := true;
       Exit;
    end;

    {process Set SQL Dialect}
    RegexObj.Expression := '^ *SET +SQL +DIALECT +([0-9]) *(\' + FTerminator + '|)';
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
    RegexObj.Expression := '^ *SET +([A-Z]+)( +[A-Z0-9]+|) *(\' + FTerminator + '|)';
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


procedure TIBXScript.SetParamValue(SQLVar: ISQLParam);
var BlobID: TISC_QUAD;
    ix: integer;
begin
  if (SQLVar.SQLType = SQL_BLOB) and (Pos(ibx_blob,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(ibx_blob)+1,length(SQLVar.Name)-length(ibx_blob)));
    SQLVar.AsBlob := FBlobData[ix].BlobIntf;
    Exit;
  end;

  if (SQLVar.SQLType = SQL_ARRAY) and (Pos(ibx_array,SQLVar.Name) = 1) then
  begin
    ix := StrToInt(system.copy(SQLVar.Name,length(ibx_array)+1,length(SQLVar.Name)-length(ibx_array)));
    SQLVar.AsArray := FArrayData[ix].ArrayIntf;
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

procedure TIBXScript.SetShowPerformanceStats(AValue: boolean);
begin
  if FShowPerformanceStats = AValue then Exit;
  FShowPerformanceStats := AValue;
  if assigned(DataOutputFormatter) then
    DataOutputFormatter.ShowPerformanceStats := AValue;
end;

procedure TIBXScript.SetState(AState: TSQLStates);
begin
  if FStackIndex > 16 then
    raise Exception.Create(sStackOverFlow);
  FStack[FStackIndex] := FState;
  Inc(FStackIndex);
  FState := AState
end;

procedure TIBXScript.StartXMLTag(xmltag: TXMLTag);
begin
  if FXMLTagIndex > 19 then
    raise Exception.Create(sXMLStackOverFlow);
  Inc(FXMLTagIndex);
  FXMLTagStack[FXMLTagIndex] := xmltag;
  case xmltag of
  xtBinary:
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
      Inc(FArrayData[FCurrentArray].CurrentRow);
  end;
end;

procedure TIBXScript.ProcessAttributeValue(attrValue: string);
begin
  case FXMLTagStack[FXMLTagIndex] of
  xtBinary:
    if FAttributeName = 'subtype' then
      FBlobData[FCurrentBlob].SubType := StrToInt(attrValue)
    else
      raise Exception.CreateFmt(sXMLAttributeError,[attrValue]);

  xtArray:
    if FAttributeName = 'sqltype' then
      FArrayData[FCurrentArray].SQLType := StrToInt(attrValue)
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
      raise Exception.CreateFmt(sXMLAttributeError,[attrValue]);

  xtElt:
    if FAttributeName = 'ix' then
      with FArrayData[FCurrentArray] do
        Index[CurrentRow] :=  StrToInt(attrValue)
     else
        raise Exception.CreateFmt(sXMLAttributeError,[attrValue]);
  end;
end;

procedure TIBXScript.ProcessBoundsList(boundsList: string);
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
        raise Exception.CreateFmt(sInvalidBoundsList,[boundsList]);
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

procedure TIBXScript.ClearStatement;
begin
  FSQLText := '';
  FState := stInit;
  FHasBegin := false;
  FXMLTag := xtNone;
  FXMLTagIndex := 0;
  SetLength(FBlobData,0);
  FCurrentBlob := -1;
  SetLength(FArrayData,0);
  FCurrentArray := -1;
end;

end.

