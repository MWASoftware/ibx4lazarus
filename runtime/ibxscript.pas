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

interface

uses Classes, IBDatabase,  IBSQL, IBHeader;

type
  TSQLSymbol = (sqNone,sqSpace,sqSemiColon,sqSingleQuotes,sqDoubleQuotes,
                sqEnd,sqBegin,sqCommit,sqRollback,sqString,sqCommentStart,
                sqCommentEnd,sqCommentLine,sqAsterisk,sqForwardSlash,
                sqDeclare,sqEOL,sqTerminator, sqReconnect);

  TSQLStates =  (stInit, stError, stInSQL, stNested,stInSingleQuotes,
                 stInDoubleQuotes, stInComment, stInCommentLine,
                 stInDeclaration, stInCommit, stInReconnect);

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


  { TIBXScript }

  TIBXScript = class(TComponent)
  private
    FDatabase: TIBDatabase;
    FEcho: boolean;
    FOnErrorLog: TLogEvent;
    FOnProgressEvent: TOnProgressEvent;
    FOnSelectSQL: TOnSelectSQL;
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
    FStack: array [0..16] of TSQLStates;
    FStackindex: integer;
    FGetParamValue: TGetParamValue;
    FOnOutputLog: TLogEvent;
    FTerminator: char;
    FAutoDDL: boolean;
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
    procedure SetParamValue(SQLVar: TIBXSQLVAR);
    procedure SetState(AState: TSQLStates);
    function PopState: TSQLStates;
    function ProcessSetStatement(stmt: string): boolean;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function PerformUpdate(const SQLFile: string;  AutoDDL: boolean): boolean; overload;
    function PerformUpdate(const SQLStream: TStream;   AutoDDL: boolean): boolean; overload;
  published
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property Echo: boolean read FEcho write FEcho default true;  {Echo Input to Log}
    property Transaction: TIBTransaction read FTransaction write FTransaction;
    property StopOnFirstError: boolean read FStopOnFirstError write FStopOnFirstError default true;
    property GetParamValue: TGetParamValue read FGetParamValue write FGetParamValue; {resolve parameterized queries}
    property OnOutputLog: TLogEvent read FOnOutputLog write FOnOutputLog; {Log handler}
    property OnErrorLog: TLogEvent read FOnErrorLog write FOnErrorLog;
    property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
    property OnSelectSQL: TOnSelectSQL read FOnSelectSQL write FOnSelectSQL; {Handle Select SQL Statements}
  end;

implementation

uses Sysutils, IB, RegExpr;

resourcestring
  sFailed      = 'Update Failed - %s';

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
      raise Exception.Create('Entered Error State');
    Symbol := GetSymbol(Line,index);
    if not (Symbol in [sqSpace,sqEOL]) then
      NonSpace := true;
    case Symbol of
    sqSpace:
      if not (FState in [stInComment,stInCommentLine]) then
        AddToSQL(' ');

    sqTerminator:
      if not (FState in [stInComment,stInCommentLine]) then
        case FState of
        stInSQL:
          begin
            FState := stInit;
            ExecSQL
          end;

       stInCommit:
          begin
            DoCommit;
            FState := stInit
          end;

       stInReconnect:
         begin
           DoReconnect;
           FState := stInit
         end;

       stNested:
         AddToSQL(FTerminator);

       stInDeclaration:
         begin
           FState := PopState;
           AddToSQL(';');
         end;
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
        FState := PopState
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
        AddToSQL('END');
        case FState of
        stInSingleQuotes,
        stInDoubleQuotes:
          {Ignore};
        stNested:
          begin
            if FNested = 0 then
            begin
              PopState;
              FState := stInit;
              ExecSQL
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
        AddToSQL('BEGIN');
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

    sqDeclare:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        AddToSQL('DECLARE');
        if FState in [stInit,stInSQL] then
          SetState(stInDeclaration)
      end;

    sqCommit:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState = stInit then
          FState := stInCommit
        else
          raise Exception.Create('Commit not allowed here')
      end;

    sqReconnect:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState = stInit then
          FState := stInReconnect
        else
          raise Exception.Create('Reconnect not allowed here')
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
          FState := PopState;
        stInDoubleQuotes,
        stInSingleQuotes:
          raise Exception.Create('Unterminated string');
        end;
        if NonSpace then AddToSQL(#13#10);
        Exit;
      end;
    else
      raise Exception.CreateFmt('Unknown Symbol %d',[Symbol]);
    end
  end
end;

function TIBXScript.AnalyseSQL(Lines: TStringList): boolean;
var I: integer;
begin
  Result := true;
  FSQLText := '';
  FState := stInit;
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
        FSQLText := '';
        FState := stInit;
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
  FLastSymbol := sqNone;
end;

destructor TIBXScript.Destroy;
begin
  if FISQL <> nil then FISQL.Free;
  if FInternalTransaction <> nil then FInternalTransaction.Free;
  inherited;
end;

procedure TIBXScript.DoCommit;
begin
  with GetTransaction do
    if InTransaction then Commit;
  if not GetTransaction.InTransaction then
    GetTransaction.StartTransaction;
end;

procedure TIBXScript.DoReconnect;
begin
  with GetTransaction do
    if InTransaction then Commit;
  Database.Connected := false;
  Database.Connected := true;
  if not GetTransaction.InTransaction then
    GetTransaction.StartTransaction;
end;

procedure TIBXScript.ExecSQL;
var DDL: boolean;
    I: integer;
begin
 if FSQLText <> '' then
 begin
   if ProcessSetStatement(FSQLText) then {Handle Set Statement}
   begin
     FSQLText := '';
     Exit;
   end;

   FISQL.SQL.Text := FSQLText;
   FISQL.Transaction := GetTransaction;
   with FISQL.Transaction do
     if not InTransaction then StartTransaction;
   FISQL.ParamCheck := true;
   FISQL.Prepare;
   if FISQL.SQLType in [SQLInsert, SQLUpdate, SQLDelete] then
     for I := 0 to FISQL.Params.Count - 1 do
       SetParamValue(FISQL.Params[I]);

   if FISQL.SQLType = SQLSelect then
   begin
     if assigned(OnSelectSQL) then
       OnSelectSQL(self,FSQLText)
     else
       raise Exception.Create('Select SQL Statements are not supported');
   end
   else
   begin
     DDL := FISQL.SQLType = SQLDDL;
     if DDL then
     {must not interpret PSQL params}
     begin
       FISQL.FreeHandle;
       FISQL.ParamCheck := false;
     end;
     FISQL.ExecQuery;
     if FAutoDDL and DDL then
       FISQL.Transaction.Commit;
     FISQL.Close;
   end;
   FISQL.SQL.Clear;
   FSQLText := ''
 end
end;

function TIBXScript.GetNextSymbol(C: char): TSQLSymbol;
begin
    if C = FTerminator then
      Result := sqTerminator
    else
    case C of
    ' ',#9:
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
        FLastSymbol := sqNone
      end
      else
      if FLastSymbol = sqForwardSlash then
      begin
        Result := sqCommentLine;
        FLastSymbol := sqNone
      end;

    sqAsterisk:
      if FLastSymbol = sqForwardSlash then
      begin
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
        Result := sqReconnect;
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

function TIBXScript.PerformUpdate(const SQLFile: string;
                                     AutoDDL: boolean): boolean;
var F: TFileStream;
begin
  F := TFileStream.Create(SQLFile,fmOpenRead or fmShareDenyNone);
  try
    Result := PerformUpdate(F,AutoDDL)
  finally
    F.Free
  end;
end;

function TIBXScript.PerformUpdate(const SQLStream: TStream; AutoDDL: boolean): boolean;
var Lines: TStringList;
    FNotConnected: boolean;
begin
  FTerminator := ';';
  FAutoDDL := AutoDDL;
  FNotConnected := not Database.Connected;
  Database.Connected := true;
  Add2Log(DateTimeToStr(Now) + ' Update Started');
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
    if Result then
      Add2Log(DateTimeToStr(Now) + ' Update Completed')
  except on E:Exception do
    begin
      Add2Log(DateTimeToStr(Now) + ' ' + Format(sFailed,[E.Message]));
      with GetTransaction do
        if InTransaction then Rollback;
      Result := false
    end
  end;
  with GetTransaction do
    if InTransaction then Commit;
  if FNotConnected then
    Database.Connected := false;
end;

function TIBXScript.PopState: TSQLStates;
begin
  if FStackIndex = 0 then
    raise Exception.Create('Stack Underflow');
  Dec(FStackIndex);
  Result := FStack[FStackIndex]
end;

function TIBXScript.ProcessSetStatement(stmt: string): boolean;
var  RegexObj: TRegExpr;
begin
  Result := false;
  RegexObj := TRegExpr.Create;
  try
    {Process Set Term}
    RegexObj.Expression := 'SET +TERM +(.) *(\' + FTerminator + '|)';
    if RegexObj.Exec(AnsiUpperCase(stmt)) then
    begin
       FTerminator := RegexObj.Match[1][1];
       Result := true;
       Exit;
    end;

    {Process AutoDDL}
    RegexObj.Expression := 'SET +AUTODDL +([a-zA-Z]+) *(\' + FTerminator + '|)';
    if RegexObj.Exec(AnsiUpperCase(stmt)) then
    begin
      if  AnsiUpperCase(RegexObj.Match[1]) = 'ON' then
        FAutoDDL := true
      else
      if  AnsiUpperCase(RegexObj.Match[1]) = 'OFF' then
        FAutoDDL := false
      else
        raise Exception.CreateFmt('Invalid AUTODDL Statement - %s', [RegexObj.Match[0]]);

      Result := true;
    end;
  finally
    RegexObj.Free;
  end;

end;

procedure TIBXScript.SetParamValue(SQLVar: TIBXSQLVAR);
var BlobID: TISC_QUAD;
begin
  if assigned(FGetParamValue) and (SQLVar.SQLType = SQL_BLOB) then
  begin
    Add2Log('Resolving Query Parameter: ' + SQLVar.Name);
    GetParamValue(self,SQLVar.Name,BlobID);
    if (BlobID.gds_quad_high = 0) and (BlobID.gds_quad_low = 0) then
      SQLVar.Clear
    else
      SQLVar.AsQuad := BlobID
  end
  else
    raise Exception.Create('Parameterised Queries are not supported');
end;

procedure TIBXScript.SetState(AState: TSQLStates);
begin
  if FStackIndex > 16 then
    raise Exception.Create('Stack Overflow');
  FStack[FStackIndex] := FState;
  Inc(FStackIndex);
  FState := AState
end;

end.

