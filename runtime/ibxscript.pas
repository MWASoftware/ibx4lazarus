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

uses Classes, IBDatabase,  IBSQL, IBHeader, ComCtrls;

type
  TSQLSymbol = (sqNone,sqSpace,sqSemiColon,sqSingleQuotes,sqDoubleQuotes,
                sqEnd,sqBegin,sqCommit,sqRollback,sqString,sqCommentStart,
                sqCommentEnd,sqCommentLine,sqAsterisk,sqForwardSlash,
                sqDeclare,sqEOL,sqTerminator, sqReconnect);

  TSQLStates =  (stInit, stError, stInSQL, stNested,stInSingleQuotes,
                 stInDoubleQuotes, stInComment, stInCommentLine,
                 stInDeclaration, stInCommit, stInReconnect);

  TGetParamValue = procedure(Sender: TObject; Name: string; var BlobID: TISC_QUAD) of object;
  TLogEvent = procedure(Sender: TObject; Msg: string) of Object;

  {
  TIBXScript: runs an SQL script in the specified file or stream. The text is parsed
  into SQL statements which are executed in turn. The intentions is to be ISQL
  compatible but with extensions:

  * SET TERM and Set AutoDDL are both supported

  * New Command: RECONNECT. Performs a commit followed by disconnecting and
    reconnecting to the database.

  * Procedure Bodies (BEGIN .. END blocks) are self-terminating and do not need
    and extra terminator. If a terminator is present, this is treated as an
    empty statement. The result is ISQL compatible, but does not require the
    use of SET TERM.

  * DML statements may have arguments in IBX format (e.g UPDATE MYTABLE Set data = :mydata).
    Arguments are valid only for BLOB columns and are resolved using the GetParamValue
    event. This returns the blobid to be used. A typical use of the event is to
    read binary data from a file, save it in a blob stream and return the blob id.

  An update log is generated and written using the LogProc Event handler.
  }

  TProgressBarInterface = class
  public
    procedure Reset(Total: integer); virtual; abstract;
    procedure Increment; virtual; abstract;
  end;


  { TIBXScript }

  TIBXScript = class(TComponent)
  private
    FDatabase: TIBDatabase;
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
    FLogProc: TLogEvent;
    FTerminator: char;
    FAutoDDL: boolean;
    procedure Add2Log(const Msg: string);
    procedure AddToSQL(const Symbol: string);
    function AnalyseSQL(Lines: TStringList; ProgressBar: TProgressBarInterface): boolean;
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
    function ProcessSetStatement(Line: string): string;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function PerformUpdate(const SQLFile: string; ProgressBar: TProgressBarInterface; AutoDDL: boolean): boolean; overload;
    function PerformUpdate(const SQLStream: TStream; ProgressBar: TProgressBarInterface; AutoDDL: boolean): boolean; overload;
    function PerformUpdate(const SQLFile: string; ProgressBar: TProgressBar; AutoDDL: boolean): boolean; overload;
    function PerformUpdate(const SQLStream: TStream; ProgressBar: TProgressBar; AutoDDL: boolean): boolean; overload;
  published
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property Transaction: TIBTransaction read FTransaction write FTransaction;
    property GetParamValue: TGetParamValue read FGetParamValue write FGetParamValue;
    property LogProc: TLogEvent read FLogProc write FLogProc;
  end;

  { TThreadProgressBar }

  TThreadProgressBar = class(TProgressBarInterface)
  private
    FProgressBar: TProgressBar;
    FOwner: TThread;
    FTotal: integer;
    procedure InternalReset;
    procedure InternalIncrement;
  public
    constructor Create(Owner: TThread; ProgressBar: TProgressBar);
    procedure Reset(Total: integer); override;
    procedure Increment; override;
  end;

  { TInlineProgressBar }

  TInlineProgressBar = class(TProgressBarInterface)
  private
    FProgressBar: TProgressBar;
  public
    constructor Create(ProgressBar: TProgressBar);
    destructor Destroy; override;
    procedure Reset(Total: integer); override;
    procedure Increment; override;
  end;


implementation

uses Sysutils, IB, RegExpr, Forms;

resourcestring
  sFailed      = 'Update Failed - %s';

{ TIBXScript }

procedure TIBXScript.Add2Log(const Msg: string);
begin
  if assigned(FLogProc) then FLogProc(self,Msg)
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

function TIBXScript.AnalyseSQL(Lines: TStringList;
  ProgressBar: TProgressBarInterface): boolean;
var I: integer;
begin
  Result := true;
  FSQLText := '';
  FState := stInit;
  FLastSymbol := sqNone;
  for I := 0 to Lines.Count - 1 do
  begin
    Add2Log(Lines[I]);
    if ProgressBar <> nil then ProgressBar.Increment;
    try
      AnalyseLine(ProcessSetStatement(Lines[I]));
    except on E:Exception do
      begin
        Add2Log(E.Message);
        Result := false;
        Exit
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
  FState := stInit;
  FISQL := TIBSQL.Create(self);
  FISQL.ParamCheck := false;
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
   FISQL.SQL.Text := FSQLText;
   FISQL.Transaction := GetTransaction;
   with FISQL.Transaction do
     if not InTransaction then StartTransaction;
   try
     FISQL.Prepare;
   except
      on E:EIBInterBaseError do
          if E.IBErrorCode = 335544569 then //try again with ParamCheck true
          begin
            FISQL.ParamCheck := true;
            try
              FISQL.SQL.Text := '';
              FISQL.SQL.Text := FSQLText;
              FISQL.Prepare;
              for I := 0 to FISQL.Params.Count - 1 do
                SetParamValue(FISQL.Params[I]);
            finally
              FISQL.ParamCheck := false
            end
          end
   end;
   if FISQL.SQLType = SQLSelect then
     raise Exception.Create('Select SQL Statements are not supported');
   DDL := FISQL.SQLType = SQLDDL;
   FISQL.ExecQuery;
   if FAutoDDL and DDL then
     FISQL.Transaction.Commit;
   FISQL.Close;
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
        Result := sqReconnect
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
                                     ProgressBar: TProgressBarInterface;
                                     AutoDDL: boolean): boolean;
var F: TFileStream;
begin
  F := TFileStream.Create(SQLFile,fmOpenRead or fmShareDenyNone);
  try
    Result := PerformUpdate(F,ProgressBar,AutoDDL)
  finally
    F.Free
  end;
end;

function TIBXScript.PerformUpdate(const SQLStream: TStream;
  ProgressBar: TProgressBarInterface; AutoDDL: boolean): boolean;
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
      if assigned(ProgressBar) then
        ProgressBar.Reset(Lines.Count);

      Result := AnalyseSQL(Lines,ProgressBar)
    finally
      Lines.Free
    end;
    if Result then
      Add2Log(DateTimeToStr(Now) + ' Update Completed')
  except on E:Exception do
    begin
      Add2Log(DateTimeToStr(Now) + Format(sFailed,[E.Message]));
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

function TIBXScript.PerformUpdate(const SQLFile: string;
  ProgressBar: TProgressBar; AutoDDL: boolean): boolean;
var F: TFileStream;
begin
  F := TFileStream.Create(SQLFile,fmOpenRead or fmShareDenyNone);
  try
    Result := PerformUpdate(F,ProgressBar,AutoDDL)
  finally
    F.Free
  end;
end;

function TIBXScript.PerformUpdate(const SQLStream: TStream;
  ProgressBar: TProgressBar; AutoDDL: boolean): boolean;
var PB: TInlineProgressBar;
begin
  PB := TInlineProgressBar.Create(ProgressBar);
  try
    Result := PerformUpdate(SQLStream,PB,AutoDDL);
  finally
    PB.Free;
  end;
end;

function TIBXScript.PopState: TSQLStates;
begin
  if FStackIndex = 0 then
    raise Exception.Create('Stack Underflow');
  Dec(FStackIndex);
  Result := FStack[FStackIndex]
end;

function TIBXScript.ProcessSetStatement(Line: string): string;
var  RegexObj: TRegExpr;
begin
  Result := Line;
  RegexObj := TRegExpr.Create;
  try
    {Process Set Term}
    RegexObj.Expression := 'SET +TERM +(.) *\' + FTerminator;
    if RegexObj.Exec(AnsiUpperCase(Result)) then
    begin
       FTerminator := RegexObj.Match[1][1];
       system.Delete(Result,RegexObj.MatchPos[0], RegexObj.MatchLen[0]);
    end;

    {Process AutoDDL}
    RegexObj.Expression := 'SET +AUTODDL +(.) *\' + FTerminator;
    if RegexObj.Exec(AnsiUpperCase(Result)) then
    begin
      if  AnsiUpperCase(RegexObj.Match[1]) = 'ON' then
        FAutoDDL := true
      else
      if  AnsiUpperCase(RegexObj.Match[1]) = 'OFF' then
        FAutoDDL := false
      else
        raise Exception.CreateFmt('Invalid AUTODDL Statement - %s', [RegexObj.Match[0]]);

      system.Delete(Result,RegexObj.MatchPos[0], RegexObj.MatchLen[0]);
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
    GetParamValue(self,SQLVar.Name,BlobID);
    if (BlobID.gds_quad_high = 0) and (BlobID.gds_quad_low = 0) then
      SQLVar.Clear
    else
      SQLVar.AsQuad := BlobID
  end
end;

procedure TIBXScript.SetState(AState: TSQLStates);
begin
  if FStackIndex > 16 then
    raise Exception.Create('Stack Overflow');
  FStack[FStackIndex] := FState;
  Inc(FStackIndex);
  FState := AState
end;

{ TThreadProgressBar }

procedure TThreadProgressBar.InternalReset;
begin
  FProgressBar.Position := 0;
  FProgressBar.Max := FTotal
end;

procedure TThreadProgressBar.InternalIncrement;
begin
  FProgressBar.Stepit
end;

constructor TThreadProgressBar.Create(Owner: TThread; ProgressBar: TProgressBar);
begin
  inherited Create;
  FOwner := Owner;
  FProgressBar := ProgressBar
end;

procedure TThreadProgressBar.Reset(Total: integer);
begin
  FTotal := Total;
  TThread.Synchronize(FOwner,@InternalReset);
end;

procedure TThreadProgressBar.Increment;
begin
  TThread.Synchronize(FOwner,@InternalIncrement)
end;

{ TInlineProgressBar }

constructor TInlineProgressBar.Create(ProgressBar: TProgressBar);
begin
  inherited Create;
  FProgressBar := ProgressBar
end;

destructor TInlineProgressBar.Destroy;
begin
  Reset(100);
  inherited Destroy;
end;

procedure TInlineProgressBar.Reset(Total: integer);
begin
  FProgressBar.Position := 0;
  FProgressBar.Max := Total;
end;

procedure TInlineProgressBar.Increment;
begin
  FProgressBar.Stepit;
  Application.ProcessMessages;
end;


end.

