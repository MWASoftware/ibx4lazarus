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
unit IBSQLParser;

{$Mode Delphi}

{$codepage UTF8}

interface

uses Classes, DB;

{
  The SQL Parser is a partial SQL Parser intended to parser a Firebird DML (Select)
  statement with the intention of being able to modify the "Where", Having" and
  "Order By" clauses. It is not an SQL validator as while invalid SQL may be detected,
  there are many cases of non-compliant SQL that will still be parsed successfully.

  In use, when a TSelectSQLParser object is created, it is passed a Select SQL Statement
  that is then parsed into its constituent clauses. CTEs are brought out separately, as is
  each union. The main clauses are made visible as text properties. Some, such as the
  order by clause can be replaced fully. The Where and Having Clauses are manipulated by
  the Add2WhereClause and the Add2HavingClause.

  Add2WhereClause allows an SQL Condition statement (e.g. A = 1) to be appended to the
  current WhereClause, ANDing or ORing in the new condition. Normally, Add2WhereClause
  only manipulates the first Select in a UNION, and conditions must be applied separately
  to each member of the union. However, Add2WhereClause can also apply the same SQL
  condition to each member of the UNION.

  Add2HavingClause behaves identically, except that it applies to the Having Clause.

  TSelectSQLParser.Reset will return the Where, Having and OrderBy Clauses of all members
  of the Union to their initial state. ResetWhereClause, ResetHavingClause and
  ResetOrderByClause allow each clause to be individually reset to their initial
  state.

  The property SQLText is used to access the current Select SQL statement including
  CTEs and UNIONs and modified clauses.

}

type
  TSQLSymbol = (sqNone,sqSpace,sqSemiColon,sqSingleQuotes,sqDoubleQuotes, sqComma,
                sqString,sqCommentStart,sqUnion,sqAll,sqColon,
                sqCommentEnd,sqCommentLine,sqAsterisk,sqForwardSlash,
                sqSelect,sqFrom,sqWhere,sqGroup,sqOrder,sqBy,sqOpenBracket,
                sqCloseBracket,sqHaving,sqPlan,sqEOL,sqWith,sqRecursive,sqAs);

  TSQLStates =  (stInit, stError, stInSelect,stInFrom,stInWhere,stInGroupBy,
                 stInHaving,stInPlan, stNestedSelect,stInSingleQuotes, stInGroup,
                 stInDoubleQuotes, stInComment, stInCommentLine, stInOrder,
                 stNestedWhere,stNestedFrom,stInOrderBy,stDone,stUnion,
                 stInParam,stNestedGroupBy,stCTE,stCTE1,stCTE2,stCTE3, stCTEquoted,
                 stInCTE,stCTEClosed);

  PCTEDef = ^TCTEDef;
  TCTEDef = record
    Recursive: boolean;
    Name: string;
    Text: string;
  end;

  { TSelectSQLParser }

  TSelectSQLParser = class
  private
    FDataSet: TDataSet;
    FHavingClause: string;
    FOriginalHavingClause: string;
    FOnSQLChanging: TNotifyEvent;
    FSelectClause: string;
    FGroupClause: string;
    FUnionAll: boolean;
    FWhereClause: string;
    FOriginalWhereClause: string;
    FOrderByClause: string;
    FOriginalOrderByClause: string;
    FPlanClause: string;
    FFromClause: string;
    FState: TSQLStates;
    FString: string;
    FLastSymbol: TSQLSymbol;
    FLastChar: char;
    FStack: array [0..16] of TSQLStates;
    FStackindex: integer;
    FIndex: integer;
    FStartLine: integer;
    FUnion: TSelectSQLParser;
    FAllowUnionAll: boolean;
    FLiteral: string;
    FParamList: TStringList;
    FCTEs: TList;
    FCTE: TCTEDef;
    FNested: integer;
    FDestroying: boolean;
    FOwner: TSelectSQLParser;
    procedure AddToSQL(const Word: string);
    procedure CTEClear;
    function GetCTE(Index: integer): PCTEDef;
    function GetCTECount: integer;
    function GetSQlText: string;
    function Check4ReservedWord(const Text: string): TSQLSymbol;
    procedure AnalyseLine(const Line: string);
    procedure AnalyseSQL(Lines: TStrings);
    procedure InitCTE;
    procedure AddCTE;
    function GetNextSymbol(C: char): TSQLSymbol;
    function GetSymbol(const Line: string; var index: integer): TSQLSymbol;
    function PopState: TSQLStates;
    procedure SetState(AState: TSQLStates);
    procedure SetSelectClause(const Value: string);
    procedure SetOrderByClause(const Value: string);
    procedure SetGroupClause(const Value: string);
    procedure SetFromClause(const Value: string);
  protected
    constructor Create(aOwner: TSelectSQLParser; SQLText: TStrings; StartLine, StartIndex: integer); overload;
    procedure Changed;
  public
    constructor Create(aDataSet: TDataSet; SQLText: TStrings); overload;
    constructor Create(aDataSet: TDataSet; const SQLText: string); overload;
    destructor Destroy; override;
    procedure Add2WhereClause(const Condition: string; OrClause: boolean=false;
      IncludeUnions: boolean = false);
    procedure Add2HavingClause(const Condition: string; OrClause: boolean=false;
      IncludeUnions: boolean = false);
    procedure DropUnion;
    function GetFieldPosition(AliasName: string): integer;
    procedure ResetWhereClause;
    procedure ResetHavingClause;
    procedure ResetOrderByClause;
    procedure Reset;
    property CTEs[Index: integer]: PCTEDef read GetCTE;
    property CTECount: integer read GetCTECount;
    property DataSet: TDataSet read FDataSet;
    property SelectClause: string read FSelectClause write SetSelectClause;
    property FromClause: string read FFromClause write SetFromClause;
    property GroupClause: string read FGroupClause write SetGroupClause;
    property HavingClause: string read FHavingClause write FHavingClause;
    property PlanClause: string read FPlanClause;
    property WhereClause: string read FWhereClause write FWhereClause;
    property OrderByClause: string read FOrderByClause write SetOrderByClause;
    property SQLText: string read GetSQLText;
    property Union: TSelectSQLParser read FUnion;
    property UnionAll: boolean read FUnionAll write FUnionAll;
             {When true this is joined by "Union All" to the parent Select}
    property ParamList: TStringList read FParamList;
    property OnSQLChanging: TNotifyEvent read FOnSQLChanging write FOnSQLChanging;
  end;

  TFilterCallback = procedure(Parser: TSelectSQLParser; Key: integer) of object; 

implementation

uses Sysutils, IBCustomDataSet;

resourcestring
  sNoEndToThis    = 'Unterminated string';
  sBadBy          = 'Unexpected symbol "BY" in: %s';
  sBadSymbol      = 'Unknown Symbol';
  sIncomplete     = 'Incomplete Union';
  sBadSQL         = 'Error processing SQL "%s" - %s';
  sStackUnderFlow = 'Stack Underflow';
  sStackOverFlow  = 'Stack Overflow';
  sBadParameter   = 'Bad SQL Parameter';

{ TSelectSQLParser }

procedure TSelectSQLParser.AddToSQL(const Word: string);
begin
  case FState of
  stNestedSelect,
  stInSelect:
    FSelectClause := FSelectClause + Word;
  stNestedFrom,
  stInFrom:
    FFromClause := FFromClause + Word;
  stNestedWhere,
  stInWhere:
    FWhereClause := FWhereClause + Word;
  stNestedGroupBy,
  stInGroupBy:
    FGroupClause := FGroupClause + Word;
  stInHaving:
    FHavingClause := FHavingClause + Word;
  stInPlan:
    FPlanClause := FPlanClause + Word;
  stInOrderBy:
    FOrderByClause := FOrderByClause + Word;
  stInDoubleQuotes,
  stInSingleQuotes:
    FLiteral := FLiteral + Word;
  stInCTE:
    FCTE.Text := FCTE.Text + Word;
  stCTE2:
    FCTE.Name := Trim(FCTE.Name + Word);
  end;
end;

procedure TSelectSQLParser.CTEClear;
var i: integer;
begin
  for i := 0 to FCTEs.Count - 1 do
    dispose(PCTEDef(FCTEs[i]));
  FCTEs.Clear;
end;

function TSelectSQLParser.GetCTE(Index: integer): PCTEDef;
begin
  if (Index < 0) or (index >= FCTEs.Count) then
     raise Exception.Create('CTE Index out of bounds');

  Result := FCTEs[Index]
end;

function TSelectSQLParser.GetCTECount: integer;
begin
  Result := FCTEs.Count;
end;

procedure TSelectSQLParser.Add2WhereClause(const Condition: string;
  OrClause: boolean; IncludeUnions: boolean);
begin
  if WhereClause <> '' then
    if OrClause then
      FWhereClause := '(' + WhereClause + ') OR (' + Condition + ')'
    else
      FWhereClause := '(' + WhereClause + ') AND (' + Condition + ')'
  else
    FWhereClause := Condition;
  if IncludeUnions and (Union <> nil) then
    Union.Add2WhereClause(Condition,OrClause,IncludeUnions);
  Changed;
end;

procedure TSelectSQLParser.Add2HavingClause(const Condition: string;
  OrClause: boolean; IncludeUnions: boolean);
begin
  if HavingClause <> '' then
    if OrClause then
      FHavingClause := '(' + HavingClause + ') OR (' + Condition + ')'
    else
      FHavingClause := '(' + HavingClause + ') AND (' + Condition + ')'
  else
    FHavingClause := Condition;
  if IncludeUnions and (Union <> nil) then
    Union.Add2HavingClause(Condition,OrClause,IncludeUnions);
  Changed;
end;

procedure TSelectSQLParser.AnalyseLine(const Line: string);
var Symbol: TSQLSymbol;
begin
  while true do
  begin
    if FState = stError then
      raise Exception.Create('Entered Error State');
    Symbol := GetSymbol(Line,FIndex);
    if (FState = stInParam) and (Symbol <> sqString) then
      raise Exception.Create(sBadParameter);

    case Symbol of
    sqSpace:
      if not (FState in [stInComment,stInCommentLine]) then
        AddToSQL(' ');

    sqColon:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        AddToSQL(':');
        if not (FState in [stInSingleQuotes,stInDoubleQuotes]) then
          SetState(stInParam);
      end;

    sqSemiColon:
      if not (FState in [stInComment,stInCommentLine]) then
        case FState of
        stInWhere,stInGroupBy,
        stInHaving,stInPlan,stInFrom:
          begin
            FState := stDone;
            Exit
          end;

        stInSingleQuotes, stInDoubleQuotes:
          AddToSQL(';');

         else
          raise Exception.Create('Unexpected ";"')
        end;

    sqAsterisk:
      if not (FState in [stInComment,stInCommentLine]) then
       AddToSQL('*');

    sqForwardSlash:
      if not (FState in [stInComment,stInCommentLine]) then
       AddToSQL('/');

    sqOpenBracket:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FNested = 0 then
        case FState of
        stInSelect,
        stNestedSelect:
          SetState(stNestedSelect);

        stInFrom,
        stNestedFrom:
          SetState(stNestedFrom);

        stInWhere,
        stNestedWhere:
          SetState(stNestedWhere);

        stInGroupBy,
        stNestedGroupBy:
          SetState(stNestedGroupBy);

        stCTE3:
          begin
            FState := stCTEClosed;
            SetState(stInCTE);
          end;
        end;
        if (FNested > 0 ) or (FState <> stInCTE) then
          AddToSQL('(');
        Inc(FNested);
      end;

    sqCloseBracket:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        Dec(FNested);
        if (FNested > 0) or (FState <> stInCTE) then
          AddToSQL(')');
        if FNested = 0 then
        begin
          if FState = stInCTE then
             FState := PopState
          else
          if FState in [stNestedSelect,stNestedFrom,stNestedWhere,stNestedGroupBy] then
            FState := PopState;
        end;
        if FState = stCTEClosed then
          AddCTE;
      end;

    sqComma:
      if FState = stCTEClosed then
         FState := stCTE
      else
        AddToSQL(',');

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
          begin
            FState := PopState;
            AddToSQL(FLiteral)
          end;
        stInDoubleQuotes:
          {Ignore};
        else
         begin
          FLiteral := '';
          SetState(stInSingleQuotes)
         end
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
          begin
            FState := PopState;
            AddToSQL(FLiteral)
          end;
        else
         begin
          FLiteral := '';
          SetState(stInDoubleQuotes)
         end
        end;
        AddToSQL('"')
      end;

    sqString:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState = stInParam then
        begin
          FState := PopState;
          ParamList.Add(FString)
        end
        else
        if FState in [stCTE, stCTE1] then
          FState := stCTE2;
        AddToSQL(FString)
      end;

    sqEOL:
      begin
        case FState of
        stInCommentLine:
          FState := PopState;
        stInDoubleQuotes,
        stInSingleQuotes:
          Begin
            FLiteral := FLiteral + #$0A;
            Exit;
          End;
        end;
        AddToSQL(' ');
        Exit;
      end;

      sqSelect:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stNestedSelect,stInCTE] then
          AddToSql(FString)
        else
          FState := stInSelect;

      sqFrom:
        if FState = stInSelect then
          FState := stInFrom
        else
          AddToSql(FString);
{        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,
          stNestedGroupBy,stNestedSelect] then
          AddToSql(FString)
        else
          FState := stInFrom;}

      sqGroup:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stInCTE] then
          AddToSql(FString)
        else
          FState := stInGroup;

      sqWhere:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stNestedSelect,stInCTE] then
          AddToSql(FString)
        else
          FState := stInWhere;

      sqHaving:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stInCTE] then
          AddToSql(FString)
        else
          FState := stInHaving;

      sqPlan:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stInCTE] then
          AddToSql(FString)
        else
          FState := stInPlan;

      sqOrder:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
          FState := stInOrder;

      sqUnion:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stInCTE] then
          AddToSql(FString)
        else
        begin
          FState := stUnion;
          Exit
        end;

      sqAll:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stInCTE] then
          AddToSql(FString)
        else
        if (FState = stInit) and FAllowUnionAll and not FUnionAll then
           FUnionAll := true
        else
         raise Exception.Create('Unexpected symbol "all"');

      sqBy:
        case FState of
        stInGroup:
          FState := stInGroupBy;
        stInOrder:
          FState := stInOrderBy;
        stNestedFrom,stNestedWhere,stInCTE,
        stInSingleQuotes,
        stInDoubleQuotes:
          AddToSql(FString);
        else
          raise Exception.CreateFmt(sBadBy,[Line])
        end;

      sqWith:
        if FState = stInit then
        begin
          FState := stCTE;
          InitCTE;
        end
        else
          raise Exception.Create('Unexpected symbol "with"');

      sqRecursive:
        if FState = stCTE then
        begin
          FCTE.Recursive := true;
          FState := stCTE1
        end
      else
        raise Exception.Create('Unexpected symbol "recursive"');

      sqAs:
      if FState = stCTE2 then
        FState := stCTE3
      else
        AddToSQL('as');

    else
      raise Exception.Create(sBadSymbol);
    end
  end
end;

procedure TSelectSQLParser.AnalyseSQL(Lines: TStrings);
var I: integer;
begin
 try
  for I := FStartLine to Lines.Count - 1 do
  try
      AnalyseLine(Lines[I]);
      case FState of
      stDone:
        break;
      stUnion:
        begin
          if FIndex > length(Lines[I]) then
            if I+1 < Lines.Count then
              FUnion := TSelectSQLParser.Create(self,Lines,I+1,1)
            else
              raise Exception.Create(sIncomplete)
          else
            FUnion := TSelectSQLParser.Create(self,Lines,I,FIndex);
          Exit
        end;
      end;
      FIndex := 1;
  except on E: Exception do
    raise Exception.CreateFmt(sBadSQL,[Lines[I],E.Message])
  end;
 finally
  FOriginalWhereClause := WhereClause;
  FOriginalHavingClause := HavingClause;
  FOriginalOrderByClause := OrderByClause
 end;
end;

procedure TSelectSQLParser.InitCTE;
begin
  with FCTE do
  begin
    Recursive := false;
    Name := '';
    Text := '';
  end;
end;

procedure TSelectSQLParser.AddCTE;
var cte: PCTEDef;
begin
  new(cte);
  cte^.Name := FCTE.Name;
  cte^.Recursive := FCTE.Recursive;
  cte^.Text := FCTE.Text;
  FCTEs.add(cte);
  InitCTE;
end;

function TSelectSQLParser.Check4ReservedWord(const Text: string): TSQLSymbol;
begin
      Result := sqString;
      if CompareText(Text,'select') = 0 then
        Result := sqSelect
      else
      if CompareText(Text,'from') = 0 then
        Result := sqFrom
      else
      if CompareText(Text,'where') = 0 then
        Result := sqWhere
      else
      if CompareText(Text,'group') = 0 then
        Result := sqGroup
      else
      if CompareText(Text,'by') = 0 then
        Result := sqBy
      else
      if CompareText(Text,'having') = 0 then
        Result := sqHaving
      else
      if CompareText(Text,'plan') = 0 then
        Result := sqPlan
      else
      if CompareText(Text,'union') = 0 then
        Result := sqUnion
      else
      if CompareText(Text,'all') = 0 then
        Result := sqAll
      else
      if CompareText(Text,'order') = 0 then
        Result := sqOrder
      else
      if CompareText(Text,'with') = 0 then
        Result := sqWith
      else
      if CompareText(Text,'recursive') = 0 then
        Result := sqRecursive
      else
      if CompareText(Text,'as') = 0 then
        Result := sqAs
end;

constructor TSelectSQLParser.Create(aDataSet: TDataSet; SQLText: TStrings);
begin
  FDataSet := aDataSet;
  Create(nil,SQLText,0,1)
end;

constructor TSelectSQLParser.Create(aDataSet: TDataSet; const SQLText: string);
var Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := SQLText;
    Create(aDataSet,Lines)
  finally
    Lines.Free
  end
end;

constructor TSelectSQLParser.Create(aOwner: TSelectSQLParser;
  SQLText: TStrings; StartLine, StartIndex: integer);
begin
  inherited Create;
  FOwner := aOwner;
  FParamList := TStringList.Create;
  FCTEs := TList.Create;
  FLastSymbol := sqNone;
  FState := stInit;
  FStartLine := StartLine;
  FIndex := StartIndex;
  FAllowUnionAll := true;
  AnalyseSQL(SQLText);
end;

procedure TSelectSQLParser.Changed;
begin
  if FOwner <> nil then
    FOwner.Changed
  else
  if assigned(FOnSQLChanging) and not FDestroying then
     OnSQLChanging(self)
end;

function TSelectSQLParser.GetNextSymbol(C: char): TSQLSymbol;
begin
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
    '(':
      Result := sqOpenBracket;
    ')':
      Result := sqCloseBracket;
    ':':
      Result := sqColon;
    ',':
      Result := sqComma;
    else
      begin
        Result := sqString;
        FLastChar := C
      end
    end
end;

function TSelectSQLParser.GetSymbol(const Line: string; var index: integer): TSQLSymbol;
begin
  Result := FLastSymbol;
  if Result = sqString then
      FString := FLastChar;
  FLastSymbol := sqNone;

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

    sqSpace:
      if FLastSymbol = sqSpace then
        FLastSymbol := sqNone;

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

  if (Result = sqString)  and not (FState in [stInComment,stInCommentLine, stInSingleQuotes,stInDoubleQuotes])then
    Result := Check4ReservedWord(FString);

  if (index > Length(Line)) then
  begin
    if  (Result = sqNone) then
      Result := sqEOL
    else
    if (FLastSymbol = sqNone) and (Result <> sqEOL) then
      FLastSymbol := sqEOL;
  end;

end;

function TSelectSQLParser.GetSQlText: string;
var SQL: TStringList;
    I: integer;
begin
  SQL := TStringList.Create;
  try
    for I := 0 to CTECount - 1 do
    begin
      if I = 0 then
      begin
        if CTEs[I]^.Recursive then
          SQL.Add('WITH RECURSIVE ' + CTEs[I]^.Name + ' AS (' + CTES[I]^.Text + ')')
        else
          SQL.Add('WITH ' + CTEs[I]^.Name + ' AS (' + CTES[I]^.Text +')')
      end
      else
      begin
        SQL.Add(',');
        SQL.Add(CTEs[I]^.Name + ' AS (' + CTES[I]^.Text +')')
      end
    end;
    if CTECount > 0 then
      SQL.Add('');
    SQL.Add('SELECT ' + SelectClause + #13#10' FROM ' + FromClause);
    if WhereClause <> '' then
      SQL.Add('Where ' + WhereClause);
    if GroupClause <> '' then
      SQL.Add('GROUP BY ' + GroupClause);
    if HavingClause <> '' then
      SQL.Add('HAVING ' + HavingClause);
    if PlanClause <> '' then
      SQL.Add('PLAN ' + PlanClause);
    if OrderByClause <> '' then
      SQL.Add('ORDER BY ' + OrderByClause);
    if Union <> nil then
    begin
      if Union.UnionAll then
         SQL.Add('UNION ALL')
      else
        SQL.Add('UNION');
      SQL.Add(Union.SQLText)
    end;
    Result := SQL.Text
  finally
    SQL.Free
  end
end;

function TSelectSQLParser.PopState: TSQLStates;
begin
  if FStackIndex = 0 then
    raise Exception.Create(sStackUnderFlow);
  Dec(FStackIndex);
  Result := FStack[FStackIndex]
end;

procedure TSelectSQLParser.SetState(AState: TSQLStates);
begin
  if FStackIndex > 16 then
    raise Exception.Create(sStackOverFlow);
  FStack[FStackIndex] := FState;
  Inc(FStackIndex);
  FState := AState
end;

procedure TSelectSQLParser.SetSelectClause(const Value: string);
begin
  if FSelectClause <> Value then
  begin
    FSelectClause := Value;
    Changed
  end;
end;

procedure TSelectSQLParser.SetFromClause(const Value: string);
begin
  if FFromClause <> Value then
  begin
    FFromClause := Value;
    Changed
  end;
end;

procedure TSelectSQLParser.SetGroupClause(const Value: string);
begin
  if FGroupClause <> Value then
  begin
    FGroupClause := Value;
    Changed
  end;
end;

procedure TSelectSQLParser.SetOrderByClause(const Value: string);
begin
  if Union <> nil then
    Union.OrderByClause := Value
  else
  if FOrderByClause <> Value then
  begin
    FOrderByClause := Value;
    Changed
  end;
end;

procedure TSelectSQLParser.DropUnion;
begin
  if FUnion <> nil then
  begin
    FUnion.Free;
    FUnion := nil;
    Changed
  end
end;

function TSelectSQLParser.GetFieldPosition(AliasName: string): integer;
begin
  if assigned(FDataSet) and (FDataSet is TIBCustomDataset) then
    Result := TIBCustomDataset(FDataSet).GetFieldPosition(AliasName)
  else
    Result := 0;
end;

procedure TSelectSQLParser.ResetWhereClause;
begin
  FWhereClause := FOriginalWhereClause;
  if Union <> nil then
     Union.ResetWhereClause;
  Changed
end;

procedure TSelectSQLParser.ResetHavingClause;
begin
  FHavingClause := FOriginalHavingClause;
  if Union <> nil then
     Union.ResetHavingClause;
  Changed
end;

procedure TSelectSQLParser.ResetOrderByClause;
begin
  FOrderbyClause := FOriginalOrderByClause;
  if Union <> nil then
     Union.ResetOrderByClause;
  Changed
end;

procedure TSelectSQLParser.Reset;
begin
  ResetWhereClause;
  ResetHavingClause;
  ResetOrderByClause
end;

destructor TSelectSQLParser.Destroy;
begin
  FDestroying := true;
  DropUnion;
  if FParamList <> nil then FParamList.Free;
  if FCTEs <> nil then
  begin
    CTEClear;
    FCTEs.Free;
  end;
  inherited;
end;

end.


