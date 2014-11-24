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
unit IBSQLParserUnit;

{$Mode Delphi}

interface

uses Classes;

type
  TSQLSymbol = (sqNone,sqSpace,sqSemiColon,sqSingleQuotes,sqDoubleQuotes,
                sqString,sqCommentStart,sqUnion,sqColon,
                sqCommentEnd,sqCommentLine,sqAsterisk,sqForwardSlash,
                sqSelect,sqFrom,sqWhere,sqGroup,sqOrder,sqBy,sqOpenBracket,
                sqCloseBracket,sqHaving,sqPlan,sqEOL,sqWith);

  TSQLStates =  (stInit, stError, stInSelect,stInFrom,stInWhere,stInGroupBy,
                 stInHaving,stInPlan, stNestedSelect,stInSingleQuotes, stInGroup,
                 stInDoubleQuotes, stInComment, stInCommentLine, stInOrder,
                 stNestedWhere,stNestedFrom,stInOrderBy,stDone,stUnion,
                 stInParam,stNestedGroupBy,stCTE,stInCTE);

  { TSelectSQLParser }

  TSelectSQLParser = class
  private
    FHavingClause: string;
    FOnSQLChanging: TNotifyEvent;
    FSelectClause: string;
    FGroupClause: string;
    FWhereClause: string;
    FOriginalWhereClause: string;
    FOrderByClause: string;
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
    FLiteral: string;
    FParamList: TStringList;
    procedure AddToSQL(const Word: string);
    function GetSQlText: string;
    function Check4ReservedWord(const Text: string): TSQLSymbol;
    procedure AnalyseLine(const Line: string);
    procedure AnalyseSQL(Lines: TStrings);
    function GetNextSymbol(C: char): TSQLSymbol;
    function GetSymbol(const Line: string; var index: integer): TSQLSymbol;
    function PopState: TSQLStates;
    procedure SetState(AState: TSQLStates);
    procedure SetSelectClause(const Value: string);
    procedure SetOrderByClause(const Value: string);
    procedure SetGroupClause(const Value: string);
    procedure SetFromClause(const Value: string);
  protected
    constructor Create(SQLText: TStrings; StartLine, StartIndex: integer); overload;
    procedure Changed;
  public
    constructor Create(SQLText: TStrings); overload;
    constructor Create(const SQLText: string); overload;
    destructor Destroy; override;
    procedure Add2WhereClause(const Condition: string; OrClause: boolean=false);
    procedure DropUnion;
    procedure ResetWhereClause;
    procedure Reset;
    property SelectClause: string read FSelectClause write SetSelectClause;
    property FromClause: string read FFromClause write SetFromClause;
    property GroupClause: string read FGroupClause write SetGroupClause;
    property HavingClause: string read FHavingClause write FHavingClause;
    property PlanClause: string read FPlanClause;
    property WhereClause: string read FWhereClause write FWhereClause;
    property OrderByClause: string read FOrderByClause write SetOrderByClause;
    property SQLText: string read GetSQLText;
    property Union: TSelectSQLParser read FUnion;
    property ParamList: TStringList read FParamList;
    property OnSQLChanging: TNotifyEvent read FOnSQLChanging write FOnSQLChanging;
  end;

  TFilterCallback = procedure(Parser: TSelectSQLParser; Key: integer) of object; 

implementation

uses Sysutils;

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
  end;
end;

procedure TSelectSQLParser.Add2WhereClause(const Condition: string; OrClause: boolean);
begin
  if WhereClause <> '' then
    if OrClause then
      FWhereClause := '(' + WhereClause + ') or (' + Condition + ')' 
    else
      FWhereClause := WhereClause + ' AND ' + Condition
  else
    FWhereClause := Condition;
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

        stCTE:
          SetState(stInCTE);

        end;
        AddToSQL('(')
      end;

    sqCloseBracket:
      if not (FState in [stInComment,stInCommentLine]) then
      begin
        if FState in [stNestedSelect,stNestedFrom,stNestedWhere,stNestedGroupBy,stInCTE] then
          FState := PopState;
        AddToSQL(')')
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
        end;
        AddToSQL(FString)
      end;

    sqEOL:
      begin
        case FState of
        stInCommentLine:
          FState := PopState;
        stInDoubleQuotes,
        stInSingleQuotes:
          raise Exception.Create(sNoEndToThis);
        end;
        AddToSQL(' ');
        Exit;
      end;

      sqSelect:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stNestedSelect,stCTE] then
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
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
          FState := stInGroup;

      sqWhere:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere,stNestedSelect] then
          AddToSql(FString)
        else
          FState := stInWhere;

      sqHaving:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
          FState := stInHaving;

      sqPlan:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
          FState := stInPlan;

      sqOrder:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
          FState := stInOrder;

      sqUnion:
        if FState in [stInSingleQuotes,stInDoubleQuotes,stNestedFrom,stNestedWhere] then
          AddToSql(FString)
        else
        begin
          FState := stUnion;
          Exit
        end;

      sqBy:
        case FState of
        stInGroup:
          FState := stInGroupBy;
        stInOrder:
          FState := stInOrderBy;
        stNestedFrom,stNestedWhere,
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
          AddtoSQL(FString);
        end

    else
      raise Exception.Create(sBadSymbol);
    end
  end
end;

procedure TSelectSQLParser.AnalyseSQL(Lines: TStrings);
var I: integer;
begin
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
              FUnion := TSelectSQLParser.Create(Lines,I+1,1)
            else
              raise Exception.Create(sIncomplete)
          else
            FUnion := TSelectSQLParser.Create(Lines,I,FIndex);
          Exit
        end;
      end;
      FIndex := 1;
  except on E: Exception do
    raise Exception.CreateFmt(sBadSQL,[Lines[I],E.Message])
  end;
  FOriginalWhereClause := WhereClause
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
      if CompareText(Text,'order') = 0 then
        Result := sqOrder
      else
      if CompareText(Text,'with') = 0 then
        Result := sqWith
end;

constructor TSelectSQLParser.Create(SQLText: TStrings);
begin
  Create(SQLText,0,1)
end;

constructor TSelectSQLParser.Create(const SQLText: string);
var Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := SQLText;
    Create(Lines)
  finally
    Lines.Free
  end
end;

constructor TSelectSQLParser.Create(SQLText: TStrings; StartLine,
  StartIndex: integer);
begin
  inherited Create;
  FParamList := TStringList.Create;
  FLastSymbol := sqNone;
  FState := stInit;
  FStartLine := StartLine;
  FIndex := StartIndex;
  AnalyseSQL(SQLText);
end;

procedure TSelectSQLParser.Changed;
begin
  if assigned(FOnSQLChanging) then
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

  if (Result = sqString)  and not (FState in [stInComment,stInCommentLine])then
    Result := Check4ReservedWord(FString);

  if (index > Length(Line)) then
    if Result = sqNone then
      Result := sqEOL
    else
    if (FLastSymbol = sqNone) and (Result <> sqEOL) then
      FLastSymbol := sqEOL;

end;

function TSelectSQLParser.GetSQlText: string;
var SQL: TStringList;
begin
  SQL := TStringList.Create;
  try
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
  if Union <> nil then Union.SelectClause := Value;
  FSelectClause := Value;
  Changed
end;

procedure TSelectSQLParser.SetFromClause(const Value: string);
begin
  if Union <> nil then
    Union.FromClause := Value
  else
  FFromClause := Value;
  Changed
end;

procedure TSelectSQLParser.SetGroupClause(const Value: string);
begin
  if Union <> nil then
    Union.GroupClause := Value
  else
  FGroupClause := Value;
  Changed
end;

procedure TSelectSQLParser.SetOrderByClause(const Value: string);
begin
  if Union <> nil then
    Union.OrderByClause := Value
  else
    FOrderByClause := Value;
  Changed
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

procedure TSelectSQLParser.ResetWhereClause;
begin
  FWhereClause := FOriginalWhereClause;
  Changed
end;

procedure TSelectSQLParser.Reset;
begin
  ResetWhereClause
end;

destructor TSelectSQLParser.Destroy;
begin
  DropUnion;
  if FParamList <> nil then FParamList.Free;
  inherited;
end;

end.


