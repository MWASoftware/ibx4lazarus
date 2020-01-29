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

uses Classes, DB, IBUtils;

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

  { TSelectSQLTokeniser }

  {The select SQL tokeniser returns each successive clause. The token returned
   identifies the reserved word that starts the clause. e.g. sqltFrom and the tokentext
   is the remainder of the clause.}

  TSelectSQLTokeniser = class(TSQLwithNamedParamsTokeniser)
  private
    type
      TSQLState = (stDefault, stWith, stInCTE, stInRecursiveCTE, stCTEAs, stInNextCTE,
                   stInSelect, stInFrom,stInWhere,stInGroupBy,
                   stInHaving,stInPlan, stInUnion, stInUnionAll, stUnionEnd,
                   stInOrderBy, stInRows, stNotASelectStmt);

      TSQLStates = set of TSQLState;
  private
    FSQLState: TSQLState;
    FNested: integer;
    FNextToken: TSQLTokens;
    FPrevCTEToken: TSQLTokens;
    FClause: string;
    FParamList: TStrings;
    FHasText: boolean;
    FUnionMember: boolean;
    function GetNotaSelectStmt: boolean;
  protected
    FCTEName: string;
    procedure Assign(source: TSQLTokeniser); override;
    function TokenFound(var token: TSQLTokens): boolean; override;
    procedure Reset; override;
  public
    constructor Create(UnionMember: boolean);
    destructor Destroy; override;
    procedure Clear; virtual;
    property ParamList: TStrings read FParamList;
    property NotaSelectStmt: boolean read GetNotaSelectStmt;
  end;

  { TSelectSQLParser }

  TSelectSQLParser = class(TSelectSQLTokeniser)
  public
    type
      PCTEDef = ^TCTEDef;
      TCTEDef = record
        Recursive: boolean;
        Name: string;
        Text: string;
      end;

  private
    FDataSet: TDataSet;
    FOwner: TSelectSQLParser;
    FOnSQLChanging: TNotifyEvent;
    FDestroying: boolean;

    {Properties set after analysis}
    FSelectClause: string;
    FFromClause: string;
    FWhereClause: string;
    FGroupClause: string;
    FHavingClause: string;
    FPlanClause: string;
    FUnionAll: boolean;
    FOrderByClause: string;
    FRowsClause: string;
    FUnion: TSelectSQLParser;
    FCTEs: array of PCTEDef;

    {Saved values}
    FOriginalWhereClause: string;
    FOriginalOrderByClause: string;
    FOriginalHavingClause: string;

    {Input buffer}
    FInString: string;
    FIndex: integer;

    procedure AnalyseSQL;
    function GetCTE(Index: integer): PCTEDef;
    function GetCTECount: integer;
    function AddCTE(aName: string; Recursive: boolean; text: string): PCTEDef;
    procedure FlushCTEs;
    function GetSQLText: string;
    procedure SetSelectClause(const Value: string);
    procedure SetOrderByClause(const Value: string);
    procedure SetGroupClause(const Value: string);
    procedure SetFromClause(const Value: string);
  protected
    constructor Create(aOwner: TSelectSQLParser); overload;
    procedure Assign(source: TSQLTokeniser); override;
    procedure Changed;
    function GetChar: char; override;
  public
    constructor Create(aDataSet: TDataSet; SQLText: TStrings); overload;
    constructor Create(aDataSet: TDataSet; const SQLText: string); overload;
    destructor Destroy; override;
    procedure Add2WhereClause(const Condition: string; OrClause: boolean=false;
      IncludeUnions: boolean = false);
    procedure Add2HavingClause(const Condition: string; OrClause: boolean=false;
      IncludeUnions: boolean = false);
    procedure Clear; override;
    procedure DropUnion;
    function GetFieldPosition(AliasName: string): integer;
    procedure ResetWhereClause;
    procedure ResetHavingClause;
    procedure ResetOrderByClause;
    procedure RestoreClauseValues;
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
    property RowsClause: string read FRowsClause;
    property SQLText: string read GetSQLText;
    property Union: TSelectSQLParser read FUnion;
    property UnionAll: boolean read FUnionAll write FUnionAll;
             {When true this is joined by "Union All" to the parent Select}
    property OnSQLChanging: TNotifyEvent read FOnSQLChanging write FOnSQLChanging;
  end;

  TFilterCallback = procedure(Parser: TSelectSQLParser; Key: integer) of object;

implementation

uses Sysutils, IBCustomDataSet, IB, IBMessages;

{ TSelectSQLParser }

procedure TSelectSQLParser.AnalyseSQL;
var token: TSQLTokens;
begin
 while not EOF do
 begin
   token := GetNextToken;
//   writeln('HL: ',token,',',TokenText);
   case token of
   sqltSelect:
     FSelectClause := Trim(TokenText);
   sqltWith:
     AddCTE(FCTEName,false,Trim(TokenText));
   sqltRecursive:
     AddCTE(FCTEName,true,Trim(TokenText));
   sqltFrom:
     FFromClause := Trim(TokenText);
   sqltWhere:
     FWhereClause := Trim(TokenText);
   sqltGroup:
     FGroupClause := Trim(TokenText);
   sqltHaving:
     FHavingClause := Trim(TokenText);
   sqltPlan:
     FPlanClause := Trim(TokenText);
   sqltUnion:
     begin
       FUnion := TSelectSQLParser.Create(self);
       Assign(FUnion); {copy back state}
       FNextToken := sqltSpace;
     end;
   sqltOrder:
     FOrderByClause := Trim(TokenText);
   sqltRows:
     FRowsClause := Trim(TokenText);
   sqltAll:
     begin
       FUnion := TSelectSQLParser.Create(self);
       FUnion.FUnionAll := true;
       Assign(FUnion); {copy back state}
       FNextToken := sqltSpace;
     end;
   end;
 end;
 FOriginalWhereClause := WhereClause;
 FOriginalHavingClause := HavingClause;
 FOriginalOrderByClause := OrderByClause
end;

function TSelectSQLParser.GetCTE(Index: integer): PCTEDef;
begin
  if (Index < 0) or (index >= GetCTECount) then
     raise Exception.Create('CTE Index out of bounds');

  Result := FCTEs[Index]
end;

function TSelectSQLParser.GetCTECount: integer;
begin
  Result := Length(FCTEs);
end;

function TSelectSQLParser.AddCTE(aName: string; Recursive: boolean; text: string
  ): PCTEDef;
var index: integer;
begin
  new(Result);
  Result^.Name := aName;
  Result^.Recursive := Recursive;
  Result^.text := text;
  index := Length(FCTEs);
  SetLength(FCTEs,index+1);
  FCTEs[index] := Result;
end;

procedure TSelectSQLParser.FlushCTEs;
var i: integer;
begin
  for i := 0 to Length(FCTEs) - 1 do
    dispose(FCTEs[i]);
  SetLength(FCTEs,0);
end;

function TSelectSQLParser.GetSQLText: string;
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
          SQL.Add('WITH RECURSIVE ' + CTEs[I]^.Name + ' AS ' + CTES[I]^.Text )
        else
          SQL.Add('WITH ' + CTEs[I]^.Name + ' AS ' + CTES[I]^.Text)
      end
      else
      begin
        SQL.Strings[SQL.Count-1] := SQL.Strings[SQL.Count-1] + ',';
        SQL.Add(CTEs[I]^.Name + ' AS ' + CTES[I]^.Text)
      end
    end;
    if CTECount > 0 then
      SQL.Add('');
    SQL.Add('SELECT ' + SelectClause);
    SQL.Add('FROM ' + FromClause);
    if WhereClause <> '' then
      SQL.Add('WHERE ' + WhereClause);
    if GroupClause <> '' then
      SQL.Add('GROUP BY ' + GroupClause);
    if HavingClause <> '' then
      SQL.Add('HAVING ' + HavingClause);
    if PlanClause <> '' then
      SQL.Add('PLAN ' + PlanClause);
    if Union <> nil then
    begin
      if Union.UnionAll then
         SQL.Add('UNION ALL')
      else
        SQL.Add('UNION');
      SQL.Add(Union.SQLText)
    end;
    if OrderByClause <> '' then
      SQL.Add('ORDER BY ' + OrderByClause);
    if RowsClause <> '' then
      SQL.Add('ROWS ' + RowsClause);
    Result := SQL.Text
  finally
    SQL.Free
  end
end;

procedure TSelectSQLParser.SetSelectClause(const Value: string);
begin
  if FSelectClause <> Value then
  begin
    FSelectClause := Value;
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

procedure TSelectSQLParser.SetGroupClause(const Value: string);
begin
  if FGroupClause <> Value then
  begin
    FGroupClause := Value;
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

constructor TSelectSQLParser.Create(aOwner: TSelectSQLParser);
begin
  inherited Create(aOwner <> nil);
  FOwner := aOwner;
  if assigned(FOwner) then
  begin
    FDataSet := FOwner.DataSet;
    {copy state}
    Assign(aOwner);
  end;
  AnalyseSQL;
end;

procedure TSelectSQLParser.Assign(source: TSQLTokeniser);
begin
  inherited Assign(source);
  if source is TSelectSQLParser then
  begin
    FInString := TSelectSQLParser(source).FInString;
    FIndex := TSelectSQLParser(source).FIndex;
  end;
end;

procedure TSelectSQLParser.Changed;
begin
  if FOwner <> nil then
    FOwner.Changed
  else
  if assigned(FOnSQLChanging) and not FDestroying then
     OnSQLChanging(self)
end;

function TSelectSQLParser.GetChar: char;
begin
  if FIndex <= Length(FInString) then
  begin
    Result := FInString[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

constructor TSelectSQLParser.Create(aDataSet: TDataSet; SQLText: TStrings);
begin
  FDataSet := aDataSet;
  FInString := SQLText.Text;
  FIndex := 1;
  Create(nil);
end;

constructor TSelectSQLParser.Create(aDataSet: TDataSet; const SQLText: string);
begin
  FDataSet := aDataSet;
  FInString := SQLText;
  FIndex := 1;
  Create(nil);
end;

destructor TSelectSQLParser.Destroy;
begin
  FDestroying := true;
  Clear;
  inherited Destroy;
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

procedure TSelectSQLParser.Clear;
begin
 inherited Clear;
 DropUnion;
 FlushCTEs;
 FInString := '';
 FIndex := 1;
 FSelectClause := '';
 FFromClause := '';
 FWhereClause := '';
 FGroupClause := '';
 FHavingClause := '';
 FPlanClause := '';
 FUnionAll := false;
 FOrderByClause := '';
 FOriginalWhereClause := '';
 FOriginalOrderByClause := '';
 FOriginalHavingClause := '';
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
  if assigned(FDataSet) and (FDataSet is TIBCustomDataSet) then
    Result := TIBCustomDataSet(FDataSet).GetFieldPosition(AliasName)
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

procedure TSelectSQLParser.RestoreClauseValues;
begin
 ResetWhereClause;
 ResetHavingClause;
 ResetOrderByClause
end;

{ TSelectSQLTokeniser }

function TSelectSQLTokeniser.GetNotaSelectStmt: boolean;
begin
  Result := FSQLState = stNotASelectStmt;
end;

procedure TSelectSQLTokeniser.Assign(source: TSQLTokeniser);
begin
  inherited Assign(source);
  if source is TSelectSQLTokeniser then
  begin
    FSQLState := TSelectSQLTokeniser(source).FSQLState;
    FNested := TSelectSQLTokeniser(source).FNested;
    FNextToken := TSelectSQLTokeniser(source).FNextToken;
    FPrevCTEToken := TSelectSQLTokeniser(source).FPrevCTEToken;
    FClause := TSelectSQLTokeniser(source).FClause;
  end;
end;

function TSelectSQLTokeniser.TokenFound(var token: TSQLTokens): boolean;

  procedure swap(var a,b: TSQLTokens);
  var c: TSQLTokens;
  begin
    c:= a;
    a := b;
    b := c;
  end;

  procedure ChangeState(AllowStates: TSQLStates; DoNesting: boolean);
  var SaveState: TSQLState;
  begin
    SaveState := FSQLState;
    if DoNesting then
      case token of
      sqltOpenBracket:
        begin
          Inc(FNested);
          Exit;
        end;

      sqltCloseBracket:
        Begin
          Dec(FNested);
          Exit;
        end;
      end;

    if FNested = 0 then
    case token of
    sqltFrom:
      FSQLState := stInFrom;

    sqltWhere:
      FSQLState := stInWhere;

    sqltGroup:
      FSQLState := stInGroupBy;

    sqltHaving:
      FSQLState := stInHaving;

    sqltPlan:
      FSQLState := stInPlan;

    sqltUnion:
      FSQLState := stInUnion;

    sqltOrder:
      begin
        if FUnionMember then
        {stop and return to owning object}
        begin
          ResetQueue(sqltEOF,'');
          QueueToken(token);
          ReleaseQueue;
          token := sqltEOF;
          FSQLState := stUnionEnd;
        end
        else
          FSQLState := stInOrderBy;
      end;

    sqltRows:
      if FUnionMember then
      {stop and return to owning object}
      begin
        ResetQueue(sqltEOF,'');
        QueueToken(token);
        ReleaseQueue;
        token := sqltEOF;
        FSQLState := stUnionEnd;
      end
      else
        FSQLState := stInRows;

    sqltAll:
      FSQLState := stInUnionAll;
   end;
   if not (FSQLState in AllowStates + [stDefault]) then
     FSQLState := SaveState
   else
   if SaveState <> FSQLState then
     swap(token,FNextToken);
  end;

  var StateOnEntry: TSQLState;
      DoNotReturnToken: boolean;

  function TokenIncomplete: boolean;
  begin
     {we are not done if we are in not the default state and no state change,
      unless we are a union member and the state is stUnionEnd}
     Result :=  (FUnionMember and (StateOnEntry = stUnionEnd)) or
                ((StateOnEntry = stDefault) or  (StateOnEntry = FSQLState));
  end;

begin
  Result := inherited TokenFound(token);
  if not Result or NotaSelectStmt then Exit;

//  writeln(token);
  StateOnEntry := FSQLState;
  DoNotReturnToken := false;

  if not (token in [sqltComment,sqltCommentLine]) then
  begin
    if token in [sqltParam, sqltQuotedParam] then
      FParamList.Add(TokenText);

    if (token = sqltEOF) or ((FNested = 0) and
      not (token in [sqltQuotedString,sqltIdentifierInDoubleQuotes]) and
      (TokenText = DefaultTerminator)) then
    begin
      if not FHasText then
        FSQLState := stNotASelectStmt {empty statements are not select statements}
      else
      if FSQLState <> stUnionEnd then
        FSQLState := stDefault;
      swap(token,FNextToken);
    end
    else
    if not (token in [sqltSpace,sqltEOL,sqltCR]) then
    case FSQLState of
    stDefault:
      begin
        if FNested = 0 then {not inside a pair of brackets}
        case token of
        sqltSelect:
          FSQLState := stInSelect;

        sqltWith:
          FSQLState := stWith;

        else
          FSQLState := stNotASelectStmt;
        end;
        FNextToken := token;
      end;

    stWith:
      begin
        case token of
        sqltRecursive:
          begin
            FSQLState := stInRecursiveCTE;
            FNextToken := token;
          end;

        sqltIdentifier:
          begin
            FCTEName := TokenText;
            FSQLState := stCTEAs;
            FPrevCTEToken := FNextToken;
            token := FNextToken;
          end

        else
          IBError(ibxErrorParsing,['with']);
        end;
        DoNotReturnToken := true;
      end;

    stInRecursiveCTE:
      case token of
      sqltIdentifier:
        begin
          FCTEName := TokenText;
          FSQLState := stCTEAs;
          token := FNextToken;
          FPrevCTEToken := FNextToken;
          DoNotReturnToken := true;
        end;

      else
        IBError(ibxErrorParsing,['with recursive']);
      end;

    stInNextCTE:
      case token of
      sqltIdentifier:
        begin
          FCTEName := TokenText;
          FSQLState := stCTEAs;
          token := FPrevCTEToken;
          DoNotReturnToken := true;
        end;

      else
        IBError(ibxErrorParsing,['with']);
      end;

    stCTEAs:
      if token = sqltAs then
      begin
        FSQLState := stInCTE;
        DoNotReturnToken := true;
      end
    else
      IBError(ibxErrorParsing,['with']);

    stInCTE:
      begin
        case token of
        sqltOpenBracket:
          Inc(FNested);

        sqltCloseBracket:
          Dec(FNested);

        sqltComma:
          if FNested = 0 then
          begin
            FSQLState := stInNextCTE;
            token := FNextToken;
          end;

        sqltSelect:
          if FNested = 0 then
          begin
            FSQLState := stInSelect;
            swap(FNextToken,token);
          end;
        end;
      end;

    stInSelect:
      ChangeState([stInFrom],true);

    stInFrom:
      ChangeState([stInWhere,stInGroupBy, stInHaving,stInPlan, stInUnion,stUnionEnd,
                        stInOrderBy, stInRows],true);

    stInWhere:
      ChangeState([stInGroupBy, stInHaving,stInPlan, stInUnion,stUnionEnd,
                       stInOrderBy, stInRows],true);

    stInGroupBy:
      if token = sqltBy then
      begin
        FClause := '';
        SetTokenText('');
        DoNotReturnToken := true;
      end
      else
        ChangeState([stInHaving,stInPlan, stInUnion, stUnionEnd, stInOrderBy, stInRows],false);

    stInHaving:
      ChangeState([stInPlan, stInUnion, stUnionEnd, stInOrderBy, stInRows],true);

    stInPlan:
      ChangeState([stInUnion, stUnionEnd, stInOrderBy, stInRows],true);

    stInUnion:
      case token of
      sqltAll:
        begin
          FSQLState := stDefault;
          FNextToken := token;
        end;
      else
        begin
          ResetQueue(token);
          ReleaseQueue;
          swap(token,FNextToken);
          FSQLState := stDefault;
        end;
      end;

    stUnionEnd: {On return from union clause}
      ChangeState([stInOrderBy, stInRows],false);

    stInOrderBy:
      if token = sqltBy then
      begin
        FClause := '';
        SetTokenText('');
        DoNotReturnToken := true;
      end
      else
        ChangeState([stInRows],false);

    stInRows:
      ChangeState([],false);

    end;

    {On EOF or state change return the next element, otherwise just add to text buffer}

    if (token <> sqltEOF) and TokenIncomplete  then
    begin
      if StateOnEntry <> stDefault then
      case token of
      sqltQuotedString:
        FClause += '''' + SQLSafeString(TokenText) + '''';

      sqltIdentifierInDoubleQuotes:
        FClause += '"' + StringReplace(TokenText,'"','""',[rfReplaceAll]) + '"';

      sqltParam:
        FClause += ':' + TokenText;

      sqltQuotedParam:
        FClause += ':"' + StringReplace(TokenText,'"','""',[rfReplaceAll]) + '"';

      else
        FClause += TokenText;
      end;
      DoNotReturnToken := true;
    end
    else
    begin
      FHasText := true;
      SetTokenText(FClause);
      FClause := '';
    end;

  end;
  Result := not DoNotReturnToken ;
end;

procedure TSelectSQLTokeniser.Reset;
begin
  inherited Reset;
  FSQLState := stDefault;
  FNested := 0;
  FNextToken := sqltSpace;
end;

constructor TSelectSQLTokeniser.Create(UnionMember: boolean);
begin
  inherited Create;
  FUnionMember := UnionMember;
  FParamList := TStringList.Create;
end;

destructor TSelectSQLTokeniser.Destroy;
begin
  if assigned(FParamList) then FParamList.Free;
  FParamList := nil;
  inherited Destroy;
end;

procedure TSelectSQLTokeniser.Clear;
begin
  Reset;
  FHasText := false;
  if assigned(FParamList) then
    FParamList.Clear;
end;



end.


