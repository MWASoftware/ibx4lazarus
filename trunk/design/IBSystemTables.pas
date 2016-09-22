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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit IBSystemTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBSQL, IBDatabase, StdCtrls, IB;

type

  { TIBSystemTables }

  TIBSystemTables = class
  private
    FGetTableNames: TIBSQL;
    FGetFieldNames: TIBSQL;
    FGetPrimaryKeys: TIBSQL;
    FTestSQL: TIBSQL;
    FTableAndColumnSQL: TIBSQL;
    FGetGeneratorsSQL: TIBSQL;
    FGetProcedures: TIBSQL;
    FGetProcedureParams: TIBSQL;
    FGetProcedureInfo: TIBSQL;
    function GetSQLType(SQLType: TIBSQLStatementTypes): string;
    procedure AddWhereClause(TableName: string; QuotedStrings: boolean; SQL: TStrings;
       UseOldValues: boolean = false);
    procedure GetProcParams(ProcName: string; ParamList: TStrings; InputParams: boolean); overload;
    function GetWord(S: string; WordNo: integer): string;
    function RemoveSQLText(aMessage: string): string;
 public
    constructor Create;
    destructor Destroy; override;
    procedure SelectDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
    procedure GetTableNames(TableNames: TStrings);
    procedure GetFieldNames(TableName: string; FieldNames: TStrings;
              IncludePrimaryKeys:boolean=true; IncludeReadOnlyFields: boolean = true);
    procedure GetPrimaryKeys(TableName: string; PrimaryKeys: TStrings);
    procedure GetTableAndColumns(SelectSQL: string; var FirstTableName: string;
                Columns: TStrings);
    procedure GetProcedureNames(ProcNames: TStrings; WithOutputParams: boolean=false);
    procedure GetProcParams(ProcName: string; var ExecuteOnly: boolean;
                            InputParams, OutputParams: TStrings); overload;
    procedure GetGenerators(GeneratorNames: TStrings);
    procedure GenerateSelectSQL(TableName: string; QuotedStrings: boolean; FieldNames,SQL: TStrings);
    procedure GenerateRefreshSQL(TableName: string; QuotedStrings: boolean; FieldNames,SQL: TStrings);
    procedure GenerateInsertSQL(TableName: string; QuotedStrings: boolean; FieldNames, SQL: TStrings);
    procedure GenerateModifySQL(TableName: string; QuotedStrings: boolean; FieldNames,SQL: TStrings);
    procedure GenerateDeleteSQL(TableName: string; QuotedStrings: boolean; SQL: TStrings);
    procedure GenerateExecuteSQL(ProcName: string; QuotedStrings: boolean; ExecuteOnly: boolean;
              InputParams, OutputParams, ExecuteSQL: TStrings);
    function GetStatementType(SQL: string; var IsStoredProcedure: boolean): TIBSQLStatementTypes;
    function GetFieldNames(FieldList: TListBox): TStrings;
    procedure TestSQL(SQL: string; GenerateParamNames: boolean = false);
  end;

implementation

uses Dialogs, IBUtils, FBMessages;

{ TIBSystemTables }

const
  sqlGETTABLES = 'Select Trim(RDB$RELATION_NAME) as TableName From RDB$RELATIONS ' +
                 'Where RDB$SYSTEM_FLAG = 0 ' +
                 'Order by 1';

  sqlGETALLFIELDS = 'Select Trim(RDB$FIELD_NAME) as ColumnName FROM RDB$RELATION_FIELDS ' +
                 'Where RDB$RELATION_NAME = :TableName ' +
                 'order by RDB$FIELD_POSITION asc ';

  sqlGETFIELDS = 'Select Trim(RF.RDB$FIELD_NAME) as ColumnName FROM RDB$RELATION_FIELDS RF ' +
                 'JOIN RDB$FIELDS B On B.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE '+
                 'Where RF.RDB$RELATION_NAME = :TableName and B.RDB$COMPUTED_SOURCE is NULL ' +
                 'order by RF.RDB$FIELD_POSITION asc ';

  sqlGETPRIMARYKEYS = 'Select Trim(S.RDB$FIELD_NAME) as ColumnName From '+
                      '(Select RDB$INDEX_NAME,RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS Order by RDB$FIELD_POSITION ASC) S ' +
                      'JOIN RDB$RELATION_CONSTRAINTS C On C.RDB$INDEX_NAME = S.RDB$INDEX_NAME ' +
                      'Where C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and RDB$RELATION_NAME = :TableName';

  sqlUPDATEFIELDS = 'Select Trim(RF.RDB$FIELD_NAME) as ColumnName FROM RDB$RELATION_FIELDS RF ' +
                    'JOIN RDB$FIELDS B On B.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE '+
                    'Where RF.RDB$RELATION_NAME = :TableName  and RF.RDB$FIELD_NAME not in ' +
                    '(Select RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS S JOIN RDB$RELATION_CONSTRAINTS C On C.RDB$INDEX_NAME = S.RDB$INDEX_NAME '+
                     'Where C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and C.RDB$RELATION_NAME = RF.RDB$RELATION_NAME) and B.RDB$COMPUTED_SOURCE is NULL ' +
                     'order by 1 asc ';

  sqlALLUPDATEFIELDS = 'Select Trim(RDB$FIELD_NAME) as ColumnName FROM RDB$RELATION_FIELDS RF ' +
                    'Where RF.RDB$RELATION_NAME = :TableName  and RDB$FIELD_NAME not in ' +
                    '(Select RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS S JOIN RDB$RELATION_CONSTRAINTS C On C.RDB$INDEX_NAME = S.RDB$INDEX_NAME '+
                     'Where C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and C.RDB$RELATION_NAME = RF.RDB$RELATION_NAME)' +
                     'order by 1 asc ';

  sqlGETGENERATORNAMES = 'Select RDB$GENERATOR_NAME FROM RDB$GENERATORS '+
                         'Where RDB$SYSTEM_FLAG = 0 '+
                         'Order by 1 asc';

  sqlGETPROCEDURES = 'Select Trim(RDB$PROCEDURE_NAME) as ProcName, RDB$PROCEDURE_INPUTS, '+
                     'RDB$PROCEDURE_OUTPUTS From RDB$PROCEDURES '+
		     'Where RDB$SYSTEM_FLAG = 0 and RDB$PROCEDURE_TYPE <= :ProcType Order by 1 asc';

  sqlGETPROCPARAM  = 'Select Trim(P.RDB$PARAMETER_NAME) as ParamName '+
                     'From RDB$PROCEDURE_PARAMETERS P '+
		     'JOIN RDB$FIELDS F On F.RDB$FIELD_NAME = P.RDB$FIELD_SOURCE '+
		     'Where P.RDB$SYSTEM_FLAG = 0 and P.RDB$PROCEDURE_NAME = :ProcName and P.RDB$PARAMETER_TYPE = :type '+
		     'Order by P.RDB$PARAMETER_NUMBER asc';

  sqlCheckProcedureNames = 'Select * From RDB$PROCEDURES Where Upper(Trim(RDB$PROCEDURE_NAME)) = Upper(:ProcName)';

  sqlGETPROCEDUREINFO = 'Select RDB$PROCEDURE_TYPE From RDB$PROCEDURES Where Upper(Trim(RDB$PROCEDURE_NAME)) = Upper(:ProcName)';

function TIBSystemTables.GetSQLType(SQLType: TIBSQLStatementTypes): string;
begin
  case SQLType of
  SQLUnknown:              Result := 'Unknown';
  SQLSelect:               Result := 'Select';
  SQLInsert:               Result := 'Insert';
  SQLUpdate:               Result := 'Update';
  SQLDelete:               Result := 'Delete';
  SQLDDL:                  Result := 'DDL';
  SQLGetSegment:           Result := 'GetSegment';
  SQLPutSegment:           Result := 'PutSegment';
  SQLExecProcedure:        Result := 'Execute Procedure';
  SQLStartTransaction:     Result := 'StartTransaction';
  SQLCommit:               Result := 'Commit';
  SQLRollback:             Result := 'Rollback';
  SQLSelectForUpdate:      Result := 'Select for Update';
  SQLSetGenerator:         Result := 'Set Generator';
  end;
end;

procedure TIBSystemTables.AddWhereClause(TableName: string;
  QuotedStrings: boolean; SQL: TStrings; UseOldValues: boolean);
var WhereClause: string;
    Separator: string;
    Count: integer;
    Prefix: string;
begin
  if not assigned(FGetPrimaryKeys.Database) or not FGetPrimaryKeys.Database.Connected or
    not assigned(FGetPrimaryKeys.Transaction) then
    Exit;
  Count := 0;
  WhereClause := 'Where';
  Separator := ' A.';
  if UseOldValues then
    Prefix := ':OLD_'
  else
    Prefix := ':';
  FGetPrimaryKeys.Prepare;
  FGetPrimaryKeys.ParamByName('TableName').AsString := TableName;
  FGetPrimaryKeys.ExecQuery;
  try
    while not FGetPrimaryKeys.EOF do
    begin
      Inc(Count);
      if QuotedStrings then
        WhereClause := WhereClause + Separator + '"' + FGetPrimaryKeys.FieldByName('ColumnName').AsString +
               '" = ' + Prefix+ AnsiUpperCase(FGetPrimaryKeys.FieldByName('ColumnName').AsString)
      else
        WhereClause := WhereClause + Separator + QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,FGetPrimaryKeys.FieldByName('ColumnName').AsString) +
               ' = ' + Prefix + AnsiUpperCase(FGetPrimaryKeys.FieldByName('ColumnName').AsString);
      Separator := ' AND A.';
      FGetPrimaryKeys.Next
    end;
  finally
    FGetPrimaryKeys.Close
  end;
  if Count > 0 then
    SQL.Add(WhereClause)
end;

procedure TIBSystemTables.GetProcParams(ProcName: string; ParamList: TStrings;
  InputParams: boolean);
begin
  if not assigned(FGetProcedureParams.Database) or not FGetProcedureParams.Database.Connected or
    not assigned(FGetProcedureParams.Transaction) then
    Exit;
  ParamList.Clear;
  with FGetProcedureParams do
  begin
    with Transaction do
      if not InTransaction then StartTransaction;
    Prepare;
    ParamByName('ProcName').AsString := ProcName;
    if InputParams then
      ParamByName('type').AsInteger := 0
    else
      ParamByName('type').AsInteger := 1;
    ExecQuery;
    try
      while not EOF do
      begin
        ParamList.Add(FieldByName('ParamName').AsString);
        Next;
      end;
    finally
      Close
    end;
  end;
end;

function TIBSystemTables.GetWord(S: string; WordNo: integer): string;
const
    SpaceChars = [' ',#$0a,#$0d,#$09,'('];
var I: integer;
    StartIdx: integer;
    InWhiteSpace: boolean;
begin
  Result := '';
  StartIdx := 1;
  InWhiteSpace := true;
  for I := 1 to Length(S) do
  begin
    if InWhiteSpace then
    begin
      if not (S[I] in SpaceChars) then
      begin
        StartIdx := I;
        InWhiteSpace := false
      end
    end
    else
    begin
      if S[I] in SpaceChars then
      begin
        Dec(WordNo);
        if WordNo = 0 then
        begin
          Result := System.copy(S,StartIdx,I - StartIdx);
          Exit
        end;
        InWhiteSpace := true
      end
    end
  end;
end;

function TIBSystemTables.RemoveSQLText(aMessage: string): string;
var idx: integer;
begin
  idx := Pos(sSQLErrorSeparator,aMessage);
  if idx > 0 then
     Result := system.copy(aMessage,1,idx)
  else
    Result := aMessage;
end;

constructor TIBSystemTables.Create;
begin
  FGetTableNames := TIBSQL.Create(nil);
  FGetFieldNames := TIBSQL.Create(nil);
  FGetPrimaryKeys := TIBSQL.Create(nil);
  FGetProcedures := TIBSQL.Create(nil);
  FTestSQL := TIBSQL.Create(nil);
  FTableAndColumnSQL := TIBSQL.Create(nil);
  FTableAndColumnSQL.GenerateParamNames := true; {Permissive}
  FGetGeneratorsSQL := TIBSQL.Create(nil);
  FGetProcedureParams := TIBSQL.Create(nil);
  FGetProcedureInfo := TIBSQL.Create(nil);
end;

destructor TIBSystemTables.Destroy;
begin
  if assigned(FGetFieldNames) then FGetFieldNames.Free;
  if assigned(FGetTableNames) then FGetTableNames.Free;
  if assigned(FTestSQL) then FTestSQL.Free;
  if assigned(FGetPrimaryKeys) then FGetPrimaryKeys.Free;
  if assigned(FTableAndColumnSQL) then FTableAndColumnSQL.Free;
  if assigned(FGetGeneratorsSQL) then FGetGeneratorsSQL.Free;
  if assigned(FGetProcedures) then FGetProcedures.Free;
  if assigned(FGetProcedureParams) then FGetProcedureParams.Free;
  if assigned(FGetProcedureInfo) then FGetProcedureInfo.Free;
  inherited Destroy;
end;

procedure TIBSystemTables.SelectDatabase(Database: TIBDatabase;
  Transaction: TIBTransaction);
begin
    FGetTableNames.Database := Database;
    FGetTableNames.Transaction := Transaction;
    FGetTableNames.SQL.Text := sqlGETTABLES;
    FGetFieldNames.Database := Database;
    FGetFieldNames.Transaction := Transaction;
    FGetFieldNames.SQL.Text := sqlGETFIELDS;
    FTestSQL.Database := Database;
    FTestSQL.Transaction := Transaction;
    FGetPrimaryKeys.Database := Database;
    FGetPrimaryKeys.Transaction := Transaction;
    FGetPrimaryKeys.SQL.Text := sqlGETPRIMARYKEYS;
    FTableAndColumnSQL.Database := Database;
    FTableAndColumnSQL.Transaction := Transaction;
    FGetGeneratorsSQL.Database := Database;
    FGetGeneratorsSQL.Transaction := Transaction;
    FGetGeneratorsSQL.SQL.Text := sqlGETGENERATORNAMES;
    FGetProcedureParams.Database := Database;
    FGetProcedureParams.Transaction := Transaction;
    FGetProcedureParams.SQL.Text := sqlGETPROCPARAM;
    FGetProcedureInfo.Database := Database;
    FGetProcedureInfo.Transaction := Transaction;
    FGetProcedureInfo.SQL.Text := sqlGETPROCEDUREINFO;
    FGetProcedures.Database := Database;
    FGetProcedures.Transaction := Transaction;
    FGetProcedures.SQL.Text := sqlGETPROCEDURES;
end;

procedure TIBSystemTables.GetTableNames(TableNames: TStrings);
begin
  if not assigned(FGetTableNames.Database) or not FGetTableNames.Database.Connected or
    not assigned(FGetTableNames.Transaction) then
    Exit;
  with FGetTableNames.Transaction do
    if not InTransaction then StartTransaction;
  TableNames.Clear;
  FGetTableNames.ExecQuery;
  try
    while not FGetTableNames.EOF do
    begin
      TableNames.Add(FGetTableNames.FieldByName('TableName').AsString);
      FGetTableNames.Next
    end;
  finally
    FGetTableNames.Close
  end;
end;

procedure TIBSystemTables.GetFieldNames(TableName: string;
  FieldNames: TStrings; IncludePrimaryKeys: boolean;
  IncludeReadOnlyFields: boolean);
begin
  if not assigned(FGetFieldNames.Database) or not FGetFieldNames.Database.Connected or
    not assigned(FGetFieldNames.Transaction) then
    Exit;
  with FGetFieldNames.Transaction do
    if not InTransaction then StartTransaction;
  FieldNames.Clear;
  if IncludePrimaryKeys then
  begin
    if IncludeReadOnlyFields then
      FGetFieldNames.SQL.Text := sqlGETALLFIELDS
    else
      FGetFieldNames.SQL.Text := sqlGETFIELDS
  end
  else
  if  IncludeReadOnlyFields then
    FGetFieldNames.SQL.Text := sqlALLUPDATEFIELDS
  else
      FGetFieldNames.SQL.Text := sqlUPDATEFIELDS;
  FGetFieldNames.Prepare;
  FGetFieldNames.ParamByName('TableName').AsString := TableName;
  FGetFieldNames.ExecQuery;
  try
    while not FGetFieldNames.EOF do
    begin
      FieldNames.Add(FGetFieldNames.FieldByName('ColumnName').AsString);
      FGetFieldNames.Next
    end;
  finally
    FGetFieldNames.Close
  end;
end;

procedure TIBSystemTables.GetPrimaryKeys(TableName: string; PrimaryKeys: TStrings);
begin
  if not assigned(FGetPrimaryKeys.Database) or not FGetPrimaryKeys.Database.Connected or
    not assigned(FGetPrimaryKeys.Transaction) then
    Exit;
  with FGetPrimaryKeys.Transaction do
    if not InTransaction then StartTransaction;
  PrimaryKeys.Clear;
  FGetPrimaryKeys.Prepare;
  FGetPrimaryKeys.ParamByName('TableName').AsString := TableName;
  FGetPrimaryKeys.ExecQuery;
  try
    while not FGetPrimaryKeys.EOF do
    begin
      PrimaryKeys.Add(FGetPrimaryKeys.FieldByName('ColumnName').AsString);
      FGetPrimaryKeys.Next
    end;
  finally
    FGetPrimaryKeys.Close
  end;
end;

procedure TIBSystemTables.GetTableAndColumns(SelectSQL: string;
  var FirstTableName: string; Columns: TStrings);
var I: integer;
begin
  FirstTableName := '';
  if not assigned(FTableAndColumnSQL.Database) or not FTableAndColumnSQL.Database.Connected or
    not assigned(FTableAndColumnSQL.Transaction) or (Trim(SelectSQL) = '') then
    Exit;
  with FTableAndColumnSQL.Transaction do
    if not InTransaction then StartTransaction;
  FTableAndColumnSQL.SQL.Text := SelectSQL;
  try
    FTableAndColumnSQL.Prepare;
    case FTableAndColumnSQL.SQLStatementType of
    SQLSelect:
      begin
        if FTableAndColumnSQL.MetaData.Count > 0 then
          FirstTableName := FTableAndColumnSQL.MetaData[0].GetRelationName
        else
          FirstTableName := '';
        if assigned(Columns) then
        begin
          Columns.Clear;
          for I := 0 to FTableAndColumnSQL.MetaData.Count - 1 do
              Columns.Add(FTableAndColumnSQL.MetaData[I].Name)
        end;
      end;
    { If not a select statement then return table or procedure name
      as First Table Name }
    SQLUpdate:
      FirstTableName := GetWord(SelectSQL,2);

    else
      FirstTableName := GetWord(SelectSQL,3);
    end
  except on E:EIBError do
//      ShowMessage(E.Message);
  end;
end;

procedure TIBSystemTables.GetProcedureNames(ProcNames: TStrings; WithOutputParams: boolean);
begin
  if not assigned(FGetProcedures.Database) or not FGetProcedures.Database.Connected or
    not assigned(FGetProcedures.Transaction) then
    Exit;
  ProcNames.Clear;
  with FGetProcedures do
  begin
    with Transaction do
      if not InTransaction then StartTransaction;
    Prepare;
    if WithOutputParams then
      ParamByName('ProcType').AsInteger := 1
    else
      ParamByName('ProcType').AsInteger := 2;
    ExecQuery;
    try
      while not EOF do
      begin
        ProcNames.Add(FieldByName('ProcName').AsString);
        Next;
      end;
    finally
      Close
    end;
  end;
end;

procedure TIBSystemTables.GetProcParams(ProcName: string;
  var ExecuteOnly: boolean; InputParams, OutputParams: TStrings);
begin
  GetProcParams(ProcName,InputParams,true);
  GetProcParams(ProcName,OutputParams,false);
  ExecuteOnly := OutputParams.Count = 0;
  if not ExecuteOnly then
  with FGetProcedureInfo do
  begin
    with Transaction do
      if not InTransaction then StartTransaction;
    Prepare;
    ParamByName('ProcName').AsString := ProcName;
    ExecQuery;
    try
      if not EOF then
        ExecuteOnly := FieldByName('RDB$PROCEDURE_TYPE').AsInteger = 2
    finally
      Close
    end;
  end;
end;

procedure TIBSystemTables.GetGenerators(GeneratorNames: TStrings);
begin
  if not assigned(FGetGeneratorsSQL.Database) or not FGetGeneratorsSQL.Database.Connected or
    not assigned(FGetGeneratorsSQL.Transaction) then
    Exit;
  GeneratorNames.Clear;
  with FGetGeneratorsSQL do
  begin
    with Transaction do
      if not InTransaction then StartTransaction;
    ExecQuery;
    try
      while not EOF do
      begin
        GeneratorNames.Add(FieldByName('RDB$GENERATOR_NAME').AsString);
        Next;
      end;
    finally
      Close
    end;
  end;

end;

procedure TIBSystemTables.GenerateSelectSQL(TableName: string; QuotedStrings: boolean; FieldNames,SQL: TStrings);
var SelectSQL: string;
    Separator : string;
    I: integer;
begin
  SQL.Clear;
  if not assigned(FGetFieldNames.Database) or not FGetFieldNames.Database.Connected or
    not assigned(FGetFieldNames.Transaction) then
  begin
    Messagedlg('No Database Connected',mtError,[mbOK],0);
    Exit;
  end;
  SelectSQL := 'Select';
  Separator := ' A.';
  for I := 0 to FieldNames.Count - 1 do
  begin
    if QuotedStrings then
      SelectSQL := SelectSQL + Separator + '"' + FieldNames[I] + '"'
    else
      SelectSQL := SelectSQL + Separator + QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,FieldNames[I]);
    Separator := ', A.';
  end;
  SelectSQL := SelectSQL + ' From ' + TableName + ' A';
  SQL.Add(SelectSQL);
end;

procedure TIBSystemTables.GenerateRefreshSQL(TableName: string; QuotedStrings: boolean; FieldNames,SQL: TStrings);
begin
  GenerateSelectSQL(TableName,QuotedStrings,FieldNames,SQL);
  AddWhereClause(TableName,QuotedStrings,SQL)
end;

procedure TIBSystemTables.GenerateInsertSQL(TableName: string;
  QuotedStrings: boolean; FieldNames,SQL: TStrings);
var InsertSQL: string;
    Separator: string;
    I: integer;
begin
  SQL.Clear;
  InsertSQL := 'Insert Into ' + TableName + '(';
  Separator := '';
  for I := 0 to FieldNames.Count - 1 do
    begin
      if QuotedStrings then
         InsertSQL := InsertSQL + Separator + '"' + FieldNames[I] + '"'
      else
         InsertSQL := InsertSQL + Separator +  QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,FieldNames[I]) ;
      Separator := ', ';
    end;
  InsertSQL := InsertSQL + ')';
  SQL.Add(InsertSQL);
  InsertSQL := 'Values(';
  Separator := ':';
  for I := 0 to FieldNames.Count - 1 do
    begin
       InsertSQL := InsertSQL + Separator +  AnsiUpperCase(FieldNames[I]) ;
       Separator := ', :';
    end;
  InsertSQL := InsertSQL + ')';
  SQL.Add(InsertSQL);
end;

procedure TIBSystemTables.GenerateModifySQL(TableName: string; QuotedStrings: boolean;
          FieldNames,SQL: TStrings);
var UpdateSQL: string;
    Separator: string;
    I: integer;
begin
  SQL.Clear;
  Separator := #$0d#$0a'  A.';
  UpdateSQL := 'Update ' + TableName + ' A Set ';
  for I := 0 to FieldNames.Count - 1 do
    begin
      if QuotedStrings then
        UpdateSQL := UpdateSQL + Separator + '"' + FieldNames[I] + '" = :' + AnsiUpperCase(FieldNames[I])
      else
        UpdateSQL := UpdateSQL + Separator + QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,FieldNames[I]) + ' = :' + AnsiUpperCase(FieldNames[I]);
      Separator := ','#$0d#$0a'  A.';
    end;
  SQL.Add(UpdateSQL);
  AddWhereClause(TableName,QuotedStrings,SQL,true)
end;

procedure TIBSystemTables.GenerateDeleteSQL(TableName: string; QuotedStrings: boolean; SQL: TStrings);
begin
  SQL.Clear;
  SQL.Add('Delete From ' + TableName + ' A');
  AddWhereClause(TableName,QuotedStrings,SQL)
end;

procedure TIBSystemTables.GenerateExecuteSQL(ProcName: string;
  QuotedStrings: boolean; ExecuteOnly: boolean; InputParams, OutputParams,
  ExecuteSQL: TStrings);
var SQL: string;
    I: integer;
    Separator: string;
begin
  Separator := '';
  if not ExecuteOnly and (OutputParams.Count > 0) then //Select Query
  begin
    SQL := 'Select ';
    for I := 0 to OutputParams.Count - 1 do
    begin
      if QuotedStrings then
        SQL := SQL + Separator + '"' + OutputParams[I] + '"'
      else
        SQL := SQL + Separator + QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,OutputParams[I]);
      Separator := ', ';
    end;
    SQL := SQL + ' From ' + ProcName;
    if InputParams.Count > 0 then
    begin
      Separator := '(:';
      for I := 0 to InputParams.Count - 1 do
      begin
        SQL := SQL + Separator + AnsiUpperCase(InputParams[I]);
        Separator := ', :';
      end;
      SQL := SQL + ')'
    end
  end
  else // Execute Procedure
  begin
    if QuotedStrings then
      SQL := 'Execute Procedure "' + ProcName + '"'
    else
      SQL := 'Execute Procedure ' + QuoteIdentifierIfNeeded(FGetFieldNames.Database.SQLDialect,ProcName);
    if InputParams.Count > 0 then
    begin
      Separator := ' :';
      for I := 0 to InputParams.Count - 1 do
      begin
        if QuotedStrings then
          SQL := SQL + Separator + '"' + InputParams[I] + '"'
        else
          SQL := SQL + Separator + AnsiUpperCase(InputParams[I]);
        Separator := ', :';
      end;
    end
  end;
  ExecuteSQL.Text := SQL
end;

function TIBSystemTables.GetStatementType(SQL: string;
  var IsStoredProcedure: boolean): TIBSQLStatementTypes;
var TableName: string;
begin
  Result := sqlUnknown;
  if not assigned(FTestSQL.Database) or not FTestSQL.Database.Connected or
    not assigned(FTestSQL.Transaction) or (Trim(SQL) = '') then
    Exit;
  IsStoredProcedure := false;
  FTestSQL.SQL.Text := SQL;
  FTestSQL.GenerateParamNames := true; {permissive}
  try
    FTestSQL.Prepare;
    Result := FTestSQL.SQLStatementType
  except on E:EIBError do
//      ShowMessage(E.Message);
  end;
  if (Result = SQLSelect) and (FTestSQL.MetaData.Count > 0)  then
  begin
    TableName := FTestSQL.MetaData[0].GetRelationName;
    FTestSQL.SQL.Text := sqlCheckProcedureNames;
    FTestSQL.Prepare;
    FTestSQL.ParamByName('ProcName').AsString := TableName;
    FTestSQL.ExecQuery;
    try
      IsStoredProcedure := not FTestSQL.EOF;
    finally
      FTestSQL.Close
    end;
  end;
end;

function TIBSystemTables.GetFieldNames(FieldList: TListBox): TStrings;
var I: integer;
begin
  Result := TStringList.Create;
  try
    if FieldList.SelCount = 0 then
      Result.Assign(FieldList.Items)
    else
      for I := 0 to FieldList.Items.Count - 1 do
        if FieldList.Selected[I] then
          Result.Add(FieldList.Items[I]);
  except
    Result.Free;
    raise
  end;
end;

  procedure TIBSystemTables.TestSQL(SQL: string;
   GenerateParamNames: boolean);
begin
  if not assigned(FTestSQL.Database) or not FTestSQL.Database.Connected or
    not assigned(FTestSQL.Transaction) then
  begin
    Messagedlg('No Database Connected',mtError,[mbOK],0);
    Exit;
  end;
  FTestSQL.GenerateParamNames := GenerateParamNames;
  FTestSQL.SQL.Text := SQL;
  try
    FTestSQL.Prepare;
    ShowMessage('SQL '+ GetSQLType(FTestSQL.SQLStatementType) + ' Statement Looks OK');
  except on E:EIBError do
      ShowMessage(RemoveSQLText(E.Message));
  end;
end;

end.
