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
 *  The Original Code is (C) 2011-17 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBSQLEditFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, LResources, Forms,
  Controls, ActnList, Menus, Dialogs, ComCtrls, ExtCtrls, StdCtrls, IBQuery,
  IBSQL, IBDatabase, IBUpdate, IBDatabaseInfo, IBLookupComboEditBox,
  IBCustomDataset, db, LazSynTextArea, IB;

type

  { TIBSQLEditFrame }

  TIBSQLEditFrame = class(TFrame)
    ReadOnlyFieldsSource: TDataSource;
    IBUpdate6: TIBUpdate;
    ReadOnlyFields: TIBQuery;
    IBUpdate5: TIBUpdate;
    IdentityColsSource: TDataSource;
    FieldNameList: TIBQuery;
    FieldsSource: TDataSource;
    DatabaseInfo: TIBDatabaseInfo;
    IdentityCols: TIBQuery;
    IBUpdate1: TIBUpdate;
    IBUpdate2: TIBUpdate;
    IBUpdate3: TIBUpdate;
    IBUpdate4: TIBUpdate;
    SQLTransaction: TIBTransaction;
    IdentifyStatementSQL: TIBSQL;
    PrimaryKeys: TIBQuery;
    PrimaryKeySource: TDataSource;
    ProcInputParams: TIBQuery;
    ProcInputSource: TDataSource;
    ProcOutputParams: TIBQuery;
    ProcOutputSource: TDataSource;
    Redo: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    Undo: TAction;
    SaveToFile: TAction;
    LoadFromFile: TAction;
    BtnImages: TImageList;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UserProcedures: TIBQuery;
    UserProcSource: TDataSource;
    UserTables: TIBQuery;
    UserTableSource: TDataSource;
    WrapText: TAction;
    Clear: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    PopupMenu1: TPopupMenu;
    SelectAll: TAction;
    Paste: TAction;
    CopyText: TAction;
    Cut: TAction;
    ActionList1: TActionList;
    SQLText: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure ClearExecute(Sender: TObject);
    procedure CopyTextExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CutUpdate(Sender: TObject);
    procedure FieldNameListBeforeOpen(DataSet: TDataSet);
    procedure IBUpdate1ApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure LoadFromFileExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure PasteUpdate(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure RedoUpdate(Sender: TObject);
    procedure SaveToFileExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure UndoUpdate(Sender: TObject);
    procedure UserProceduresAfterOpen(DataSet: TDataSet);
    procedure UserProceduresBeforeClose(DataSet: TDataSet);
    procedure UserProceduresBeforeOpen(DataSet: TDataSet);
    procedure UserTablesAfterOpen(DataSet: TDataSet);
    procedure UserTablesBeforeClose(DataSet: TDataSet);
    procedure UserTablesBeforeOpen(DataSet: TDataSet);
    procedure WrapTextExecute(Sender: TObject);
    procedure WrapTextUpdate(Sender: TObject);
  private
    FDatabase: TIBDatabase;
    FExcludeIdentityColumns: boolean;
    FExecuteOnlyProcs: boolean;
    FIncludeReadOnlyFields: boolean;
    FIncludeSystemTables: boolean;
    FOnUserTablesOpened: TNotifyEvent;
    FOpening: boolean;
    FSelectProcs: boolean;
    procedure AddWhereClause(QuotedStrings: boolean; SQL: TStrings;
      UseOldValues: boolean);
    function GetSQLType(SQLType: TIBSQLStatementTypes): string;
    procedure GetFieldNames(Dataset: TDataset; var FieldNames: TStrings;
      aIncludeReadOnly: boolean = true);
    procedure GenerateSelectSQL(TableName: string; QuotedStrings: boolean; FieldNames,PrimaryKeyNames, SQL: TStrings); overload;
    procedure GenerateInsertSQL(TableName: string; QuotedStrings: boolean;
      FieldNames, ReadOnlyFieldNames,  SQL: TStrings); overload;
    procedure GenerateModifySQL(TableName: string; QuotedStrings: boolean;
      FieldNames, ReadOnlyFieldNames, SQL: TStrings); overload;
    procedure GenerateExecuteSQL(ProcName: string; QuotedStrings: boolean; ExecuteOnly: boolean;
              InputParams, OutputParams, ExecuteSQL: TStrings); overload;
    procedure GenerateDeleteSQL(TableName: string; QuotedStrings: boolean; SQL: TStrings); overload;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetExcludeIdentityColumns(AValue: boolean);
    procedure SetExecuteOnlyProcs(AValue: boolean);
    procedure SetIncludeReadOnlyFields(AValue: boolean);
    procedure SetIncludeSystemTables(AValue: boolean);
    procedure SetSelectProcs(AValue: boolean);

  public
    constructor Create(aOwner: TComponent); override;
    procedure DoWrapText(Lines: TStrings); overload;
    procedure DoWrapText; overload;
    procedure UnWrapText;
    procedure RefreshAll;
    procedure SelectAllFields(Checked: boolean);
    procedure GenerateSelectSQL(QuotedStrings: boolean); overload;
    procedure GenerateSelectSQL(QuotedStrings: boolean; SQL: TStrings; AddReadOnlyFields: boolean = false); overload;
    procedure GenerateRefreshSQL(QuotedStrings: boolean);
    procedure GenerateRefreshSQL(QuotedStrings: boolean; SQL: TStrings; AddReadOnlyFields: boolean = false);
    procedure GenerateExecuteSQL(QuotedStrings: boolean); overload;
    procedure GenerateInsertSQL(QuotedStrings: boolean); overload;
    procedure GenerateInsertSQL(QuotedStrings: boolean; SQL: TStrings); overload;
    procedure GenerateModifySQL(QuotedStrings: boolean; aIncludePrimaryKeys: boolean); overload;
    procedure GenerateModifySQL(QuotedStrings: boolean; SQL: TStrings; aIncludePrimaryKeys: boolean); overload;
    procedure GenerateDeleteSQL(QuotedStrings: boolean); overload;
    procedure GenerateDeleteSQL(QuotedStrings: boolean; SQL: TStrings); overload;
    function GetStatementType(var IsStoredProcedure: boolean): TIBSQLStatementTypes;
    procedure InsertSelectedPrimaryKey;
    procedure InsertSelectedFieldName;
    procedure InsertTableName;
    procedure InsertProcName;
    procedure InsertSelectedInputParam;
    procedure InsertSelectedOutputParam;
    function SyncQueryBuilder: TIBSQLStatementTypes; overload;
    function SyncQueryBuilder(SQL: TStrings): TIBSQLStatementTypes; overload;
    procedure TestSQL(GenerateParamNames: boolean);
    property Database: TIBDatabase read FDatabase write SetDatabase;
    property IncludeReadOnlyFields: boolean read FIncludeReadOnlyFields write SetIncludeReadOnlyFields;
    property IncludeSystemTables: boolean read FIncludeSystemTables write SetIncludeSystemTables;
    property ExcludeIdentityColumns: boolean read FExcludeIdentityColumns write SetExcludeIdentityColumns;
    property ExecuteOnlyProcs: boolean read FExecuteOnlyProcs write SetExecuteOnlyProcs;
    property SelectProcs: boolean read FSelectProcs write SetSelectProcs;
    property OnUserTablesOpened: TNotifyEvent read FOnUserTablesOpened write FOnUserTablesOpened;
  end;

implementation

Uses IBUtils, FBMessages, Variants;

{$R *.lfm}

const
  sNoPrimaryKeys = 'RF.RDB$FIELD_NAME not in ' +
                    '(Select RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS S JOIN RDB$RELATION_CONSTRAINTS C On C.RDB$INDEX_NAME = S.RDB$INDEX_NAME '+
                     'Where C.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and C.RDB$RELATION_NAME = RF.RDB$RELATION_NAME)';

{ TIBSQLEditFrame }

procedure TIBSQLEditFrame.CutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.SelText <> '';
end;

procedure TIBSQLEditFrame.FieldNameListBeforeOpen(DataSet: TDataSet);
begin
  if not IncludeReadOnlyFields then
    (DataSet as TIBQuery).Parser.Add2WhereClause('B.RDB$COMPUTED_SOURCE is NULL');
  (DataSet as TIBQuery).Parser.Add2WhereClause(sNoPrimaryKeys);
  if ExcludeIdentityColumns and (DatabaseInfo.ODSMajorVersion >= 12) then
    (DataSet as TIBQuery).Parser.Add2WhereClause('RF.RDB$IDENTITY_TYPE is NULL');
end;

procedure TIBSQLEditFrame.IBUpdate1ApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  //do nothing - dummy to allow edits without database update
end;

procedure TIBSQLEditFrame.LoadFromFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    SQLText.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TIBSQLEditFrame.PasteExecute(Sender: TObject);
begin
  SQLText.PasteFromClipboard;
end;

procedure TIBSQLEditFrame.PasteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanPaste;
end;

procedure TIBSQLEditFrame.RedoExecute(Sender: TObject);
begin
  SQLText.Redo;
end;

procedure TIBSQLEditFrame.RedoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanRedo;
end;

procedure TIBSQLEditFrame.SaveToFileExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SQLText.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TIBSQLEditFrame.SelectAllExecute(Sender: TObject);
begin
  SQLText.SelectAll;
end;

procedure TIBSQLEditFrame.SelectAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.Lines.Count > 0;
end;

procedure TIBSQLEditFrame.UndoExecute(Sender: TObject);
begin
  SQLText.Undo;
end;

procedure TIBSQLEditFrame.UndoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanUndo;
end;

procedure TIBSQLEditFrame.UserProceduresAfterOpen(DataSet: TDataSet);
begin
  ProcInputParams.Active := true;
  ProcOutputParams.Active := true;
end;

procedure TIBSQLEditFrame.UserProceduresBeforeClose(DataSet: TDataSet);
begin
  ProcInputParams.Active := false;
  ProcOutputParams.Active := false;
end;

procedure TIBSQLEditFrame.UserProceduresBeforeOpen(DataSet: TDataSet);
begin
  if ExecuteOnlyProcs and SelectProcs then Exit;
  if ExecuteOnlyProcs then
    (DataSet as TIBQuery).Parser.Add2WhereClause('RDB$PROCEDURE_TYPE = 2');
  if SelectProcs then
    (DataSet as TIBQuery).Parser.Add2WhereClause('RDB$PROCEDURE_TYPE = 1 AND RDB$PROCEDURE_OUTPUTS > 0');
end;

procedure TIBSQLEditFrame.UserTablesAfterOpen(DataSet: TDataSet);
begin
  FieldNameList.Active := true;
  PrimaryKeys.Active := true;
  IdentityCols.Active := DatabaseInfo.ODSMajorVersion >= 12;
  ReadOnlyFields.Active := true;
  FOpening := true;
  try
    if assigned(FOnUserTablesOpened) then
      OnUserTablesOpened(self);
  finally
    FOpening := false;
  end;
end;

procedure TIBSQLEditFrame.UserTablesBeforeClose(DataSet: TDataSet);
begin
  FieldNameList.Active := false;
  PrimaryKeys.Active := false;
  IdentityCols.Active := false;
  ReadOnlyFields.Active := false;
end;

procedure TIBSQLEditFrame.UserTablesBeforeOpen(DataSet: TDataSet);
begin
  if not IncludeSystemTables then
    (DataSet as TIBQuery).Parser.Add2WhereClause('RDB$SYSTEM_FLAG = 0');
end;

procedure TIBSQLEditFrame.WrapTextExecute(Sender: TObject);
begin
  UnWrapText;
  DoWrapText;
end;

procedure TIBSQLEditFrame.WrapTextUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.Lines.Count > 0;
end;

procedure TIBSQLEditFrame.SetDatabase(AValue: TIBDatabase);
var i: integer;
begin
  if FDatabase = AValue then Exit;
  FDatabase := AValue;
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  SQLTransaction.DefaultDatabase := FDatabase;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TIBCustomDataset then
      TIBCustomDataset(Components[i]).Database := FDatabase
    else
    if Components[i] is TIBSQL then
      TIBSQL(Components[i]).Database := FDatabase
    else
    if Components[i] is TIBDatabaseInfo then
      TIBDatabaseInfo(Components[i]).Database := FDatabase;
  if (FDatabase <> nil) and FDatabase.Connected then
    SQLTransaction.Active := true;
end;

procedure TIBSQLEditFrame.SetExcludeIdentityColumns(AValue: boolean);
begin
  if FExcludeIdentityColumns = AValue then Exit;
  FExcludeIdentityColumns := AValue;
  RefreshAll;
end;

procedure TIBSQLEditFrame.SetExecuteOnlyProcs(AValue: boolean);
begin
  if FExecuteOnlyProcs = AValue then Exit;
  FExecuteOnlyProcs := AValue;
  RefreshAll;
end;

procedure TIBSQLEditFrame.SetIncludeReadOnlyFields(AValue: boolean);
begin
  if FIncludeReadOnlyFields = AValue then Exit;
  FIncludeReadOnlyFields := AValue;
  RefreshAll;
end;

procedure TIBSQLEditFrame.SetIncludeSystemTables(AValue: boolean);
begin
  if FIncludeSystemTables = AValue then Exit;
  FIncludeSystemTables := AValue;
  RefreshAll;
  SyncQueryBuilder;
end;

procedure TIBSQLEditFrame.SetSelectProcs(AValue: boolean);
begin
  if FSelectProcs = AValue then Exit;
  FSelectProcs := AValue;
  RefreshAll;
end;

constructor TIBSQLEditFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIncludeReadOnlyFields := true;
end;

procedure TIBSQLEditFrame.DoWrapText;
begin
  DoWrapText(SQLText.Lines);
  if assigned(SQLText.OnChange) then
    SQLText.OnChange(self);
end;

type
  THackedSynEdit = class(TSynEdit)
  public
    property TextArea: TLazSynTextArea read FTextArea;
  end;

procedure TIBSQLEditFrame.DoWrapText(Lines: TStrings);

var NewLines: TStringList;
    i: integer;
    MaxWidth: integer;
    MaxChars: integer;
    Line: string;
    Tokeniser: TSynSQLSyn;
    SplitAt: integer;
    SQLParam: boolean;
begin
  NewLines := TStringList.Create;
  Tokeniser := TSynSQLSyn.Create(nil); {use the highligher as a tokeniser}
  try
    Tokeniser.SQLDialect := sqlInterbase6;
    SQlText.Canvas.Font := SQLText.Font;
    with THackedSynEdit(SQLText).TextArea do
      MaxWidth := Right - Left;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      repeat
        if (Length(Line) = 0) or (SQlText.Canvas.TextWidth(Line) <= MaxWidth) then
        begin
          NewLines.Add(Line);
          break; {next line}
        end
        else
        begin
          {Need to split the line at the last complete SQL token}
          MaxChars := SQlText.Canvas.TextFitInfo(Line,MaxWidth);
          SQLParam := false;
          Tokeniser.ResetRange;
          Tokeniser.SetLine(Line,i);
          SplitAt := 0;
          while (Tokeniser.GetTokenPos < MaxChars) and not Tokeniser.GetEol do
          begin
            if not SQLParam then
              SplitAt := Tokeniser.GetTokenPos; {combine param indicator with param}
            SQLParam := Tokeniser.GetToken =  ':';
            Tokeniser.Next;
          end;

          if SplitAt <= 0 then {token overflows line}
          begin
            NewLines.Add(Line);
            break; {next line}
          end;
          NewLines.Add(system.copy(Line,1,SplitAt));
          system.Delete(Line,1,SplitAt);
        end;
      until Length(Line) = 0;
    end;
    Lines.Assign(NewLines);
  finally
    NewLines.Free;
    Tokeniser.Free;
  end;
end;

const
  Separators = [' ',#$09,',','.',':'];

procedure TIBSQLEditFrame.UnWrapText;
var Line: string;
    i: integer;
begin
  Line := '';
  with SQLText do
  begin
    for i := 0 to Lines.Count - 1 do
    begin
      if (Length(Line) > 0) and not (Line[Length(Line)] in Separators) then
        Line := Line + ' ';
     Line := Line + Lines[i];
    end;

    if assigned(OnChange) then
      OnChange(self);
    Lines.Text := Line;
  end;
end;

procedure TIBSQLEditFrame.RefreshAll;
begin
  if UserTables.Active then
  begin
    UserTables.Active := false;
    UserTables.Active := true;
  end;
  if UserProcedures.Active then
  begin
    UserProcedures.Active := false;
    UserProcedures.Active := true;
  end;
end;

procedure TIBSQLEditFrame.SelectAllFields(Checked: boolean);

  procedure DoSelectAllFields(Dataset: TDataset; aValue: boolean);
  begin
    with Dataset do
    if Active then
    begin
      DisableControls;
      try
        First;
        while not Eof do
        begin
          Edit;
          FieldByName('Selected').AsInteger := ord(aValue);
          Post;
          Next;
        end;
        First;
      finally
        EnableControls;
      end;
    end;
  end;

begin
  if FOpening or (Database = nil) or not Database.Connected then Exit;
  DoSelectAllFields(FieldNameList,Checked);
  DoSelectAllFields(PrimaryKeys,Checked);
  DoSelectAllFields(IdentityCols,Checked);
  DoSelectAllFields(ReadOnlyFields,Checked);
end;

procedure TIBSQLEditFrame.GenerateSelectSQL(QuotedStrings: boolean);
begin
  GenerateSelectSQL(QuotedStrings,SQLText.Lines);
end;

procedure TIBSQLEditFrame.GenerateRefreshSQL(QuotedStrings: boolean);
begin
  GenerateRefreshSQL(QuotedStrings,SQLText.Lines);
end;

procedure TIBSQLEditFrame.GenerateSelectSQL(QuotedStrings: boolean;
  SQL: TStrings; AddReadOnlyFields: boolean);
var FieldNames: TStrings;
    PrimaryKeyNames: TStrings;
    ReadOnlyFieldNames: TStrings;
begin
  SQL.Clear;
  FieldNames := TStringList.Create;
  PrimaryKeyNames := TStringList.Create;
  ReadOnlyFieldNames := TStringList.Create;
  try
    GetFieldNames(PrimaryKeys,PrimaryKeyNames);
    GetFieldNames(FieldNameList,FieldNames);
    if not IncludeReadOnlyFields and AddReadOnlyFields then
    begin
      GetFieldNames(ReadOnlyFields,ReadOnlyFieldNames,true);
      FieldNames.AddStrings(ReadOnlyFieldNames);
    end;
    GenerateSelectSQL(UserTables.FieldByName('RDB$RELATION_NAME').AsString,QuotedStrings,FieldNames,PrimaryKeyNames,SQL);
  finally
    FieldNames.Free;
    PrimaryKeyNames.Free;
    ReadOnlyFieldNames.Free;
  end;
  DoWrapText(SQL);
end;

procedure TIBSQLEditFrame.GenerateRefreshSQL(QuotedStrings: boolean;
  SQL: TStrings; AddReadOnlyFields: boolean);
begin
  SQL.Clear;
  GenerateSelectSQL(QuotedStrings,SQL,AddReadOnlyFields);
  AddWhereClause(QuotedStrings,SQL,false);
end;

procedure TIBSQLEditFrame.GenerateExecuteSQL(QuotedStrings: boolean);
var InputParams: TStrings;
    OutputParams: TStrings;
begin
  SQLText.Lines.Clear;

  InputParams := TStringList.Create;
  OutputParams := TStringList.Create;
  try
    GetFieldNames(ProcInputParams,InputParams);
    GetFieldNames(ProcOutputParams,OutputParams);
    GenerateExecuteSQL(UserProcedures.FieldByName('RDB$PROCEDURE_NAME').AsString,
      QuotedStrings, UserProcedures.FieldByName('RDB$PROCEDURE_TYPE').AsInteger = 2,
      InputParams,OutputParams,SQLText.Lines);
  finally
    InputParams.Free;
    OutputParams.Free;
  end;
end;

procedure TIBSQLEditFrame.GenerateInsertSQL(QuotedStrings: boolean);
begin
  GenerateInsertSQL(QuotedStrings,SQLText.Lines);
end;

procedure TIBSQLEditFrame.GenerateModifySQL(QuotedStrings: boolean;
  aIncludePrimaryKeys: boolean);
begin
  GenerateModifySQL(QuotedStrings,SQLText.Lines,aIncludePrimaryKeys);
end;

procedure TIBSQLEditFrame.GenerateDeleteSQL(QuotedStrings: boolean);
begin
  GenerateDeleteSQL(QuotedStrings,SQLText.Lines);
end;

procedure TIBSQLEditFrame.GenerateInsertSQL(QuotedStrings: boolean; SQL: TStrings);
var FieldNames: TStrings;
    ReadOnlyFieldNames: TStrings;
    InsertFields: TStrings;
    I: integer;
begin
  SQL.Clear;
  FieldNames := TStringList.Create;
  ReadOnlyFieldNames := TStringList.Create;
  InsertFields := TStringList.Create;
  try
    GetFieldNames(PrimaryKeys,InsertFields);
    for I := InsertFields.Count - 1 downto 0 do
      if IdentityCols.Active and IdentityCols.Locate('ColumnName;Selected',VarArrayOf([InsertFields[I],1]),[loCaseInsensitive]) then
        InsertFields.Delete(I);
    GetFieldNames(FieldNameList,FieldNames,false);
    InsertFields.AddStrings(FieldNames);
    GetFieldNames(ReadOnlyFields,ReadOnlyFieldNames,true);
    GenerateInsertSQL(UserTables.FieldByName('RDB$RELATION_NAME').AsString,QuotedStrings,InsertFields,ReadOnlyFieldNames,SQL);
  finally
    FieldNames.Free;
    ReadOnlyFieldNames.Free;
    InsertFields.Free;
  end;
  DoWrapText(SQL);
end;

procedure TIBSQLEditFrame.GenerateModifySQL(QuotedStrings: boolean;
  SQL: TStrings; aIncludePrimaryKeys: boolean);
var FieldNames: TStrings;
    ReadOnlyFieldNames: TStrings;
    UpdateFields: TStrings;
    i, index: integer;
begin
  SQL.Clear;
  FieldNames := TStringList.Create;
  ReadOnlyFieldNames := TStringList.Create;
  UpdateFields := TStringList.Create;
  try
    if aIncludePrimaryKeys then
      GetFieldNames(PrimaryKeys,UpdateFields);
    GetFieldNames(FieldNameList,FieldNames,false);
    UpdateFields.AddStrings(FieldNames);
    GetFieldNames(ReadOnlyFields,ReadOnlyFieldNames,true);
    GenerateModifySQL(UserTables.FieldByName('RDB$RELATION_NAME').AsString,
           QuotedStrings,UpdateFields,ReadOnlyFieldNames,SQL);
  finally
    FieldNames.Free;
    ReadOnlyFieldNames.Free;
    UpdateFields.Free;
  end;
end;

procedure TIBSQLEditFrame.GenerateDeleteSQL(QuotedStrings: boolean; SQL: TStrings);
begin
  SQL.Clear;
  GenerateDeleteSQL(UserTables.FieldByName('RDB$RELATION_NAME').AsString,QuotedStrings,SQL)
end;

procedure TIBSQLEditFrame.CutExecute(Sender: TObject);
begin
  SQLText.CutToClipboard;
end;

procedure TIBSQLEditFrame.CopyTextExecute(Sender: TObject);
begin
  SQLText.CopyToClipboard;
end;

procedure TIBSQLEditFrame.ClearExecute(Sender: TObject);
begin
  SQLText.Lines.Clear;
end;

procedure TIBSQLEditFrame.AddWhereClause(
  QuotedStrings: boolean; SQL: TStrings; UseOldValues: boolean);
var WhereClause: string;
    Separator: string;
    Count: integer;
    Prefix: string;
    ColumnName: string;
begin
  Count := 0;
  WhereClause := 'Where';
  Separator := ' A.';
  if UseOldValues then
    Prefix := ':OLD_'
  else
    Prefix := ':';
  with PrimaryKeys do
  begin
    DisableControls;
    try
      if State = dsEdit then Post;
      First;
      while not EOF do
      begin
        if FieldByName('Selected').AsInteger <> 0 then
        begin
          Inc(Count);
          ColumnName := FieldByName('ColumnName').AsString;
          if QuotedStrings then
            WhereClause := WhereClause + Separator + '"' + ColumnName +
                   '" = ' + Prefix+ AnsiUpperCase(ColumnName)
          else
            WhereClause := WhereClause + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,ColumnName) +
                   ' = ' + Prefix + AnsiUpperCase(ColumnName);
          Separator := ' AND A.';
        end;
        Next;
      end;
    finally
      EnableControls
    end;
  end;
  if Count > 0 then
    SQL.Add(WhereClause);
end;

function TIBSQLEditFrame.GetSQLType(SQLType: TIBSQLStatementTypes): string;
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

procedure TIBSQLEditFrame.GetFieldNames(Dataset: TDataset;
  var FieldNames: TStrings; aIncludeReadOnly: boolean);
begin
  with DataSet do
  begin
    DisableControls;
    try
      if State = dsEdit then Post;
      First;
      while not EOF do
      begin
        if (FieldByName('Selected').AsInteger <> 0) and (aIncludeReadOnly or (FieldByName('ReadOnly').AsInteger = 0)) then
          FieldNames.Add(FieldByName('ColumnName').AsString);
        Next;
      end;
    finally
      EnableControls
    end;
  end;
end;

procedure TIBSQLEditFrame.GenerateSelectSQL(TableName: string;
  QuotedStrings: boolean; FieldNames, PrimaryKeyNames, SQL: TStrings);
var SelectSQL: string;
    Separator : string;
    I: integer;
    Lines: TStrings;
begin
  SelectSQL := 'Select';
  Separator := ' A.';
  for I := 0 to PrimaryKeyNames.Count - 1 do
  begin
    if QuotedStrings then
      SelectSQL := SelectSQL + Separator + '"' + PrimaryKeyNames[I] + '"'
    else
      SelectSQL := SelectSQL + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,PrimaryKeyNames[I]);
    Separator := ', A.';
  end;
  for I := 0 to FieldNames.Count - 1 do
  begin
    if QuotedStrings then
      SelectSQL := SelectSQL + Separator + '"' + FieldNames[I] + '"'
    else
      SelectSQL := SelectSQL + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,FieldNames[I]);
    Separator := ', A.';
  end;
  SelectSQL := SelectSQL + ' From ' + TableName + ' A';
  Lines := TStringList.Create;
  try
    Lines.Text := SelectSQL;
    SQL.AddStrings(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TIBSQLEditFrame.GenerateInsertSQL(TableName: string;
  QuotedStrings: boolean; FieldNames, ReadOnlyFieldNames, SQL: TStrings);
var InsertSQL: string;
    Separator: string;
    Lines: TStrings;
    I: integer;
begin
  Lines := TStringList.Create;
  try
    InsertSQL := 'Insert Into ' + TableName + '(';
    Separator := '';
    for I := 0 to FieldNames.Count - 1 do
      begin
        if QuotedStrings then
           InsertSQL := InsertSQL + Separator + '"' + FieldNames[I] + '"'
        else
           InsertSQL := InsertSQL + Separator +  QuoteIdentifierIfNeeded(Database.SQLDialect,FieldNames[I]) ;
        Separator := ', ';
      end;
    InsertSQL := InsertSQL + ')';
    Lines.Add(InsertSQL);
    InsertSQL := 'Values(';
    Separator := ':';
    for I := 0 to FieldNames.Count - 1 do
      begin
         InsertSQL := InsertSQL + Separator +  AnsiUpperCase(FieldNames[I]) ;
         Separator := ', :';
      end;
    InsertSQL := InsertSQL + ')';
    Lines.Add(InsertSQL);
    InsertSQL := '';
    Separator := ' RETURNING ';
    if IdentityCols.Active and (IdentityCols.RecordCount > 0) then
    begin
      IdentityCols.First;
      while not IdentityCols.Eof do
      begin
        if (IdentityCols.FieldByName('Selected').AsInteger <> 0) and
           (not PrimaryKeys.Active or not PrimaryKeys.Locate('columnName;Selected',
                VarArrayOf([IdentityCols.FieldByName('ColumnName').AsString,0]),[loCaseInsensitive])) then
        begin
          InsertSQL := InsertSQL + Separator + IdentityCols.FieldByName('ColumnName').AsString;
          Separator := ', ';
        end;
        IdentityCols.Next;
      end;
    end;
    for I := 0 to ReadOnlyFieldNames.Count - 1 do
      begin
        if QuotedStrings then
          InsertSQL := InsertSQL + Separator + '"' + ReadOnlyFieldNames[I] + '"'
        else
          InsertSQL := InsertSQL + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,ReadOnlyFieldNames[I]);
        Separator := ', ';
      end;
    Lines.Add(InsertSQL);
    SQL.AddStrings(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TIBSQLEditFrame.GenerateModifySQL(TableName: string;
  QuotedStrings: boolean; FieldNames, ReadOnlyFieldNames, SQL: TStrings);
var UpdateSQL: string;
    Separator: string;
    I: integer;
begin
  Separator := '  A.';
  UpdateSQL := 'Update ' + TableName + ' A Set ';
  SQL.Add(UpdateSQL);
  for I := 0 to FieldNames.Count - 1 do
    begin
      if QuotedStrings then
        UpdateSQL := Separator + '"' + FieldNames[I] + '" = :' + AnsiUpperCase(FieldNames[I])
      else
        UpdateSQL := Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,FieldNames[I]) + ' = :' + AnsiUpperCase(FieldNames[I]);
      if I < FieldNames.Count - 1 then
        UpdateSQL := UpdateSQL + ',';
      SQL.Add(UpdateSQL);
    end;
  AddWhereClause(QuotedStrings,SQL,true);
  Separator := ' RETURNING A.';
  UpdateSQL := '';
  for I := 0 to ReadOnlyFieldNames.Count - 1 do
    begin
      if QuotedStrings then
        UpdateSQL := UpdateSQL + Separator + '"' + ReadOnlyFieldNames[I] + '"'
      else
        UpdateSQL := UpdateSQL + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,ReadOnlyFieldNames[I]);
      Separator := ', A.';
    end;
  SQL.Add(UpdateSQL);
end;

procedure TIBSQLEditFrame.GenerateDeleteSQL(TableName: string;
  QuotedStrings: boolean; SQL: TStrings);
begin
  SQL.Add('Delete From ' + TableName + ' A');
  AddWhereClause(QuotedStrings,SQL,true)
end;

const
  sqlCheckProcedureNames = 'Select * From RDB$PROCEDURES Where Upper(Trim(RDB$PROCEDURE_NAME)) = Upper(:ProcName)';

function TIBSQLEditFrame.GetStatementType(var IsStoredProcedure: boolean
  ): TIBSQLStatementTypes;
var TableName: string;
begin
  Result := sqlUnknown;
  if not assigned(Database) or not Database.Connected or (Trim(SQLText.Lines.Text) = '') then
    Exit;
  IsStoredProcedure := false;
  with TIBSQL.Create(nil) do
  try
    Database := self.Database;
    Transaction := SQLTransaction;
    SQL.Assign(SQLText.Lines);
    GenerateParamNames := true; {permissive}
    try
      Prepare;
      Result := SQLStatementType
    except on E:EIBError do
  //      ShowMessage(E.Message);
    end;
    if (Result = SQLSelect) and (MetaData.Count > 0)  then
    begin
      TableName := MetaData[0].GetRelationName;
      SQL.Text := sqlCheckProcedureNames;
      Prepare;
      ParamByName('ProcName').AsString := TableName;
      ExecQuery;
      try
        IsStoredProcedure := not EOF;
      finally
        Close
      end;
    end;
  finally
    Free
  end;
end;

procedure TIBSQLEditFrame.GenerateExecuteSQL(ProcName: string;
  QuotedStrings: boolean; ExecuteOnly: boolean; InputParams, OutputParams,
  ExecuteSQL: TStrings);
var SQL: string;
    I: integer;
    Separator: string;
    Lines: TStrings;
begin
  Lines := TStringList.Create;
  try
    Separator := '';
    if not ExecuteOnly and (OutputParams.Count > 0) then //Select Query
    begin
      SQL := 'Select ';
      for I := 0 to OutputParams.Count - 1 do
      begin
        if QuotedStrings then
          SQL := SQL + Separator + '"' + OutputParams[I] + '"'
        else
          SQL := SQL + Separator + QuoteIdentifierIfNeeded(Database.SQLDialect,OutputParams[I]);
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
        SQL := 'Execute Procedure ' + QuoteIdentifierIfNeeded(Database.SQLDialect,ProcName);
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
    Lines.Add(SQL + ';');
    ExecuteSQL.AddStrings(Lines);
  finally
    Lines.Free
  end
end;

procedure TIBSQLEditFrame.InsertSelectedPrimaryKey;
begin
  SQLText.SelText := PrimaryKeys.FieldByName('ColumnName').AsString;
  SQLText.SetFocus
end;

procedure TIBSQLEditFrame.InsertSelectedFieldName;
begin
  SQLText.SelText := FieldNameList.FieldByName('ColumnName').AsString;
  SQLText.SetFocus
end;

procedure TIBSQLEditFrame.InsertTableName;
begin
  SQLText.SelText := UserTables.FieldByName('RDB$RELATION_NAME').AsString;
  SQLText.SetFocus
end;

procedure TIBSQLEditFrame.InsertProcName;
begin
  SQLText.SelText := UserProcedures.FieldByName('RDB$PROCEDURE_NAME').AsString;
  SQLText.SetFocus
end;

procedure TIBSQLEditFrame.InsertSelectedInputParam;
begin
  SQLText.SelText := ProcInputParams.FieldByName('ColumnName').AsString;
  SQLText.SetFocus
end;

procedure TIBSQLEditFrame.InsertSelectedOutputParam;
begin
  SQLText.SelText := ProcOutputParams.FieldByName('ColumnName').AsString;
  SQLText.SetFocus
end;

function GetWord(S: string; WordNo: integer): string;
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

function TIBSQLEditFrame.SyncQueryBuilder: TIBSQLStatementTypes;
begin
  Result := SyncQueryBuilder(SQLText.Lines);
end;

function TIBSQLEditFrame.SyncQueryBuilder(SQL: TStrings): TIBSQLStatementTypes;
var TableName: string;
begin
  if (Database = nil) or not Database.Connected then Exit;

  Result := SQLUnknown;
  TableName := '';
  try
    IdentifyStatementSQL.Transaction.Active := true;
    IdentifyStatementSQL.SQL.Assign(SQL);
    IdentifyStatementSQL.Prepare;
    Result := IdentifyStatementSQL.SQLStatementType;
    case Result  of
    SQLSelect:
      begin
        if IdentifyStatementSQL.MetaData.Count > 0 then
          TableName := IdentifyStatementSQL.MetaData[0].GetRelationName
        else
          Exit;
        UserTables.Locate('RDB$RELATION_NAME',TableName,[loCaseInsensitive]);
      end;
    { If not a select statement then return table or procedure name
      as First Table Name }
    SQLUpdate:
      begin
        TableName := GetWord(IdentifyStatementSQL.SQL.Text,2);
        UserTables.Locate('RDB$RELATION_NAME',TableName,[loCaseInsensitive]);
      end;

    SQLInsert:
      begin
        TableName := GetWord(IdentifyStatementSQL.SQL.Text,3);
        UserTables.Locate('RDB$RELATION_NAME',TableName,[loCaseInsensitive]);
      end;

    SQLDelete:
      begin
        TableName := GetWord(IdentifyStatementSQL.SQL.Text,3);
        UserTables.Locate('RDB$RELATION_NAME',TableName,[loCaseInsensitive]);
      end;

    SQLExecProcedure:
      begin
        TableName := GetWord(IdentifyStatementSQL.SQL.Text,3);
        UserProcedures.Locate('RDB$PROCEDURE_NAME',TableName,[loCaseInsensitive]);
      end;
    end
  except on E:EIBError do
//      ShowMessage(E.Message);
  end;
end;

procedure TIBSQLEditFrame.TestSQL(GenerateParamNames: boolean);
begin
  if not assigned(Database) or not Database.Connected then
  begin
    Messagedlg('No Database Connected',mtError,[mbOK],0);
    Exit;
  end;
  with TIBSQL.Create(nil) do
  try
    Database := self.Database;
    Transaction := SQLTransaction;
    GenerateParamNames := GenerateParamNames;
    SQL.Assign(SQLText.Lines);
    try
      Prepare;
      ShowMessage('SQL '+ GetSQLType(SQLStatementType) + ' Statement Looks OK');
    except on E:EIBError do
      ShowMessage(E.Message);
    end;
  finally
    Free
  end;
end;


end.

