unit IBSQLEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IBSystemTables, IBSQL, IBDatabase;

type

  { TIBSQLEditorForm }

  TIBSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    IncludePrimaryKeys: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SelectFieldsList: TListBox;
    ProcOutputList: TListBox;
    SelectPrimaryKeys: TListBox;
    InsertFieldsList: TListBox;
    ModifyFieldsList: TListBox;
    ModifyPrimaryKeys: TListBox;
    DeletePrimaryKeys: TListBox;
    ProcInputList: TListBox;
    PageControl: TPageControl;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    SelectPage: TTabSheet;
    InsertPage: TTabSheet;
    ModifyPage: TTabSheet;
    DeletePage: TTabSheet;
    ExecutePage: TTabSheet;
    SelectTableNames: TComboBox;
    InsertTableNames: TComboBox;
    ModifyTableNames: TComboBox;
    DeleteTableNames: TComboBox;
    ProcedureNames: TComboBox;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure DeletePageShow(Sender: TObject);
    procedure DeleteTableNamesCloseUp(Sender: TObject);
    procedure ExecutePageShow(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IncludePrimaryKeysClick(Sender: TObject);
    procedure InsertPageShow(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure ModifyPageShow(Sender: TObject);
    procedure ModifyTableNamesCloseUp(Sender: TObject);
    procedure ProcedureNamesCloseUp(Sender: TObject);
    procedure SelectFieldsListDblClick(Sender: TObject);
    procedure SelectPageShow(Sender: TObject);
    procedure SelectTableNamesCloseUp(Sender: TObject);
    procedure InsertTableNamesCloseUp(Sender: TObject);
  private
    { private declarations }
    FTableName: string;
    FIBSystemTables: TIBSystemTables;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
  end;

var
  IBSQLEditorForm: TIBSQLEditorForm;

function EditIBSQL(DataSet: TIBSQL): boolean;

implementation

function EditIBSQL(DataSet: TIBSQL): boolean;
begin
  Result := false;
  if assigned(DataSet) and assigned(DataSet.Database) then
  begin
    if not assigned(DataSet.Transaction) then
    begin
      ShowMessage('No Default Transaction!');
      Exit
    end;

    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  end;

  with TIBSQLEditorForm.Create(Application) do
  try
    SetDatabase(DataSet.Database,DataSet.Transaction);
    SQLText.Lines.Assign(DataSet.SQL);
    Result := ShowModal = mrOK;
    if Result then
      DataSet.SQL.Assign(SQLText.Lines)
  finally
    Free
  end;

end;

{ TIBSQLEditorForm }

procedure TIBSQLEditorForm.FormShow(Sender: TObject);
var IsProcedureName: boolean;
begin
  if Trim(SQLText.Text) = '' then
  begin
    case FIBSystemTables.GetStatementType(SQLText.Text,IsProcedureName) of
    SQLSelect:
      if IsProcedureName then
        PageControl.ActivePage := ExecutePage
      else
        PageControl.ActivePage := SelectPage;
    SQLInsert:  PageControl.ActivePage := InsertPage;
    SQLUpdate:  PageControl.ActivePage := ModifyPage;
    SQLDelete:  PageControl.ActivePage := DeletePage;
    SQLExecProcedure: PageControl.ActivePage := ExecutePage;
    end;
    FIBSystemTables.GetTableAndColumns(SQLText.Text,FTableName,nil)
  end;
end;

procedure TIBSQLEditorForm.IncludePrimaryKeysClick(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.DeletePageShow(Sender: TObject);
var TableName: string;
begin
  FIBSystemTables.GetTableNames(DeleteTableNames.Items);
  if Trim(SQLText.Text) = '' then
  begin
    if FTableName <> '' then
      DeleteTableNames.ItemIndex := DeleteTableNames.Items.IndexOf(FTableName)
    else
    if DeleteTableNames.Items.Count > 0 then
      DeleteTableNames.ItemIndex := 0
  end
  else
  begin
    FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
    DeleteTableNames.ItemIndex := DeleteTableNames.Items.IndexOf(TableName);
  end;
  FIBSystemTables.GetPrimaryKeys(DeleteTableNames.Text,DeletePrimaryKeys.Items);

end;

procedure TIBSQLEditorForm.Button3Click(Sender: TObject);
var FieldNames: TStrings;
begin
  FieldNames := nil;
  if PageControl.ActivePage = SelectPage then
  begin
    FieldNames := FIBSystemTables.GetFieldNames(SelectFieldsList);
    FIBSystemTables.GenerateSelectSQL(SelectTableNames.Text,QuoteFields.Checked,FieldNames,SQLText.Lines);
  end
  else
  if PageControl.ActivePage = InsertPage then
  begin
    FieldNames := FIBSystemTables.GetFieldNames(InsertFieldsList);
    FIBSystemTables.GenerateInsertSQL(InsertTableNames.Text,QuoteFields.Checked,FieldNames,SQLText.Lines);
  end
  else
  if PageControl.ActivePage = ModifyPage then
  begin
    FieldNames := FIBSystemTables.GetFieldNames(ModifyFieldsList);
    FIBSystemTables.GenerateModifySQL(ModifyTableNames.Text,QuoteFields.Checked,FieldNames,SQLText.Lines);
  end
  else
  if PageControl.ActivePage = DeletePage then
     FIBSystemTables.GenerateDeleteSQL(DeleteTableNames.Text,QuoteFields.Checked,SQLText.Lines)
  else
  if PageControl.ActivePage = ExecutePage then
     FIBSystemTables.GenerateExecuteSQL(ProcedureNames.Text,QuoteFields.Checked,
             ProcInputList.Items,ProcOutputList.Items,SQLText.Lines);

  if FieldNames <> nil then
    FieldNames.Free
end;

procedure TIBSQLEditorForm.Button4Click(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Text);
end;

procedure TIBSQLEditorForm.DeleteTableNamesCloseUp(Sender: TObject);
begin
  FTableName := DeleteTableNames.Text;
  FIBSystemTables.GetPrimaryKeys(DeleteTableNames.Text,DeletePrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ExecutePageShow(Sender: TObject);
var ProcName: string;
    IsProcedureName: boolean;
begin
  FIBSystemTables.GetProcedureNames(ProcedureNames.Items);
  if ProcedureNames.Items.Count > 0 then
  begin
    if (FIBSystemTables.GetStatementType(SQLText.Text,IsProcedureName) = SQLExecProcedure) or IsProcedureName then
    begin
      FIBSystemTables.GetTableAndColumns(SQLText.Text,ProcName,nil);
      ProcedureNames.ItemIndex := ProcedureNames.Items.IndexOf(ProcName)
    end
    else
      ProcedureNames.ItemIndex := 0;
  end;
  FIBSystemTables.GetProcParams(ProcedureNames.Text,ProcInputList.Items,ProcOutputList.Items);
end;

procedure TIBSQLEditorForm.InsertPageShow(Sender: TObject);
var TableName: string;
begin
  FIBSystemTables.GetTableNames(InsertTableNames.Items);
  if Trim(SQLText.Text) = '' then
  begin
    if FTableName <> '' then
      InsertTableNames.ItemIndex := InsertTableNames.Items.IndexOf(FTableName)
    else
    if InsertTableNames.Items.Count > 0 then
      InsertTableNames.ItemIndex := 0
  end
  else
  begin
    FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
    InsertTableNames.ItemIndex := InsertTableNames.Items.IndexOf(TableName);
  end;
  FIBSystemTables.GetFieldNames(InsertTableNames.Text,InsertFieldsList.Items);

end;

procedure TIBSQLEditorForm.Label13Click(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ModifyPageShow(Sender: TObject);
var TableName: string;
begin
   FIBSystemTables.GetTableNames(ModifyTableNames.Items);
  if Trim(SQLText.Text) = '' then
  begin
    if FTableName <> '' then
      ModifyTableNames.ItemIndex := ModifyTableNames.Items.IndexOf(FTableName)
    else
    if ModifyTableNames.Items.Count > 0 then
      ModifyTableNames.ItemIndex := 0;
  end
  else
  begin
    FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
    ModifyTableNames.ItemIndex := ModifyTableNames.Items.IndexOf(TableName);
  end;
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ModifyTableNamesCloseUp(Sender: TObject);
begin
  FTableName := ModifyTableNames.Text;
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ProcedureNamesCloseUp(Sender: TObject);
begin
    FIBSystemTables.GetProcParams(ProcedureNames.Text,ProcInputList.Items,ProcOutputList.Items);
end;

procedure TIBSQLEditorForm.SelectFieldsListDblClick(Sender: TObject);
begin
  SQLText.SelText:= (Sender as TListBox).Items[(Sender as TListBox).ItemIndex];
end;

procedure TIBSQLEditorForm.SelectPageShow(Sender: TObject);
var TableName: string;
begin
  FIBSystemTables.GetTableNames(SelectTableNames.Items);
  if Trim(SQLText.Text) = '' then
  begin
    if FTableName <> '' then
      SelectTableNames.ItemIndex := SelectTableNames.Items.IndexOf(FTableName)
    else
    if SelectTableNames.Items.Count > 0 then
      SelectTableNames.ItemIndex := 0;
  end
  else
  begin
    FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
    SelectTableNames.ItemIndex := SelectTableNames.Items.IndexOf(TableName);
  end;
  FIBSystemTables.GetFieldNames(SelectTableNames.Text,SelectFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(SelectTableNames.Text,SelectPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.SelectTableNamesCloseUp(Sender: TObject);
begin
  FTableName := SelectTableNames.Text;
  FIBSystemTables.GetFieldNames(SelectTableNames.Text,SelectFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(SelectTableNames.Text,SelectPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.InsertTableNamesCloseUp(Sender: TObject);
begin
  FTableName := InsertTableNames.Text;
  FIBSystemTables.GetFieldNames(InsertTableNames.Text,InsertFieldsList.Items);
end;

constructor TIBSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
end;

destructor TIBSQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

procedure TIBSQLEditorForm.SetDatabase(Database: TIBDatabase;
  Transaction: TIBTransaction);
begin
  FIBSystemTables.SelectDatabase(Database,Transaction)
end;

initialization
  {$I ibsqleditor.lrs}

end.

