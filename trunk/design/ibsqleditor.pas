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
unit IBSQLEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IBSystemTables, IBSQL, IBDatabase, IB,
  LCLVersion;

type

  { TIBSQLEditorForm }

  TIBSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GenerateParams: TCheckBox;
    GenerateBtn: TButton;
    IBTransaction1: TIBTransaction;
    Label5: TLabel;
    SelectProcedure: TLabel;
    TestBtn: TButton;
    IncludePrimaryKeys: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
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
    procedure GenerateBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
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
    FExecuteOnly: boolean;
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabase(Database: TIBDatabase);
  end;

var
  IBSQLEditorForm: TIBSQLEditorForm;

function EditIBSQL(DataSet: TIBSQL): boolean;

implementation

{$R *.lfm}

uses InterfaceBase
  {$if lcl_fullversion >= 01070000}, LCLPlatformDef {$ENDIF};

function EditIBSQL(DataSet: TIBSQL): boolean;
begin
  Result := false;
  if assigned(DataSet.Database)  then
    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBSQLEditorForm.Create(Application) do
  try
    SetDatabase(DataSet.Database);
    SQLText.Lines.Assign(DataSet.SQL);
    GenerateParams.Checked := DataSet.GenerateParamNames;
    Result := ShowModal = mrOK;
    if Result then
      begin
            DataSet.SQL.Assign(SQLText.Lines);
            DataSet.GenerateParamNames := GenerateParams.Checked
      end;
  finally
    Free
  end;

end;

{ TIBSQLEditorForm }

procedure TIBSQLEditorForm.FormShow(Sender: TObject);
var IsProcedureName: boolean;
begin
  if WidgetSet.LCLPlatform = lpGtk2 then
    PageControl.TabPosition := tpLeft
  else
  PageControl.TabPosition := tpTop;
  GenerateBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  TestBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  if Trim(SQLText.Text) <> '' then
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
    else
      PageControl.ActivePage := SelectPage;
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

procedure TIBSQLEditorForm.GenerateBtnClick(Sender: TObject);
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
     FIBSystemTables.GenerateExecuteSQL(ProcedureNames.Text,QuoteFields.Checked, FExecuteOnly,
             ProcInputList.Items,ProcOutputList.Items,SQLText.Lines);

  if FieldNames <> nil then
    FieldNames.Free
end;

procedure TIBSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Text,GenerateParams.Checked);
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
  FIBSystemTables.GetProcParams(ProcedureNames.Text,FExecuteOnly,ProcInputList.Items,ProcOutputList.Items);
  SelectProcedure.Visible :=  not FExecuteOnly;
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
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked,false);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ModifyTableNamesCloseUp(Sender: TObject);
begin
  FTableName := ModifyTableNames.Text;
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items,IncludePrimaryKeys.checked,false);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ProcedureNamesCloseUp(Sender: TObject);
begin
    FIBSystemTables.GetProcParams(ProcedureNames.Text,FExecuteOnly,ProcInputList.Items,ProcOutputList.Items);
    SelectProcedure.Visible := not FExecuteOnly
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
  try
  FIBSystemTables.GetFieldNames(SelectTableNames.Text,SelectFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(SelectTableNames.Text,SelectPrimaryKeys.Items);
  except {ignore}  end;
end;

procedure TIBSQLEditorForm.InsertTableNamesCloseUp(Sender: TObject);
begin
  FTableName := InsertTableNames.Text;
  FIBSystemTables.GetFieldNames(InsertTableNames.Text,InsertFieldsList.Items);
end;

procedure TIBSQLEditorForm.Loaded;
begin
  inherited Loaded;
  {$IFDEF WINDOWS}
  if assigned(PageControl) then
    PageControl.TabPosition := tpTop;
  {$ENDIF}
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

procedure TIBSQLEditorForm.SetDatabase(Database: TIBDatabase);
begin
  IBTransaction1.DefaultDatabase := Database;
  FIBSystemTables.SelectDatabase(Database,IBTransaction1)
end;

end.
