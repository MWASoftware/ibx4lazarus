unit IBSQLEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IBSystemTables;

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
    SelectTableName: TComboBox;
    InsertTableNames: TComboBox;
    ModifyTableNames: TComboBox;
    DeleteTableNames: TComboBox;
    ProcedureNames: TComboBox;
    procedure DeletePageShow(Sender: TObject);
    procedure DeleteTableNamesCloseUp(Sender: TObject);
    procedure ExecutePageShow(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InsertPageShow(Sender: TObject);
    procedure ModifyPageShow(Sender: TObject);
    procedure ModifyTableNamesCloseUp(Sender: TObject);
    procedure SelectFieldsListDblClick(Sender: TObject);
    procedure SelectPageShow(Sender: TObject);
    procedure SelectTableNameCloseUp(Sender: TObject);
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

implementation

{ TIBSQLEditorForm }

procedure TIBSQLEditorForm.FormShow(Sender: TObject);
begin
  if SQLText.Text <> '' then
  case FIBSystemTables.GetStatementType(SQLText.Text) of
  SQLSelect:  PageControl.ActivePage := SelectPage;
  SQLInsert:  PageControl.ActivePage := InsertPage;
  SQLUpdate:  PageControl.ActivePage := ModifyPage;
  SQLDelete:  PageControl.ActivePage := DeletePage;
  SQLExecProcedure: PageControl.ActivePage := ExecutePage;
  end;
  FIBSystemTables.GetTableAndColumns(SQLText.Text,FTableName,nil)
end;

procedure TIBSQLEditorForm.DeletePageShow(Sender: TObject);
begin
   FIBSystemTables.GetTableNames(DeleteTableNames);
  if FTableName <> '' then
    DeleteTableNames.ItemIndex := DeleteTableNames.Items.IndexOf(FTableName);
  FIBSystemTables.GetPrimaryKeys(DeleteTableNames.Text,DeletePrimaryKeys.Items);

end;

procedure TIBSQLEditorForm.DeleteTableNamesCloseUp(Sender: TObject);
begin
  FTableName := DeleteTableNames.Text;
  FIBSystemTables.GetPrimaryKeys(DeleteTableNames.Text,DeletePrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ExecutePageShow(Sender: TObject);
begin
  FIBSystemTables.GetProcedureNames(ProcedureNames);
  if ProcedureNames.Items.Count > 0 then
    ProcedureNames.ItemIndex := 0;
  FIBSystemTables.GetProcParams(ProcedureNames.Text,ProcInputList,ProcOutputList);
end;

procedure TIBSQLEditorForm.InsertPageShow(Sender: TObject);
begin
  FIBSystemTables.GetTableNames(InsertTableNames);
  if FTableName <> '' then
    InsertTableNames.ItemIndex := InsertTableNames.Items.IndexOf(FTableName);
  FIBSystemTables.GetFieldNames(InsertTableNames.Text,InsertFieldsList.Items);

end;

procedure TIBSQLEditorForm.ModifyPageShow(Sender: TObject);
begin
   FIBSystemTables.GetTableNames(ModifyTableNames);
  if FTableName <> '' then
    ModifyTableNames.ItemIndex := ModifyTableNames.Items.IndexOf(FTableName);
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.ModifyTableNamesCloseUp(Sender: TObject);
begin
  FTableName := ModifyTableNames.Text;
  FIBSystemTables.GetFieldNames(ModifyTableNames.Text,ModifyFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(ModifyTableNames.Text,ModifyPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.SelectFieldsListDblClick(Sender: TObject);
begin
  SQLText.SelText:= (Sender as TListBox).Items[(Sender as TListBox).ItemIndex];
end;

procedure TIBSQLEditorForm.SelectPageShow(Sender: TObject);
begin
  FIBSystemTables.GetTableNames(SelectTableNames);
  if FTableName <> '' then
    SelectTableNames.ItemIndex := SelectTableNames.Items.IndexOf(FTableName);
  FIBSystemTables.GetFieldNames(SelectTableNames.Text,SelectFieldsList.Items);
  FIBSystemTables.GetPrimaryKeys(SelectTableNames.Text,SelectPrimaryKeys.Items);
end;

procedure TIBSQLEditorForm.SelectTableNameCloseUp(Sender: TObject);
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

