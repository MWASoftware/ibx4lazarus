unit ibselectsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, IBSystemTables, IBDatabase;

type

  { TIBSelectSQLEditorForm }

  TIBSelectSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    FieldList: TListBox;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl: TPageControl;
    PrimaryKeyList: TListBox;
    ProcedureNames: TComboBox;
    ProcInputList: TListBox;
    ProcOutputList: TListBox;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    TableNamesCombo: TComboBox;
    SelectPage: TTabSheet;
    ExecutePage: TTabSheet;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ExecutePageShow(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrimaryKeyListDblClick(Sender: TObject);
    procedure ProcedureNamesCloseUp(Sender: TObject);
    procedure SelectPageShow(Sender: TObject);
    procedure TableNamesComboCloseUp(Sender: TObject);
  private
    { private declarations }
    FIBSystemTables: TIBSystemTables;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
  end; 

var
  IBSelectSQLEditorForm: TIBSelectSQLEditorForm;

function EditSQL(Database: TIBDatabase; Transaction: TIBTransaction; SelectSQL: TStrings): boolean;

implementation

uses IBSQL;

function EditSQL(Database: TIBDatabase; Transaction: TIBTransaction; SelectSQL: TStrings): boolean;
begin
  Result := false;
  if assigned(Database) then
  begin
    if not assigned(Transaction) then
    begin
      if not assigned(Database.DefaultTransaction)then
      begin
        ShowMessage('No Default Transaction!');
        Exit
      end;
      Transaction := Database.DefaultTransaction
    end;

    try
      Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  end;

  with TIBSelectSQLEditorForm.Create(Application) do
  try
    SetDatabase(Database,Transaction);
    SQLText.Lines.Assign(SelectSQL);
    Result := ShowModal = mrOK;
    if Result then
     SelectSQL.Assign(SQLText.Lines)
  finally
    Free
  end;
end;

{ TIBSelectSQLEditorForm }

procedure TIBSelectSQLEditorForm.FormShow(Sender: TObject);
var IsProcedureName: boolean;
begin
  if Trim(SQLText.Text) <> '' then
  begin
    FIBSystemTables.GetStatementType(SQLText.Text,IsProcedureName);
    if IsProcedureName then
      PageControl.ActivePage := ExecutePage
    else
      PageControl.ActivePage := SelectPage;
  end
  else
    PageControl.ActivePage := SelectPage;
end;

procedure TIBSelectSQLEditorForm.PrimaryKeyListDblClick(Sender: TObject);
begin
  SQLText.SelText := PrimaryKeyList.Items[PrimaryKeyList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBSelectSQLEditorForm.ProcedureNamesCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetProcParams(ProcedureNames.Text,ProcInputList.Items,ProcOutputList.Items);
end;

procedure TIBSelectSQLEditorForm.SelectPageShow(Sender: TObject);
var TableName: string;
begin
  TableNamesCombo.Items.Clear;
  FIBSystemTables.GetTableNames(TableNamesCombo.Items);
  if TableNamesCombo.Items.Count > 0 then
  begin
    TableNamesCombo.ItemIndex := 0;
    if Trim(SQLText.Text) <> '' then
    begin
      FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
      TableNamesCombo.ItemIndex := TableNamesCombo.Items.IndexOf(TableName)
    end;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items);
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
end;

procedure TIBSelectSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  SQLText.SelText := FieldList.Items[FieldList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBSelectSQLEditorForm.Button3Click(Sender: TObject);
var FieldNames: TStrings;
begin
  if PageControl.ActivePage = ExecutePage then
    FIBSystemTables.GenerateExecuteSQL(ProcedureNames.Text,QuoteFields.Checked,
          ProcInputList.Items,ProcOutputList.Items,SQLText.Lines)
  else
  begin
    FieldNames :=  FIBSystemTables.GetFieldNames(FieldList);
    try
      FIBSystemTables.GenerateSelectSQL(TableNamesCombo.Text,QuoteFields.Checked,FieldNames,SQLText.Lines)
    finally
      FieldNames.Free
    end;
  end;
end;

procedure TIBSelectSQLEditorForm.Button4Click(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text)
end;

procedure TIBSelectSQLEditorForm.ExecutePageShow(Sender: TObject);
var ProcName: string;
    IsProcedureName: boolean;
begin
  FIBSystemTables.GetProcedureNames(ProcedureNames.Items,true);
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

procedure TIBSelectSQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items);
  FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
end;

constructor TIBSelectSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
end;

destructor TIBSelectSQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

procedure TIBSelectSQLEditorForm.SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
begin
  FIBSystemTables.SelectDatabase(Database,Transaction)
end;

initialization
  {$I ibselectsqleditor.lrs}

end.

