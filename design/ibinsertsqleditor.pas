unit ibinsertsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IBSystemTables, IBDatabase;

type

  { TIBInsertSQLEditorForm }

  TIBInsertSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    FieldList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    TableNamesCombo: TComboBox;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  IBInsertSQLEditorForm: TIBInsertSQLEditorForm;

function EditSQL(Database: TIBDatabase; Transaction: TIBTransaction; SelectSQL: TStrings): boolean;

implementation

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

  with TIBInsertSQLEditorForm.Create(Application) do
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

{ TIBInsertSQLEditorForm }

procedure TIBInsertSQLEditorForm.FormShow(Sender: TObject);
var TableName: string;
begin
  TableNamesCombo.Items.Clear;
  FIBSystemTables.GetTableNames(TableNamesCombo.Items);
  if TableNamesCombo.Items.Count > 0 then
  begin
    TableNamesCombo.ItemIndex := 0;
    if SQLText.Text <> '' then
    begin
      FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
      TableNamesCombo.ItemIndex := TableNamesCombo.Items.IndexOf(TableName)
    end;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items);
  end;
end;

procedure TIBInsertSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  SQLText.SelText := FieldList.Items[FieldList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBInsertSQLEditorForm.Button3Click(Sender: TObject);
var FieldNames: TStrings;
begin
  FieldNames :=  FIBSystemTables.GetFieldNames(FieldList);
  try
    FIBSystemTables.GenerateInsertSQL(TableNamesCombo.Text,QuoteFields.Checked,
               FieldNames,SQLText.Lines)
  finally
    FieldNames.Free
  end;
end;

procedure TIBInsertSQLEditorForm.Button4Click(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text)
end;

procedure TIBInsertSQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items);
end;

constructor TIBInsertSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
end;

destructor TIBInsertSQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

procedure TIBInsertSQLEditorForm.SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
begin
  FIBSystemTables.SelectDatabase(Database,Transaction)
end;

initialization
  {$I ibinsertsqleditor.lrs}

end.

