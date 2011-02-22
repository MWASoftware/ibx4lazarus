unit ibmodifysqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IBSystemTables, IBDatabase;

type

  { TIBModifySQLEditorForm }

  TIBModifySQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    IncludePrimaryKeys: TCheckBox;
    FieldList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PrimaryKeyList: TListBox;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    TableNamesCombo: TComboBox;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrimaryKeyListDblClick(Sender: TObject);
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
  IBModifySQLEditorForm: TIBModifySQLEditorForm;

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

  with TIBModifySQLEditorForm.Create(Application) do
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

{ TIBModifySQLEditorForm }

procedure TIBModifySQLEditorForm.FormShow(Sender: TObject);
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
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,IncludePrimaryKeys.checked);
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
end;

procedure TIBModifySQLEditorForm.PrimaryKeyListDblClick(Sender: TObject);
begin
  SQLText.SelText := PrimaryKeyList.Items[PrimaryKeyList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBModifySQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  SQLText.SelText := FieldList.Items[FieldList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBModifySQLEditorForm.Button3Click(Sender: TObject);
var FieldNames: TStrings;
begin
  FieldNames :=  FIBSystemTables.GetFieldNames(FieldList);
  try
    FIBSystemTables.GenerateModifySQL(TableNamesCombo.Text,QuoteFields.Checked,
             FieldNames,SQLText.Lines)
  finally
    FieldNames.Free
  end;
end;

procedure TIBModifySQLEditorForm.Button4Click(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text)
end;

procedure TIBModifySQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,IncludePrimaryKeys.checked);
  FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
end;

constructor TIBModifySQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
end;

destructor TIBModifySQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

procedure TIBModifySQLEditorForm.SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
begin
  FIBSystemTables.SelectDatabase(Database,Transaction)
end;

initialization
  {$I ibmodifysqleditor.lrs}

end.

