unit ibdeletesqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IBSystemTables, IBDatabase;

type

  { TIBDeleteSQLEditorForm }

  TIBDeleteSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PrimaryKeyList: TListBox;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    TableNamesCombo: TComboBox;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  IBDeleteSQLEditorForm: TIBDeleteSQLEditorForm;

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

  with TIBDeleteSQLEditorForm.Create(Application) do
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

{ TIBDeleteSQLEditorForm }

procedure TIBDeleteSQLEditorForm.FormShow(Sender: TObject);
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
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
end;

procedure TIBDeleteSQLEditorForm.PrimaryKeyListDblClick(Sender: TObject);
begin
  SQLText.SelText := PrimaryKeyList.Items[PrimaryKeyList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBDeleteSQLEditorForm.Button3Click(Sender: TObject);
begin
  FIBSystemTables.GenerateDeleteSQL(TableNamesCombo.Text,QuoteFields.Checked,SQLText.Lines)
end;

procedure TIBDeleteSQLEditorForm.Button4Click(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text)
end;

procedure TIBDeleteSQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
end;

constructor TIBDeleteSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
end;

destructor TIBDeleteSQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

procedure TIBDeleteSQLEditorForm.SetDatabase(Database: TIBDatabase; Transaction: TIBTransaction);
begin
  FIBSystemTables.SelectDatabase(Database,Transaction)
end;

initialization
  {$I ibdeletesqleditor.lrs}

end.

