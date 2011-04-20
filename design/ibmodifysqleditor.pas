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

unit ibmodifysqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
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

{$R *.lfm}

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
  try
  FIBSystemTables.GetTableNames(TableNamesCombo.Items);
  if TableNamesCombo.Items.Count > 0 then
  begin
    TableNamesCombo.ItemIndex := 0;
    if Trim(SQLText.Text) <> '' then
    begin
      FIBSystemTables.GetTableAndColumns(SQLText.Text,TableName,nil);
      TableNamesCombo.ItemIndex := TableNamesCombo.Items.IndexOf(TableName)
    end;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,IncludePrimaryKeys.checked);
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
  except {ignore}  end;
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

end.

