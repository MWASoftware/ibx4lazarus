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

unit ibinsertsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IBSystemTables, IBDatabase;

type

  { TIBInsertSQLEditorForm }

  TIBInsertSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GenerateBtn: TButton;
    TestBtn: TButton;
    FieldList: TListBox;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    TableNamesCombo: TComboBox;
    procedure GenerateBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
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
    procedure SetDatabase(Database: TIBDatabase);
  end; 

var
  IBInsertSQLEditorForm: TIBInsertSQLEditorForm;

function EditSQL(Database: TIBDatabase; SelectSQL: TStrings): boolean;

implementation

{$R *.lfm}

function EditSQL(Database: TIBDatabase; SelectSQL: TStrings): boolean;
begin
 Result := false;

 if assigned(Database) then
    try
      Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBInsertSQLEditorForm.Create(Application) do
  try
    SetDatabase(Database);
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
  GenerateBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  TestBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
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
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,true,false);
  end;
  except {ignore}
  end;
end;

procedure TIBInsertSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  SQLText.SelText := FieldList.Items[FieldList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBInsertSQLEditorForm.GenerateBtnClick(Sender: TObject);
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

procedure TIBInsertSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text)
end;

procedure TIBInsertSQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,true,false);
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

procedure TIBInsertSQLEditorForm.SetDatabase(Database: TIBDatabase);
begin
  IBTransaction1.DefaultDatabase := Database;
  FIBSystemTables.SelectDatabase(Database,IBTransaction1)
end;

end.

