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
  StdCtrls, ComCtrls, IBSystemTables, IBDatabase, IBCustomDataSet, IB;

type

  { TIBInsertSQLEditorForm }

  TIBInsertSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ExecuteOnlyIndicator: TLabel;
    FieldList: TListBox;
    GenerateBtn: TButton;
    GenerateParams: TCheckBox;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    PageControl: TPageControl;
    ProcedureNames: TComboBox;
    ProcInputList: TListBox;
    ProcOutputList: TListBox;
    TableNamesCombo: TComboBox;
    InsertPage: TTabSheet;
    ExecutePage: TTabSheet;
    TestBtn: TButton;
    IBTransaction1: TIBTransaction;
    Label3: TLabel;
    QuoteFields: TCheckBox;
    SQLText: TMemo;
    procedure ExecutePageShow(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure InsertPageShow(Sender: TObject);
    procedure ProcedureNamesCloseUp(Sender: TObject);
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

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;

implementation

uses IBSQL;

{$R *.lfm}

 function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;
begin
 Result := false;

 if assigned(DataSet) and assigned(DataSet.Database) then
    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBInsertSQLEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
      SetDatabase(DataSet.Database);
      GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    SQLText.Lines.Assign(SelectSQL);
    Result := ShowModal = mrOK;
    if Result then
    begin
     SelectSQL.Assign(SQLText.Lines);
     if assigned(DataSet) then
          DataSet.GenerateParamNames := GenerateParams.Checked
    end;
  finally
    Free
  end;
end;

{ TIBInsertSQLEditorForm }

procedure TIBInsertSQLEditorForm.FormShow(Sender: TObject);
var IsProcedureName: boolean;
    SQLType: TIBSQLStatementTypes;
begin
  GenerateBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  TestBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
 if Trim(SQLText.Text) <> '' then
 begin
   try
     SQLType := FIBSystemTables.GetStatementType(SQLText.Text,IsProcedureName);
   except  end;
   if SQLType = SQLExecProcedure then
     PageControl.ActivePage := ExecutePage
   else
     PageControl.ActivePage := InsertPage;
 end
 else
   PageControl.ActivePage := InsertPage;
end;

procedure TIBInsertSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  SQLText.SelText := FieldList.Items[FieldList.ItemIndex];
  SQLText.SetFocus
end;

procedure TIBInsertSQLEditorForm.GenerateBtnClick(Sender: TObject);
var FieldNames: TStrings;
begin
 if PageControl.ActivePage = ExecutePage then
   FIBSystemTables.GenerateExecuteSQL(ProcedureNames.Text,QuoteFields.Checked,true,
         ProcInputList.Items,ProcOutputList.Items,SQLText.Lines)
 else
 begin
  FieldNames :=  FIBSystemTables.GetFieldNames(FieldList);
  try
    FIBSystemTables.GenerateInsertSQL(TableNamesCombo.Text,QuoteFields.Checked,
               FieldNames,SQLText.Lines)
  finally
    FieldNames.Free
  end;
 end;
end;

procedure TIBInsertSQLEditorForm.ExecutePageShow(Sender: TObject);
var ProcName: string;
    IsProcedureName: boolean;
begin
  FIBSystemTables.GetProcedureNames(ProcedureNames.Items,false);
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
  ProcedureNamesCloseUp(nil);
end;

procedure TIBInsertSQLEditorForm.InsertPageShow(Sender: TObject);
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
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,true,false);
  end;
  except {ignore}
  end;
end;

procedure TIBInsertSQLEditorForm.ProcedureNamesCloseUp(Sender: TObject);
var ExecuteOnly: boolean;
begin
  FIBSystemTables.GetProcParams(ProcedureNames.Text,ExecuteOnly,ProcInputList.Items,ProcOutputList.Items);
  ExecuteOnlyIndicator.Visible := ExecuteOnly;
end;

procedure TIBInsertSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text,GenerateParams.Checked)
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
