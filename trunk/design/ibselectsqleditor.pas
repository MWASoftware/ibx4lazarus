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

unit ibselectsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, IBSystemTables, IBDatabase, IBCustomDataSet, IB;

type

  { TIBSelectSQLEditorForm }

  TIBSelectSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GenerateBtn: TButton;
    GenerateParams: TCheckBox;
    SelectProcedure: TLabel;
    TestBtn: TButton;
    FieldList: TListBox;
    IBTransaction1: TIBTransaction;
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
    procedure GenerateBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
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
    FExecuteOnly: boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabase(Database: TIBDatabase);
  end; 


function EditSQL(DataSet: TIBCustomDataSet;  SelectSQL: TStrings): boolean;

implementation

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

  with TIBSelectSQLEditorForm.Create(Application) do
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

{ TIBSelectSQLEditorForm }

procedure TIBSelectSQLEditorForm.FormShow(Sender: TObject);
var IsProcedureName: boolean;
begin
  GenerateBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  TestBtn.Enabled := (IBTransaction1.DefaultDatabase <> nil) and IBTransaction1.DefaultDatabase.Connected;
  if Trim(SQLText.Text) <> '' then
  begin
    try
      FIBSystemTables.GetStatementType(SQLText.Text,IsProcedureName);
    except  end;
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
  FIBSystemTables.GetProcParams(ProcedureNames.Text,FExecuteOnly,ProcInputList.Items,ProcOutputList.Items);
  SelectProcedure.Visible := not FExecuteOnly
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

procedure TIBSelectSQLEditorForm.GenerateBtnClick(Sender: TObject);
var FieldNames: TStrings;
begin
  if PageControl.ActivePage = ExecutePage then
    FIBSystemTables.GenerateExecuteSQL(ProcedureNames.Text,QuoteFields.Checked,FExecuteOnly,
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

procedure TIBSelectSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  FIBSystemTables.TestSQL(SQLText.Lines.Text,GenerateParams.Checked)
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
  FIBSystemTables.GetProcParams(ProcedureNames.Text,FExecuteOnly,ProcInputList.Items,ProcOutputList.Items);
  SelectProcedure.Visible := not FExecuteOnly
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

procedure TIBSelectSQLEditorForm.SetDatabase(Database: TIBDatabase);
begin
  IBTransaction1.DefaultDatabase := Database;
  FIBSystemTables.SelectDatabase(Database,IBTransaction1)
end;

end.
