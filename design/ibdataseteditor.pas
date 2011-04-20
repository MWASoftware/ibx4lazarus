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

unit IBDataSetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, IBSystemTables, IBCustomDataSet;

type

  { TIBDataSetEditorForm }

  TIBDataSetEditorForm = class(TForm)
    Button1: TButton;
    CancelButton: TButton;
    FieldsPage: TTabSheet;
    GenerateButton: TButton;
    GroupBox1: TGroupBox;
    PrimaryKeyList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OkButton: TButton;
    PageControl: TPageControl;
    QuoteFields: TCheckBox;
    SQLMemo: TMemo;
    SQLPage: TTabSheet;
    StatementType: TRadioGroup;
    FieldList: TListBox;
    TableNamesCombo: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure SQLMemoChange(Sender: TObject);
    procedure SQLPageShow(Sender: TObject);
    procedure StatementTypeClick(Sender: TObject);
    procedure TableNamesComboCloseUp(Sender: TObject);
  private
    { private declarations }
    FDataSet: TIBDataSet;
    FIBSystemTables: TIBSystemTables;
    FDirty: boolean;
    FCurrentStatement: integer;
    FSelectSQL: TStringList;
    FModifySQL: TStringList;
    FInsertSQL: TStringList;
    FDeleteSQL: TStringList;
    FRefreshSQL: TStringList;
    procedure UpdateSQLMemo;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDataSet(AObject: TIBDataSet);
  end;

var
  IBDataSetEditorForm: TIBDataSetEditorForm;

function EditIBDataSet(DataSet: TIBDataSet): boolean;

implementation

{$R *.lfm}

function EditIBDataSet(DataSet: TIBDataSet): boolean;
begin
  Result := false;
  if assigned(DataSet) and assigned(DataSet.Database) then
  begin
    if not assigned(DataSet.Transaction) then
    begin
      ShowMessage('No Default Transaction!');
      Exit
    end;

    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  end;

  with TIBDataSetEditorForm.Create(Application) do
  try
    SetDataSet(DataSet);
    Result := ShowModal = mrOK
  finally
    Free
  end;

end;

{ TIBDataSetEditorForm }

procedure TIBDataSetEditorForm.FormShow(Sender: TObject);
var TableName: string;
begin
  PageControl.ActivePage := FieldsPage;
  FModifySQL.Assign(FDataSet.ModifySQL);
  FInsertSQL.Assign(FDataSet.InsertSQL);
  FDeleteSQL.Assign(FDataSet.DeleteSQL);
  FRefreshSQL.Assign(FDataSet.RefreshSQL);
  FSelectSQL.Assign(FDataSet.SelectSQL);
  FCurrentStatement := -1;
  TableNamesCombo.Items.Clear;
  FIBSystemTables.GetTableNames(TableNamesCombo.Items);
  if TableNamesCombo.Items.Count > 0 then
  begin
    TableNamesCombo.ItemIndex := 0;
    if FSelectSQL.Text <> '' then
    try
       FIBSystemTables.GetTableAndColumns(FSelectSQL.Text,TableName,nil);
       TableNamesCombo.ItemIndex := TableNamesCombo.Items.IndexOf(TableName);
    except end;//ignore
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,false);
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
end;

procedure TIBDataSetEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    UpdateSQLMemo;
    FDataSet.ModifySQL.Assign(FModifySQL);
    FDataSet.InsertSQL.Assign(FInsertSQL);
    FDataSet.DeleteSQL.Assign(FDeleteSQL);
    FDataSet.RefreshSQL.Assign(FRefreshSQL);
    FDataSet.SelectSQL.Assign(FSelectSQL);
  end;
end;

procedure TIBDataSetEditorForm.Button1Click(Sender: TObject);
begin
  if SQLMemo.Lines.Text <> '' then
    FIBSystemTables.TestSQL(SQLMemo.Lines.Text);
end;

procedure TIBDataSetEditorForm.GenerateButtonClick(Sender: TObject);
var FieldNames: TStringList;
    I: integer;
begin
  FieldNames := TStringList.Create;
  try
    FRefreshSQL.Clear;
    FSelectSQL.Clear;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldNames);
    FIBSystemTables.GenerateSelectSQL(TableNamesCombo.Text,QuoteFields.Checked,FieldNames,FSelectSQL);
    FIBSystemTables.GenerateRefreshSQL(TableNamesCombo.Text,QuoteFields.Checked,FieldNames,FRefreshSQL);
    FIBSystemTables.GenerateDeleteSQL(TableNamesCombo.Text,QuoteFields.Checked,FDeleteSQL);
    FIBSystemTables.GenerateInsertSQL(TableNamesCombo.Text,QuoteFields.Checked,
        FieldNames,FInsertSQL);
    if FieldList.SelCount = 0 then
    begin
      FIBSystemTables.GenerateModifySQL(TableNamesCombo.Text,QuoteFields.Checked,
        FieldList.Items,FModifySQL);

    end
    else
    begin
      FieldNames.Clear;
      for I := 0 to FieldList.Items.Count - 1 do
        if FieldList.Selected[I] then
          FieldNames.Add(FieldList.Items[I]);
      FIBSystemTables.GenerateModifySQL(TableNamesCombo.Text,QuoteFields.Checked,
        FieldNames,FModifySQL);
    end;
    FDirty := false;
    PageControl.ActivePage := SQLPage;
  finally
    FieldNames.Free
  end;
end;

procedure TIBDataSetEditorForm.SQLMemoChange(Sender: TObject);
begin
  FDirty := true
end;

procedure TIBDataSetEditorForm.SQLPageShow(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBDataSetEditorForm.StatementTypeClick(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBDataSetEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,false);
  FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
end;

procedure TIBDataSetEditorForm.UpdateSQLMemo;
begin
  if FDirty then
    case FCurrentStatement of
    0: // Select
        FSelectSQL.Assign(SQLMemo.Lines);
    1: //Modify
        FModifySQL.Assign(SQLMemo.Lines);
    2: //Insert
        FInsertSQL.Assign(SQLMemo.Lines);
    3: // Delete
        FDeleteSQL.Assign(SQLMemo.Lines);
    4: //Refresh
        FRefreshSQL.Assign(SQLMemo.Lines);
    end;
  FDirty := false;
  case StatementType.ItemIndex of
  0: //Select
     SQLMemo.Lines.Assign(FSelectSQL);
  1: // Modify
    SQLMemo.Lines.Assign(FModifySQL)  ;
  2: //Insert
    SQLMemo.Lines.Assign(FInsertSQL)  ;
  3: // Delete
    SQLMemo.Lines.Assign(FDeleteSQL)  ;
  4: //Refresh
    SQLMemo.Lines.Assign(FRefreshSQL)  ;
  end;
  FCurrentStatement := StatementType.ItemIndex;
end;

constructor TIBDataSetEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TIBDataSetEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  if assigned(FModifySQL) then FModifySQL.Free;
  if assigned(FInsertSQL) then FInsertSQL.Free;
  if assigned(FDeleteSQL) then FDeleteSQL.Free;
  if assigned(FRefreshSQL) then FRefreshSQL.Free;
 if assigned(FSelectSQL) then FSelectSQL.Free;
  inherited Destroy;
end;

procedure TIBDataSetEditorForm.SetDataSet(AObject: TIBDataSet);
begin
  FDataSet := AObject;
  if assigned(FDataSet) then
    FIBSystemTables.SelectDatabase(FDataSet.Database,FDataSet.Transaction);
end;

end.

