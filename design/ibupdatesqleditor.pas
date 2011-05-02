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
unit IBUpdateSQLEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, IBSystemTables, IBUpdateSQL;

type

  { TIBUpdateSQLEditorForm }

  TIBUpdateSQLEditorForm = class(TForm)
    Button1: TButton;
    CancelButton: TButton;
    FieldsPage: TTabSheet;
    GenerateButton: TButton;
    GroupBox1: TGroupBox;
    IncludePrimaryKeys: TCheckBox;
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
    FUpdateObject: TIBUpdateSQL;
    FIBSystemTables: TIBSystemTables;
    FDirty: boolean;
    FCurrentStatement: integer;
    FModifySQL: TStringList;
    FInsertSQL: TStringList;
    FDeleteSQL: TStringList;
    FRefreshSQL: TStringList;
    procedure UpdateSQLMemo;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpdateObject(AObject: TIBUpdateSQL);
  end;

var
  IBUpdateSQLEditorForm: TIBUpdateSQLEditorForm;

function EditIBUpdateSQL(UpdateObject: TIBUpdateSQL): boolean;

implementation

uses IBQuery;

{$R *.lfm}

function EditIBUpdateSQL(UpdateObject: TIBUpdateSQL): boolean;
begin
  Result := false;
  if assigned(UpdateObject.DataSet) and assigned(UpdateObject.DataSet.Database) then
  begin
    if not assigned(UpdateObject.DataSet.Transaction) then
    begin
      ShowMessage('No Default Transaction!');
      Exit
    end;

    try
      UpdateObject.DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  end;

  with TIBUpdateSQLEditorForm.Create(Application) do
  try
    SetUpdateObject(UpdateObject);
    Result := ShowModal = mrOK
  finally
    Free
  end;

end;

{ TIBUpdateSQLEditorForm }

procedure TIBUpdateSQLEditorForm.FormShow(Sender: TObject);
var TableName: string;
begin
  PageControl.ActivePage := FieldsPage;
  FModifySQL.Assign(FUpdateObject.ModifySQL);
  FInsertSQL.Assign(FUpdateObject.InsertSQL);
  FDeleteSQL.Assign(FUpdateObject.DeleteSQL);
  FRefreshSQL.Assign(FUpdateObject.RefreshSQL);
  FCurrentStatement := -1;
  TableNamesCombo.Items.Clear;
  FIBSystemTables.GetTableNames(TableNamesCombo.Items);
  if TableNamesCombo.Items.Count > 0 then
  begin
    TableNamesCombo.ItemIndex := 0;
    if assigned(FUpdateObject.DataSet) and (FUpdateObject.DataSet is TIBQuery) and
       ((FUpdateObject.DataSet as TIBQuery).SQL.Text <> '') then
    try
       FIBSystemTables.GetTableAndColumns((FUpdateObject.DataSet as TIBQuery).SQL.Text,TableName,nil);
       TableNamesCombo.ItemIndex := TableNamesCombo.Items.IndexOf(TableName);
    except  //ignore
    end;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,IncludePrimaryKeys.checked,false);
    FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
  end;
end;

procedure TIBUpdateSQLEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    UpdateSQLMemo;
    FUpdateObject.ModifySQL.Assign(FModifySQL);
    FUpdateObject.InsertSQL.Assign(FInsertSQL);
    FUpdateObject.DeleteSQL.Assign(FDeleteSQL);
    FUpdateObject.RefreshSQL.Assign(FRefreshSQL);
  end;
end;

procedure TIBUpdateSQLEditorForm.Button1Click(Sender: TObject);
begin
  if SQLMemo.Lines.Text <> '' then
    FIBSystemTables.TestSQL(SQLMemo.Lines.Text);
end;

procedure TIBUpdateSQLEditorForm.GenerateButtonClick(Sender: TObject);
var FieldNames: TStringList;
    I: integer;
begin
  FieldNames := TStringList.Create;
  try
    FUpdateObject.RefreshSQL.Clear;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldNames);
    FIBSystemTables.GenerateRefreshSQL(TableNamesCombo.Text,QuoteFields.Checked,FieldNames,FRefreshSQL);
    FIBSystemTables.GenerateDeleteSQL(TableNamesCombo.Text,QuoteFields.Checked,FDeleteSQL);
    FieldNames.Clear;
    FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldNames,true,false);
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

procedure TIBUpdateSQLEditorForm.SQLMemoChange(Sender: TObject);
begin
  FDirty := true
end;

procedure TIBUpdateSQLEditorForm.SQLPageShow(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBUpdateSQLEditorForm.StatementTypeClick(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBUpdateSQLEditorForm.TableNamesComboCloseUp(Sender: TObject);
begin
  FIBSystemTables.GetFieldNames(TableNamesCombo.Text,FieldList.Items,IncludePrimaryKeys.checked,false);
  FIBSystemTables.GetPrimaryKeys(TableNamesCombo.Text,PrimaryKeyList.Items);
end;

procedure TIBUpdateSQLEditorForm.UpdateSQLMemo;
begin
  if FDirty then
    case FCurrentStatement of
    0: //Modify
        FModifySQL.Assign(SQLMemo.Lines);
    1: //Insert
        FInsertSQL.Assign(SQLMemo.Lines);
    2: // Delete
        FDeleteSQL.Assign(SQLMemo.Lines);
    3: //Refresh
        FRefreshSQL.Assign(SQLMemo.Lines);
    end;
  FDirty := false;
  case StatementType.ItemIndex of
  0: // Modify
    SQLMemo.Lines.Assign(FModifySQL)  ;
  1: //Insert
    SQLMemo.Lines.Assign(FInsertSQL)  ;
  2: // Delete
    SQLMemo.Lines.Assign(FDeleteSQL)  ;
  3: //Refresh
    SQLMemo.Lines.Assign(FRefreshSQL)  ;
  end;
  FCurrentStatement := StatementType.ItemIndex;
end;

constructor TIBUpdateSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create;
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
end;

destructor TIBUpdateSQLEditorForm.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  if assigned(FModifySQL) then FModifySQL.Free;
  if assigned(FInsertSQL) then FInsertSQL.Free;
  if assigned(FDeleteSQL) then FDeleteSQL.Free;
  if assigned(FRefreshSQL) then FRefreshSQL.Free;
  inherited Destroy;
end;

procedure TIBUpdateSQLEditorForm.SetUpdateObject(AObject: TIBUpdateSQL);
begin
  FUpdateObject := AObject;
  if assigned(FUpdateObject.DataSet) then
    FIBSystemTables.SelectDatabase(FUpdateObject.DataSet.Database,FUpdateObject.DataSet.Transaction);
end;


end.

