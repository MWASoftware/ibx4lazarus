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

unit ibsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ibselectsqleditor, IBSQLEditFrame, IBLookupComboEditBox,
  IBDynamicGrid, IBDatabase, IBSQL, IB;

type

  { TIBSQLEditorForm }

  TIBSQLEditorForm = class(TIBSelectSQLEditorForm)
    IncludePrimaryKeys: TCheckBox;
    TabControl1: TTabControl;
    procedure FormShow(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure IncludePrimaryKeysChange(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    procedure SetupFlags;
  protected
    procedure SetSQLStatementType(aType: TIBSQLStatementTypes); override;
  public

  end;

function EditSQL(aIBSQL: TIBSQL): boolean;

var
  IBSQLEditorForm: TIBSQLEditorForm;

implementation

{$R *.lfm}

function EditSQL(aIBSQL: TIBSQL): boolean;
begin
  Result := false;
  if assigned(aIBSQL) and assigned(aIBSQL.Database) then
    try
      aIBSQL.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBSQLEditorForm.Create(Application) do
  try
    if assigned(aIBSQL) then
    begin
        IBSQLEditFrame1.Database := aIBSQL.Database;
        GenerateParams.Checked := aIBSQL.GenerateParamNames;
    end;
    with IBSQLEditFrame1 do
    begin
      IncludePrimaryKeys := true;
      IncludeReadOnlyFields := true;
      ExecuteOnlyProcs := false;
      SQLText.Lines.Assign(aIBSQL.SQL);
    end;
    IncludePrimaryKeys.Checked := false;
    Result := ShowModal = mrOK;
    if Result then
    begin
     aIBSQL.SQL.Assign(IBSQLEditFrame1.SQLText.Lines);
     if assigned(aIBSQL) then
          aIBSQL.GenerateParamNames := GenerateParams.Checked
    end;
  finally
    Free
  end;
end;

{ TIBSQLEditorForm }

procedure TIBSQLEditorForm.TabControl1Change(Sender: TObject);
begin
  case TabControl1.TabIndex of
  4:
    PageControl.ActivePage := ExecutePage;
  else
    PageControl.ActivePage := SelectPage;
  end;
  SetupFlags;
end;

procedure TIBSQLEditorForm.FormShow(Sender: TObject);
begin
  inherited;
  SetupFlags;
end;

procedure TIBSQLEditorForm.GenerateBtnClick(Sender: TObject);
begin
  case TabControl1.TabIndex of
  0:
    IBSQLEditFrame1.GenerateSelectSQL(QuoteFields.Checked);
  1:
    IBSQLEditFrame1.GenerateInsertSQL(QuoteFields.Checked);
  2:
    IBSQLEditFrame1.GenerateModifySQL(QuoteFields.Checked);
  3:
    IBSQLEditFrame1.GenerateDeleteSQL(QuoteFields.Checked);
  4:
    IBSQLEditFrame1.GenerateExecuteSQL(QuoteFields.Checked);
  end;
end;

procedure TIBSQLEditorForm.IncludePrimaryKeysChange(Sender: TObject);
begin
  IBSQLEditFrame1.IncludePrimaryKeys := IncludePrimaryKeys.Checked;
end;

procedure TIBSQLEditorForm.SetupFlags;
begin
  IBSQLEditFrame1.IncludePrimaryKeys := (TabControl1.TabIndex <= 1) or ((TabControl1.TabIndex = 2) and IncludePrimaryKeys.Checked);
  IBSQLEditFrame1.IncludeReadOnlyFields := (TabControl1.TabIndex = 0);
  IncludePrimaryKeys.Visible := TabControl1.TabIndex = 2;
  FieldNamesGrid.Visible := TabControl1.TabIndex <> 3;
  Label2.Visible := TabControl1.TabIndex <> 3;
  PrimaryKeysGrid.Visible := TabControl1.TabIndex <> 1;
  Label4.Visible := TabControl1.TabIndex <> 1;
  SelectSelectAll.Visible := TabControl1.TabIndex <> 3;
end;

procedure TIBSQLEditorForm.SetSQLStatementType(aType: TIBSQLStatementTypes);
begin
  inherited SetSQLStatementType(aType);
  case aType of
  SQLSelect:
    TabControl1.TabIndex := 0;
  SQLInsert:
    TabControl1.TabIndex := 1;
  SQLUpdate:
    TabControl1.TabIndex := 2;
  SQLDelete:
    TabControl1.TabIndex := 3;
  else
    TabControl1.TabIndex := 4;
  end;
end;

end.

