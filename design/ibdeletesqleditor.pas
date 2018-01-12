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
unit ibdeletesqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, 
    ibselectsqleditor, IBDatabase, IBCustomDataset;

type

  { TIBDeleteSQLEditorForm }

  TIBDeleteSQLEditorForm = class(TIBSelectSQLEditorForm)
    procedure GenerateBtnClick(Sender: TObject);
  private

  public

  end;

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;

var
  IBDeleteSQLEditorForm: TIBDeleteSQLEditorForm;

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

  with TIBDeleteSQLEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
        IBSQLEditFrame1.Database := DataSet.Database;
        GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    with IBSQLEditFrame1 do
    begin
      IncludeReadOnlyFields := false;
      ExecuteOnlyProcs := true;
      SQLText.Lines.Assign(SelectSQL);
    end;
    Result := ShowModal = mrOK;
    if Result then
    begin
     SelectSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
     if assigned(DataSet) then
          DataSet.GenerateParamNames := GenerateParams.Checked
    end;
  finally
    Free
  end;
end;

{ TIBDeleteSQLEditorForm }

procedure TIBDeleteSQLEditorForm.GenerateBtnClick(Sender: TObject);
begin
  if PageControl.ActivePage = ExecutePage then
    IBSQLEditFrame1.GenerateExecuteSQL(QuoteFields.Checked)
  else
    IBSQLEditFrame1.GenerateDeleteSQL(QuoteFields.Checked);
end;

end.

