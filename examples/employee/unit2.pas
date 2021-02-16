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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, db,
  DBTreeView,IBTreeView, IBQuery;

type

  { TSelectDeptDlg }

  TSelectDeptDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    Depts: TIBQuery;
    DeptsTreeView: TIBTreeView;
    Label1: TLabel;
    procedure DeptsTreeViewDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDeptKeyPath: string;
    FDept_no: string;
  public
    { public declarations }
    function ShowModal(DeptKeyPath: string; var Dept_no: string): TModalResult;
  end;

var
  SelectDeptDlg: TSelectDeptDlg;

implementation

{$R *.lfm}

{ TSelectDeptDlg }

procedure TSelectDeptDlg.FormShow(Sender: TObject);
begin
  Depts.Active := true;
  if FDeptKeyPath <> '' then
    DeptsTreeView.FindNode(StrIntListToVar(FDeptKeyPath),true); {Find and Select Current Dept}
end;

function TSelectDeptDlg.ShowModal(DeptKeyPath: string; var Dept_no: string
  ): TModalResult;
begin
  FDeptKeyPath := DeptKeyPath;
  Result := inherited ShowModal;
  Dept_no := FDept_no;
end;

procedure TSelectDeptDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FDept_no := '';
  if assigned(DeptsTreeView.Selected) then
    FDept_no := TDBTreeNode(DeptsTreeView.Selected).KeyValue;
  Depts.Active := false
end;

procedure TSelectDeptDlg.DeptsTreeViewDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

