unit SelectDeptDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, db,
  IBTreeView, IBQuery;

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
    FDept_no := TIBTreeNode(DeptsTreeView.Selected).KeyValue;
  Depts.Active := false
end;

procedure TSelectDeptDlg.DeptsTreeViewDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

