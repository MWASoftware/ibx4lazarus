unit Unit3; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, EditBtn, Unit2, IBCustomDataSet, IBQuery, db;

type

  { TAddEmployeeDlg }

  TAddEmployeeDlg = class(TEditEmployeeDlg)
    procedure EmployeesAfterInsert(DataSet: TDataSet);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FNewEmpNo: integer;
  public
    { public declarations }
    function ShowModal(var Emp_no: integer): TModalResult;
  end; 

var
  AddEmployeeDlg: TAddEmployeeDlg;

implementation

{$R *.lfm}

uses Unit1;

{ TAddEmployeeDlg }

procedure TAddEmployeeDlg.EmployeesAfterInsert(DataSet: TDataSet);
begin
  inherited;
  FNewEmpNo := DataSet.FieldByName('EMP_NO').AsInteger
end;

procedure TAddEmployeeDlg.FormShow(Sender: TObject);
begin
  inherited;
  Employees.Append
end;

function TAddEmployeeDlg.ShowModal(var Emp_no: integer): TModalResult;
begin
  Result := inherited ShowModal(-1);
  if Result = mrOK then
    Emp_no := FNewEmpNo;
end;

end.
