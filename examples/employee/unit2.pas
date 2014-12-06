unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, EditBtn, IBCustomDataSet, IBQuery, IBLookupComboEditBox, db;

type

  { TEditEmployeeDlg }

  TEditEmployeeDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CountriesCOUNTRY: TIBStringField;
    CountriesCURRENCY: TIBStringField;
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    Datasource4: TDatasource;
    DateEdit1: TDateEdit;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    IBDataSet1: TIBDataSet;
    IBDataSet1DEPT_NO: TIBStringField;
    IBDataSet1EMP_NO: TSmallintField;
    IBDataSet1FIRST_NAME: TIBStringField;
    IBDataSet1FULL_NAME: TIBStringField;
    IBDataSet1HIRE_DATE: TDateTimeField;
    IBDataSet1JOB_CODE: TIBStringField;
    IBDataSet1JOB_COUNTRY: TIBStringField;
    IBDataSet1JOB_GRADE: TSmallintField;
    IBDataSet1LAST_NAME: TIBStringField;
    IBDataSet1PHONE_EXT: TIBStringField;
    IBDataSet1SALARY: TIBBCDField;
    Countries: TIBQuery;
    Depts: TIBDataSet;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    IBLookupComboEditBox2: TIBLookupComboEditBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    procedure DateEdit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBDataSet1AfterClose(DataSet: TDataSet);
    procedure IBDataSet1AfterInsert(DataSet: TDataSet);
    procedure IBDataSet1AfterScroll(DataSet: TDataSet);
    procedure IBDataSet1BeforeOpen(DataSet: TDataSet);
    procedure IBDataSet1BeforePost(DataSet: TDataSet);
  private
    { private declarations }
    FEmp_no: integer;
  public
    { public declarations }
    function ShowModal(Emp_no: integer): TModalResult;
  end; 

var
  EditEmployeeDlg: TEditEmployeeDlg;

implementation

{$R *.lfm}

uses Unit1;

{ TEditEmployeeDlg }

procedure TEditEmployeeDlg.IBDataSet1AfterScroll(DataSet: TDataSet);
begin
  DateEdit1.Date := DataSet.FieldByName('HIRE_DATE').AsDateTime
end;

procedure TEditEmployeeDlg.IBDataSet1BeforeOpen(DataSet: TDataSet);
begin
  Depts.Active:= true;
  Countries.Active := true;
  IBDataSet1.ParamByName('EMP_NO').AsInteger := FEmp_no
end;

procedure TEditEmployeeDlg.DateEdit1Change(Sender: TObject);
begin
  IBDataSet1.Edit;
  IBDataSet1.FieldByName('HIRE_DATE').AsDateTime  :=  DateEdit1.Date
end;

procedure TEditEmployeeDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if IBDataSet1.State in [dsEdit,dsInsert] then
  begin
    if ModalResult = mrOK then
      IBDataSet1.Post
    else
      IBDataSet1.Cancel
  end;
  IBDataSet1.Active := false
end;

procedure TEditEmployeeDlg.FormShow(Sender: TObject);
begin
  IBDataSet1.Active := true
end;

procedure TEditEmployeeDlg.IBDataSet1AfterClose(DataSet: TDataSet);
begin
  Depts.Active := false;
  Countries.Active := false;
end;

procedure TEditEmployeeDlg.IBDataSet1AfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('HIRE_DATE').AsDateTime := Now;
  DataSet.FieldByName('JOB_CODE').AsString := 'Eng';
  DataSet.FieldByName('JOB_GRADE').AsInteger := 2;
  DataSet.FieldByName('Salary').AsFloat := 70000;
end;

procedure TEditEmployeeDlg.IBDataSet1BeforePost(DataSet: TDataSet);
begin
  DataSet.FieldByName('HIRE_DATE').AsDateTime  :=  DateEdit1.Date
end;

function TEditEmployeeDlg.ShowModal(Emp_no: integer): TModalResult;
begin
  FEmp_no := Emp_no;
  Result := inherited ShowModal
end;

end.

