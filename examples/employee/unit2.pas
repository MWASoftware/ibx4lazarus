unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, EditBtn, IBCustomDataSet, IBQuery, IBLookupComboEditBox, IBDateEdit,
  db;

type

  { TEditEmployeeDlg }

  TEditEmployeeDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CountriesCOUNTRY: TIBStringField;
    CountriesCURRENCY: TIBStringField;
    Datasource1: TDatasource;
    Datasource2: TDatasource;
    DataSource3: TDataSource;
    Datasource4: TDatasource;
    JobGradeDBComboBox: TDBComboBox;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBLookupComboBox1: TDBLookupComboBox;
    CountryDBLookupComboBox: TDBLookupComboBox;
    JobTitleDBLookupComboBox: TDBLookupComboBox;
    Employees: TIBDataSet;
    EmployeesDEPT_NO: TIBStringField;
    EmployeesEMP_NO: TSmallintField;
    EmployeesFIRST_NAME: TIBStringField;
    EmployeesFULL_NAME: TIBStringField;
    EmployeesHIRE_DATE: TDateTimeField;
    EmployeesJOB_CODE: TIBStringField;
    EmployeesJOB_COUNTRY: TIBStringField;
    EmployeesJOB_GRADE: TSmallintField;
    EmployeesLAST_NAME: TIBStringField;
    EmployeesPHONE_EXT: TIBStringField;
    EmployeesSALARY: TIBBCDField;
    Countries: TIBQuery;
    IBDateEdit1: TIBDateEdit;
    Depts: TIBQuery;
    JobCodes: TIBQuery;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure CountriesAfterOpen(DataSet: TDataSet);
    procedure CountriesBeforeOpen(DataSet: TDataSet);
    procedure EmployeesAfterOpen(DataSet: TDataSet);
    procedure EmployeesJOB_GRADEChange(Sender: TField);
    procedure JobCodesAfterOpen(DataSet: TDataSet);
    procedure JobGradeDBComboBoxCloseUp(Sender: TObject);
    procedure EmployeesJOB_CODEChange(Sender: TField);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure EmployeesAfterClose(DataSet: TDataSet);
    procedure EmployeesAfterInsert(DataSet: TDataSet);
    procedure EmployeesBeforeOpen(DataSet: TDataSet);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure JobCodesBeforeOpen(DataSet: TDataSet);
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

procedure TEditEmployeeDlg.EmployeesBeforeOpen(DataSet: TDataSet);
begin
  Depts.Active:= true;
  Employees.ParamByName('EMP_NO').AsInteger := FEmp_no
end;

procedure TEditEmployeeDlg.EmployeesSALARYGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if not Sender.IsNull and DisplayText then
     aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  else
     aText := Sender.AsString
end;

procedure TEditEmployeeDlg.JobCodesBeforeOpen(DataSet: TDataSet);
begin
  JobCodes.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  JobCodes.ParamByName('JOB_COUNTRY').AsString := EmployeesJOB_COUNTRY.AsString
end;

procedure TEditEmployeeDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Employees.State in [dsEdit,dsInsert] then
  begin
    if ModalResult = mrOK then
      Employees.Post
    else
      Employees.Cancel
  end;
  Employees.Active := false
end;

procedure TEditEmployeeDlg.EmployeesJOB_CODEChange(Sender: TField);
begin
  Countries.Active := false;
  Countries.Active := true;
end;

procedure TEditEmployeeDlg.JobGradeDBComboBoxCloseUp(Sender: TObject);
begin
  (Sender as TCustomDBComboBox).EditingDone
end;

procedure TEditEmployeeDlg.CountriesBeforeOpen(DataSet: TDataSet);
begin
  Countries.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  Countries.ParamByName('JOB_CODE').AsString := EmployeesJOB_CODE.AsString
end;

procedure TEditEmployeeDlg.EmployeesAfterOpen(DataSet: TDataSet);
begin
  Countries.Active := true;
  JobCodes.Active := true;
end;

procedure TEditEmployeeDlg.EmployeesJOB_GRADEChange(Sender: TField);
begin
  Countries.Active := false;
  JobCodes.Active := false;
  Countries.Active := true;
  JobCodes.Active := true;
end;

procedure TEditEmployeeDlg.JobCodesAfterOpen(DataSet: TDataSet);
begin
  JobTitleDBLookupComboBox.KeyValue := Employees.FieldByName('JOB_CODE').AsString
end;

procedure TEditEmployeeDlg.CountriesAfterOpen(DataSet: TDataSet);
begin
  CountryDBLookupComboBox.KeyValue := Employees.FieldByName('JOB_COUNTRY').AsString
end;

procedure TEditEmployeeDlg.FormShow(Sender: TObject);
begin
  Employees.Active := true
end;

procedure TEditEmployeeDlg.EmployeesAfterClose(DataSet: TDataSet);
begin
  Depts.Active := false;
  Countries.Active := false;
  JobCodes.Active := false;
end;

procedure TEditEmployeeDlg.EmployeesAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('HIRE_DATE').AsDateTime := Now;
  DataSet.FieldByName('JOB_CODE').AsString := 'SRep';
  DataSet.FieldByName('JOB_COUNTRY').AsString := 'USA';
  DataSet.FieldByName('DEPT_NO').AsString := '000';
  DataSet.FieldByName('JOB_GRADE').AsInteger := 4;
  DataSet.FieldByName('Salary').AsFloat := 20000;
end;

function TEditEmployeeDlg.ShowModal(Emp_no: integer): TModalResult;
begin
  FEmp_no := Emp_no;
  Result := inherited ShowModal
end;

end.
