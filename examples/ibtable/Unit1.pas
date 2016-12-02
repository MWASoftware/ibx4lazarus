unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, db, IBDatabase, IBTable, IBCustomDataSet, IBDynamicGrid, IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDataSource;
    DataSource2: TDataSource;
    IBDatabase1: TIBDatabase;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    Employees: TIBTable;
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
    Depts: TIBTable;
    DeptsBUDGET: TIBBCDField;
    DeptsDEPARTMENT: TIBStringField;
    DeptsDEPT_NO: TIBStringField;
    DeptsHEAD_DEPT: TIBStringField;
    DeptsLOCATION: TIBStringField;
    DeptsMNGR_NO: TSmallintField;
    DeptsPHONE_NO: TIBStringField;
    IBTransaction1: TIBTransaction;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure DeptsAfterOpen(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Close;
        Exit
      end;
    On E:Exception do
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
end;

procedure TForm1.EmployeesSALARYGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if DisplayText and not Sender.IsNull then
    aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  else
    aText := Sender.AsString;
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  Depts.Active := true;
end;

procedure TForm1.DeptsAfterOpen(DataSet: TDataSet);
begin
  Employees.Active := true;
end;

end.

