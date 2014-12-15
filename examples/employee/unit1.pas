unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ActnList, EditBtn, DbCtrls, ExtCtrls, IBDatabase, IBQuery,
  IBCustomDataSet, IBUpdateSQL, IBDynamicGrid, IBDateEdit, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button4: TButton;
    Button5: TButton;
    CancelChanges: TAction;
    CountrySource: TDataSource;
    Countries: TIBDataSet;
    BeforeDate: TDateEdit;
    AfterDate: TDateEdit;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBText1: TDBText;
    IBDateEdit1: TIBDateEdit;
    IBDynamicGrid1: TIBDynamicGrid;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    EmpoyeeEditorPanel: TPanel;
    TotalsQueryTOTALSALARIES: TIBBCDField;
    TotalsSource: TDataSource;
    EmployeesDEPARTMENT: TIBStringField;
    EmployeesSALARY: TIBBCDField;
    TotalsQuery: TIBQuery;
    IBUpdateSQL1: TIBUpdateSQL;
    Label1: TLabel;
    Label2: TLabel;
    SaveChanges: TAction;
    DeleteEmployee: TAction;
    EditEmployee: TAction;
    AddEmployee: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    EmployeeSource: TDataSource;
    IBDatabase1: TIBDatabase;
    Employees: TIBQuery;
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
    IBTransaction1: TIBTransaction;
    procedure AddEmployeeExecute(Sender: TObject);
    procedure BeforeDateChange(Sender: TObject);
    procedure CancelChangesExecute(Sender: TObject);
    procedure DeleteEmployeeExecute(Sender: TObject);
    procedure EditEmployeeExecute(Sender: TObject);
    procedure EditEmployeeUpdate(Sender: TObject);
    procedure EmployeesAfterOpen(DataSet: TDataSet);
    procedure EmployeesBeforeClose(DataSet: TDataSet);
    procedure EmployeesBeforeOpen(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure EmployeesAfterDelete(DataSet: TDataSet);
    procedure EmployeesAfterTransactionEnd(Sender: TObject);
    procedure EmployeesPostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
  private
    { private declarations }
    FDirty: boolean;
    FClosing: boolean;
    procedure Reopen(Data: PtrInt);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses Unit2, Unit3, IB;

function ExtractDBException(msg: string): string;
var Lines: TStringList;
begin
     Lines := TStringList.Create;
     try
       Lines.Text := msg;
       if pos('exception',Lines[0]) = 1 then
         Result := Lines[2]
       else
         Result := msg
     finally
       Lines.Free
     end;
end;

{ TForm1 }

procedure TForm1.EmployeesSALARYGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if DisplayText then
  begin
    if Sender.IsNUll then
      aText := ''
    else
      aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  end
  else
    aText := Sender.AsString
end;

procedure TForm1.SaveChangesExecute(Sender: TObject);
begin
  Employees.Transaction.Commit
end;

procedure TForm1.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FDirty
end;

procedure TForm1.Reopen(Data: PtrInt);
begin
  with IBTransaction1 do
    if not InTransaction then StartTransaction;
  Employees.Active := true
end;

procedure TForm1.AddEmployeeExecute(Sender: TObject);
var NewEmpNo: integer;
begin
  if AddEmployeeDlg.ShowModal(NewEmpNo) = mrOK then
  begin
    FDirty := true;
    Employees.Active := false;
    Employees.Active := true;
    Employees.Locate('EMP_NO',NewEmpNo,[])
  end;
end;

procedure TForm1.BeforeDateChange(Sender: TObject);
begin
  Employees.Active := false;
  Employees.Active := true
end;

procedure TForm1.CancelChangesExecute(Sender: TObject);
begin
  Employees.Transaction.Rollback
end;

procedure TForm1.DeleteEmployeeExecute(Sender: TObject);
begin
  if MessageDlg(
    Format('Remove %s from Employee List?',[Employees.FieldByName('Full_Name').AsString]),
    mtConfirmation,[mbYes,mbNo],0) = mrYes then
    Employees.Delete
end;

procedure TForm1.EditEmployeeExecute(Sender: TObject);
begin
  if EditEmployeeDlg.ShowModal(Employees.FieldByName('Emp_No').AsInteger) = mrOK then
  begin
    FDirty := true;
    Employees.Refresh
  end;
end;

procedure TForm1.EditEmployeeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Employees.Active and (Employees.RecordCount > 0)
end;

procedure TForm1.EmployeesAfterOpen(DataSet: TDataSet);
begin
  TotalsQuery.Active := true
end;

procedure TForm1.EmployeesBeforeClose(DataSet: TDataSet);
begin
  TotalsQuery.Active := false
end;

procedure TForm1.EmployeesBeforeOpen(DataSet: TDataSet);
begin
  if BeforeDate.Date > 0 then
     (DataSet as TIBQuery).Parser.Add2WhereClause('HIRE_DATE < :BeforeDate');
  if AfterDate.Date > 0 then
     (DataSet as TIBQuery).Parser.Add2WhereClause('HIRE_DATE > :AfterDate');
  if BeforeDate.Date > 0 then
     (DataSet as TIBQuery).ParamByName('BeforeDate').AsDateTime := BeforeDate.Date;
  if AfterDate.Date > 0 then
   (DataSet as TIBQuery).ParamByName('AfterDate').AsDateTime := AfterDate.Date;

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  IBTransaction1.Commit;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Countries.Active := true;
  Employees.Active := true;
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  with IBTransaction1 do
    if not InTransaction then StartTransaction
end;

procedure TForm1.IBDatabase1BeforeDisconnect(Sender: TObject);
begin
  FClosing := true
end;

procedure TForm1.EmployeesAfterDelete(DataSet: TDataSet);
begin
  FDirty := true
end;

procedure TForm1.EmployeesAfterTransactionEnd(Sender: TObject);
begin
  FDirty := false;
  if not FClosing then
    Application.QueueAsyncCall(@Reopen,0)
end;

procedure TForm1.EmployeesPostError(DataSet: TDataSet; E: EDatabaseError;
  var DataAction: TDataAction);
begin
  if E is EIBError then
   begin
       MessageDlg(ExtractDBException(EIBError(E).message),mtError,[mbOK],0);
       DataSet.Cancel;
       DataAction  := daAbort
   end;
end;

end.
