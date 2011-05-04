unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ActnList, IBDatabase, IBQuery, IBCustomDataSet, IBUpdateSQL, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    CancelChanges: TAction;
    IBUpdateSQL1: TIBUpdateSQL;
    SaveChanges: TAction;
    DeleteEmployee: TAction;
    EditEmployee: TAction;
    AddEmployee: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBQuery1DEPT_NO: TIBStringField;
    IBQuery1EMP_NO: TSmallintField;
    IBQuery1FIRST_NAME: TIBStringField;
    IBQuery1FULL_NAME: TIBStringField;
    IBQuery1HIRE_DATE: TDateTimeField;
    IBQuery1JOB_CODE: TIBStringField;
    IBQuery1JOB_COUNTRY: TIBStringField;
    IBQuery1JOB_GRADE: TSmallintField;
    IBQuery1LAST_NAME: TIBStringField;
    IBQuery1PHONE_EXT: TIBStringField;
    IBQuery1SALARY: TIBBCDField;
    IBTransaction1: TIBTransaction;
    procedure AddEmployeeExecute(Sender: TObject);
    procedure CancelChangesExecute(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DeleteEmployeeExecute(Sender: TObject);
    procedure EditEmployeeExecute(Sender: TObject);
    procedure EditEmployeeUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBQuery1AfterDelete(DataSet: TDataSet);
    procedure IBQuery1AfterOpen(DataSet: TDataSet);
    procedure IBQuery1AfterTransactionEnd(Sender: TObject);
    procedure IBQuery1BeforeClose(DataSet: TDataSet);
    procedure IBQuery1BeforeOpen(DataSet: TDataSet);
    procedure IBQuery1PostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure IBQuery1SALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
  private
    { private declarations }
    FDirty: boolean;
    FClosing: boolean;
    FLastEmp_no: integer;
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

procedure TForm1.IBQuery1SALARYGetText(Sender: TField; var aText: string;
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
  IBQuery1.Transaction.Commit
end;

procedure TForm1.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FDirty
end;

procedure TForm1.Reopen(Data: PtrInt);
begin
  with IBTransaction1 do
    if not InTransaction then StartTransaction;
  IBQuery1.Active := true
end;

procedure TForm1.AddEmployeeExecute(Sender: TObject);
var NewEmpNo: integer;
begin
  if AddEmployeeDlg.ShowModal(NewEmpNo) = mrOK then
  begin
    FDirty := true;
    IBQuery1.Active := false;
    FLastEmp_no := NewEmpNo;
    IBQuery1.Active := true
  end;
end;

procedure TForm1.CancelChangesExecute(Sender: TObject);
begin
  IBQuery1.Transaction.Rollback
end;

procedure TForm1.DBGrid1DblClick(Sender: TObject);
begin
  if IBQuery1.Active and (IBQuery1.RecordCount > 0) then
    EditEmployeeExecute(nil)
end;

procedure TForm1.DeleteEmployeeExecute(Sender: TObject);
begin
  if MessageDlg(
    Format('Remove %s from Employee List?',[IBQuery1.FieldByName('Full_Name').AsString]),
    mtConfirmation,[mbYes,mbNo],0) = mrYes then
    IBQuery1.Delete
end;

procedure TForm1.EditEmployeeExecute(Sender: TObject);
begin
  if EditEmployeeDlg.ShowModal(IBQuery1.FieldByName('Emp_No').AsInteger) = mrOK then
  begin
    FDirty := true;
    IBQuery1.Refresh
  end;
end;

procedure TForm1.EditEmployeeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBQuery1.Active and (IBQuery1.RecordCount > 0)
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FLastEmp_no := -1
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

procedure TForm1.IBQuery1AfterDelete(DataSet: TDataSet);
begin
  FDirty := true
end;

procedure TForm1.IBQuery1AfterOpen(DataSet: TDataSet);
begin
  if FLastEmp_no <> -1 then
    DataSet.Locate('EMP_NO',FLastEmp_no,[])
end;

procedure TForm1.IBQuery1AfterTransactionEnd(Sender: TObject);
begin
  FDirty := false;
  if not FClosing then
    Application.QueueAsyncCall(@Reopen,0)
end;

procedure TForm1.IBQuery1BeforeClose(DataSet: TDataSet);
begin
  FLastEmp_no := DataSet.FieldByName('Emp_no').AsInteger
end;

procedure TForm1.IBQuery1BeforeOpen(DataSet: TDataSet);
begin
end;

procedure TForm1.IBQuery1PostError(DataSet: TDataSet; E: EDatabaseError;
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

