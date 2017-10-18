unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ActnList, EditBtn, DbCtrls, ExtCtrls, Buttons, IBDatabase, IBQuery,
  IBCustomDataSet, IBUpdateSQL, IBDynamicGrid, IBLookupComboEditBox,
  db, DBExtCtrls, DBControlGrid, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    EditLocationAction: TAction;
    EditJobCodeAction: TAction;
    DBEdit6: TDBEdit;
    DBControlGrid1: TDBControlGrid;
    DBEdit7: TDBEdit;
    DBEdit8: TDBEdit;
    DBText1: TDBText;
    EmployeesDEPT_KEY_PATH: TIBStringField;
    EmployeesDEPT_PATH: TIBStringField;
    EmployeesJOB_TITLE: TIBStringField;
    SelectDept: TAction;
    Button4: TButton;
    Button5: TButton;
    CancelChanges: TAction;
    SalaryRange: TComboBox;
    BeforeDate: TDateEdit;
    AfterDate: TDateEdit;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
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
    IBDateEdit1: TDBDateEdit;
    JobGradeDBComboBox: TDBComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    EmployeeEditorPanel: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TotalsQuery: TIBQuery;
    TotalsQueryTOTALSALARIES: TIBBCDField;
    Label1: TLabel;
    Label2: TLabel;
    SaveChanges: TAction;
    DeleteEmployee: TAction;
    EditEmployee: TAction;
    AddEmployee: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button3: TButton;
    EmployeeSource: TDataSource;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    TotalsSource: TDataSource;
    procedure EditJobCodeActionExecute(Sender: TObject);
    procedure EditJobCodeActionUpdate(Sender: TObject);
    procedure EditLocationActionExecute(Sender: TObject);
    procedure EmployeesAfterPost(DataSet: TDataSet);
    procedure JobGradeDBComboBoxCloseUp(Sender: TObject);
    procedure SelectDeptExecute(Sender: TObject);
    procedure AddEmployeeExecute(Sender: TObject);
    procedure BeforeDateChange(Sender: TObject);
    procedure CancelChangesExecute(Sender: TObject);
    procedure DeleteEmployeeExecute(Sender: TObject);
    procedure EditEmployeeUpdate(Sender: TObject);
    procedure EmployeesAfterInsert(DataSet: TDataSet);
    procedure EmployeesAfterOpen(DataSet: TDataSet);
    procedure EmployeesBeforeClose(DataSet: TDataSet);
    procedure EmployeesBeforeOpen(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure EmployeesAfterDelete(DataSet: TDataSet);
    procedure EmployeesAfterTransactionEnd(Sender: TObject);
    procedure EmployeesPostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
    procedure TotalsQueryTOTALSALARIESGetText(Sender: TField;
      var aText: string; DisplayText: Boolean);
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

uses IB, Unit2, Unit4, Unit5;

const sNoName = '<no name>';

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

procedure TForm1.TotalsQueryTOTALSALARIESGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if DisplayText then
  begin
    if Sender.IsNUll then
      aText := ''
    else
      aText := FormatFloat('Total Salary Bill = $#,##0.00',Sender.AsFloat)
  end
  else
    aText := Sender.AsString
end;

procedure TForm1.Reopen(Data: PtrInt);
begin
  with IBTransaction1 do
    if not InTransaction then StartTransaction;
  Employees.Active := true;
end;

procedure TForm1.AddEmployeeExecute(Sender: TObject);
begin
  Employees.Append;
  DBControlGrid1.SetFocus;
end;

procedure TForm1.SelectDeptExecute(Sender: TObject);
var Dept_No: string;
begin
  if SelectDeptDlg.ShowModal(EmployeesDEPT_KEY_PATH.AsString,Dept_No) = mrOK then
  begin
    Employees.Edit;
    EmployeesDEPT_NO.AsString := Dept_No;
    try
      Employees.Post;
    except
      Employees.Cancel;
      raise;
    end;
  end;
end;

procedure TForm1.EmployeesAfterPost(DataSet: TDataSet);
begin
  Employees.Refresh
end;

procedure TForm1.EditJobCodeActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Employees.Active and (Employees.RecordCount > 0 )
end;

procedure TForm1.EditLocationActionExecute(Sender: TObject);
var Country: string;
begin
  Country := EmployeesJOB_COUNTRY.AsString;
  if EditLocation.ShowModal(EmployeesJOB_GRADE.AsInteger, EmployeesJOB_CODE.AsString,
     Country) = mrOK then
     begin
       Employees.Edit;
       try
         EmployeesJOB_COUNTRY.AsString := Country;
         Employees.Post;
       except
         Employees.Cancel;
         raise
       end;
     end;
end;

procedure TForm1.EditJobCodeActionExecute(Sender: TObject);
var JobCode: string;
begin
  JobCode := EmployeesJOB_CODE.AsString;
  if EditJobCode.ShowModal(EmployeesJOB_GRADE.AsInteger,EmployeesJOB_COUNTRY.AsString,
    JobCode) = mrOK then
    begin
      Employees.Edit;
      try
        EmployeesJOB_CODE.AsString := JobCode;
        Employees.Post;
      except
        Employees.Cancel;
        raise
      end;
    end;
end;

procedure TForm1.JobGradeDBComboBoxCloseUp(Sender: TObject);
begin
  JobGradeDBComboBox.EditingDone; //See http://bugs.freepascal.org/view.php?id=27186
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
    Employees.Delete;
    DBControlGrid1.SetFocus;
end;

procedure TForm1.EditEmployeeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Employees.Active and (Employees.RecordCount > 0)
end;

procedure TForm1.EmployeesAfterInsert(DataSet: TDataSet);
begin
  EmployeesJOB_COUNTRY.AsString := 'USA';
  EmployeesJOB_CODE.AsString := 'SRep';
  EmployeesJOB_GRADE.AsInteger := 4;
  EmployeesSALARY.AsCurrency := 20000;
  EmployeesFIRST_NAME.AsString := sNoName;
  EmployeesLAST_NAME.AsString := sNoName;
  EmployeesHIRE_DATE.AsDateTime := now;
  EmployeesDEPT_NO.AsString := '000';
  FDirty := true;
end;

procedure TForm1.EmployeesAfterOpen(DataSet: TDataSet);
begin
  TotalsQuery.Active := true;
  DBControlGrid1.SetFocus;
end;

procedure TForm1.EmployeesBeforeClose(DataSet: TDataSet);
begin
  TotalsQuery.Active := false
end;

procedure TForm1.EmployeesBeforeOpen(DataSet: TDataSet);
begin
  if BeforeDate.Date > 0 then
     (DataSet as TIBParserDataSet).Parser.Add2WhereClause('HIRE_DATE < :BeforeDate');
  if AfterDate.Date > 0 then
     (DataSet as TIBParserDataSet).Parser.Add2WhereClause('HIRE_DATE > :AfterDate');

  case SalaryRange.ItemIndex of
  1:
    (DataSet as TIBParserDataSet).Parser.Add2WhereClause('Salary < 40000');
  2:
    (DataSet as TIBParserDataSet).Parser.Add2WhereClause('Salary >= 40000 and Salary < 100000');
  3:
    (DataSet as TIBParserDataSet).Parser.Add2WhereClause('Salary >= 100000');
  end;



  {Parameter value must be set after all SQL changes have been made}
  if BeforeDate.Date > 0 then
     (DataSet as TIBParserDataSet).ParamByName('BeforeDate').AsDateTime := BeforeDate.Date;
  if AfterDate.Date > 0 then
   (DataSet as TIBParserDataSet).ParamByName('AfterDate').AsDateTime := AfterDate.Date;

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit;
end;

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
  Application.QueueAsyncCall(@Reopen,0);
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

