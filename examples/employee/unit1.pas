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
            
unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ActnList, EditBtn, DbCtrls, ExtCtrls, Buttons, IBDatabase, IBQuery,
  IBCustomDataSet, IBUpdateSQL, IBDynamicGrid, IBLookupComboEditBox,
  db, DBExtCtrls, IBDynamicInterfaces;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBEdit6: TDBEdit;
    DBNavigator1: TDBNavigator;
    EmployeesDEPT_KEY_PATH: TIBStringField;
    EmployeesDEPT_PATH: TIBStringField;
    EmployeesTEst: TStringField;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    IBLookupComboEditBox2: TIBLookupComboEditBox;
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
    SelectDept: TAction;
    Button4: TButton;
    Button5: TButton;
    CancelChanges: TAction;
    SalaryRange: TComboBox;
    CountrySource: TDataSource;
    BeforeDate: TDateEdit;
    AfterDate: TDateEdit;
    DeptsSource: TDataSource;
    Depts: TIBQuery;
    JobCodeSource: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBText1: TDBText;
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
    IBDynamicGrid1: TIBDynamicGrid;
    Countries: TIBQuery;
    JobCodes: TIBQuery;
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
    JobGradeChangeTimer: TTimer;
    JobCodeChangeTimer: TTimer;
    TotalsQueryTOTALSALARIES: TIBBCDField;
    TotalsSource: TDataSource;
    TotalsQuery: TIBQuery;
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
    IBTransaction1: TIBTransaction;
    procedure EmployeesValidatePost(Sender: TObject; var CancelPost: boolean);
    procedure IBDynamicGrid1SetParams(Sender : TObject;
      SQLParamProvider : IDynamicSQLParam);
    procedure IBDynamicGrid1UpdateSQL(Sender : TObject;
      SQLEditor : IDynamicSQLEditor);
    procedure JobCodeChangeTimerTimer(Sender: TObject);
    procedure JobGradeChangeTimerTimer(Sender: TObject);
    procedure JobGradeDBComboBoxCloseUp(Sender: TObject);
    procedure SelectDeptExecute(Sender: TObject);
    procedure AddEmployeeExecute(Sender: TObject);
    procedure BeforeDateChange(Sender: TObject);
    procedure CancelChangesExecute(Sender: TObject);
    procedure CountriesBeforeOpen(DataSet: TDataSet);
    procedure DeleteEmployeeExecute(Sender: TObject);
    procedure EditEmployeeExecute(Sender: TObject);
    procedure EditEmployeeUpdate(Sender: TObject);
    procedure EmployeesAfterInsert(DataSet: TDataSet);
    procedure EmployeesAfterOpen(DataSet: TDataSet);
    procedure EmployeesAfterScroll(DataSet: TDataSet);
    procedure EmployeesBeforeClose(DataSet: TDataSet);
    procedure EmployeesJOB_CODEChange(Sender: TField);
    procedure EmployeesJOB_GRADEChange(Sender: TField);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure EmployeesAfterDelete(DataSet: TDataSet);
    procedure EmployeesAfterTransactionEnd(Sender: TObject);
    procedure EmployeesPostError(DataSet: TDataSet; E: EDatabaseError;
      var DataAction: TDataAction);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure JobCodesBeforeOpen(DataSet: TDataSet);
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

uses IB, Unit2, IBMessages;

const
  sNoName = '<no name>';

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

procedure TForm1.JobCodesBeforeOpen(DataSet: TDataSet);
begin
  JobCodes.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  JobCodes.ParamByName('JOB_COUNTRY').AsString := EmployeesJOB_COUNTRY.AsString
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
  Employees.EnableStatistics := true;
  Employees.Active := true;
  Countries.Active := true;
  JobCodes.Active := true;
  Depts.Active := true;
end;

procedure TForm1.AddEmployeeExecute(Sender: TObject);
begin
  Employees.Append
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
    IBDynamicGrid1.ShowEditorPanel;
  end;
end;

procedure TForm1.EmployeesValidatePost(Sender: TObject; var CancelPost: boolean
  );
begin
  {Cancel if no name entered}
  CancelPost := (EmployeesLAST_NAME.AsString = sNoName) and  (EmployeesFIRST_NAME.AsString = sNoName);
end;

procedure TForm1.IBDynamicGrid1SetParams(Sender : TObject;
  SQLParamProvider : IDynamicSQLParam);
begin
  if BeforeDate.Date <> NullDate then
     SQLParamProvider.SetParamValue('BeforeDate',BeforeDate.Date);
  if AfterDate.Date <> NullDate then
   SQLParamProvider.SetParamValue('AfterDate',AfterDate.Date);
end;

procedure TForm1.IBDynamicGrid1UpdateSQL(Sender : TObject;
  SQLEditor : IDynamicSQLEditor);
begin
   if BeforeDate.Date <> NullDate then
     SQLEditor.Add2WhereClause('HIRE_DATE < :BeforeDate');
  if AfterDate.Date <> NullDate then
     SQLEditor.Add2WhereClause('HIRE_DATE > :AfterDate');

  case SalaryRange.ItemIndex of
  1:
    SQLEditor.Add2WhereClause('Salary < 40000');
  2:
    SQLEditor.Add2WhereClause('Salary >= 40000 and Salary < 100000');
  3:
    SQLEditor.Add2WhereClause('Salary >= 100000');
  end;
end;

procedure TForm1.JobCodeChangeTimerTimer(Sender: TObject);
begin
  Countries.Active := false;
  Countries.Active := true;
  JobCodeChangeTimer.Interval := 0;
end;

procedure TForm1.JobGradeChangeTimerTimer(Sender: TObject);
begin
  Countries.Active := false;
  JobCodes.Active := false;
  Countries.Active := true;
  JobCodes.Active := true;
  JobGradeChangeTimer.Interval := 0;
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

procedure TForm1.CountriesBeforeOpen(DataSet: TDataSet);
begin
  Countries.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  Countries.ParamByName('JOB_CODE').AsString := EmployeesJOB_CODE.AsString
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
  IBDynamicGrid1.ShowEditorPanel;
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
  IBDynamicGrid1.SetFocus;
end;

procedure TForm1.EmployeesAfterScroll(DataSet: TDataSet);
begin
  JobGradeChangeTimer.Interval := 200;
end;

procedure TForm1.EmployeesBeforeClose(DataSet: TDataSet);
begin
  TotalsQuery.Active := false
end;

procedure TForm1.EmployeesJOB_CODEChange(Sender: TField);
begin
  JobCodeChangeTimer.Interval := 200;
end;

procedure TForm1.EmployeesJOB_GRADEChange(Sender: TField);
begin
  JobGradeChangeTimer.Interval := 200;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  try
    if IBTransaction1.InTransaction then
      IBTransaction1.Commit;

  except on E: Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      IBDatabase1.ForceClose;
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  IBDatabase1.FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  Application.ExceptionDialog := aedOkMessageBox;
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
  Reopen(0);
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
       MessageDlg(EIBError(E).message,mtError,[mbOK],0);
       DataSet.Cancel;
       DataAction  := daAbort
   end;
end;

end.

