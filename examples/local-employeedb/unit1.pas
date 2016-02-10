unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ActnList, EditBtn, DbCtrls, ExtCtrls, Buttons, IBDatabase, IBQuery,
  IBCustomDataSet, IBSQL, IBDynamicGrid, IBLookupComboEditBox,
  IBLocalDBSupport, db, DBExtCtrls, Menus;

const
  RequiredVersionNo = 2;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckVersionTablePresent: TIBSQL;
    DBImage1: TDBImage;
    EmployeesPHOTO1: TBlobField;
    GetDBVersionNoQuery: TIBSQL;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel3: TPanel;
    Quit: TAction;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    RestoreDatabase: TAction;
    SaveDatabase: TAction;
    NewDatabase: TAction;
    DBEdit6: TDBEdit;
    EmployeesDEPT_KEY_PATH: TIBStringField;
    EmployeesDEPT_PATH: TIBStringField;
    IBLocalDBSupport1: TIBLocalDBSupport;
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
    procedure DBImage1DBImageRead(Sender: TObject; S: TStream;
      var GraphExt: string);
    procedure EmployeesAfterPost(DataSet: TDataSet);
    procedure EmployeesValidatePost(Sender: TObject; var CancelPost: boolean);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBLocalDBSupport1GetDBVersionNo(Sender: TObject;
      var VersionNo: integer);
    procedure JobCodeChangeTimerTimer(Sender: TObject);
    procedure JobGradeChangeTimerTimer(Sender: TObject);
    procedure JobGradeDBComboBoxCloseUp(Sender: TObject);
    procedure NewDatabaseExecute(Sender: TObject);
    procedure QuitExecute(Sender: TObject);
    procedure RestoreDatabaseExecute(Sender: TObject);
    procedure SaveDatabaseExecute(Sender: TObject);
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
    procedure EmployeesBeforeOpen(DataSet: TDataSet);
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
    FCurrentDBVersion: integer;
    { private declarations }
    FDirty: boolean;
    FNoAutoReopen: boolean;
    procedure Reopen(Data: PtrInt);
    function GetDBVersionNo: integer;
  public
    { public declarations }
    property CurrentDBVersion: integer read FCurrentDBVersion;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses IB, Unit2;

const
  sNoName = '<no name>';

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
  Countries.Active := true;
  Employees.Active := true;
  JobCodes.Active := true;
  Depts.Active := true;
end;

function TForm1.GetDBVersionNo: integer;
begin
  FCurrentDBVersion := 0;
  Result := 0;
  FNoAutoReopen := true;
  try
    with IBTransaction1 do
      if not InTransaction then StartTransaction;
    try
      with CheckVersionTablePresent do
      begin
        ExecQuery;
        try
          if EOF then Exit;
        finally
          Close;
        end;
      end;

      with GetDBVersionNoQuery do
      begin
        ExecQuery;
        try
          Result := FieldByName('VersionNo').AsInteger;
          FCurrentDBVersion := Result;
        finally
          Close;
        end;
      end;
    finally
      IBTransaction1.Commit;
    end;
  finally
    FNoAutoReopen := false
  end;
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

procedure TForm1.EmployeesAfterPost(DataSet: TDataSet);
begin
  Employees.Refresh
end;

procedure TForm1.DBImage1DBImageRead(Sender: TObject; S: TStream;
  var GraphExt: string);
begin
  GraphExt := 'png';
end;

procedure TForm1.EmployeesValidatePost(Sender: TObject; var CancelPost: boolean
  );
begin
  {Cancel if no name entered}
  CancelPost := (EmployeesLAST_NAME.AsString = sNoName) and  (EmployeesFIRST_NAME.AsString = sNoName);
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  with IBLocalDBSupport1 do
    if CurrentDBVersionNo = RequiredVersionNo  then
      ReOpen(0);
end;

procedure TForm1.IBLocalDBSupport1GetDBVersionNo(Sender: TObject;
  var VersionNo: integer);
begin
  VersionNo := GetDBVersionNo;
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

procedure TForm1.NewDatabaseExecute(Sender: TObject);
begin
  FNoAutoReopen := true;
  try
    {Ensure Transaction End}
    if IBTransaction1.InTransaction then
      IBTransaction1.Rollback;
  finally
    FNoAutoReopen := false;
  end;
  IBLocalDBSupport1.NewDatabase;
end;

procedure TForm1.QuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RestoreDatabaseExecute(Sender: TObject);
begin
  FNoAutoReopen := true;
  try
    {Ensure all changes saved}
    if IBTransaction1.InTransaction then
      IBTransaction1.Commit;
  finally
    FNoAutoReopen := false;
  end;
  IBLocalDBSupport1.RestoreDatabase;
end;

procedure TForm1.SaveDatabaseExecute(Sender: TObject);
begin
  FNoAutoReopen := true;
  try
    {Ensure all changes saved}
    if IBTransaction1.InTransaction then
      IBTransaction1.Commit;
  finally
    FNoAutoReopen := false;
  end;
  IBLocalDBSupport1.SaveDatabase;
  {Start new Transaction and open dataset}
  ReOpen(0);
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
  FNoAutoReopen := true;
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  try
    IBDatabase1.Connected := true;
  except On E:Exception do
    begin
     MessageDlg(E.Message,mtError,[mbOK],0);
     Close;
     Exit
    end;
  end;

  {If upgrade failed or downgrade not pending then exit}
  with IBLocalDBSupport1 do
    if (CurrentDBVersionNo < RequiredVersionNo) or
       ((CurrentDBVersionNo >  RequiredVersionNo) and not DowngradePending) then
    Close;
end;

procedure TForm1.EmployeesAfterDelete(DataSet: TDataSet);
begin
  FDirty := true
end;

procedure TForm1.EmployeesAfterTransactionEnd(Sender: TObject);
begin
  FDirty := false;
  if not FNoAutoReopen then
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

