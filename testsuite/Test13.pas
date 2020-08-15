unit Test13;

{$mode objfpc}{$H+}

{Test 13: Open and read from Employee Database with ISQLMonitor}

{  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

  ISQLMonitor is used to trace the activity.

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, IB, IBCustomDataSet, IBDatabase,
  IBQuery, IBInternals, IBSQLMonitor;

const
  aTestID    = '13';
  aTestTitle = 'Open and read from Employee Database with ISQLMonitor';

type

{ TTest13 }

  TTest13 = class(TIBXTestBase)
  private
    FIBSQLMonitor: TIBSQLMonitor;
    FLog: TStringList;
    procedure HandleOnSQL(EventText: String; EventTime : TDateTime);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    destructor Destroy; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

const
  sqlExample =
'with recursive Depts As (   '+
'Select DEPT_NO, DEPARTMENT, HEAD_DEPT, cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH,'+
'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH '+
'From DEPARTMENT Where HEAD_DEPT is NULL '+
'UNION ALL '+
'Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || D.DEPARTMENT as DEPT_PATH,'+
'Depts.DEPT_KEY_PATH || '';'' || D.DEPT_NO as DEPT_KEY_PATH '+
'From DEPARTMENT D '+
'JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO '+
')'+

'Select First 2 A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE,'+
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

  { TTest13 }

procedure TTest13.HandleOnSQL(EventText: String; EventTime: TDateTime);
begin
  FLog.Add('*Monitor* '+DateTimeToStr(EventTime)+' '+EventText);
end;

procedure TTest13.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBSQLMonitor := TIBSQLMonitor.Create(Application);
  FIBSQLMonitor.TraceFlags := [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc];
  IBDatabase.TraceFlags := [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc];
  FIBSQLMonitor.OnSQL := @HandleOnSQL;
  FIBSQLMonitor.Enabled := true;
  FLog := TStringList.Create;
end;

function TTest13.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest13.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest13.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
end;

destructor TTest13.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

procedure TTest13.RunTest(CharSet: AnsiString; SQLDialect: integer);
var stats: TPerfCounters;
    i: integer;
begin
  writeln(OutFile);
  EnableMonitoring;
  CheckSynchronize(1);
  with IBQuery do
  begin
     AllowAutoActivateTransaction := true;
     Unidirectional := true;
     SQL.Text := sqlExample;
     EnableStatistics := true;
     Active := true;
     PrintDataSet(IBQuery);

     if GetPerfStatistics(stats) then
       WritePerfStats(stats);
     PrintAffectedRows(IBQuery);
     writeln(OutFile);
     writeln(OutFile,'Reconnect');
     IBDatabase.ReConnect;
     Unidirectional := false;
     Active := true;
     PrintDataSet(IBQuery);
  end;
  IBDatabase.Connected := false;
  CheckSynchronize(1);
  DisableMonitoring;
  for i := 0 to FLog.Count - 1 do
    writeln(OutFile,FLog[i]);
end;

initialization
  RegisterTest(TTest13);

end.

