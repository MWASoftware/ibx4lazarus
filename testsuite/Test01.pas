unit Test01;

{$mode objfpc}{$H+}

{Test 1: Open and read from Employee Database}

{  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, IB, IBCustomDataSet, IBDatabase, IBQuery;

const
  aTestID    = '01';
  aTestTitle = 'Open and read from Employee Database';

type

{ TTest1 }

  TTest1 = class(TIBXTestBase)
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
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

'Select A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE,'+
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

  { TTest1 }

function TTest1.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest1.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest1.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
end;

procedure TTest1.RunTest(CharSet: AnsiString; SQLDialect: integer);
var stats: TPerfCounters;
begin
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
     Active := true;
     PrintDataSet(IBQuery);
  end;
end;

initialization
  RegisterTest(TTest1);

end.

