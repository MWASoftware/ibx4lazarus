(*
 *  IBX Test suite. This program is used to test the IBX non-visual
 *  components and provides a semi-automated pass/fail check for each test.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)      
unit Test07;

{$mode objfpc}{$H+}

{Test 7: Open and read from Employee Database using TIBSQL}

{  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, IB, IBCustomDataSet, IBDatabase, IBSQL;

const
  aTestID    = '07';
  aTestTitle = 'Open and read from Employee Database using IBSQL';

type

{ TTest07 }

  TTest07 = class(TIBXTestBase)
  private
    FIBSQL: TIBSQL;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
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

  { TTest07 }

procedure TTest07.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBSQL := TIBSQL.Create(Application);
  FIBSQL.Database := IBDatabase;
end;

function TTest07.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest07.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest07.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadWriteTransaction;
end;

procedure TTest07.RunTest(CharSet: AnsiString; SQLDialect: integer);
var stats: TPerfCounters;
begin
  IBDatabase.Connected := true;
  with FIBSQL do
  begin
     SQL.Text := sqlExample + ' Order By 1';
     Transaction.Active := true;
     Prepare;
     PrintMetaData(MetaData);
     Statement.EnableStatistics(true);
     writeln(OutFile,Plan);
     ExecQuery;
     try
       while not EOF do
       begin
         ReportResult(Current);
         Next;
       end;
     finally
       Close;
     end;

     if Statement.GetPerfStatistics(stats) then
       WritePerfStats(stats);
     PrintAffectedRows(IBQuery);
     writeln(OutFile);
     writeln(OutFile,'------------------------------------------------------');
     writeln(OutFile,'With Named Parameter');
     SQL.Text := sqlExample + ' Where Hire_Date < :HireDate';
     Transaction.Active := true;
     ParamByName('HireDate').AsDateTime := StrToDateTime('1/1/1991');
     ExecQuery;
     try
       while not EOF do
       begin
         ReportResult(Current);
         Next;
       end;
     finally
       Close;
     end;
     writeln(OutFile);
     writeln(OutFile,'With Positional Parameter');
     ParamCheck := false;
     SQL.Text := sqlExample + ' Where Hire_Date < ?';
     Transaction.Active := true;
     Params[0].AsDateTime := StrToDateTime('1/1/1990');
     ExecQuery;
     try
       while not EOF do
       begin
         ReportResult(Current);
         Next;
       end;
     finally
       Close;
     end;

     writeln(OutFile);
     writeln(OutFile,'Get Employee Project');
     ParamCheck := true;
     Transaction.Active := true;
     SQL.Text := 'Select * From GET_EMP_PROJ(:EMP_NO)';
     ParamByName('EMP_NO').AsInteger := 4;
     PrintMetaData(MetaData);
     ExecQuery;
     try
       while not EOF do
       begin
         ReportResult(Current);
         Next;
       end;
     finally
       Close;
     end;

     writeln(OutFile);
     writeln(OutFile,'Call Delete Employee - exception expected');
     SQL.Text := 'Execute Procedure Delete_EMPLOYEE :EMP_NO';
     ParamByName('EMP_NO').AsInteger := 11;
     try
       ExecQuery;
     except on E:Exception do
       writeln(OutFile,'Terminated with Exception:',E.Message);
     end;
     Transaction.Rollback;
  end;
end;

initialization
  RegisterTest(TTest07);

end.

