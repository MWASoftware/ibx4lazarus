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
unit Test01;

{$mode objfpc}{$H+}

{Test 1: Open and read from Employee Database}

{  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

  The test is run first with unidirectional buffering and then with bi-directional
  buffering.

  Both client side and server side filters are tested with bi-directional buffering.

  Finally, a beforeOpen handler is used to dynamically filter the results.

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, DB, IB, IBCustomDataSet, IBDatabase, IBQuery;

const
  aTestID    = '01';
  aTestTitle = 'Open and read from Employee Database';

type

{ TTest1 }

  TTest1 = class(TIBXTestBase)
  private
    procedure ClientSideFilter(DataSet: TDataSet; var Accept: Boolean);
    procedure HandleBeforeOpen(DataSet: TDataSet);
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

procedure TTest1.ClientSideFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := DataSet.FieldByName('HIRE_DATE').AsDateTime > EncodeDate(1994 ,1,1);
end;

procedure TTest1.HandleBeforeOpen(DataSet: TDataSet);
begin
  (DataSet as TIBParserDataSet).Parser.Add2WhereClause('FIRST_NAME = :FN');
  (DataSet as TIBQuery).ParamByName('FN').AsString := 'Claudia';
end;

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
     writeln(OutFile,'Read dataset unidirectional buffering');
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
     writeln(OutFile,'Read dataset bidirectional buffering');
     IBDatabase.ReConnect;
     Unidirectional := false;
     Active := true;
     PrintDataSet(IBQuery);
     Active := false;
     writeln(OutFile,'Server Side Filter: Hire Date < 1/1/90');
     SQLFiltered := true;
     SQLFilterParams.Text := 'HIRE_DATE < ''1990.01.01''';
     Active := true;
     PrintDataSet(IBQuery);
     Active := false;
     writeln(Outfile,'Client side Filter: Hire Date > 1/1/94');
     SQLFiltered := false;
     Filtered := true;
     OnFilterRecord := @ClientSideFilter;
     Active := true;
     PrintDataSet(IBQuery);
     Active := false;
     Filtered := false;
     writeln(Outfile,'TIBQuery with open parameters - select only records with First Name = Claudia');
     BeforeOpen := @HandleBeforeOpen;
     Active := true;
     PrintDataSet(IBQuery);

  end;
end;

initialization
  RegisterTest(TTest1);

end.

