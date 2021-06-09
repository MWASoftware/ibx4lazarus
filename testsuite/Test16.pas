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
unit Test16;

{$mode objfpc}{$H+}

{Test 16: TIBTable in master/detail relationship}

{ Description
  Open two tables: DEPARATMENT (Master) and EMPLOYEE (Slave) from example
  database and navigate in a master/detail relationship.

  Update employee record to illustrate update returning.

  Insert employee record to illustrate insert returning and generator

  Delete Employee record

  Rollback transaction
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBTable,
  IBCustomDataset;

const
  aTestID    = '16';
  aTestTitle = 'TIBTable in master/detail relationship';

type

{ TTest16 }

  TTest16 = class(TIBXTestBase)
  private
    FDepartmentTable: TIBTable;
    FEmployeeTable: TIBTable;
    FDataSource: TDataSource;
    procedure HandleDeptOpen(aDataSet: TDataSet);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest16 }

procedure TTest16.HandleDeptOpen(aDataSet: TDataSet);
begin
  FEmployeeTable.Active := true;
end;

procedure TTest16.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FDepartmentTable := TIBTable.Create(Application);
  with FDepartmentTable do
  begin
    Database := IBDatabase;
    TableName := 'DEPARTMENT';
    IndexFieldNames := 'DEPT_NO';
    ReadOnly := true;
    AfterOpen := @HandleDeptOpen;
  end;
  FDataSource := TDataSource.Create(Application);
  FDataSource.DataSet := FDepartmentTable;
  FEmployeeTable := TIBTable.Create(Application);
  with FEmployeeTable do
  begin
    Database := IBDatabase;
    TableName := 'EMPLOYEE';
    MasterSource := FDataSource;
    IndexFieldNames := 'DEPT_NO';
    MasterFields := 'DEPT_NO';
    with GeneratorField do
    begin
      ApplyOnEvent := gaeOnNewRecord;
      Field := 'EMP_NO';
      Generator := 'EMP_NO_GEN';
    end;
  end;
end;

function TTest16.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest16.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest16.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadWriteTransaction;
end;

procedure TTest16.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  try
    FDepartmentTable.Active := true;
    writeln(OutFile);
    writeln(OutFile,'Department No ',FDepartmentTable.FieldByName('DEPT_NO').AsString,' Selected');
    PrintDataSet(FEmployeeTable);
    if FDepartmentTable.Locate('DEPT_NO','600',[]) then
    begin
      writeln(OutFile);
      writeln(OutFile,'Department No ',FDepartmentTable.FieldByName('DEPT_NO').AsString,' Selected');
      PrintDataSet(FEmployeeTable);
    end
    else
      writeln(OutFile,'Unable to locate Dept 600');
    if FDepartmentTable.Locate('DEPT_NO','621',[]) then
    begin
      writeln(OutFile);
      writeln(OutFile,'Department No ',FDepartmentTable.FieldByName('DEPT_NO').AsString,' Selected');
      PrintDataSet(FEmployeeTable);
    end
    else
      writeln(OutFile,'Unable to locate Dept 621');

    writeln(OutFile);
    writeln(OutFile,'Update Row with Computed Field');
    with FEmployeeTable do
    begin
      Edit;
      FieldByName('FIRST_NAME').AsString := 'Boris';
      Post;
    end;
    PrintDataSetRow(FEmployeeTable);
  finally
    IBTransaction.Rollback;
    IBDatabase.Connected := false;
  end;
end;

initialization
  RegisterTest(TTest16);

end.

