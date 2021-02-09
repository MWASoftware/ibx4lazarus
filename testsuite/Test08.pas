unit Test08;

{$mode objfpc}{$H+}

{Test 8: TIBDataset: Locate, Bookmark and Lookup}

{  This test uses IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.
  It includes SQL parsing, and parameter setting prior to opening. The dataset
  is also searched abd bookmarked

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, DB, IB, IBCustomDataSet, IBDatabase, IBQuery;

const
  aTestID    = '08';
  aTestTitle = 'TIBDataset: Locate, Bookmark and Lookup';

type

  { TCalcQuery }

  TCalcQuery = class(TIBQuery)
  protected
    procedure CreateFields; override;
  end;

  { TTest08 }

  TTest08 = class(TIBXTestBase)
  private
    FQuery: TIBQuery;
    procedure HandleBeforeOpen(DataSet: TDataSet);
    procedure HandleCalcFields(DataSet: TDataSet);
    procedure PrintFields(aDataSet: TDataSet);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses Variants, DateUtils;

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
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO order by 1';

{ TCalcQuery }

procedure TCalcQuery.CreateFields;
var  f: TFieldDef;
begin
  FieldDefs.Update;
  f := FieldDefs.AddFieldDef;
  f.Name := 'HireYear';
  f.DataType := ftInteger;
  f.InternalCalcField := true;
  f := FieldDefs.AddFieldDef;
  f.Name := 'HireDatePlus1';
  f.DataType := ftDate;
  f.InternalCalcField := true;
  inherited CreateFields;
  FieldByName('HireYear').FieldKind := fkCalculated;
  FieldByName('HireDatePlus1').FieldKind := fkCalculated;
end;

  { TTest08 }

procedure TTest08.HandleBeforeOpen(DataSet: TDataSet);
begin
  (Dataset as TIBParserDataset).Parser.Add2WhereClause('A.HIRE_DATE < :HIREDATE');
  (DataSet as TIBQuery).ParamByName('HireDate').AsString := '1/5/1991';
end;

procedure TTest08.HandleCalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('HireYear').AsInteger := YearOf(DataSet.FieldByName('HIRE_DATE').AsDateTime);
  DataSet.FieldByName('HireDatePlus1').AsDateTime := DataSet.FieldByName('HIRE_DATE').AsDateTime + 1;
end;

procedure TTest08.PrintFields(aDataSet: TDataSet);
var i: integer;
begin
  for i := 0 to aDataSet.Fields.Count - 1 do
    with aDataSet.Fields[i] do
    writeln(OutFile,'Field No ',FieldNo,' Name = ',FieldName, ' DataType = ',DataType);
end;

procedure TTest08.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FQuery := TCalcQuery.Create(Application);
  FQuery.Database := IBDatabase;
  FQuery.BeforeOpen := @HandleBeforeOpen;
  FQuery.OnCalcFields := @HandleCalcFields;
end;

function TTest08.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest08.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest08.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
  FQuery.SQL.Text := sqlExample;
end;

procedure TTest08.RunTest(CharSet: AnsiString; SQLDialect: integer);
var stats: TPerfCounters;
    aBookmark: TBookmark;
    aResultFields: variant;
    i: integer;
begin
  with FQuery do
  begin
     AllowAutoActivateTransaction := true;
     Active := true;
     PrintFields(FQuery);
     if Locate('EMP_NO', 12, []) then
       PrintDataSetRow(FQuery)
     else
     begin
       writeln(OutFile,'Error: Row not found for EMP_NO = 12');
       Exit;
     end;

     aBookmark := Bookmark;
     if Locate('FIRST_NAME;LAST_NAME', VarArrayOf(['roger','Reeves']), [loCaseInsensitive]) then
       PrintDataSetRow(FQuery)
     else
     begin
       writeln(OutFile,'Error: Row not found for Roger Reeves');
       Exit;
     end;
     Bookmark := aBookmark;
     writeln(OutFile,'Back to EMP_NO = 12');
     PrintDataSetRow(FQuery);

     writeln(OutFile,'Locate Employee 20, First Name and Last Name');
     aResultFields := Lookup('EMP_NO',20,'FIRST_NAME;LAST_NAME');
     if varType(aResultFields) <> varNull then
     begin
       for i := VarArrayLowBound(aResultFields,1) to VarArrayHighBound(aResultFields,1) do
         writeln(OutFile,'Field No. ',i,' = ',aResultFields[i]);
     end
     else
     begin
       writeln(OutFile,'Lookup Failed');
       Exit;
     end;


     writeln(OutFile);
     writeln(OutFile,'Print All');
     PrintDataSet(FQuery);
     Active := false;
  end;
end;

initialization
  RegisterTest(TTest08);

end.

