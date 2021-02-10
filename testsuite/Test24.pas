unit Test24;

{$mode objfpc}{$H+}

{Test 24: IB Parser Tests}

{Iterates through a set of test strings, parsing each one and displaying the
 results including any parameters found. Note last string is the empty string}


interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBSQLParser, IBUtils;

const
  aTestID    = '24';
  aTestTitle = 'IB Parser Tests';

const
  TestStrings: array [0..13] of string = (
  'Select distinct A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE, 2.2, 2..30 '  +
       'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH' + LF +
       'From EMPLOYEE A' +  LF +
       'JOIN Depts D On D.DEPT_NO = A.DEPT_NO;',

  'with recursive Depts As (' +
        'Select DEPT_NO, DEPARTMENT, "HEAD_DEPT", cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH /* test */,' +
        'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH ' + LF +
        'From DEPARTMENT Where HEAD_DEPT is NULL ' + LF +
        'UNION ALL' + CRLF +
        'Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || D.DEPARTMENT as DEPT_PATH,' +
        'Depts.DEPT_KEY_PATH || '';'' || D.DEPT_NO as DEPT_KEY_PATH ' +
        'From DEPARTMENT  D'+ CR +
        'JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO' + LF +
        ')' +
        '--ignore' + LF +
        'Select distinct A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE, 2.2, 2..30 '  +
        'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH' + LF +
        'From EMPLOYEE A' +  LF +
        'JOIN Depts D On D.DEPT_NO = A.DEPT_NO',

        'Update EMPLOYEE A Set '#13#10'  A.DEPT_NO = :DEPT_NO,'#13#10 +
        '  A.FIRST_NAME = ''Mr/Ms '' || :FIRST_NAME,'#13#10+
        '  A.HIRE_DATE = :HIRE_DATE,'#13#10+
        '  A.JOB_CODE = :JOB_CODE,'#13#10'  A.JOB_COUNTRY = :JOB_COUNTRY,'#13#10+
        '  A.JOB_GRADE = :JOB_GRADE,'#13#10'  A.LAST_NAME = :LAST_NAME,'#13#10+
        '  A.PHONE_EXT = :PHONE_EXT,'#13#10'  A.SALARY = :SALARY ' +
        'Where A.EMP_NO = :OLD_EMP_NO;',

        'INSERT INTO EMPLOYEE (EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE,' +
        'DEPT_NO, JOB_CODE, JOB_GRADE, JOB_COUNTRY, SALARY /* what''s this */) '+
        'VALUES (:EMP_NO, :FIRST_NAME, :LAST_NAME, :PHONE_EXT, :HIRE_DATE, //end comment' + CRLF +
        ':DEPT_NO, :"JOB""CODE", ''Tester''''s way'''''''''', :JOB_COUNTRY, :"$SALARY")',

        'Select * from EMPLOYEE Where EMP_NO = :EMP_NO',

        'Select DEPT_NO, DEPARTMENT, "HEAD_DEPT", cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH /* test */,' +
        'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH ' + LF +
        'From DEPARTMENT Where HEAD_DEPT is NULL ' + LF +
        'UNION ALL' + CRLF +
        'Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || D.DEPARTMENT as DEPT_PATH,' +
        'Depts.DEPT_KEY_PATH || '';'' || D.DEPT_NO as DEPT_KEY_PATH ' +
        'From DEPARTMENT  D'+ CR +
        'JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO',

        'Select DEPT_NO, DEPARTMENT, "HEAD_DEPT", cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH /* test */,' +
        'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH ' + LF +
        'From DEPARTMENT Where HEAD_DEPT is NULL ' + LF +
        'UNION ALL' + CRLF +
        'Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || D.DEPARTMENT as DEPT_PATH,' +
        'Depts.DEPT_KEY_PATH || '';'' || D.DEPT_NO as DEPT_KEY_PATH ' +
        'From DEPARTMENT  D'+ CR +
        'JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO '+
        'Order by 1,2',

        'with test1 as (Select * from Employee), test2 as (Select Emp_no, ''A test'' from Employee) '+
        'Select * from test1',

      'with recursive Account_Types_List As('#10+
      'Select ACCOUNTTYPE_ID, PARENT, ACCOUNTTYPENAME,'#10+
      '  ACCOUNTTYPENAME as SortName, '''' as INDENT'#10+
      'From MYLB_ACCOUNTTYPES'#10+
      'Where PARENT is Null'#10+
      'UNION ALL'#10+
      'Select A.ACCOUNTTYPE_ID, A.PARENT, A.ACCOUNTTYPENAME,'#10+
      'L.SortName || A.ACCOUNTTYPENAME as SortName, L.INDENT || ''    '' as INDENT'#10+
      'From MYLB_ACCOUNTTYPES A'#10+
      'Join Account_Types_List L On A.PARENT = L.ACCOUNTTYPE_ID)'#10+
      #10+
      'Select ACCOUNTTYPE_ID, PARENT, INDENT || ACCOUNTTYPENAME as Name'#10+
      'From Account_Types_List'#10+
      #10+
      'Order by SortName',

      'SELECT r.RDB$DB_KEY, cast(''Local'' as VarChar(6)) as MAPTYPE, r.RDB$MAP_NAME,'#10+
      '    Case'#10+
      '      When r.RDB$MAP_USING = ''P'' and r.RDB$MAP_PLUGIN is not null then cast (''Plugin '' || r.RDB$MAP_PLUGIN as VarChar(20))'#10+
      '      When r.RDB$MAP_USING = ''P'' and r.RDB$MAP_PLUGIN is null then ''Any Plugin'''#10+
      '      When r.RDB$MAP_USING = ''S'' then ''Any Plugin Serverwide'''#10+
      '      When r.RDB$MAP_USING = ''M'' then ''Mapping'''#10+
      '      When r.RDB$MAP_USING = ''*'' then ''*'''#10+
      '      else'#10+
      '        ''Using '' || r.RDB$MAP_USING || '','' || coalesce(r.RDB$MAP_PLUGIN,'''') End as MAP_USING,'#10+
      '    r.RDB$MAP_USING, r.RDB$MAP_PLUGIN,'#10+
      '    r.RDB$MAP_DB, r.RDB$MAP_FROM_TYPE, r.RDB$MAP_FROM,'#10+
      '    Trim(r.RDB$MAP_FROM_TYPE) || '': '' || Trim(r.RDB$MAP_FROM) as MAP_FROM,'#10+
      '    r.RDB$MAP_TO_TYPE,'#10+
      '    T.RDB$TYPE_NAME as MAP_TO_TYPE, r.RDB$MAP_TO,'#10+
      '    Trim(T.RDB$TYPE_NAME) || '': '' || Trim(r.RDB$MAP_TO) as MAP_TO,'#10+
      '    r.RDB$SYSTEM_FLAG, r.RDB$DESCRIPTION'#10+
      'FROM RDB$AUTH_MAPPING r'#10+
      'JOIN RDB$TYPES T On T.RDB$TYPE = r.RDB$MAP_TO_TYPE and T.RDB$FIELD_NAME = ''RDB$MAP_TO_TYPE'''#10+
      'UNION'#10+
      'SELECT r.RDB$DB_KEY, ''Global'', r.SEC$MAP_NAME,'#10+
      '    Case'#10+
      '      When r.SEC$MAP_USING = ''P'' and r.SEC$MAP_PLUGIN is not null then cast (''Plugin '' || r.SEC$MAP_PLUGIN as VarChar(20))'#10+
      '      When r.SEC$MAP_USING = ''P'' and r.SEC$MAP_PLUGIN is null then ''Any Plugin'''#10+
      '      When r.SEC$MAP_USING = ''S'' then ''Any Plugin Serverwide'''#10+
      '      When r.SEC$MAP_USING = ''M'' then ''Mapping'''#10+
      '      When r.SEC$MAP_USING = ''*'' then ''*'''#10+
      '      else'#10+
      '        ''Using '' || r.SEC$MAP_USING || '','' || coalesce(r.SEC$MAP_PLUGIN,'''') End as MAP_USING,'#10+
      '    r.SEC$MAP_USING, r.SEC$MAP_PLUGIN,'#10+
      '    r.SEC$MAP_DB, r.SEC$MAP_FROM_TYPE,  r.SEC$MAP_FROM,'#10+
      '    Trim(r.SEC$MAP_FROM_TYPE) || '': '' || Trim(r.SEC$MAP_FROM) as MAP_FROM,'#10+
      '    r.SEC$MAP_TO_TYPE,'#10+
      '    T.RDB$TYPE_NAME as MAP_TO_TYPE,r.SEC$MAP_TO,'#10+
      '    Trim(T.RDB$TYPE_NAME) || '': '' || Trim(r.SEC$MAP_TO) as MAP_TO,'#10+
      '    null,null'#10+
      'FROM SEC$GLOBAL_AUTH_MAPPING r'#10+
      'JOIN RDB$TYPES T On T.RDB$TYPE = r.SEC$MAP_TO_TYPE and T.RDB$FIELD_NAME = ''RDB$MAP_TO_TYPE''',

      'Select * From A '+
      'Group By a1,b1 '+
      'Union '+
      'Select * From B Rows 3 to 4',

      'Select * From A '+
      'Union '+
      'Select * From B Order by 1 Rows 3 to 4',

      'Select * From A '+
      'Union '+
      'Select * From B '+
      'Union '+
      'Select * FROM C Order by 1',

       '')  ;
type

{ TTest24 }

  TTest24 = class(TIBXTestBase)
  private
    procedure WriteSelect(parser: TSelectSQLParser);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest24 }

procedure TTest24.WriteSelect(parser: TSelectSQLParser);
var j: integer;
begin
  with parser do
  begin
    for j := 0 to CTECount -1 do
    begin
      write(OutFile,'CTE',j);
      if CTES[j]^.Recursive then write(OutFile,' recursive');
      writeln(': ',CTES[j]^.Name,' as ',CTEs[j]^.text);
    end;
    writeln('Select:' ,SelectClause);
    writeln('From: ',FromClause);
    writeln('Where: ',WhereClause);
    writeln('Group By: ',GroupClause);
    writeln('Having: ',HavingClause);
    if Union <> nil then
    begin
      write(OutFile,'Union');
      if Union.UnionAll then write(OutFile,' All');
      writeln(OutFile);
      WriteSelect(Union);
    end;
    writeln('Order by: ',OrderByClause);
    writeln('Plan: ',PlanClause);
    writeln('Rows: ',RowsClause);
    writeln('SQL: ',SQLText);
    writeln('Params: ');
    for j := 0 to ParamList.Count - 1 do
      write(OutFile,ParamList[j],',');
    writeln(OutFile);
    writeln(OutFile);
  end;
end;

function TTest24.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest24.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest24.RunTest(CharSet: AnsiString; SQLDialect: integer);
var i: integer;
    parser: TSelectSQLParser;
begin
  for i := 0 to Length(TestStrings) -1 do
  begin
    parser :=  TSelectSQLParser.Create(nil,TestStrings[i]);
    try
    with parser do
    begin
      writeln(OutFile,'Test String ',i);
      writeln(OutFile,'---------------------------------------------------');
      writeln(OutFile,TestStrings[i]);
      writeln(OutFile,'---------------------------------------------------');
      writeln(OutFile);
      if NotaSelectStmt then
      begin
        writeln(OutFile,'Not a Select Statement');
        continue;
      end;
      WriteSelect(parser);
      if i = 4 then
        Add2WhereClause('JOB_CODE = 2');
      if i = 5 then
         OrderByClause := '1';
      writeln(Outfile,'Updated SQL');
      writeln(OutFile,SQLText);
    end;
    finally
      parser.Free
    end;
  end;
end;

initialization
  RegisterTest(TTest24);

end.

