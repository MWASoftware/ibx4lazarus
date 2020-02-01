unit Test02;

{$mode objfpc}{$H+}

{Test 2: Test use of the Local DB Manager}

{
  Requires Firebird Embedded Server - with FB 2.5 and lower this implies FBEmbedded

  Creates a new local database from a backup archive.

  Updates the schema

  Prints out results of a query.
}

interface

uses
  Classes, SysUtils, CustApp, TestManager, IBXTestManager, IB, IBCMLocalDBSupport, IBSQL,
  IBQuery, IBDatabase;

const
  aTestID    = '2';
  aTestTitle = 'Test 2: create, bring up-to-date and read from local database';

type

{ TTest2 }

  TTest2 = class(TIBXTestBase)
  private
    FLocalDB: TIBCMLocalDBSupport;
    procedure HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
    procedure HandleLogMessage(Sender: TObject; Msg: string);
    procedure GetSharedDirectory(Sender: TObject; var SharedDataDir: string);
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
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

'Select First 2 A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE,'+
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, A.PHOTO, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

  { TTest2 }

procedure TTest2.HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
begin
  VersionNo := 0;
  IBTransaction.Active := true;
  try
    with TIBSQL.Create(Owner.Application) do
    try
      Database := IBDatabase;
      Transaction := IBTransaction;
      SQL.Text := 'Select * From RDB$RELATIONS Where RDB$RELATION_NAME = ''DBVERSIONINFO''';
      ExecQuery;
      try
        if EOF then Exit;
      finally
        Close;
      end;
    finally
      Free
    end;

    with TIBSQL.Create(Owner.Application)  do
    try
      Database := IBDatabase;
      Transaction := IBTransaction;
      SQL.Text := 'Select VersionNo From DBVersionInfo';
      ExecQuery;
      try
        VersionNo := FieldByName('VersionNo').AsInteger;
      finally
        Close;
      end;
    finally
      Free;
    end;
  finally
    IBTransaction.Commit;
  end;
end;

procedure TTest2.HandleLogMessage(Sender: TObject; Msg: string);
begin
  writeln(OutFile,Msg);
end;

procedure TTest2.GetSharedDirectory(Sender: TObject; var SharedDataDir: string);
begin
  SharedDataDir := 'resources';
end;

procedure TTest2.CreateObjects(Application: TCustomApplication);
begin
  inherited CreateObjects(Application);
  FLocalDB := TIBCMLocalDBSupport.Create(Application);
  FLocalDB.Database := IBDatabase;
  FLocalDB.VendorName := 'MWA Software';
  FLocalDB.OnGetDBVersionNo := @HandleGetDBVersionNo;
  FLocalDB.OnLogMessage := @HandleLogMessage;
  FLocalDB.OnGetSharedDataDir := @GetSharedDirectory;
end;

function TTest2.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest2.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest2.InitTest;
begin
  IBDatabase.DatabaseName := 'nemo';
  FLocalDB.DatabaseName := ExtractDBName(Owner.GetNewDatabaseName);
  FLocalDB.EmptyDBArchive := 'employee.gbk';
  FLocalDB.RequiredVersionNo := 2;
  FLocalDB.UpgradeConfFile := 'upgrade.conf';
  ReadOnlyTransaction;
end;

procedure TTest2.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  with IBQuery do
  begin
     AllowAutoActivateTransaction := true;
     SQL.Text := sqlExample;
     Active := true;
     PrintDataSet(IBQuery);
  end;
  IBDatabase.DropDatabase;
end;

initialization
  RegisterTest(TTest2);

end.

