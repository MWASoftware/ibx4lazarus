unit Test06;

{$mode objfpc}{$H+}

{Test 6: Multi-Database Transaction}

{ creates two databases and adds a record to each as a multi-database transaction.
  Commit and print results.
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBDatabase, IBSQL;

const
  aTestID    = '06';
  aTestTitle = 'Multi-Database Transaction';

type

{ TTest6 }

  TTest6 = class(TIBXTestBase)
  private
    FIBDatabase2: TIBDatabase;
    FIBTransaction2: TIBTransaction;
    FIBMultiTransaction: TIBTransaction;
    FIBSQL: TIBSQL;
    procedure HandleCreateDatebase(Sender: TObject);
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
  InsertSQL = 'Insert into IBXTEST(TABLEKEY, F1) VALUES(GEN_ID(IBXGEN,1),?) Returning TABLEKEY';
  SelectSQL = 'Select TABLEKEY, F1 From IBXTest';

{ TTest6 }

procedure TTest6.HandleCreateDatebase(Sender: TObject);
begin
  InitialiseDatabase(FIBDatabase2);
end;

procedure TTest6.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBDatabase2 := TIBDatabase.Create(Application);
  FIBDatabase2.FirebirdLibraryPathName := Owner.ClientLibraryPath;
  FIBDatabase2.LoginPrompt := false;
  FIBDatabase2.Params.Add('user_name=' + Owner.GetUserName);
  FIBDatabase2.Params.Add('password=' + Owner.GetPassword);
  FIBDatabase2.Params.Add('lc_ctype=UTF8');
  FIBDatabase2.OnCreateDatabase := @HandleCreateDatebase;
  FIBDatabase2.Name := 'Test_Database2_' + GetTestID;
  FIBTransaction2 := TIBTransaction.Create(Application);
  FIBTransaction2.DefaultDatabase := FIBDatabase2;
  FIBDatabase2.DefaultTransaction := FIBTransaction2;
  FIBTransaction2.Name := 'Test_Transaction2_' + GetTestID;
  FIBTransaction2.Params.Add('concurrency');
  FIBTransaction2.Params.Add('wait');
  FIBTransaction2.Params.Add('write');
  FIBMultiTransaction := TIBTransaction.Create(Application);
  FIBMultiTransaction.AddDatabase(IBDatabase);
  FIBMultiTransaction.AddDatabase(FIBDatabase2);
  FIBMultiTransaction.Name := 'Multi_Transaction';
  FIBMultiTransaction.Params.Add('concurrency');
  FIBMultiTransaction.Params.Add('wait');
  FIBMultiTransaction.Params.Add('write');
  FIBSQL := TIBSQL.Create(Application);
  FIBSQL.ParamCheck := false;
end;

function TTest6.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest6.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest6.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
  FIBDatabase2.DatabaseName := Owner.GetSecondNewDatabaseName;
  FIBDatabase2.CreateIfNotExists := true;
end;

procedure TTest6.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  try
    IBDatabase.Connected := true;
    FIBDatabase2.Connected := true;
    FIBMultiTransaction.Active := true;
    FIBSQL.Database := IBDatabase;
    FIBSQL.Transaction := FIBMultiTransaction;
    FIBSQL.SQL.Text := InsertSQL;
    FIBSQL.Params[0].AsInteger := 1;
    writeln(OutFile,'Add Row to First Database');
    FIBSQL.ExecQuery;
    ReportResult(FIBSQL.Current);
    FIBSQL.Database := FIBDatabase2;
    FIBSQL.Params[0].AsInteger := 2;
    writeln(OutFile,'Add Row to Second Database');
    FIBSQL.ExecQuery;
    ReportResult(FIBSQL.Current);
    FIBMultiTransaction.Commit;
    FIBSQL.Database := IBDatabase;
    FIBSQL.Transaction := IBTransaction;
    FIBSQL.SQL.Text := SelectSQL;
    IBTransaction.Active := true;
    FIBSQL.Prepare;
    writeln(OutFile,'Query First Database');
    ReportResults(FIBSQL.Statement);
    FIBSQL.Database := FIBDatabase2;
    FIBSQL.Transaction := FIBTransaction2;
    FIBTransaction2.Active := true;
    FIBSQL.Prepare;
    writeln(OutFile,'Query Second Database');
    ReportResults(FIBSQL.Statement);
  finally
    IBDatabase.DropDatabase;
    FIBDatabase2.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest6);

end.

