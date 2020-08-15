unit Test14;

{$mode objfpc}{$H+}

{ Test 14: IBStored Proc with packages}

{
This demonstrates the use of TIBStoredProc with the Firebird 3 packages example
provided with the Firebird 3 source code. A database is created in a temporary
location when the application is first run. Once this completes, IBStoredProc2 calls
the global stored procedure "test", which populates the temporary table FB$OUT.

IBStoredProc1 then calls the GET_LINES procedure in the FB$OUT package. It returns
a text blob which is then printed out.

}


interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBStoredProc,
  IBDatabase;

const
  aTestID    = '14';
  aTestTitle = 'IBStored Proc with packages';

type

{ TTest14 }

  TTest14 = class(TIBXTestBase)
  private
    FIBStoredProc1: TIBStoredProc;
    FIBStoredProc2: TIBStoredProc;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    procedure InitialiseDatabase(aDatabase: TIBDatabase); override;
    function SkipTest: boolean; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest14 }

procedure TTest14.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  IBQuery.SQL.Text := 'Select A.LINE_NUM, A.CONTENT From FB$OUT_TABLE A';
  FIBStoredProc1 := TIBStoredProc.Create(Application);
  FIBStoredProc1.Database := IBDatabase;
  FIBStoredProc2 := TIBStoredProc.Create(Application);
  FIBStoredProc2.Database := IBDatabase;
end;

function TTest14.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest14.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest14.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  FIBStoredProc1.PackageName := 'FB$OUT';
  FIBStoredProc1.StoredProcName := 'GET_LINES';
  FIBStoredProc2.StoredProcName := 'TEST';
  ReadWriteTransaction;
end;

procedure TTest14.InitialiseDatabase(aDatabase: TIBDatabase);
begin
  if aDatabase.attachment.GetODSMajorVersion < 12 then
  begin
    aDatabase.DropDatabase;
    raise ESkipException.Create('This test requires Firebird 3');
  end;
  RunScript(aDatabase,'resources/fbout-header.sql');
  RunScript(aDatabase,'resources/fbout-body.sql');
  RunScript(aDatabase,'resources/fbout-test.sql');
  IBTransaction.Commit;
end;

function TTest14.SkipTest: boolean;
begin
  Result := FirebirdAPI.GetClientMajor < 3;
end;

procedure TTest14.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    FIBStoredProc2.ExecProc;
    IBTransaction.Commit;
    IBTransaction.Active := true;
    IBQuery.Active := true;
    PrintDataSet(IBQuery);
    FIBStoredProc1.ExecProc;
    writeln(OutFile,FIBStoredProc1.ParamByName('LINES').AsString);
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest14);

end.

