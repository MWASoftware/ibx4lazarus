unit Test21;

{$mode objfpc}{$H+}

{Test 21: Big dataset test}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBSQL, MD5;

const
  aTestID    = '21';
  aTestTitle = 'Test 21: Big dataset test';

type

{ TTest21 }

  TTest21 = class(TIBXTestBase)
  private
    FIBSQL: TIBSQL;
    function StuffDatabase: TMDDigest;
    function ReadDatabase: TMDDigest;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses DateUtils;

const
  testText = 'The quick brown fox jumped over the lazy dog';
  RecordCount = 100000;

{ TTest21 }

function TTest21.StuffDatabase: TMDDigest;
var i: integer;
    HashString: AnsiString;
    MD5Context: TMDContext;
    Started: TDateTime;
begin
  Started := Now;
  writeln(Outfile,'Loading data into database table. Started at ',DateTimeToStr(Started));
  MDInit(MD5Context,MD_VERSION_5);
  IBTransaction.Active := true;
  for i := 1 to RecordCount do
  with FIBSQL do
  begin
    Params[0].AsInteger := i;
    Params[1].AsString := DateTimeToStr(Now) + testText + testText + testText + testText;
    Params[2].AsDateTime := Now;
    HashString := Params[0].Asstring + Params[1].AsString + DateTimeToStr(Params[2].AsDateTime);
    ExecQuery;
    MDUpdate(MD5Context,PAnsiChar(HashString)^,Length(HashString));
  end;
  IBTransaction.Commit;
  MDFinal(MD5Context,Result);
  writeln(OutFile, 'Data load completed at ',DateTimeToStr(Now), ' Elapsed Time = ',
    (MilliSecondsBetween(Now,Started)),' ms, ',RecordCount,' records loaded');
  writeln(Outfile,' MD5 checksum = ',MD5Print(Result));
end;

function TTest21.ReadDatabase: TMDDigest;
var MD5Context: TMDContext;
    Started: TDateTime;
    HashString: AnsiString;
    Count: integer;
begin
  MDInit(MD5Context,MD_VERSION_5);
  IBTransaction.Active := true;
  Started := Now;
  Count := 0;
  writeln(Outfile,'Database Read started at ',DateTimeToStr(Started));
  with IBQuery do
  begin
    Active := true;
    while not EOF do
    begin
      Inc(Count);
      HashString := Fields[0].AsString + Fields[1].AsString + DateTimeToStr(Fields[2].AsDateTime);
      MDUpdate(MD5Context,PAnsiChar(HashString)^,Length(HashString));
      Next;
    end;
  end;
  writeln(OutFile, 'Read Dataset completed at ',DateTimeToStr(Now), ' Elapsed Time = ',
    (MilliSecondsBetween(Now,Started)), ' ms, ',Count,' records read');
  MDFinal(MD5Context,Result);
  writeln(Outfile,' MD5 checksum = ',MD5Print(Result));
end;

procedure TTest21.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBSQL := TIBSQL.Create(Application);
  with FIBSQL do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    SQL.Text :=  'Insert into LotsOfData(RowID,Mytext,theDate) Values(?,?,?)';
    ParamCheck := false;
  end;
  IBQuery.SQL.Text := 'Select RowID,MyText,theDate from LotsOfData';
end;

function TTest21.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest21.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest21.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest21.RunTest(CharSet: AnsiString; SQLDialect: integer);
var Digest: TMD5Digest;
begin
  IBDatabase.CreateDatabase;
  try
    Digest := StuffDatabase;  {This creates a database holding a large 100,000 record table}
    if MD5Match(Digest,ReadDatabase) then
      writeln(Outfile,'Test Completed successfully')
    else
      writeln(Outfile,'Test failed. MD5 checksum error');
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest21);

end.

