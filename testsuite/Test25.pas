unit Test25;

{$mode objfpc}{$H+}

{Test 25: TIBTable Tests}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB, IBTable,
  IBCustomDataSet, IBExtract;

const
  aTestID    = '25';
  aTestTitle = 'TIBTable Tests';

type

{ TTest25 }

  TTest25 = class(TIBXTestBase)
  private
    FIBTable: TIBTable;
    FIBExtract: TIBExtract;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest25 }

procedure TTest25.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBTable := TIBTable.Create(Application);
  with FIBTable do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    TableName := 'TestTable';
    with FieldDefs do
    begin
      Add('MYKEY',ftInteger,4);
      Add('TEXTFIELD',ftString,32);
    end;
    IndexDefs.Add('PrimaryIndex','MYKEY',[ixPrimary]);
  end;
  FIBExtract := TIBExtract.Create(Application);
  FIBExtract.Database := IBDatabase;
  FIBExtract.Transaction := IBTransaction;
  FIBExtract.CaseSensitiveObjectNames := true;
end;

function TTest25.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest25.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest25.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest25.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  try
    FIBTable.CreateTable;
    IBTransaction.Commit;
    IBTransaction.Active := true;
    writeln(Outfile,'IBTable after create');
    FIBExtract.ExtractObject(eoTable,'TestTable');
    writeln(Outfile,FIBExtract.Items.Text);
    FIBTable.Active := true;
    PrintDataset(FIBTable);
    writeln(Outfile,'Add 2 rows');
    FIBTable.AppendRecord([1,'Test 1']);
    FIBTable.AppendRecord([2,'Test 2']);
    PrintDataset(FIBTable);
    writeln(Outfile,'Update first row');
    with FIBTable do
    begin
      if Locate('MYKEY',1,[]) then
      begin
        Edit;
        FieldByName('TextField').Asstring := 'Updated Test 1';
        Post;
      end;
    end;
    PrintDataset(FIBTable);
    writeln(Outfile,'Delete first row');
    with FIBTable do
      if Locate('MYKEY',1,[]) then
        Delete;
    PrintDataset(FIBTable);
    writeln(Outfile,'Empty the Table');
    FIBTable.EmptyTable;
    PrintDataset(FIBTable);
    writeln(Outfile,'Now drop the table');
    FIBTable.Active := false;
    FIBTable.DeleteTable;
    IBTransaction.Commit;
    IBTransaction.Active := true;
    writeln(Outfile,'Extract table after drop - should be empty');
    FIBExtract.ExtractObject(eoTable,'TestTable');
    writeln(Outfile,FIBExtract.Items.Text);
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest25);

end.

