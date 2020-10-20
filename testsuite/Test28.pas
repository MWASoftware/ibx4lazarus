unit Test28;

{$mode objfpc}{$H+}

{Test 28: Create Local Database from Script}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBCMLocalDBSupport,
  IBExtract, IBSQL;

const
  aTestID    = '28';
  aTestTitle = 'Create Local Database from Script';

type

{ TTest28 }

  TTest28 = class(TIBXTestBase)
  private
    FLocalDB: TIBCMLocalDBSupport;
    FExtract: TIBExtract;
    procedure HandleExtractLine(Sender: TObject; start, count: integer);
    procedure HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
    procedure HandleLogMessage(Sender: TObject; Msg: string);
    procedure GetSharedDirectory(Sender: TObject; var SharedDataDir: string);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest28 }

procedure TTest28.HandleExtractLine(Sender: TObject; start, count: integer);
var i: integer;
begin
  for i := 0 to count - 1 do
    writeln(OutFile,FExtract.Items[start + i]);
end;

procedure TTest28.HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
begin
  VersionNo := 0;
  IBTransaction.Active := true;
  try
    with TIBSQL.Create(Owner) do
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

    with TIBSQL.Create(Owner)  do
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

procedure TTest28.HandleLogMessage(Sender: TObject; Msg: string);
begin
  writeln(OutFile,Msg);
end;

procedure TTest28.GetSharedDirectory(Sender: TObject; var SharedDataDir: string
  );
begin
  SharedDataDir := 'resources/Test28';
end;

procedure TTest28.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FLocalDB := TIBCMLocalDBSupport.Create(Application);
  FLocalDB.Database := IBDatabase;
  FLocalDB.VendorName := 'MWA Software';
  FLocalDB.OnGetDBVersionNo := @HandleGetDBVersionNo;
  FLocalDB.OnLogMessage := @HandleLogMessage;
  FLocalDB.OnGetSharedDataDir := @GetSharedDirectory;
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
  FExtract.OnExtractLines := @HandleExtractLine;
end;

function TTest28.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest28.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest28.InitTest;
begin
  IBDatabase.DatabaseName := 'nemo';
  FLocalDB.DatabaseName := ExtractDBName(Owner.GetNewDatabaseName);
  FLocalDB.EmptyDBArchive := 'schema.sql';
  FLocalDB.RequiredVersionNo := 1;
end;

procedure TTest28.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  try
    writeln(Outfile,'Show schema for ',IBDatabase.DatabaseName);
    FExtract.ExtractObject(eoDatabase,'',[etGrantsToUser,etData]);

    writeln(Outfile,'Now test out an upgrade failure');
    IBDatabase.connected := false;
    FLocalDB.RequiredVersionNo := 2;
    FLocalDB.UpgradeConfFile := 'upgrade.conf';
    IBDatabase.connected := true;
    writeln(Outfile,'Schema after failed upgrade is');
    FExtract.ExtractObject(eoDatabase,'',[etGrantsToUser,etData]);
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest28);

end.

