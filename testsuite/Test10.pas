unit Test10;

{$mode objfpc}{$H+}

{Test 10: Create Database from SQL Script and Extract SQL}


interface

uses
  Classes, SysUtils,  TestApplication, IBXTestbase, IB, IBExtract, IBDatabase;

const
  aTestID    = '10';
  aTestTitle = 'Create Database from SQL Script and Extract SQL';

type

{ TTest10 }

  TTest10 = class(TIBXTestBase)
  private
    FExtract: TIBExtract;
    procedure HandleExtractLine(Sender: TObject; start, count: integer);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    procedure InitialiseDatabase(aDatabase: TIBDatabase); override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest10 }

procedure TTest10.HandleExtractLine(Sender: TObject; start, count: integer);
var i: integer;
begin
  for i := 0 to count - 1 do
    writeln(OutFile,FExtract.Items[start + i]);
end;

procedure TTest10.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
  FExtract.OnExtractLines := @HandleExtractLine;
end;

function TTest10.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest10.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest10.InitTest;
begin
  ReadWriteTransaction;
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
end;

procedure TTest10.InitialiseDatabase(aDatabase: TIBDatabase);
begin
  IBXScriptObj.StopOnFirstError := false;
  inherited InitialiseDatabase(aDatabase);
end;

procedure TTest10.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  writeln(OutFile);
  writeln(OutFile,'Extracting Database Schema and Data');
  FExtract.ExtractObject(eoDatabase,'',[etData,etGrantsToUser]);
  WriteStrings(FExtract.Items);
  FExtract.Items.SaveToFile(GetOutFile);
  writeln(OutFile,'Schema written to ',GetOutFile);
  IBDatabase.DropDatabase;
end;

initialization
  RegisterTest(TTest10);

end.

