unit Test10;

{$mode objfpc}{$H+}

{Test 1: Test 10: Create Database from SQL Script and Extract SQL}


interface

uses
  Classes, SysUtils, CustApp, TestApplication, IBXTestbase, IB, IBExtract;

const
  aTestID    = '10';
  aTestTitle = 'Test 10: Create Database from SQL Script and Extract SQL';

type

{ TTest10 }

  TTest10 = class(TIBXTestBase)
  private
    FExtract: TIBExtract;
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitialiseDatabase; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest10 }

procedure TTest10.CreateObjects(Application: TCustomApplication);
begin
  inherited CreateObjects(Application);
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
end;

function TTest10.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest10.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest10.InitialiseDatabase;
begin
  writeln(OutFile,'Creating Database');
  RunScript('resources/Test10.sql');
end;

procedure TTest10.InitTest;
begin
  ReadWriteTransaction;
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
end;

procedure TTest10.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  writeln(OutFile);
  writeln(OutFile,'Extracting Database Schema and Data');
  FExtract.ExtractObject(eoDatabase,'',[etData,etGrantsToUser]);
  IBDatabase.DropDatabase;
end;

initialization
  RegisterTest(TTest10);

end.

