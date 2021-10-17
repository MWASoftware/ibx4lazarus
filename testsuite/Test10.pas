(*
 *  IBX Test suite. This program is used to test the IBX non-visual
 *  components and provides a semi-automated pass/fail check for each test.
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)      
unit Test10;

{$mode objfpc}{$H+}

{Test 10: Create Database from SQL Script and Extract SQL}


interface

uses
  Classes, SysUtils,  TestApplication, IBXTestBase, IB, IBExtract, IBDatabase;

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

