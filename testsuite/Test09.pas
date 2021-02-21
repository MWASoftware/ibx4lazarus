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
unit Test09;

{$mode objfpc}{$H+}

{Test 9: Extract DDL from example Employee Database}

interface

uses
  Classes, SysUtils,  TestApplication, IBXTestbase, IB, IBExtract;

const
  aTestID    = '09';
  aTestTitle = 'Extract DDL from example Employee Database';

type

{ TTest09 }

  TTest09 = class(TIBXTestBase)
  private
    FExtract: TIBExtract;
    procedure HandleExtractLine(Sender: TObject; start, count: integer);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest09 }

procedure TTest09.HandleExtractLine(Sender: TObject; start, count: integer);
var i: integer;
begin
  for i := 0 to count - 1 do
    writeln(OutFile,FExtract.Items[start + i]);
end;

procedure TTest09.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
  FExtract.OnExtractLines := @HandleExtractLine;
end;

function TTest09.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest09.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest09.InitTest;
begin
  ReadOnlyTransaction;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
end;

procedure TTest09.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  writeln(OutFile);
  writeln(OutFile,'Extracting Database Schema and Data');
  FExtract.ExtractObject(eoDatabase,'',[etGrantsToUser]);
end;

initialization
  RegisterTest(TTest09);

end.

