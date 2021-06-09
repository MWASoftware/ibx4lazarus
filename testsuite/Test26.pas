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
unit Test26;

{$mode objfpc}{$H+}

{Test 26: IBXScript data out and exceptional conditions}

{ Dumps data in CSV, Insert and Table formats + error conditions
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, IB,
  IBXScript, IBDataOutput;

const
  aTestID    = '26';
  aTestTitle = 'IBXScript data out and exceptional conditions';

type

{ TTest26 }

  TTest26 = class(TIBXTestBase)
  private
    FDataOutputCSVFormater: TIBCSVDataOut;
    FDataOutputSQLFormater: TIBInsertStmtsOut;
    FDataOutputBlockFormater: TIBBlockFormatOut;
    procedure HandleOutputLog(Sender: TObject; Msg: string);
    procedure HandleErrorLog(Sender: TObject; Msg: string);
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
  sQuery = 'Select * From PROJECT;';
  sTestSET = 'Set Blah;' + LINEENDING + 'Select First 2 * from JOB;';

{ TTest26 }

procedure TTest26.HandleOutputLog(Sender: TObject; Msg: string);
begin
  writeln(Outfile,Msg);
end;

procedure TTest26.HandleErrorLog(Sender: TObject; Msg: string);
begin
  writeln(Outfile,'Script Error: ',Msg);
end;

procedure TTest26.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
//  IBXScriptObj.Echo := false;
  IBXScriptObj.OnOutputLog := @HandleOutputLog;
  IBXScriptObj.OnErrorLog := @HandleErrorLog;
  FDataOutputCSVFormater := TIBCSVDataOut.Create(Application);
  FDataOutputSQLFormater := TIBInsertStmtsOut.Create(Application);
  FDataOutputBlockFormater := TIBBlockFormatOut.Create(Application);
end;

function TTest26.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest26.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest26.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
end;

procedure TTest26.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    IBXScriptObj.DataOutputFormatter := FDataOutputCSVFormater;
    writeln(Outfile,'Dump project table to CSV');
    IBXScriptObj.ExecSQLScript(sQuery);
    IBXScriptObj.DataOutputFormatter := FDataOutputSQLFormater;
    writeln(Outfile,'Dump project table to Insert Statements');
    IBXScriptObj.ExecSQLScript(sQuery);
    IBXScriptObj.DataOutputFormatter := FDataOutputBlockFormater;
    writeln(Outfile,'Dump project table to Data Block Format');
    IBXScriptObj.ExecSQLScript(sQuery);
    writeln(Outfile,'Unknown SET statement - stop on first error');
    IBXScriptObj.StopOnFirstError := true;
    IBXScriptObj.ExecSQLScript(sTestSET);
    writeln(Outfile,'Unknown SET statement - continue after error');
    IBXScriptObj.StopOnFirstError := false;
    IBXScriptObj.ExecSQLScript(sTestSET);
  finally
    IBDatabase.Connected := false;
  end;
end;

initialization
  RegisterTest(TTest26);

end.

