unit Test26;

{$mode objfpc}{$H+}

{Test 26: IBXScript data out and exceptional conditions}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB,
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
    writeln(Outfile,'Dump employee table to CSV');
    IBXScriptObj.ExecSQLScript(sQuery);
    IBXScriptObj.DataOutputFormatter := FDataOutputSQLFormater;
    writeln(Outfile,'Dump employee table to Insert Statements');
    IBXScriptObj.ExecSQLScript(sQuery);
    IBXScriptObj.DataOutputFormatter := FDataOutputBlockFormater;
    writeln(Outfile,'Dump employee table to Data Block Format');
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

