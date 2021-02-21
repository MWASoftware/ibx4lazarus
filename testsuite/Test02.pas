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
unit Test02;

{$mode objfpc}{$H+}

{Test 2: Database Event Handlers and Idle disconnect}

{ This test opens and closes a database in order to test the connect/disconnect
  handlers. The database is re-opened to test disconnect on idle.

  The transaction handlers are also tested, along with Transaction end on idle.

  SQL Dialect Downgrade warning tested.
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, IB;

const
  aTestID    = '02';
  aTestTitle = 'Database Event Handlers and Idle disconnect';

type

{ TTest02 }

  TTest02 = class(TIBXTestBase)
  private
    FConnectedAt: TDateTime;
    FStartedAt: TDateTime;
    procedure HandleBeforeConnect(Sender: TObject);
    procedure HandleAfterConnect(Sender: TObject);
    procedure HandleBeforeDisconnect(Sender: TObject);
    procedure HandleAfterDisconnect(Sender: TObject);
    procedure HandleIdleTime(Sender: TObject);
    procedure HandleDialectDowngradeWarning(Sender: TObject);
    procedure HandleTransactionStart(Sender: TObject);
    procedure HandleBeforeTransactionEnd(Sender: TObject);
    procedure HandleAfterTransactionEnd(Sender: TObject);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses DateUtils, IBDatabase;

{ TTest02 }

procedure TTest02.HandleBeforeConnect(Sender: TObject);
begin
  writeln(OutFile,'Before Connect');
end;

procedure TTest02.HandleAfterConnect(Sender: TObject);
begin
  FConnectedAt := Now;
  writeln(OutFile,'Connected to ' + (Sender as TIBDatabase).DatabaseName);
  PrintDPB((Sender as TIBDatabase).attachment.getDPB);
end;

procedure TTest02.HandleBeforeDisconnect(Sender: TObject);
begin
  writeln(OutFile,'Before Disconnect');
end;

procedure TTest02.HandleAfterDisconnect(Sender: TObject);
begin
  writeln(OutFile,(Sender as TIBDatabase).DatabaseName,' Disconnected after ',MilliSecondsBetween(FConnectedAt,Now),' ms');
end;

procedure TTest02.HandleIdleTime(Sender: TObject);
begin
  writeln(OutFile,'Idle Timer Expired for ',(Sender as TComponent).Name);
end;

procedure TTest02.HandleDialectDowngradeWarning(Sender: TObject);
begin
  writeln(OutFile,'Warning: SQL Dialect Downgrade of ',(Sender as TIBDatabase).DatabaseName);
end;

procedure TTest02.HandleTransactionStart(Sender: TObject);
begin
  write(OutFile,'Requested ');
  PrintTPB((Sender as TIBTransaction).TPB);
  writeln(OutFile,'Transaction Starts');
  PrintTPB((Sender as TIBTransaction).TransactionIntf.getTPB);
  FStartedAt := Now;
end;

procedure TTest02.HandleBeforeTransactionEnd(Sender: TObject);
begin
  writeln(OutFile,'Transaction Ending');
end;

procedure TTest02.HandleAfterTransactionEnd(Sender: TObject);
begin
  writeln(OutFile,'Transaction Ended after ',MilliSecondsBetween(FStartedAt,Now),' ms');
end;

procedure TTest02.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
end;

function TTest02.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest02.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest02.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  IBDatabase.BeforeConnect := @HandleBeforeConnect;
  IBDatabase.AfterConnect := @HandleAfterConnect;
  IBDatabase.BeforeDisconnect := @HandleBeforeDisconnect;
  IBDatabase.AfterDisconnect := @HandleAfterDisconnect;
  IBDatabase.OnIdleTimer := @HandleIdleTime;
  IBDatabase.OnDialectDowngradeWarning := @HandleDialectDowngradeWarning;
  IBTransaction.OnStartTransaction := @HandleTransactionStart;
  IBTransaction.BeforeTransactionEnd := @HandleBeforeTransactionEnd;
  IBTransaction.AfterTransactionEnd := @HandleAfterTransactionEnd;
  IBTransaction.OnIdleTimer := @HandleIdleTime;
  ReadOnlyTransaction;
end;

procedure TTest02.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  IBDatabase.Connected := false;
  IBDatabase.IdleTimer := 2000; {miliseconds}
  IBDatabase.Connected := true;
  while IBDatabase.Connected do
    CheckSynchronize(100);
  writeln(OutFile,'Database Closed');

  writeln(OutFile,'Transaction Events');
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  IBTransaction.Active := false;
  writeln(Outfile,'Transaction idle timer test');
  IBTransaction.IdleTimer := 1000; {millseconds}
  IBTransaction.Active := true;
  while IBTransaction.Active do
    CheckSynchronize(100);
  IBDatabase.Connected := false;

  writeln(OutFile,'SQL Dialect Downgrade test');
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  IBDatabase.SQLDialect := 1;
  ReadWriteTransaction;
  IBDatabase.Connected := true;
  if IBDatabase.Connected then
  begin
    writeln(OutFile,IBDatabase.DatabaseName,' created');
    IBDatabase.Connected := false;
    IBDatabase.SQLDialect := 3;
    IBDatabase.Connected := true;
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest02);

end.

