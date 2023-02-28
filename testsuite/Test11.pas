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
unit Test11;

{$mode objfpc}{$H+}

{Test 11: Event Handling}

{ This tests calling an event handler in response to a database event.
  A simple database is used consisting of a stored procedure only.
  Two cases are tested: event registration before and after DB Open.
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, IB, IBEvents,
  IBStoredProc;

const
  aTestID    = '11';
  aTestTitle = 'Event Handling';

type

{ TTest11 }

  TTest11 = class(TIBXTestBase)
  private
    FEvents: TIBEvents;
    FExecProc: TIBStoredProc;
    procedure EventHandler( Sender: TObject; EventName: string; EventCount: longint;
                           var CancelAlerts: Boolean);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest11 }

procedure TTest11.EventHandler(Sender: TObject; EventName: string;
  EventCount: longint; var CancelAlerts: Boolean);
begin
  writeln(OutFile,'Event Handled: ',EventName, ', Count = ',EventCount);
end;

procedure TTest11.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FEvents := TIBEvents.Create(Application);
  FEvents.Database := IBDatabase;
  FEvents.OnEventAlert := @EventHandler;
  FEvents.Events.Add('EVENT1');
  FEvents.Events.Add('EVENT2');
  FExecProc := TIBStoredProc.Create(Application);
  FExecProc.Database := IBDatabase;
end;

function TTest11.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest11.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest11.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest11.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  writeln(OutFile,'Case #1: Deferred Event Registration');
  FEvents.DeferredRegister := true;
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    CheckSynchronize(1);
    FExecProc.StoredProcName := 'CALLEVENT';
    FExecProc.Prepare;
    FExecProc.Params[0].AsString := 'EVENT1';
    FExecProc.ExecProc;
    writeln(OutFile,'Event Called');
    IBTransaction.Commit;
    CheckSynchronize(100);
    FEvents.UnRegisterEvents;
  finally
    CheckSynchronize(100);
    IBDatabase.DropDatabase;
  end;
  writeln(OutFile,'Case #2: Event Registration after DB Open');
  IBDatabase.Connected := true;
  try
    FEvents.Registered := true;
    IBTransaction.Active := true;
    CheckSynchronize(1);
    FExecProc.StoredProcName := 'CALLEVENT';
    FExecProc.Prepare;
    FExecProc.Params[0].AsString := 'EVENT2';
    FExecProc.ExecProc;
    writeln(OutFile,'Event Called');
    IBTransaction.Commit;
    CheckSynchronize(100);
//    Sleep(1000);
    FEvents.UnRegisterEvents;
  finally
    CheckSynchronize(100);
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest11);

end.

