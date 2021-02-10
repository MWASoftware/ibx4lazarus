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
    CheckSynchronize(5);
  finally
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
    CheckSynchronize(5);
    FEvents.UnRegisterEvents;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest11);

end.

