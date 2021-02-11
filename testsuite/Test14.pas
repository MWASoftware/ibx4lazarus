unit Test14;

{$mode objfpc}{$H+}

{Test 14: Open and read from Employee Database with ISQLMonitor}

{  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

  ISQLMonitor is used to trace the activity.

  The Monitor is in a separate process

}

interface

uses
  Classes, SysUtils, TestApplication, IBXTestBase, IB, IBCustomDataSet, IBDatabase,
  IBQuery, IBInternals, IBSQLMonitor, Process {$IFDEF UNIX}, BaseUnix {$ENDIF};

const
  aTestID    = '14';
  aTestTitle = 'Open and read from Employee Database with ISQLMonitor and external monitor';

type

{ TTest14 }

  TTest14 = class(TIBXTestBase)
  private
    class var FTerminated: boolean;
  private
    FIBSQLMonitor: TIBSQLMonitor;
    FLog: TStringList;
    FProcess: TProcess;
    FIsChild: boolean;
    FLogFile: Text;
    {$IFDEF UNIX}
    Foa,Fna : PSigActionRec;
    procedure SetupSignalHandler;
    {$ENDIF}
    procedure HandleOnSQL(EventText: String; EventTime : TDateTime);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    procedure ProcessResults; override;
  public
    destructor Destroy; override;
    function ChildProcess: boolean; override;
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

const
  sqlExample =
'with recursive Depts As (   '+
'Select DEPT_NO, DEPARTMENT, HEAD_DEPT, cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH,'+
'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH '+
'From DEPARTMENT Where HEAD_DEPT is NULL '+
'UNION ALL '+
'Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || D.DEPARTMENT as DEPT_PATH,'+
'Depts.DEPT_KEY_PATH || '';'' || D.DEPT_NO as DEPT_KEY_PATH '+
'From DEPARTMENT D '+
'JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO '+
')'+

'Select First 2 A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE,'+
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

  LogFileName = 'Test_ISQLMonitor'  + '.log';

  { TTest14 }

procedure TTest14.HandleOnSQL(EventText: String; EventTime: TDateTime);
begin
  if FIsChild then
  begin
    writeln(FLogFile,'*Monitor* '+DateTimeToStr(EventTime)+' '+EventText);
    Flush(FLogFile);
  end;
end;

{$IFDEF UNIX}
procedure DoSigTerm(sig : cint);cdecl;
begin
  TTest14.FTerminated := true;
end;

procedure TTest14.SetupSignalHandler;
begin
  FTerminated := false;
  new(Fna);
  new(Foa);
  Fna^.sa_Handler:=SigActionHandler(@DoSigTerm);
  fillchar(Fna^.Sa_Mask,sizeof(Fna^.sa_mask),#0);
  Fna^.Sa_Flags:=0;
  {$ifdef Linux}               // Linux specific
    Fna^.Sa_Restorer:=Nil;
  {$endif}
  if fpSigAction(SigTerm,Fna,Foa)<>0 then
  begin
    writeln(OutFile,'Error setting signal handler: ',fpgeterrno,'.');
    halt(1);
  end;
end;
{$ENDIF}

procedure TTest14.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBSQLMonitor := TIBSQLMonitor.Create(Application);
  FIBSQLMonitor.TraceFlags := [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc];
  IBDatabase.TraceFlags := [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc];
  FIBSQLMonitor.OnSQL := @HandleOnSQL;
  FLog := TStringList.Create;
  FProcess := TProcess.Create(Application);
end;

function TTest14.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest14.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest14.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
  FIsChild := Owner.TestOption <> '';
  if FIsChild then
  begin
    {$IFDEF UNIX}
    SetupSignalHandler;
    {$ENDIF}
    FIBSQLMonitor.Enabled := true;
    assignFile(FLogFile,Owner.TestOption);
    Rewrite(FLogFile);
  end
  else
  begin
     FProcess.Executable := ParamStr(0);
     FProcess.Parameters.Clear;
     FProcess.Parameters.Add('-q');
     FProcess.Parameters.Add('-t');
     FProcess.Parameters.Add(GetTestID);
     FProcess.Parameters.Add('-O');
     FProcess.Parameters.Add(LogFileName);
     FProcess.Options := [];
  end;
end;

procedure TTest14.ProcessResults;
begin
  inherited ProcessResults;
  if assigned(FLog) then
    FreeAndNil(FLog);
  if assigned(FProcess) then
    FreeAndNil(FProcess);
end;

destructor TTest14.Destroy;
begin
  if assigned(FLog) then
  FreeAndNil(FLog);
  if assigned(FProcess) then
    FreeAndNil(FProcess);
  inherited Destroy;
end;

function TTest14.ChildProcess: boolean;
begin
  Result := Owner.TestOption <> '';
end;

procedure TTest14.RunTest(CharSet: AnsiString; SQLDialect: integer);
var stats: TPerfCounters;
    i: integer;
    aLogFile: Text;
    Line: string;
begin
  writeln(OutFile);
  EnableMonitoring;
  if not FIsChild then
    FProcess.Execute
  else
  begin
    while not FTerminated do
      CheckSynchronize(1);  //loop until terminated
    Close(FLogFile);
    Exit;
  end;

  CheckSynchronize(1);
  with IBQuery do
  begin
     AllowAutoActivateTransaction := true;
     Unidirectional := true;
     SQL.Text := sqlExample;
     EnableStatistics := true;
     Active := true;
     PrintDataSet(IBQuery);

     if GetPerfStatistics(stats) then
       WritePerfStats(stats);
     PrintAffectedRows(IBQuery);
     writeln(OutFile);
     writeln(OutFile,'Reconnect');
     IBDatabase.ReConnect;
     Unidirectional := false;
     Active := true;
     PrintDataSet(IBQuery);
  end;
  IBDatabase.Connected := false;
  CheckSynchronize(1);
  DisableMonitoring;
  FProcess.Terminate(0);
  Sleep(1000);
  assignFile(aLogFile,LogFileName);
  Reset(aLogFile);
  while not EOF(aLogFile) do
  begin
     readln(aLogFile,Line);
     writeln(OutFile,Line);
  end;
end;

initialization
  RegisterTest(TTest14);

end.

