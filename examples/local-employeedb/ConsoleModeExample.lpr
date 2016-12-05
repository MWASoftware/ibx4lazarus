program ConsoleModeExample;

{
  Example Program to demonstrate console mode IBLocal Support.

  Requires Firebird Embedded Server - see readme.txt

  Compile and run the program at the command line console. No
  command line parameters are necessary. Local Database is
  created in default location as "employee2.fdb".

}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,IBDatabase, IBQuery, IBCMLocalDBSupport, IBSQL;

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

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FLocalDB: TIBCMLocalDBSupport;
    procedure DoQuery;
    procedure HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
    procedure HandleLogMessage(Sender: TObject; Msg: string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoQuery;
var i, rowno: integer;
begin
  with TIBQuery.Create(self) do
  try
     Database := FIBDatabase;
     AllowAutoActivateTransaction := true;
     SQL.Text := sqlExample;
     Active := true;
     rowno := 1;
     while not EOF do
     begin
       writeln('Record No. ',rowno);
       Inc(rowno);
       writeln;
       for i := 0 to FieldCount - 1 do
       begin
         writeln(Fields[i].FieldName + ': ',Fields[i].AsString);
       end;
       writeln;
       next;
     end;
  finally
    Free;
  end;
end;

procedure TMyApplication.HandleGetDBVersionNo(Sender: TObject;
  var VersionNo: integer);
begin
  VersionNo := 0;
  FIBTransaction.Active := true;
  try
    with TIBSQL.Create(nil) do
    try
      Database := FIBDatabase;
      Transaction := FIBTransaction;
      SQL.Text := 'Select * From RDB$RELATIONS Where RDB$RELATION_NAME = ''DBVERSIONINFO''';
      ExecQuery;
      try
        if EOF then Exit;
      finally
        Close;
      end;
    finally
      Free
    end;

    with TIBSQL.Create(nil)  do
    try
      Database := FIBDatabase;
      Transaction := FIBTransaction;
      SQL.Text := 'Select VersionNo From DBVersionInfo';
      ExecQuery;
      try
        VersionNo := FieldByName('VersionNo').AsInteger;
      finally
        Close;
      end;
    finally
      Free;
    end;
  finally
    FIBTransaction.Commit;
  end;
end;

procedure TMyApplication.HandleLogMessage(Sender: TObject; Msg: string);
begin
  writeln(stderr,Msg);
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  writeln(stderr,Title);

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  DoQuery;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  { In console Mode the application should own the database
    - ensures centralised exception handling }
  FIBDatabase := TIBDatabase.Create(self);
  FIBDatabase.LoginPrompt := false;
  FIBTransaction := TIBTransaction.Create(self);
  FIBDatabase.DatabaseName := 'employee';
  FIBDatabase.Params.Add('lc_ctype=UTF8');
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');
  FLocalDB := TIBCMLocalDBSupport.Create(self);
  FLocalDB.Database := FIBDatabase;
  FLocalDB.DatabaseName := 'employee2.fdb';
  FLocalDB.EmptyDBArchive := 'employee.gbk';
  FLocalDB.VendorName := 'MWA Software';
  FLocalDB.OnGetDBVersionNo := @HandleGetDBVersionNo;
  FLocalDB.OnLogMessage := @HandleLogMessage;
  FLocalDB.RequiredVersionNo := 2;
end;

procedure TMyApplication.WriteHelp;
begin
  writeln(stderr,'Usage: ',ExtractFileName(ExeName),' <options>');
   writeln(stderr,'Options:');
   writeln(stderr,'-h            show this information');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='Console Mode and TIBLocalSupport';
  Application.Run;
  Application.Free;
end.

