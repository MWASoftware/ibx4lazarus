program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, IBDatabase, IBQuery
  { you can add units after this };

const
  sqlExample =
'with recursive Depts As (   '+
'Select DEPT_NO, DEPARTMENT, HEAD_DEPT, cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH,'+
'cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH '+
'From DEPARTMENT Where HEAD_DEPT is NULL '+
'UNION ALL '+
'Select DEPT_NO, DEPARTMENT, HEAD_DEPT, Depts.DEPT_PATH ||  '' / '' || DEPARTMENT as DEPT_PATH,'+
'Depts.DEPT_KEY_PATH || '';'' || DEPT_NO as DEPT_KEY_PATH '+
'From DEPARTMENT '+
'JOIN Depts On HEAD_DEPT = Depts.DEPT_NO '+
')'+

'Select A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE,'+
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    procedure DoQuery;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoQuery;
var i, rowno: integer;
begin
  with TIBQuery.Create(self) do
  try
     Database := FIBDatabase;
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

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { In console Mode the application must own the database
    - ensures correct exception handling }
  FIBDatabase := TIBDatabase.Create(self);
  FIBTransaction := TIBTransaction.Create(self);
  FIBDatabase.DatabaseName := 'localhost:employee';
  FIBDatabase.Params.Add('user_name=SYSDBA'); {You may have to modify this!}
  FIBDatabase.Params.Add('password=masterkey');  {You may have to modify this!}
  FIBDatabase.Params.Add('lc_ctype=UTF-8');
  FIBTransaction.DefaultDatabase := FIBDatabase;
  DoQuery;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='IBX In Console Mode';
  Application.Run;
  Application.Free;
end.

