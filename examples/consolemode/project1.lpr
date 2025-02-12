(*
 *  IBX For Lazarus (Firebird Express)
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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
program project1;

{$mode objfpc}{$H+}

{ CONSOLE MODE EXAMPLE

  This is a simple use of IBX to access the employee database in console mode.
  The program opens the database, runs a query and writes the result to stdout.

  Key points to note:

  1. In console mode, you have to create the Database and Transaction objects
     explicitly and link them to each other.

  2. The Database properties have to be set explicitly. This includes username
     and password. In the example, these are set from literals. You could update
     this to (e.g.) parse the command line arguments from "ParamStr".
     However, this is left as an exercise for the implementor.

  3. It's a good idea to have the application own the IBDatabase. This ensures that
     exceptions are routed through your application object's exception handler.
}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, IBDatabase, IBQuery, IB
  { you can add units after this };

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
    stats: TPerfCounters;
    SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
    LargeCompFormat: string;
    ThreeSigPlacesFormat: string;
begin
  LargeCompFormat := '#' + DefaultFormatSettings.ThousandSeparator + '##0';
  ThreeSigPlacesFormat := '#0' + DefaultFormatSettings.DecimalSeparator + '000';
  with TIBQuery.Create(self) do
  try
     AllowAutoActivateTransaction := true;
     Database := FIBDatabase;
     SQL.Text := sqlExample;
     EnableStatistics := true;
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
     if GetPerfStatistics(stats) then
     begin
       writeln('Current memory = ', FormatFloat(LargeCompFormat,stats[psCurrentMemory]));
       writeln('Delta memory = ', FormatFloat(LargeCompFormat,stats[psDeltaMemory]));
       writeln('Max memory = ', FormatFloat(LargeCompFormat,stats[psMaxMemory]));
       writeln('Elapsed time= ', FormatFloat(ThreeSigPlacesFormat,stats[psRealTime]/1000),' sec');
       writeln('Cpu = ', FormatFloat(ThreeSigPlacesFormat,stats[psUserTime]/1000),' sec');
       writeln('Buffers = ', FormatFloat('#0',stats[psBuffers]));
       writeln('Reads = ', FormatFloat('#0',stats[psReads]));
       writeln('Writes = ', FormatFloat('#0',stats[psWrites]));
       writeln('Fetches = ', FormatFloat('#0',stats[psFetches]));
     end;
     if StmtHandle.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount) then
     begin
       writeln('Selects = ',SelectCount);
       writeln('Inserts = ',InsertCount);
       writeln('Updates = ',UpdateCount);
       writeln('Deletes = ',DeleteCount);
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

  { In console Mode the application should own the database
    - ensures centralised exception handling }
  FIBDatabase := TIBDatabase.Create(self);
  FIBTransaction := TIBTransaction.Create(self);
  FIBDatabase.DatabaseName := 'employee';
  FIBDatabase.Params.Add('user_name=SYSDBA'); {You may have to modify this!}
  FIBDatabase.Params.Add('password=masterkey');  {You may have to modify this!}
  FIBDatabase.Params.Add('lc_ctype=UTF8');
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
  Application.Title := 'IBX In Console Mode';
  Application.Run;
  {$IFDEF WINDOWS}
  Readln; {Gives a chance to see the program output}
  {$ENDIF}
  Application.Free;
end.

