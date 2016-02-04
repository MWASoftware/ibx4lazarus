program fbsql;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,IBDatabase, ibxscript, IBExtract;

type

  { TFBSQL }

  TFBSQL = class(TCustomApplication)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBXScript: TIBXScript;
    FExtract: TIBExtract;
    FSQL: TStringStream;
    procedure LogHandler(Sender: TObject; Msg: string);
    procedure ErrorLogHandler(Sender: TObject; Msg: string);
  protected
    procedure DoRun; override;
    procedure ShowException(E: Exception); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFBSQL }

procedure TFBSQL.LogHandler(Sender: TObject; Msg: string);
begin
  writeln( Msg);
end;

procedure TFBSQL.ErrorLogHandler(Sender: TObject; Msg: string);
begin
  writeln(stderr, Msg);
end;

procedure TFBSQL.DoRun;
var
  ErrorMsg: String;
  SQLFileName: string;
  DoExtract: boolean;
  i: integer;
begin
  writeln(stderr,'fbsql: a non-interactive SQL interpreter for Firebird');
  writeln(stderr,'Copyright (c) MWA Software 2016');
  // quick check parameters
  ErrorMsg:=CheckOptions('ahbeufprs',['help','user','pass','role']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') or (ParamCount = 0) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  SQLFileName := '';
  DoExtract := false;

  { Create Components }
  FIBDatabase := TIBDatabase.Create(self);
  FIBTransaction := TIBTransaction.Create(self);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBXScript := TIBXScript.Create(self);
  FIBXScript.Database := FIBDatabase;
  FIBXScript.Transaction := FIBTransaction;
  FIBXScript.OnOutputLog := @LogHandler;
  FIBXScript.OnErrorLog := @ErrorLogHandler;
  FExtract := TIBExtract.Create(self);
  FExtract.Database := FIBDatabase;
  FExtract.Transaction := FIBTransaction;

  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');


  if HasOption('u','user') then
    FIBDatabase.Params.Add('user_name=' + GetOptionValue('u','user'))
  else
    FIBDatabase.Params.Add('user_name=SYSDBA');

  if HasOption('p','pass') then
    FIBDatabase.Params.Add('password=' + GetOptionValue('p','pass'));

  if HasOption('r','role') then
    FIBDatabase.Params.Add('sql_role_name=' + GetOptionValue('r','role'));

  FIBDatabase.Params.Add('lc_ctype=UTF8');

  if (ParamCount = 1) and (ParamStr(ParamCount)[1] <> '-') then
    FIBDatabase.DatabaseName := ParamStr(1)
  else
  if (ParamCount > 1) and (ParamStr(ParamCount)[1] <> '-')  and (ParamStr(ParamCount - 1)[1] <> '-') then
    FIBDatabase.DatabaseName := ParamStr(ParamCount)
  else
    raise Exception.Create('Database Name Missing');

  if not HasOption('b') then
    FIBXScript.StopOnFirstError := false;

  if not HasOption('e') then
    FIBXScript.Echo := false;

  if HasOption('a') then
    DoExtract := true;

  if HasOption('f') then
    SQLFileName := GetOptionValue('f');

  if HasOption('s') then
  begin
    FSQL.WriteString(GetOptionValue('s'));
    FSQL.Position := 0;
  end;

  if not DoExtract then
  begin
    if (SQLFileName = '') and (FSQL.DataString = '') then
      raise Exception.Create('An SQL File must be provided');

    if (FSQL.DataString <> '') and (SQLFileName <> '') then
       raise Exception.Create('An SQL Script File and text cannot be simulateously requested');

    if (FSQL.DataString = '') and not FileExists(SQLFileName) then
      raise Exception.CreateFmt('SQL File "%s" not found!',[SQLFileName]);

  end;

  if DoExtract and ((SQLFileName <> '') or (FSQL.DataString <> '')) then
    raise Exception.Create('Extract and script execution cannot be simulateously requested');

  FIBDatabase.Connected := true;
  try
    if DoExtract then
    begin
      FExtract.ExtractObject(eoDatabase);
      for i := 0 to FExtract.Items.Count - 1 do
        writeln(FExtract.Items[i]);
    end
    else
    if FSQL.DataString = '' then
      FIBXScript.PerformUpdate(SQLFileName,true)
    else
      FIBXScript.PerformUpdate(FSQL,true);
  finally
    FIBDatabase.Connected := false;
  end;


  // stop program loop
  Terminate;
end;

procedure TFBSQL.ShowException(E: Exception);
begin
  writeln(stderr,'Error: ' + E.Message);
end;

constructor TFBSQL.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FSQL := TStringStream.Create('');
end;

destructor TFBSQL.Destroy;
begin
  if assigned(FSQL) then FSQL.Free;
  inherited Destroy;
end;

procedure TFBSQL.WriteHelp;
begin
  { add your help code here }
  writeln(stderr,'Usage: ',ExtractFileName(ExeName),' <options> <database name>');
  writeln(stderr,'Options:');
  writeln(stderr,'-a            write database metadata to stdout');
  writeln(stderr,'-b            stop on first error');
  writeln(stderr,'-e            echo sql statements to stdout');
  writeln(stderr,'-f <filename> execute SQL script from file');
  writeln(stderr,'-h            show this information');
  writeln(stderr,'-p <password> provide password on command line (insecure)');
  writeln(stderr,'-r <rolename> open database with this rolename');
  writeln(stderr,'-s <sql>      Execute SQL text');
  writeln(stderr,'-u <username> open database with this username (defaults to SYSDBA)');
end;

var
  Application: TFBSQL;
begin
  Application:=TFBSQL.Create(nil);
  Application.Title:='fbsql';
  Application.Run;
  Application.Free;
end.

