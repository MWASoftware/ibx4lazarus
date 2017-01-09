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
 *  The Original Code is (C) 2014 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
program fbsql;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,IBDatabase, ibxscript, IBExtract, DB, IBVersion,
  IBDataOutput, RegExpr
  {$IFDEF UNIX} ,TermIO, IOStream {$ENDIF}

  ;

const
  FExceptionTrapped: boolean = false;

type
  TInteractiveSQLProcessor = class;

  { TFBSQL }

  TFBSQL = class(TCustomApplication)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBXScript: TIBXScript;
    FISQLProcessor: TInteractiveSQLProcessor;
    FExtract: TIBExtract;
    FOutputFile: TStream;
    FDataOutputFormatter: TDataOutputFormatter;
    procedure LogHandler(Sender: TObject; Msg: string);
    procedure ErrorLogHandler(Sender: TObject; Msg: string);
    procedure loginPrompt(Database: TIBDatabase; LoginParams: TStrings);
  protected
    procedure DoRun; override;
    procedure ShowException(E: Exception); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

  { TInteractiveSQLProcessor }

  {This is a TCustomIBXScript descendent that uses the console for input/output.
   It additionally suported QUIT/EXIT Commands. The log file can either be redirected
   to the console or sent to a separate file.}

  TInteractiveSQLProcessor = class(TCustomIBXScript)
  private
    FUseLogFile: boolean;
  protected
    procedure Add2Log(const Msg: string; IsError: boolean=true); override;
    function ProcessStatement(stmt: string): boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure Run;
    property UseLogFile: boolean read FUseLogFile write FUseLogFile;
  end;

{$IFDEF UNIX}
function getpassword: string;
var oldattr, newattr: termios;
    stdinStream: TIOStream;
    c: char;
begin
  Result := '';
  stdinStream := TIOStream.Create(iosInput);
  try
    TCGetAttr(stdinStream.Handle, oldattr);
    newattr := oldattr;
    newattr.c_lflag := newattr.c_lflag and not (ICANON or ECHO);
    TCSetAttr( stdinStream.Handle, TCSANOW, newattr );
    try
      repeat
        read(c);
        if c = #10 then break;
        write('*');
        Result += c;
      until false;
      writeln;
    finally
      TCSetAttr( stdinStream.Handle, TCSANOW, oldattr );
    end;
  finally
    stdinStream.Free;
  end;
end;
{$ENDIF}
{$IFDEF WINDOWS}
function getpassword: string;
var oldmode, newmode: DWORD;
    c: char;
begin
  Result := '';
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), oldmode);
  newmode := oldmode - ENABLE_ECHO_INPUT - ENABLE_LINE_INPUT;
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),newmode);
  try
    repeat
      read(c);
      if c = #13 then break;
      write('*');
      Result += c;
    until false;
    writeln;
  finally
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),oldmode);
  end
end;
{$ENDIF}

{ TInteractiveSQLProcessor }

procedure TInteractiveSQLProcessor.Add2Log(const Msg: string; IsError: boolean);
begin
  if UseLogFile then
    inherited Add2Log(Msg,IsError)
  else
  if IsError then
    writeln(stderr,msg)
  else
    writeln(msg);
end;

function TInteractiveSQLProcessor.ProcessStatement(stmt: string): boolean;
var  RegexObj: TRegExpr;
     Terminator: char;
     ucStmt: string;
begin
  Result := inherited ProcessStatement(stmt);
  if not Result then
  begin
    Terminator := FSymbolStream.Terminator;
    ucStmt := AnsiUpperCase(stmt);
    RegexObj := TRegExpr.Create;
    try
      RegexObj.Expression := '^ *(QUIT|EXIT) *(\' + Terminator + '|)';
      if RegexObj.Exec(ucStmt) then
      begin
         TInteractiveSymbolStream(FSymbolStream).Terminated := true;
         Result := true;
      end;
    finally
      RegexObj.Free;
    end;
  end;
end;

constructor TInteractiveSQLProcessor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSymbolStream := TInteractiveSymbolStream.Create;
end;

procedure TInteractiveSQLProcessor.Run;
begin
  ProcessStream;
end;

{ TFBSQL }

procedure TFBSQL.LogHandler(Sender: TObject; Msg: string);
begin
  if FOutputFile <> nil then
    FOutputFile.WriteAnsiString(Msg + LineEnding)
  else
    writeln( Msg);
end;

procedure TFBSQL.ErrorLogHandler(Sender: TObject; Msg: string);
begin
  writeln(stderr, Msg);
end;

procedure TFBSQL.loginPrompt(Database: TIBDatabase; LoginParams: TStrings);
var password: string;
begin
  write(LoginParams.Values['user_name'] + '''s Password:');
  password := getpassword;
  if password <> '' then
    LoginParams.Values['password'] := password;
end;

procedure TFBSQL.DoRun;
var
  ErrorMsg: String;
  SQLFileName: string;
  DoExtract: boolean;
  OutputFileName: string;
  i: integer;
  ExtractTypes: TExtractTypes;
  Opts,NonOpts: TStrings;
  OutputFormat: string;
  SQLStatement: string;
begin
  writeln(stderr,'fbsql: an SQL interpreter for Firebird');
  writeln(stderr,'Built using IBX ' + IBX_VERSION);
  writeln(stderr,'Copyright (c) MWA Software 2017');

  // quick check parameters
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  try
    ErrorMsg := CheckOptions('aAhbeu:i:o:p:r:s:t:',['help','user','pass','role'],Opts,NonOpts);
    {Database name is last parameter if given and not an option}
    if (NonOpts.Count > 0) and ((Opts.Count = 0) or
             ((Opts.ValueFromIndex[Opts.Count-1] <> NonOpts[NonOpts.Count-1])) or
             (ParamCount = 1) or (ParamStr(ParamCount-1)[2] in ['!','A','h','b','e']))then
      FIBDatabase.DatabaseName := ParamStr(ParamCount);
  finally
    Opts.Free;
    NonOpts.Free;
  end;
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  SQLFileName := '';
  OutputFileName := '';
  DoExtract := false;
  ExtractTypes := [];
  FDataOutputFormatter := TIBBlockFormatOut;
  SQLStatement := '';

  {Initialise user_name and password from environment if available}

  if GetEnvironmentVariable('ISC_USER') <> '' then
    FIBDatabase.Params.Add('user_name=' + GetEnvironmentVariable('ISC_USER'));

  if GetEnvironmentVariable('ISC_PASSWORD') <> '' then
    FIBDatabase.Params.Add('password=' + GetEnvironmentVariable('ISC_PASSWORD'));

  {Process Command line options}

  if HasOption('a') then
    DoExtract := true;

  if HasOption('A') then
  begin
    DoExtract := true;
    ExtractTypes := [etData];
  end;

  if not HasOption('b') then
  begin
    FIBXScript.StopOnFirstError := false;
    FISQLProcessor.StopOnFirstError := false;
  end;

  if not HasOption('e') then
    FIBXScript.Echo := false;

  if HasOption('i') then
    SQLFileName := GetOptionValue('i');

  if HasOption('o') then
  begin
    OutputFileName := GetOptionValue('o');
    FISQLProcessor.UseLogFile := true;
  end;

  if HasOption('p','pass') then
    FIBDatabase.Params.Values['password'] := GetOptionValue('p','pass');

  if HasOption('r','role') then
    FIBDatabase.Params.Values['sql_role_name'] := GetOptionValue('r','role');

  if HasOption('s') then
    SQLStatement := GetOptionValue('s');

  if HasOption('t') then
  begin
    OutputFormat := GetOptionValue('t');
    if OutputFormat = 'CSV' then
      FDataOutputFormatter := TIBCSVDataOut
    else
    if OutputFormat = 'INS' then
      FDataOutputFormatter := TIBInsertStmtsOut
    else
    if OutputFormat = 'BLK' then
      FDataOutputFormatter := TIBBlockFormatOut
    else
      raise Exception.CreateFmt('Unrecognised data output format "%s"',[OutputFormat]);
  end;

  if HasOption('u','user') then
    FIBDatabase.Params.Values['user_name'] := GetOptionValue('u','user');

  {Validation}

  FIBDatabase.LoginPrompt := (FIBDatabase.Params.IndexOfName('user_name') <> -1) and
                              (FIBDatabase.Params.Values['password'] = '');

  if not DoExtract then
  begin
    if (SQLStatement <> '') and (SQLFileName <> '') then
       raise Exception.Create('An SQL Script File and text cannot be simultaneously requested');

    if (SQLStatement = '') and (SQLFileName <> '')  and not FileExists(SQLFileName) then
      raise Exception.CreateFmt('SQL File "%s" not found!',[SQLFileName]);

  end;

  if DoExtract and ((SQLFileName <> '') or (SQLStatement <> '')) then
    raise Exception.Create('Extract and script execution cannot be simulateously requested');

  {This is where it all happens}

  FIBXScript.DataOutputFormatter := FDataOutputFormatter.Create(self);
  FISQLProcessor.DataOutputFormatter := FDataOutputFormatter.Create(self);

  if OutputFileName <> '' then
    FOutputFile := TFileStream.Create(OutputFileName,fmCreate);

  FIBDatabase.Connected := FIBDatabase.DatabaseName <> '';
  try
    if DoExtract then
    begin
      FExtract.ExtractObject(eoDatabase,'',ExtractTypes);
      if FOutputFile <> nil then
        FExtract.Items.SaveToStream(FOutputFile)
      else
      for i := 0 to FExtract.Items.Count - 1 do
        writeln(FExtract.Items[i]);
    end
    else
    if SQLFileName <> '' then
      FIBXScript.RunScript(SQLFileName)
    else
    if SQLStatement <> '' then
      FIBXScript.ExecSQLScript(SQLStatement)
    else
      FISQLProcessor.Run;
  finally
    FIBDatabase.Connected := false;
    if FOutputFile <> nil then
      FOutputFile.Free;
  end;


  // stop program loop
  Terminate;
end;

procedure TFBSQL.ShowException(E: Exception);
begin
  FExceptionTrapped := true;
  writeln(stderr,'Error: ' + E.Message);
end;

constructor TFBSQL.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  { Create Components }
  FIBDatabase := TIBDatabase.Create(self);
  FIBDatabase.OnLogin := @loginPrompt;
  FIBDatabase.Params.Clear;
  FIBDatabase.Params.Values['lc_ctype'] := 'UTF8';
  FIBTransaction := TIBTransaction.Create(self);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBXScript := TIBXScript.Create(self);
  FIBXScript.Database := FIBDatabase;
  FIBXScript.Transaction := FIBTransaction;
  FIBXScript.OnOutputLog := @LogHandler;
  FIBXScript.OnErrorLog := @ErrorLogHandler;
  FISQLProcessor := TInteractiveSQLProcessor.Create(self);
  FISQLProcessor.Database := FIBDatabase;
  FISQLProcessor.Transaction := FIBTransaction;
  FISQLProcessor.OnOutputLog := @LogHandler;
  FISQLProcessor.OnErrorLog := @ErrorLogHandler;
  FExtract := TIBExtract.Create(self);
  FExtract.Database := FIBDatabase;
  FExtract.Transaction := FIBTransaction;

  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');

end;

procedure TFBSQL.WriteHelp;
begin
  writeln(stderr,'Usage: ',ExtractFileName(ExeName),' <options> <database name>');
  writeln(stderr,'Options:');
  writeln(stderr,'-a            write database metadata to stdout');
  writeln(stderr,'-A            write database metadata and table data to stdout');
  writeln(stderr,'-b            stop on first error');
  writeln(stderr,'-e            echo sql statements to stdout');
  writeln(stderr,'-i <filename> execute SQL script from file');
  writeln(stderr,'-h            show this information');
  writeln(stderr,'-o <filename> output to this file instead of stdout');
  writeln(stderr,'-p <password> provide password on command line (insecure)');
  writeln(stderr,'-r <rolename> open database with this rolename');
  writeln(stderr,'-s <sql>      Execute SQL text');
  writeln(stderr,'-t            specify output format for SQL Statements');
  writeln(stderr,'              BLK (default) for block format');
  writeln(stderr,'              CSV (default) for CSV format');
  writeln(stderr,'              INS (default) for Insert Statement format');
  writeln(stderr,'-u <username> open database with this username (defaults to SYSDBA)');
  writeln;
  writeln(stderr,'Environment Variables:');
  writeln(stderr,'ISC_USER      Login user Name');
  writeln(stderr,'ISC_PASSWORD  Login password');
end;

var
  Application: TFBSQL;
begin
  Application:=TFBSQL.Create(nil);
  Application.Run;
  Application.Free;
  if FExceptionTrapped then
    Halt(1);
end.

