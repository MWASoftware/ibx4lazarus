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
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,IBDatabase, ibxscript, IBExtract, IBQuery, DB, IBVersion,
  IBDataOutput;

resourcestring

  sUnknownField = 'Unknown Field Type';
  sBadGraphic   = 'Unable to generate CSV data for a Graphic Field';
  sBadParadox   = 'Unable to generate CSV data for a Paradox OLE Field';
  sBadDBase     = 'Unable to generate CSV data  for a DBase OLE Field';
  sBadBinary    = 'Unable to generate CSV data  for a Binary Field';
  sBadCursor    = 'Unable to generate CSV data  for a Cursor Field';

type

  { TFBSQL }

  TFBSQL = class(TCustomApplication)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBXScript: TIBXScript;
    FExtract: TIBExtract;
    FSQL: TStringStream;
    FUseCSVFormat: boolean;
    procedure LogHandler(Sender: TObject; Msg: string);
    procedure ErrorLogHandler(Sender: TObject; Msg: string);
    procedure HandleSelectSQL(Sender: TObject; SQLText: string);
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

procedure TFBSQL.HandleSelectSQL(Sender: TObject; SQLText: string);
var DBOutput: TIBCustomDataOutput;
    Results: TStrings;
    i: integer;
begin
  Results := TStringList.Create;
  if FUseCSVFormat then
    DBOutput := TIBCSVDataOut.Create(self)
  else
    DBOutput := TIBBlockFormatOut.Create(self);
  try
    DBOutput.Database := FIBDatabase;
    DBOutput.DataOut(SQLText,Results);
    for i := 0 to Results.Count - 1 do
      writeln(Results[i]);
  finally
    DBOutput.Free;
    Results.Free;
  end;
end;

procedure TFBSQL.DoRun;
var
  ErrorMsg: String;
  SQLFileName: string;
  DoExtract: boolean;
  DoFullExtract: boolean;
  i: integer;
begin
  writeln(stderr,'fbsql: a non-interactive SQL interpreter for Firebird');
  writeln(stderr,'Built using IBX ' + IBX_VERSION);
  writeln(stderr,'Copyright (c) MWA Software 2016');
  // quick check parameters
  ErrorMsg:=CheckOptions('aAhbceufprs',['help','user','pass','role']);
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
  DoFullExtract := false;

  {Initialise user_name and password from environment if available}

  if GetEnvironmentVariable('ISC_USER') <> '' then
    FIBDatabase.Params.Add('user_name=' + GetEnvironmentVariable('ISC_USER'));

  if GetEnvironmentVariable('ISC_PASSWORD') <> '' then
    FIBDatabase.Params.Add('password=' + GetEnvironmentVariable('ISC_PASSWORD'));

  {Process Command line options}

  if HasOption('u','user') then
    FIBDatabase.Params.Add('user_name=' + GetOptionValue('u','user'));

  if HasOption('p','pass') then
    FIBDatabase.Params.Add('password=' + GetOptionValue('p','pass'));

  if HasOption('r','role') then
    FIBDatabase.Params.Add('sql_role_name=' + GetOptionValue('r','role'));

  if (ParamCount >= 1) and (ParamStr(ParamCount)[1] <> '-')  then
    FIBDatabase.DatabaseName := ParamStr(ParamCount)
  else
    raise Exception.Create('Database Name Missing');

  if not HasOption('b') then
    FIBXScript.StopOnFirstError := false;

  if not HasOption('e') then
    FIBXScript.Echo := false;

  if HasOption('a') then
    DoExtract := true;

  if HasOption('A') then
    DoFullExtract := true;

  if HasOption('c') then
    FUseCSVFormat := true;

  if HasOption('f') then
    SQLFileName := GetOptionValue('f');

  if HasOption('s') then
  begin
    FSQL.WriteString(GetOptionValue('s'));
    FSQL.Position := 0;
  end;

  {Validation}

  if not DoExtract and not DoFullExtract then
  begin
    if (SQLFileName = '') and (FSQL.DataString = '') then
      raise Exception.Create('An SQL File must be provided');

    if (FSQL.DataString <> '') and (SQLFileName <> '') then
       raise Exception.Create('An SQL Script File and text cannot be simultaneously requested');

    if (FSQL.DataString = '') and not FileExists(SQLFileName) then
      raise Exception.CreateFmt('SQL File "%s" not found!',[SQLFileName]);

  end;

  if (DoExtract or DoFullExtract) and ((SQLFileName <> '') or (FSQL.DataString <> '')) then
    raise Exception.Create('Extract and script execution cannot be simulateously requested');

  {This is where it all happens}

  FIBDatabase.Connected := true;
  try
    if DoFullExtract then
    begin
      FExtract.ExtractObject(eoDatabase,'',[etData]);
      for i := 0 to FExtract.Items.Count - 1 do
        writeln(FExtract.Items[i]);
    end
    else
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

  { Create Components }
  FIBDatabase := TIBDatabase.Create(self);
  FIBTransaction := TIBTransaction.Create(self);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBXScript := TIBXScript.Create(self);
  FIBXScript.Database := FIBDatabase;
  FIBXScript.Transaction := FIBTransaction;
  FIBXScript.OnOutputLog := @LogHandler;
  FIBXScript.OnErrorLog := @ErrorLogHandler;
  FIBXScript.OnSelectSQL := @HandleSelectSQL;
  FExtract := TIBExtract.Create(self);
  FExtract.Database := FIBDatabase;
  FExtract.Transaction := FIBTransaction;

  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');
  FIBDatabase.Params.Add('lc_ctype=UTF8');

end;

destructor TFBSQL.Destroy;
begin
  if assigned(FSQL) then FSQL.Free;
  inherited Destroy;
end;

procedure TFBSQL.WriteHelp;
begin
  writeln(stderr,'Usage: ',ExtractFileName(ExeName),' <options> <database name>');
  writeln(stderr,'Options:');
  writeln(stderr,'-a            write database metadata to stdout');
  writeln(stderr,'-A            write database metadata and table data to stdout');
  writeln(stderr,'-b            stop on first error');
  writeln(stderr,'-c            use csv format for select query results');
  writeln(stderr,'-e            echo sql statements to stdout');
  writeln(stderr,'-f <filename> execute SQL script from file');
  writeln(stderr,'-h            show this information');
  writeln(stderr,'-p <password> provide password on command line (insecure)');
  writeln(stderr,'-r <rolename> open database with this rolename');
  writeln(stderr,'-s <sql>      Execute SQL text');
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
  Application.Title:='fbsql';
  Application.Run;
  Application.Free;
end.

