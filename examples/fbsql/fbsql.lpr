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
    FOutputFile: TStream;
    FIncludeHeader: boolean;
    FRowCount: integer;
    FPlanOptions: TPlanOptions;
    procedure LogHandler(Sender: TObject; Msg: string);
    procedure ErrorLogHandler(Sender: TObject; Msg: string);
    procedure HandleSelectSQL(Sender: TObject; SQLText: string);
    procedure HandleSetStatement(Sender: TObject; command, aValue, stmt: string; var Done: boolean);
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
  if FOutputFile <> nil then
    FOutputFile.WriteAnsiString(Msg + LineEnding)
  else
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
    DBOutput.RowCount := FRowCount;
    DBOutput.ShowPerformanceStats := FIBXScript.ShowPerformanceStats;
    DBOutput.ShowAffectedRows := FIBXScript.ShowAffectedRows;
    if FUseCSVFormat then
      TIBCSVDataOut(DBOutput).IncludeHeader := FIncludeHeader
    else
      TIBBlockFormatOut(DBOutput).IncludeHeader := FIncludeHeader;
    DBOutput.DataOut(SQLText,FPlanOptions,Results);
    if FOutputFile <> nil then
      Results.SaveToStream(FOutputFile)
    else
    for i := 0 to Results.Count - 1 do
      writeln(Results[i]);
  finally
    DBOutput.Free;
    Results.Free;
  end;
end;

procedure TFBSQL.HandleSetStatement(Sender: TObject; command, aValue,
  stmt: string; var Done: boolean);

  function Toggle(aValue: string): boolean;
  begin
    aValue := AnsiUpperCase(aValue);
    if aValue = 'ON' then
      Result := true
    else
    if aValue = 'OFF' then
      Result := false
    else
      raise Exception.CreateFmt(sInvalidSetStatement, [command,stmt]);
  end;

begin
  done := true;
  if command = 'HEADING' then
    FIncludeHeader := ((aValue = '') and not FIncludeHeader) or
                      ((aValue <> '') and Toggle(aValue))
  else
  if command = 'ROWCOUNT' then
    FRowCount := StrToInt(aValue)
  else
  if command = 'PLAN' then
  begin
    if aValue = '' then
    begin
      if FPlanOptions <>  poIncludePlan then
        FPlanOptions := poIncludePlan
      else
        FPlanOptions := poNoPlan;
    end
    else
    if Toggle(aValue) then
      FPlanOptions := poIncludePlan
    else
      FPlanOptions := poNoPlan;
  end
  else
  if command = 'PLANONLY' then
  begin
    if aValue = '' then
    begin
      if FPlanOptions <>  poPlanOnly then
        FPlanOptions := poPlanOnly
      else
        FPlanOptions := poNoPlan;
    end
    else
    if Toggle(aValue) then
      FPlanOptions := poPlanOnly
    else
    if FPlanOptions <> poIncludePlan then
      FPlanOptions := poNoPlan;
  end
  else
    done := false;
end;

procedure TFBSQL.DoRun;
var
  ErrorMsg: String;
  SQLFileName: string;
  DoExtract: boolean;
  OutputFileName: string;
  i: integer;
  ExtractTypes: TExtractTypes;
begin
  writeln(stderr,'fbsql: a non-interactive SQL interpreter for Firebird');
  writeln(stderr,'Built using IBX ' + IBX_VERSION);
  writeln(stderr,'Copyright (c) MWA Software 2016');
  // quick check parameters
  ErrorMsg:=CheckOptions('aAhbceuioprs',['help','user','pass','role']);
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
  OutputFileName := '';
  DoExtract := false;
  ExtractTypes := [];

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
  begin
    DoExtract := true;
    ExtractTypes := [etData];
  end;

  if HasOption('c') then
    FUseCSVFormat := true;

  if HasOption('i') then
    SQLFileName := GetOptionValue('i');

  if HasOption('o') then
    OutputFileName := GetOptionValue('o');

  if HasOption('s') then
  begin
    FSQL.WriteString(GetOptionValue('s'));
    FSQL.Position := 0;
  end;

  {Validation}

  if not DoExtract then
  begin
    if (SQLFileName = '') and (FSQL.DataString = '') then
      raise Exception.Create('An SQL File must be provided');

    if (FSQL.DataString <> '') and (SQLFileName <> '') then
       raise Exception.Create('An SQL Script File and text cannot be simultaneously requested');

    if (FSQL.DataString = '') and not FileExists(SQLFileName) then
      raise Exception.CreateFmt('SQL File "%s" not found!',[SQLFileName]);

  end;

  if DoExtract and ((SQLFileName <> '') or (FSQL.DataString <> '')) then
    raise Exception.Create('Extract and script execution cannot be simulateously requested');

  {This is where it all happens}

  if OutputFileName <> '' then
    FOutputFile := TFileStream.Create(OutputFileName,fmCreate);

  FIBDatabase.Connected := true;
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
    if FSQL.DataString = '' then
      FIBXScript.RunScript(SQLFileName,true)
    else
      FIBXScript.RunScript(FSQL,true);
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
  writeln(stderr,'Error: ' + E.Message);
end;

constructor TFBSQL.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FIncludeHeader := true;
  FPlanOptions := poNoPlan;
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
  FIBXScript.OnSetStatement := @HandleSetStatement;
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
  writeln(stderr,'-i <filename> execute SQL script from file');
  writeln(stderr,'-h            show this information');
  writeln(stderr,'-o <filename> output to this file instead of stdout');
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

