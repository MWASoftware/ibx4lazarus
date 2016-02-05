program fbsql;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,IBDatabase, ibxscript, IBExtract, IBQuery, DB;

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
    FQuery: TIBQuery;
    FSQL: TStringStream;
    procedure LogHandler(Sender: TObject; Msg: string);
    procedure ErrorLogHandler(Sender: TObject; Msg: string);
    procedure HandleSelectSQL(Sender: TObject; SQLText: string);
    procedure WriteCSV;
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
begin
  FQuery.SQL.Text := SQLText;
  FQuery.Active := true;
  try
    WriteCSV;
  finally
    FQuery.Active := false;
  end;
end;

procedure TFBSQL.WriteCSV;

  procedure WriteQuotedText(Text: string);
  var Index: integer;
  begin
    Index := 1;
    while Index <= Length(Text) do
      if Text[Index] = '"' then
      begin
        Insert('"',Text,Index);
        Inc(Index,2)
      end
      else
        Inc(Index,1);
    write('"' + Text + '"')
  end;

  procedure WriteFieldList(Fields: TFields);
  var I: integer;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      if I > 0 then write(',');
      write(Fields[I].FieldName)
    end;
    writeln;
  end;

  procedure WriteRecord;
  var I: integer;
  begin
    with FQuery do
    begin
      for I := 0 to FieldCount - 1 do
      begin
        if I <> 0 then write(',');
        case Fields[I].DataType of
        ftUnknown:  raise Exception.Create(sUnknownField);
        ftString:   WriteQuotedText(Fields[I].AsString);
        ftSmallint,
        ftInteger,
        ftWord,
        ftLargeInt,
        ftBoolean:  write(Fields[I].DisplayText);
        ftFloat,
        ftCurrency,
        ftFmtBCD,
        ftBCD:      write(Fields[I].AsString);
        ftDate,
        ftTime:     write(DateTimeToStr(Fields[I].AsDateTime));
        ftDateTime: WriteQuotedText(Fields[I].AsString);
        ftBytes,
        ftVarBytes,
        ftBlob,
        ftAutoInc:  write(Fields[I].AsString);
        ftMemo:     WriteQuotedText(Fields[I].AsString);
        ftGraphic:  raise Exception.Create(sBadGraphic);
        ftFmtMemo:  WriteQuotedText(Fields[I].AsString);
        ftParadoxOle: raise Exception.Create(sBadParadox);
        ftDBaseOle:   raise Exception.Create(sBadDBase);
        ftTypedBinary:raise Exception.Create(sBadBinary);
        ftCursor:    raise Exception.Create(sBadCursor);
       end
      end;
      writeln;
    end;
  end;
begin
  with FQuery do
  begin
    WriteFieldList(Fields);
    First;
    while not EOF do
    begin
      WriteRecord;
      Next
    end;
  end
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

  if HasOption('f') then
    SQLFileName := GetOptionValue('f');

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
       raise Exception.Create('An SQL Script File and text cannot be simulateously requested');

    if (FSQL.DataString = '') and not FileExists(SQLFileName) then
      raise Exception.CreateFmt('SQL File "%s" not found!',[SQLFileName]);

  end;

  if DoExtract and ((SQLFileName <> '') or (FSQL.DataString <> '')) then
    raise Exception.Create('Extract and script execution cannot be simulateously requested');

  {This is where it all happens}

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
  FQuery := TIBQuery.Create(self);
  FQuery.Database := FIBDatabase;
  FQuery.Transaction := FIBTransaction;

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
  writeln(stderr,'-b            stop on first error');
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

