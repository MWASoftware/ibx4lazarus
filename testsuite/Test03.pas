unit Test03;
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 3: Database Information}

{ Open the employee database and show database information
}

interface

uses
  Classes, SysUtils, TestManager, IBXTestManager, IB, IBDatabaseInfo, IBQuery, IBDatabase;

const
  aTestID    = '3';
  aTestTitle = 'Test 3: Database Information';

type

{ Test3 }

  Test3 = class(TIBXTestBase)
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBDatabaseInfo: TIBDatabaseInfo;
    FTableNameLookup: TIBQuery;
    procedure DoQuery;
    procedure AddPerfStats(Heading: string; stats: TStrings);
    procedure ShowBoolValue(aValue: integer; WhenTrue, WhenFalse: string);
    procedure ShowStrings(aCaption: string; List: TStrings);
    function HexString(s: AnsiString): string;
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ Test3 }

procedure Test3.DoQuery;
var S: TStrings;
    i: integer;
begin
  FIBTransaction.Active := true;
  FTableNameLookup.Active := true;
  writeln(OutFile,'Authentication Method = '+ FIBDatabase.AuthenticationMethod);
  writeln(OutFile,'Remote Protocol = ' + FIBDatabase.RemoteProtocol);
  writeln(OutFile,'Attachment SQLDialect = ' + IntToStr(FIBDatabase.DBSQLDialect));
  S := TStringList.Create;
  try
    FIBDatabase.Attachment.getFBVersion(S);
    for i := 0 to S.Count - 1 do
      writeln(OutFile,S[i]);
  finally
    S.Free;
  end;
  with FIBDatabaseInfo do
  begin
    writeln(OutFile,'Firebird Library Pathname = ' + FIBDatabase.FirebirdAPI.GetFBLibrary.GetLibraryFilePath);
    writeln(OutFile,'DB SQLDialect = ' + IntToStr(DBSQLDialect));
    writeln(OutFile,'Allocation = ' + IntToStr(Allocation));
    writeln(OutFile,'Base Level = ' + IntToStr(BaseLevel));
    writeln(OutFile,'DB File Name = ' + DBFileName);
    writeln(OutFile,'DB Site Name = ' + DBSiteName);
    writeln(OutFile,'DB Implementation No = ' + IntToStr(DBImplementationNo));
    writeln(OutFile,'Database Created: ' + DateTimeToStr(DateDBCreated));
    writeln(OutFile,'DB Implementation Class = ' + IntToStr(DBImplementationClass));
    ShowBoolValue(NoReserve, 'No Space Reserved','Space is Reserved');
    writeln(OutFile,'ODS Minor Version = ' + IntToStr(ODSMinorVersion));
    writeln(OutFile,'ODS Major Version = ' + IntToStr(ODSMajorVersion));
    writeln(OutFile,'Page Size = ' + IntToStr(PageSize));
    writeln(OutFile,'Version = ' + Version);
    writeln(OutFile,'Current Memory = ' + IntToStr(CurrentMemory));
    ShowBoolValue(ForcedWrites,'Forced Writes Enabled','Forced Writes Disabled');
    writeln(OutFile,'Max Memory = ' + IntToStr(MaxMemory));
    writeln(OutFile,'Number of Buffers = ' + IntToStr(NumBuffers));
    writeln(OutFile,'Sweep Interval = ' + IntToStr(SweepInterval));
    ShowStrings('User Names',UserNames);
    writeln(OutFile,'Fetches = ' + IntToStr(Fetches));
    writeln(OutFile,'Marks = ' + IntToStr(Marks));
    writeln(OutFile,'Reads = ' + IntToStr(Reads));
    writeln(OutFile,'Writes = ' + IntToStr(Writes));
    if ODSMajorVersion >= 12 then
    begin
      writeln(OutFile,'Pages Free = ' + IntToStr(PagesFree));
      writeln(OutFile,'Pages Used = ' + IntToStr(PagesUsed));
    end;
    writeln(OutFile,'Transaction Count = ' + IntToStr(TransactionCount));
    AddPerfStats('Backout Count',BackoutCount);
    AddPerfStats('Delete Count',DeleteCount);
    AddPerfStats('Expunge Count',ExpungeCount);
    AddPerfStats('Insert Count',InsertCount);
    AddPerfStats('Purge Count',PurgeCount);
    AddPerfStats('Read Idx Count',ReadIdxCount);
    AddPerfStats('Read Seq Count',ReadSeqCount);
    AddPerfStats('Update Count',UpdateCount);
    writeln(OutFile,'');
    ShowBoolValue(ReadOnly,'Database is Read Only','Database is Read/Write');
    writeln(OutFile,'Hex Dump of Database Page 100:');
    writeln(OutFile,HexString(GetDatabasePage(100)));
  end;
end;

procedure Test3.AddPerfStats(Heading: string; stats: TStrings);
var i: integer;
begin
  if stats.count = 0 then exit;
  writeln(OutFile,'');
  writeln(OutFile,Heading);
  for i := 0 to stats.Count - 1 do
  begin
    if FTableNameLookup.Locate('RDB$RELATION_ID',stats.Names[i],[]) then
      writeln(OutFile,'  ' + FTableNameLookup.FieldByName('RDB$RELATION_NAME').AsString + ' = ' + stats.ValueFromIndex[i]);
  end;
end;

procedure Test3.ShowBoolValue(aValue: integer; WhenTrue, WhenFalse: string);
begin
  if aValue <> 0 then
    writeln(OutFile,WhenTrue)
  else
    writeln(OutFile,WhenFalse);
end;

procedure Test3.ShowStrings(aCaption: string; List: TStrings);
var s: string;
    i: integer;
begin
  s := aCaption + ': ';
  for i := 0 to List.Count - 1 do
  begin
    if i > 0 then
      s := s + ', ';
    s := s + List[i];
  end;
 writeln(OutFile,s);
end;

function Test3.HexString(s: AnsiString): string;
var i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
    Result += Format('%x ',[byte(s[i])]);
end;

function Test3.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function Test3.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure Test3.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  FIBDatabase := TIBDatabase.Create(Owner.Application);
  FIBDatabase.FirebirdLibraryPathName := Owner.ClientLibraryPath;
  FIBDatabase.LoginPrompt := false;
  FIBTransaction := TIBTransaction.Create(Owner.Application);
  FIBDatabase.Params.Add('user_name=' + Owner.GetUserName);
  FIBDatabase.Params.Add('password=' + Owner.GetPassword);
  FIBDatabase.Params.Add('lc_ctype=UTF8');
  FIBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');
  FIBDatabaseInfo := TIBDatabaseInfo.Create(Owner.Application);
  FIBDatabaseInfo.Database := FIBDatabase;
  FTableNameLookup := TIBQuery.Create(Owner.Application);
  FTableNameLookup.Database := FIBDatabase;
  FTableNameLookup.Transaction := FIBTransaction;
  FTableNameLookup.SQL.Text := 'SELECT r.RDB$RELATION_ID, trim(r.RDB$RELATION_NAME) as RDB$RELATION_NAME FROM RDB$RELATIONS r';
  FIBDatabase.Connected := true;
  DoQuery;
end;

initialization
  RegisterTest(Test3);

end.
