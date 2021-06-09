(*
 *  IBX Test suite. This program is used to test the IBX non-visual
 *  components and provides a semi-automated pass/fail check for each test.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)      
unit Test03;

{$mode objfpc}{$H+}

{Test 3: Database Information}

{ Open the employee database and show database information.

  Also tests wire compression and hence config overrides, along with a lookup table
  for a system table.
}

interface

uses
  Classes, SysUtils,  TestApplication, IBXTestBase, IB, IBDatabaseInfo, IBQuery, IBDatabase;

const
  aTestID    = '03';
  aTestTitle = 'Database Information';

type

{ Test3 }

  Test3 = class(TIBXTestBase)
  private
    FIBDatabaseInfo: TIBDatabaseInfo;
    FTableNameLookup: TIBQuery;
    procedure AddPerfStats(Heading: string; stats: TStrings);
    function HexString(s: AnsiString): string;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ Test3 }

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

function Test3.HexString(s: AnsiString): string;
var i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
    Result += Format('%x ',[byte(s[i])]);
end;

procedure Test3.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBDatabaseInfo := TIBDatabaseInfo.Create(Application);
  FIBDatabaseInfo.Database := IBDatabase;
  FTableNameLookup := TIBQuery.Create(Application);
  FTableNameLookup.Database := IBDatabase;
  FTableNameLookup.Transaction := IBTransaction;
  FTableNameLookup.SQL.Text := 'SELECT r.RDB$RELATION_ID, trim(r.RDB$RELATION_NAME) as RDB$RELATION_NAME FROM RDB$RELATIONS r';
end;

function Test3.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function Test3.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure Test3.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
end;

procedure Test3.RunTest(CharSet: AnsiString; SQLDialect: integer);
var S: TStrings;
    i: integer;
begin
  if FirebirdAPI.GetClientMajor >= 3 then
    IBDatabase.WireCompression := true;
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  FTableNameLookup.Active := true;
  writeln(OutFile,'Authentication Method = '+ IBDatabase.AuthenticationMethod);
  writeln(OutFile,'Remote Protocol = ' + IBDatabase.RemoteProtocol);
  writeln(OutFile,'Attachment SQLDialect = ' + IntToStr(IBDatabase.DBSQLDialect));
  S := TStringList.Create;
  try
    IBDatabase.Attachment.getFBVersion(S);
    for i := 0 to S.Count - 1 do
      writeln(OutFile,S[i]);
  finally
    S.Free;
  end;
  ShowFBVersion(IBDatabase.attachment);  {Wire Compression on if includes 'Z' flag}
  with FIBDatabaseInfo do
  begin
    writeln(OutFile,'Firebird Library Pathname = ' + IBDatabase.FirebirdAPI.GetFBLibrary.GetLibraryFilePath);
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

initialization
  RegisterTest(Test3);

end.

