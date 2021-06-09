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
unit Test27;

{$mode objfpc}{$H+}

{Test 27: create, bring up-to-date and read from local database}

{
  Requires Firebird Embedded Server - with FB 2.5 and lower this implies FBEmbedded

  Creates a new local database from a backup archive.

  Updates the schema

  Prints out results of a query.
}

interface

uses
  Classes, SysUtils,  TestApplication, IBXTestBase, IB, IBCMLocalDBSupport, IBSQL,
  IBQuery, IBDatabase;

const
  aTestID    = '27';
  aTestTitle = 'create, bring up-to-date and read from local database';

type

{ TTest27 }

  TTest27 = class(TIBXTestBase)
  private
    FLocalDB: TIBCMLocalDBSupport;
    procedure HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
    procedure HandleLogMessage(Sender: TObject; Msg: string);
    procedure GetSharedDirectory(Sender: TObject; var SharedDataDir: string);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

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
'A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, A.PHOTO, D.DEPT_PATH, D.DEPT_KEY_PATH '+
'From EMPLOYEE A '+
'JOIN Depts D On D.DEPT_NO = A.DEPT_NO';

  { TTest27 }

procedure TTest27.HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
begin
  VersionNo := 0;
  IBTransaction.Active := true;
  try
    with TIBSQL.Create(Owner) do
    try
      Database := IBDatabase;
      Transaction := IBTransaction;
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

    with TIBSQL.Create(Owner)  do
    try
      Database := IBDatabase;
      Transaction := IBTransaction;
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
    IBTransaction.Commit;
  end;
end;

procedure TTest27.HandleLogMessage(Sender: TObject; Msg: string);
begin
  writeln(OutFile,Msg);
end;

procedure TTest27.GetSharedDirectory(Sender: TObject; var SharedDataDir: string);
begin
  SharedDataDir := 'resources/Test27';
end;

procedure TTest27.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FLocalDB := TIBCMLocalDBSupport.Create(Application);
  FLocalDB.Database := IBDatabase;
  FLocalDB.VendorName := 'MWA Software';
  FLocalDB.OnGetDBVersionNo := @HandleGetDBVersionNo;
  FLocalDB.OnLogMessage := @HandleLogMessage;
  FLocalDB.OnGetSharedDataDir := @GetSharedDirectory;
end;

function TTest27.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest27.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest27.InitTest;
begin
  IBDatabase.DatabaseName := 'nemo';
  FLocalDB.DatabaseName := ExtractDBName(Owner.GetNewDatabaseName);
  FLocalDB.EmptyDBArchive := 'employee.gbk';
  FLocalDB.RequiredVersionNo := 2;
  FLocalDB.UpgradeConfFile := 'upgrade.conf';
  ReadOnlyTransaction;
end;

procedure TTest27.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  try
    IBDatabase.Connected := true;
    with IBQuery do
    begin
      AllowAutoActivateTransaction := true;
      SQL.Text := sqlExample;
      Active := true;
      PrintDataSet(IBQuery);
    end;
 finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest27);

end.

