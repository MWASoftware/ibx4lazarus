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
unit Test25;

{$mode objfpc}{$H+}

{Test 25: TIBTable Tests}

{ Append, Update, Delete tests on TIBTable
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBTable,
  IBCustomDataSet, IBExtract, IBXScript;

const
  aTestID    = '25';
  aTestTitle = 'TIBTable Tests';

type

{ TTest25 }

  TTest25 = class(TIBXTestBase)
  private
    FIBTable: TIBTable;
    FIBExtract: TIBExtract;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest25 }

procedure TTest25.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBTable := TIBTable.Create(Application);
  with FIBTable do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    TableName := 'TestTable';
    with FieldDefs do
    begin
      Add('MYKEY',ftInteger,4);
      Add('TEXTFIELD',ftString,32);
    end;
    IndexDefs.Add('PrimaryIndex','MYKEY',[ixPrimary]);
  end;
  FIBExtract := TIBExtract.Create(Application);
  FIBExtract.Database := IBDatabase;
  FIBExtract.Transaction := IBTransaction;
  FIBExtract.CaseSensitiveObjectNames := true;
end;

function TTest25.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest25.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest25.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest25.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  try
    FIBTable.CreateTable;
    IBTransaction.Commit;
    IBTransaction.Active := true;
    writeln(Outfile,'IBTable after create');
    FIBExtract.ExtractObject(eoTable,'TestTable');
    writeln(Outfile,FIBExtract.Items.Text);
    FIBTable.Active := true;
    PrintDataset(FIBTable);
    writeln(Outfile,'Add 2 rows');
    FIBTable.AppendRecord([1,'Test 1']);
    FIBTable.AppendRecord([2,'Test 2']);
    PrintDataset(FIBTable);
    writeln(Outfile,'Update first row');
    with FIBTable do
    begin
      if Locate('MYKEY',1,[]) then
      begin
        Edit;
        FieldByName('TextField').Asstring := 'Updated Test 1';
        Post;
      end;
    end;
    PrintDataset(FIBTable);
    writeln(Outfile,'Delete first row');
    with FIBTable do
      if Locate('MYKEY',1,[]) then
        Delete;
    PrintDataset(FIBTable);
    writeln(Outfile,'Empty the Table');
    FIBTable.EmptyTable;
    PrintDataset(FIBTable);
    writeln(Outfile,'Now drop the table');
    FIBTable.Active := false;
    FIBTable.DeleteTable;
    IBTransaction.Commit;
    IBTransaction.Active := true;
    writeln(Outfile,'Extract table after drop - should be empty');
    FIBExtract.ExtractObject(eoTable,'TestTable');
    writeln(Outfile,FIBExtract.Items.Text);
  finally
    IBXScriptObj.ExecSQLScript('Drop Database');
  end;
end;

initialization
  RegisterTest(TTest25);

end.

