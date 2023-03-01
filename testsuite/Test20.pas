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
unit Test20;

{$mode objfpc}{$H+}

{Test 20: TIBUpdateSQL Tests}

{ Description
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB,
  IBCustomDataSet, IBUpdateSQL, IBQuery;

const
  aTestID    = '20';
  aTestTitle = 'TIBUpdateSQL Tests';

type

{ TTest20 }

  TTest20 = class(TIBXTestBase)
  private
    FUpdateSQL: TIBUpdateSQL;
    FUpdateSQL2: TIBUpdateSQL;
    FUpdateSQL3: TIBUpdateSQL;
    FIBQuery2: TIBQuery;
    FIBQuery3: TIBQuery;
    procedure HandleDeleteReturning(Sender: TObject; QryResults: IResults);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest20 }

type

  { TIBQuery2 }

  TIBQuery3 = class(TIBQuery) {used to set field Provider Flag}
  protected
    procedure DoAfterBindFields; override;
  end;

{ TIBQuery2 }

procedure TIBQuery3.DoAfterBindFields;
var field: TField;
begin
  field := FindField('ServerSideText');
  if field <> nil then
    field.ProviderFlags := field.ProviderFlags + [pfRefreshOnInsert,pfRefreshOnUpdate];
end;


procedure TTest20.HandleDeleteReturning(Sender: TObject; QryResults: IResults);
begin
  writeln(OutFile,'Delete Returning');
  ReportResult(QryResults);
  writeln(OutFile);
end;

procedure TTest20.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FUpdateSQL := TIBUpdateSQL.Create(Application);
  with IBQuery do
  begin
    UpdateObject := FUpdateSQL;
    SQL.Add('Select * From IBDataSetTest');
    GeneratorField.Field := 'KeyField';
    GeneratorField.Generator := 'AGENERATOR';
    GeneratorField.Increment := 1;
  end;
  with FUpdateSQL do
  begin
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (:KeyField,:PlainText)');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
  end;
  FIBQuery2 := TIBQuery.Create(Application);
  FUpdateSQL2 := TIBUpdateSQL.Create(Application);
  with FIBQuery2 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    UpdateObject := FUpdateSQL2;
    SQL.Add('Select * From IBDataSetTest');
    OnDeleteReturning := @HandleDeleteReturning;
  end;
  with FUpdateSQL2 do
  begin
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (Gen_ID(AGenerator,1),:PlainText) Returning KeyField, TextAndKey');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField Returning TextAndKey,ServerSideText');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField Returning KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
  end;
  FIBQuery3 := TIBQuery3.Create(Application);
  FUpdateSQL3 := TIBUpdateSQL.Create(Application);
  with FIBQuery3 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    UpdateObject := FUpdateSQL3;
    SQL.Add('Select * From IBDataSetTest');
    OnDeleteReturning := @HandleDeleteReturning;
  end;
  with FUpdateSQL3 do
  begin
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (Gen_ID(AGenerator,1),:PlainText) Returning KeyField, TextAndKey');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField Returning TextAndKey,ServerSideText');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField Returning KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
  end;
end;

function TTest20.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest20.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest20.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest20.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.CreateDatabase;
  try
    IBTransaction.Active := true;
    with IBQuery do
    begin
      Active := true;
      writeln(OutFile,'Simple Append');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(IBQuery);
      Refresh;
      writeln(OutFile,'After Refresh');
      PrintDataSetRow(IBQuery);
      writeln(OutFile,'Append and Update');
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSetRow(IBQuery);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(IBQuery);
      writeln(OutFile,'Now delete the first row');
      PrintDataSet(IBQuery);
      First;
      Delete;
      PrintDataSet(IBQuery);
      writeln(Outfile,'On Post KeyField Assignment');
      GeneratorField.ApplyOnEvent := gaeOnPostRecord;
      Append;
      FieldByName('PlainText').AsString := 'On Post KeyField test';
      PrintDataSetRow(IBQuery);
      Post;
      writeln(Outfile,'Row data after post');
      PrintDataSetRow(IBQuery);
      GeneratorField.ApplyOnEvent := gaeOnNewRecord; {restore}
    end;
    IBTransaction.Rollback;

    writeln(Outfile);
    writeln(Outfile,'Repeat with cached updates');
    IBTransaction.Active := true;
    with IBQuery do
    begin
      CachedUpdates := true;
      Active := true;
      writeln(OutFile,'Simple Append');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(IBQuery);
      Refresh;
      writeln(OutFile,'After Refresh');
      PrintDataSetRow(IBQuery);
      writeln(OutFile,'Append and Update');
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSetRow(IBQuery);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(IBQuery);
      writeln(OutFile,'Now delete the first row');
      PrintDataSet(IBQuery);
      First;
      Delete;
      PrintDataSet(IBQuery);
      ApplyUpdates;
      Active := false;
      Active := true;
      writeln(Outfile,'close and re-open and print again');
      PrintDataSet(IBQuery);
    end;
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    with FIBQuery2 do
    begin
      Active := true;
      writeln(OutFile,'FIBQuery2: Simple Append');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(FIBQuery2);
      Refresh;
      writeln(OutFile,'FIBQuery2 Refresh');
      PrintDataSetRow(FIBQuery2);
      writeln(OutFile,'Append and Update');
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSetRow(FIBQuery2);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(FIBQuery2);
      writeln(OutFile,'Now delete the first row');
      PrintDataSet(FIBQuery2);
      First;
      Delete;
      writeln(Outfile,'Dataset after delete');
      PrintDataSet(FIBQuery2);
      Active := false;
    end;
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    with FIBQuery3 do
    begin
      Active := true;
      writeln(OutFile,'FIBQuery3: Simple Append - test refresh on insert');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(FIBQuery3);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(FIBQuery3);
    end;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest20);

end.

