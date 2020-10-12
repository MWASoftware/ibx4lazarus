unit Test20;

{$mode objfpc}{$H+}

{Test 20: TIBUpdateSQL Tests}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB,
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
    FIBQuery2: TIBQuery;
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

    end;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest20);

end.

