unit Test20;

{$mode objfpc}{$H+}

{Test 20: TIBUpdateSQL Tests}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB,
  IBCustomDataSet, IBUpdateSQL;

const
  aTestID    = '20';
  aTestTitle = 'Test 20: TIBUpdateSQL Tests';

type

{ TTest20 }

  TTest20 = class(TIBXTestBase)
  private
    FUpdateSQL: TIBUpdateSQL;
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
    end
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest20);

end.

