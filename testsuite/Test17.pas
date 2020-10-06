unit Test17;

{$mode objfpc}{$H+}

{Test 17: TIBDataSet Tests}

{ Description

  TIBDataset Insert/Update/Delete tests.
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, IB, IBCustomDataSet;

const
  aTestID    = '17';
  aTestTitle = 'Test 17: TIBDataset Tests';

type

{ TTest1 }

  { TTest17 }

  TTest17 = class(TIBXTestBase)
  private
    FIBDataSet1: TIBDataSet;
    FIBDataSet2: TIBDataSet;
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

{ TTest17 }

procedure TTest17.HandleDeleteReturning(Sender: TObject; QryResults: IResults);
begin
  writeln(OutFile,'Delete Returning');
  ReportResult(QryResults);
  writeln(OutFile);
end;

procedure TTest17.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBDataSet1 := TIBDataSet.Create(Application);
  with FIBDataSet1 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    SelectSQL.Add('Select * From IBDataSetTest');
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (:KeyField,:PlainText)');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
    GeneratorField.Field := 'KeyField';
    GeneratorField.Generator := 'AGENERATOR';
    GeneratorField.Increment := 1;
  end;
  FIBDataSet2 := TIBDataSet.Create(Application);
  with FIBDataSet2 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    SelectSQL.Add('Select * From IBDataSetTest');
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (Gen_ID(AGenerator,1),:PlainText) Returning KeyField, TextAndKey');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField Returning TextAndKey,ServerSideText');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField Returning KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
    OnDeleteReturning := @HandleDeleteReturning;
  end;
end;

function TTest17.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest17.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest17.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest17.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.CreateDatabase;
  try
    IBTransaction.Active := true;
    with FIBDataSet1 do
    begin
      Active := true;
      writeln(OutFile,'FIBDataSet1: Simple Append');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(FIBDataSet1);
      Refresh;
      writeln(OutFile,'After Refresh');
      PrintDataSetRow(FIBDataSet1);
      writeln(OutFile,'Append and Update');
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSetRow(FIBDataSet1);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(FIBDataSet1);
      writeln(OutFile,'Now delete the first row');
      PrintDataSet(FIBDataSet1);
      First;
      Delete;
      PrintDataSet(FIBDataSet1);

    end;
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    with FIBDataSet2 do
    begin
      Active := true;
      writeln(OutFile,'FIBDataSet2: Simple Append');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(FIBDataSet2);
      Refresh;
      writeln(OutFile,'After Refresh');
      PrintDataSetRow(FIBDataSet2);
      writeln(OutFile,'Append and Update');
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSetRow(FIBDataSet2);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test';
      Post;
      PrintDataSetRow(FIBDataSet2);
      writeln(OutFile,'Now delete the first row');
      PrintDataSet(FIBDataSet2);
      First;
      Delete;
      PrintDataSet(FIBDataSet2);

    end;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest17);

end.

