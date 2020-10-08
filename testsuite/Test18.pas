unit Test18;

{$mode objfpc}{$H+}

{Test 18: Cached Updates}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, Db, IB, IBCustomDataSet;

const
  aTestID    = '18';
  aTestTitle = 'Test 18: Cached Updates';

type

{ TTest18 }

  TTest18 = class(TIBXTestBase)
  private
    FIBDataset: TIBDataSet;
    procedure HandleUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
                                   var UpdateAction: TIBUpdateAction);
    procedure HandleUpdateError(DataSet: TDataSet; E: EDatabaseError;
                                 UpdateKind: TUpdateKind; var TheUpdateAction: TIBUpdateAction);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest18 }

procedure TTest18.HandleUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TIBUpdateAction);
begin
  writeln(Outfile,'Update Record Called for ',UpdateKind);
  PrintDatasetRow(DataSet);
  UpdateAction := uaApply;
end;

procedure TTest18.HandleUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var TheUpdateAction: TIBUpdateAction);
begin
  writeln(Outfile,'Update Error raised: ',E.Message);
  TheUpdateAction := uaFail;
end;

procedure TTest18.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBDataSet := TIBDataSet.Create(Application);
  with FIBDataSet do
  begin
    Name := 'IBTestData';
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
    CachedUpdates := true;
    OnUpdateRecord := @HandleUpdateRecord;
    OnUpdateError := @HandleUpdateError;
  end;
end;

function TTest18.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest18.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest18.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest18.RunTest(CharSet: AnsiString; SQLDialect: integer);
var lastkey: integer;
begin
  IBDatabase.CreateDatabase;
  try
    IBTransaction.Active := true;
    with FIBDataSet do
    begin
      Active := true;
      writeln(Outfile,'Bidirectional caching');
      writeln(OutFile,'Simple Append i.e. caching of inserted records and cancel');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Cancel Updates');
      CancelUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show empty');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(Outfile);
      writeln(OutFile,'Simple Append i.e. caching of inserted records and apply updates');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      Append;
      FieldByName('PlainText').AsString := 'This is another test';
      Post;
      Append;
      FieldByName('PlainText').AsString := 'And another';
      Post;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Apply Updates');
      ApplyUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show still there');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(OutFile);
      writeln(OutFile,'Update of First and Last records and cancel');
      First;
      Edit;
      FieldByName('PlainText').AsString := 'This is an updated test';
      Post;
      Last;
      Edit;
      FieldByName('PlainText').AsString := 'This is another updated test';
      Post;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Cancel Updates');
      CancelUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show no change');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(OutFile);
      writeln(OutFile,'Update of First and Last records and apply');
      First;
      Edit;
      FieldByName('PlainText').AsString := 'This is an updated test';
      Post;
      Last;
      Edit;
      FieldByName('PlainText').AsString := 'This is another updated test';
      Post;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Apply Updates');
      ApplyUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show still there');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(OutFile);
      writeln(OutFile,'Delete First and Last records and Cancel');
      First;
      Delete;
      Last;
      Delete;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Cancel Updates');
      CancelUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show no change');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(OutFile);
      writeln(OutFile,'Delete First and Last records and Apply');
      First;
      Delete;
      Last;
      Delete;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Apply Updates');
      ApplyUpdates;
      PrintDataSet(FIBDataSet);
      writeln(Outfile,'Now reopen and show no change');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet);

      writeln(OutFile);
      writeln(OutFile, 'Test Error Handling');
      First;
      lastkey := FieldByName('KEYFIELD').AsInteger;
      Append;
      FieldByName('KEYFIELD').AsInteger := lastkey;
      Post;
      PrintDataSet(FIBDataSet);
      try
        ApplyUpdates;
      except on E: Exception do
        writeln(OutFile,'Exception caught: ',E.Message);
      end;
    end;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest18);

end.

