unit Test04;

{$mode objfpc}{$H+}

{Test 4: handling of data types up to Firebird 3}

interface

uses
  Classes, SysUtils, CustApp, TestApplication, IBXTestBase, DB, IB, IBCustomDataSet;

const
  aTestID    = '04';
  aTestTitle = 'handling of data types up to Firebird 3';

type

{ TTest04 }

  TTest04 = class(TIBXTestBase)
  private
    FDataSet: TIBDataSet;
    procedure HandleAfterInsert(DataSet: TDataSet);
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses DateUtils;

{ TTest04 }

procedure TTest04.HandleAfterInsert(DataSet: TDataSet);
var S, F: TStream;
begin
  with DataSet do
  begin
    FieldByName('F1').AsInteger := 2;
    FieldByName('f2').AsFloat := 0.314;
    FieldByName('f3').AsFloat := 0.31412345678;
    FieldByName('F4').AsFloat := 101.314;
    FieldByName('F5').AsCurrency := 101.99;
    FieldByName('F6').AsDateTime := EncodeDateTime(2007,12,25,12,30,15,0);
    FieldByName('F7').AsDateTime := EncodeDateTime(2007,12,25,12,30,29,130);
    FieldByName('F8').AsString := 'XX';
    FieldByName('F9').AsString := 'The Quick Brown Fox jumped over the lazy dog';
    FieldByName('F11').AsLargeInt := 9223372036854775807;
    FieldByName('F12').AsInteger := 65566;
    FieldByName('F13').AsDateTime := EncodeDateTime(2007,12,26,12,30,45,0);
    S := CreateBlobStream(FieldByName('F10'),bmWrite);
    F := TFileStream.Create('resources/Test04.jpg',fmOpenRead);
    try
      S.CopyFrom(F,0);
    finally
      S.Free;
      F.Free;
    end;
  end;
end;

procedure TTest04.CreateObjects(Application: TCustomApplication);
begin
  inherited CreateObjects(Application);
  FDataSet := TIBDataSet.Create(Application);
  with FDataSet do
  begin
    Database := IBDatabase;
    SelectSQL.Text := 'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,'+
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.'+
      'GRANTS, A."My Field" as MYFIELD1, A."MY Field" as MYFIELD2 From IBXTEST A';
    InsertSQL.Text :=
      'Insert Into IBXTEST(TABLEKEY, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, "f12", F13,'+
      ' GRANTS) Values(:TABLEKEY, :F1, :F2, :F3, :F4, :F5, :F6, :F7,'+
      ':F8, :F9, :F10, :F11, :F12, :F13, :GRANTS)';
    RefreshSQL.Text :=
      'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,' +
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.'+
      'GRANTS, A."My Field" as MYFIELD1, A."MY Field"  as MYFIELD2 From IBXTEST A '+
      'Where A.TABLEKEY = :TABLEKEY';
    ModifySQL.Text :=
        'Update IBXTEST A Set ' +
        '  A.F1 = :F1,' +
        '  A.F2 = :F2,' +
        '  A.F3 = :F3,' +
        '  A.F4 = :F4,' +
        '  A.F5 = :F5,' +
        '  A.F6 = :F6,' +
        '  A.F7 = :F7,' +
        '  A.F8 = :F8,' +
        '  A.F9 = :F9,' +
        '  A.F10 = :F10,' +
        '  A.F11 = :F11,' +
        '  A."f12" = :F12,' +
        '  A.F13 = :F13,' +
        '  A."My Field"  = :MYFIELD1,'+
        '  A."MY Field" = :MYFIELD2,'+
        '  A.GRANTS = :GRANTS '+
        'Where A.TABLEKEY = :OLD_TABLEKEY';
    DeleteSQL.Text :=
      'Delete From IBXTEST A '+
      'Where A.TABLEKEY = :OLD_TABLEKEY';
    DataSetCloseAction := dcSaveChanges;
    AutoCommit := acCommitRetaining;
    GeneratorField.Generator := 'IBXGEN';
    GeneratorField.Field := 'TABLEKEY';
    GeneratorField.ApplyOnEvent := gaeOnNewRecord;
    AfterInsert := @HandleAfterInsert;
  end;
end;

function TTest04.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest04.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest04.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest04.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    FDataSet.Active := true;
    writeln(OutFile,'Add a record');
    FDataSet.Append;
    FDataSet.Post;
    writeln(OutFile,'Add and edit a record');
    FDataSet.Append;
    FDataSet.Post;
    FDataSet.Edit;
    FDataSet.FieldByName('MYField1').AsString := 'My Field';
    FDataSet.FieldByName('MYFIELD2').AsString := 'MY Field';
    FDataSet.Post;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Delete a record');
    FDataSet.First;
    FDataSet.Delete;
    PrintDataSet(FDataSet);
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest04);

end.

