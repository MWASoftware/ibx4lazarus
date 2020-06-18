unit Test05;

{$mode objfpc}{$H+}

{Test 5: Firebird 4 Data Types}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB, IBCustomDataSet,
  IBExtract;

const
  aTestID    = '5';
  aTestTitle = 'Test 5: Firebird 4 Data Types';

type

{ TTest05 }

  TTest05 = class(TIBXTestBase)
  private
    FDataSet: TIBDataSet;
    FExtract: TIBExtract;
    procedure HandleExtractLine(Sender: TObject; start, count: integer);
    procedure HandleAfterInsert(DataSet: TDataSet);
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    procedure InitialiseDatabase; override;
    function SkipTest: boolean; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses FmtBCD, IBUtils;

{ TTest05 }

procedure TTest05.HandleExtractLine(Sender: TObject; start, count: integer);
var i: integer;
begin
  for i := 0 to count - 1 do
    writeln(OutFile,FExtract.Items[start + i]);
end;

procedure TTest05.HandleAfterInsert(DataSet: TDataSet);
begin
  with DataSet do
  begin
    (FieldByName('F1') as TIBDateTimeField).SetAsDateTimeTZ(EncodeDate(1918,11,11) + FBEncodeTime(0,11,0,1111),'+05:00'); ;
    (FieldByName('f2') as TIBTimeField).SetAsDateTimeTZ(EncodeTime(22,02,10,5),'-08:00');
    FieldByName('F3').AsCurrency := 12345678912.12;
    FieldByName('f4').AsBCD := StrToBCD('64100000000.011');
    FieldByName('F5').AsBCD := StrToBCD('123456123456123456123456.123456');
  end;
end;

procedure TTest05.CreateObjects(Application: TCustomApplication);
begin
  inherited CreateObjects(Application);
  FDataSet := TIBDataSet.Create(Application);
  with FDataSet do
  begin
    Database := IBDatabase;
    SelectSQL.Text := 'Select * From IBXTEST A';
    InsertSQL.Text :=
      'Insert Into IBXTEST(TABLEKEY, F1, F2, F3, F4, F5) Values(:TABLEKEY, :F1, :F2, :F3, :F4, :F5)';
    RefreshSQL.Text :=
      'Select * From IBXTEST A '+
      'Where A.TABLEKEY = :TABLEKEY';
    ModifySQL.Text :=
        'Update IBXTEST A Set ' +
        '  A.F1 = :F1,' +
        '  A.F2 = :F2,' +
        '  A.F3 = :F3,' +
        '  A.F4 = :F4,' +
        '  A.F5 = :F5 ' +
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
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
  FExtract.OnExtractLines := @HandleExtractLine;
end;

function TTest05.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest05.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest05.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest05.InitialiseDatabase;
begin
  if IBDatabase.attachment.GetODSMajorVersion < 13 then
  begin
    IBDatabase.DropDatabase;
    raise ESkipException.Create('This test requires Firebird 4');
  end;
  inherited InitialiseDatabase;
end;

function TTest05.SkipTest: boolean;
begin
  Result := FirebirdAPI.GetClientMajor < 4;
  if Result then
    writeln(OutFile,'Skipping ',TestTitle);
end;

procedure TTest05.RunTest(CharSet: AnsiString; SQLDialect: integer);
var OldDefaultFormatSettings: TFormatSettings;
begin
  OldDefaultFormatSettings := DefaultFormatSettings;
  IBDatabase.Connected := true;
  try
    DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS.zzzz';
    IBTransaction.Active := true;
    FDataSet.Active := true;
    writeln(OutFile,'Extracting Database Schema');
    FExtract.ExtractObject(eoDatabase,'',[etGrantsToUser]);
    writeln(OutFile);
    writeln(OutFile,'Add a record');
    FDataSet.Append;
    FDataSet.Post;
    PrintDataSet(FDataSet);
    writeln(OutFile,'F1 in UTC Time = ', DateTimeToStr((FDataSet.FieldByName('F1') as TIBDateTimeField).GetAsUTCDateTime));
    writeln(OutFile,'F2 in UTC Time = ', FBFormatDateTime('HH:MM:SS.zzzz',(FDataSet.FieldByName('F2') as TIBDateTimeField).GetAsUTCDateTime));
  finally
    DefaultFormatSettings := OldDefaultFormatSettings;
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest05);

end.

