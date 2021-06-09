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
unit Test05;

{$mode objfpc}{$H+}

{Test 5: Firebird 4 Data Types}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBCustomDataSet,
  IBDatabase, IBExtract;

const
  aTestID    = '05';
  aTestTitle = 'Firebird 4 Data Types';

type

{ TTest05 }

  TTest05 = class(TIBXTestBase)
  private
    FDataSet: TIBDataSet;
    FExtract: TIBExtract;
    procedure HandleExtractLine(Sender: TObject; start, count: integer);
    procedure HandleAfterInsert(DataSet: TDataSet);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    procedure InitialiseDatabase(aDatabase: TIBDatabase) override;
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
    (FieldByName('F1') as TIBDateTimeField).SetAsDateTimeTZ(EncodeDate(1918,11,11) + FBEncodeTime(0,11,0,1111),'CET'); ;
    (FieldByName('f2') as TIBTimeField).SetAsDateTimeTZ(EncodeTime(22,02,10,5),'America/Los_Angeles');
    FieldByName('F3').AsCurrency := 12345678912.12;
    FieldByName('f4').AsBCD := StrToBCD('64100000000.011');
    FieldByName('F5').AsBCD := StrToBCD('123456123456123456123456.123456');
    FieldByName('F6').AsBCD := StrToBCD('123456789123456789');
  end;
end;

procedure TTest05.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FDataSet := TIBDataSet.Create(Application);
  with FDataSet do
  begin
    Database := IBDatabase;
    SelectSQL.Text := 'Select * From IBXTEST A';
    InsertSQL.Text :=
      'Insert Into IBXTEST(TABLEKEY, F1, F2, F3, F4, F5, F6) Values(:TABLEKEY, :F1, :F2, :F3, :F4, :F5, :F6)';
    RefreshSQL.Text :=
      'Select * From IBXTEST A '+
      'Where A.TABLEKEY = :TABLEKEY';
    ModifySQL.Text :=
        'Update IBXTEST A Set ' +
        '  A.F1 = :F1,' +
        '  A.F2 = :F2,' +
        '  A.F3 = :F3,' +
        '  A.F4 = :F4,' +
        '  A.F5 = :F5,' +
        '  A.F6 = :F6 ' +
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

procedure TTest05.InitialiseDatabase(aDatabase: TIBDatabase);
begin
  if aDatabase.attachment.GetODSMajorVersion < 13 then
  begin
    aDatabase.DropDatabase;
    raise ESkipException.Create('This test requires Firebird 4');
  end;
  inherited InitialiseDatabase(aDatabase);
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
    writeln(Outfile,'TZ Text Option = GMT');
    FDataset.TZTextOption := tzGMT;
    PrintDataSet(FDataSet);
    writeln(Outfile,'TZ Text Option = Original format');
    FDataset.TZTextOption := tzOriginalID;
    PrintDataSet(FDataSet);
    writeln(Outfile,'TZ Text Option = offset with Default time zone date of 2020/7/1');
    FDataset.Active := false;
    FDataset.TZTextOption := tzOffset;
    FDataset.DefaultTZDate := EncodeDate(2020,7,1);
    FDataset.Active := true;
    PrintDataSet(FDataSet);
    writeln(Outfile,'Update a record with a non default time zone date');
    FDataset.Edit;
    (FDataSet.FieldByName('F2') as TIBDateTimeField).SetAsDateTimeTZ(EncodeTime(11,02,10,15),'America/New_York');
    FDataSet.Post;
    PrintDataSet(FDataSet);
    writeln(Outfile,'Restore original default time zone date');
    FDataset.Active := false;
    FDataset.DefaultTZDate := EncodeDate(2020,1,1);
    FDataset.Active := true;
    PrintDataSet(FDataSet);
  finally
    DefaultFormatSettings := OldDefaultFormatSettings;
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest05);

end.

