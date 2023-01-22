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
unit Test18;

{$mode objfpc}{$H+}

{Test 18: Cached Updates}

{ Description
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, Db, IB, IBCustomDataSet;

const
  aTestID    = '18';
  aTestTitle = 'Cached Updates';

type

{ TTest18 }

  TTest18 = class(TIBXTestBase)
  private
    FIBDataset: TIBDataSet;
    procedure HandleUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
                                   var UpdateAction: TIBUpdateAction);
    procedure HandleUpdateError(DataSet: TDataSet; E: EDatabaseError;
                                 UpdateKind: TUpdateKind; var TheUpdateAction: TIBUpdateAction);
    procedure DoTest(aUnidirectional: boolean);
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

procedure TTest18.DoTest(aUnidirectional: boolean);
var lastkey: integer;
begin
  with FIBDataSet do
  begin
    Unidirectional := aUnidirectional;
    Active := true;
    PrintDataSet(FIBDataSet);
    writeln(Outfile,'Unidirectional caching = ',aUnidirectional);
    writeln(OutFile,'Simple Append i.e. caching of inserted records and cancel');
    if aUnidirectional then Insert else Append;
    FieldByName('PlainText').AsString := 'This is a test';
    Post;
    if aUnidirectional then Insert else Append;
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
    if aUnidirectional then Insert else Append;
    FieldByName('PlainText').AsString := 'This is a test';
    Post;
    if aUnidirectional then Insert else Append;
    FieldByName('PlainText').AsString := 'This is another test';
    Post;
    if aUnidirectional then Insert else Append;
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
    writeln(OutFile,'Update of First and Last records and implicitly apply');
    DataSetCloseAction := dcSaveChanges;
    First;
    Edit;
    FieldByName('PlainText').AsString := 'This is an updated test (implicit apply updates)';
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
      begin
        writeln(OutFile,'Exception caught: ',E.Message);
        CancelUpdates;
      end;
    end;
  end;
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
begin
  IBDatabase.CreateDatabase;
  try
    IBTransaction.Active := true;
    DoTest(false);
//    DoTest(true); {See https://bugs.freepascal.org/view.php?id=37900}
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest18);

end.

