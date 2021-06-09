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
unit Test04;

{$mode objfpc}{$H+}

{Test 4: handling of data types up to Firebird 3.

         Tests, Append, Edit and Delete operations

         Test both default createdatabase and createdatabasefromSQL
}

interface

uses
  Classes, SysUtils,  TestApplication, IBXTestBase, DB, IB, IBCustomDataSet;

const
  aTestID    = '04';
  aTestTitle = 'Handling of data types up to Firebird 3';

type

{ TTest04 }

  TTest04 = class(TIBXTestBase)
  private
    FDataSet: TIBDataSet;
    FCreateArrayOnInsert: boolean;
    procedure HandleAfterInsert(DataSet: TDataSet);
    procedure HandleTransactionEdit(Sender: TObject);
    procedure HandleTransactionDelete(Sender: TObject);
    procedure HandleTransactionInsert(Sender: TObject);
    procedure HandleTransactionPost(Sender: TObject);
    procedure HandleTransactionExecQuery(Sender: TObject);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses DateUtils, IBSQL;

{ TTest04 }

procedure TTest04.HandleAfterInsert(DataSet: TDataSet);
var S, F: TStream;
    Str: TStringList;
    i,j: integer;
    ar: IArray;
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
    FieldByName('F9').AsString := 'The Quick Brown Fox jumps over the lazy dog';
    S := CreateBlobStream(FieldByName('F10'),bmWrite);
    F := TFileStream.Create('resources/Test04.jpg',fmOpenRead);
    try
      S.CopyFrom(F,0);
    finally
      S.Free;
      F.Free;
    end;
    FieldByName('F11').AsLargeInt := 9223372036854775807;
    FieldByName('F12').AsInteger := 65566;
    FieldByName('F13').AsDateTime := EncodeDateTime(2007,12,26,12,30,45,0);
    Str := TStringList.Create;
    try
      Str.LoadFromFile('resources/Test04.txt');
      FieldByName('F14').AsString := Str.Text;
    finally
      Str.Free;
    end;
    if FCreateArrayOnInsert then
      ar := TIBArrayField(FieldByName('MyArray')).CreateArray
    else
      ar := (DataSet as TIBCustomDataset).GetArray(TIBArrayField(FieldByName('MyArray')));
    j := 100;
    for i := 0 to 16 do
    begin
      ar.SetAsInteger([i],j);
      dec(j);
    end;
    TIBArrayField(FieldByName('MyArray')).ArrayIntf := ar;
  end;
end;

procedure TTest04.HandleTransactionEdit(Sender: TObject);
begin
  writeln(OutFile,'Transaction Edit');
end;

procedure TTest04.HandleTransactionDelete(Sender: TObject);
begin
  writeln(OutFile,'Transaction Delete');
end;

procedure TTest04.HandleTransactionInsert(Sender: TObject);
begin
  writeln(OutFile,'Transaction Insert');
end;

procedure TTest04.HandleTransactionPost(Sender: TObject);
begin
  writeln(OutFile,'Transaction Post');
end;

procedure TTest04.HandleTransactionExecQuery(Sender: TObject);
begin
  write(OutFile,'Transaction Exec Query ');
  if Sender is TIBSQL then
    writeln(OutFile,'"',(Sender as TIBSQL).SQL.Text,'"')
  else
    writeln(OutFile);
end;

procedure TTest04.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FDataSet := TIBDataSet.Create(Application);
  with FDataSet do
  begin
    Database := IBDatabase;
    with IBTransaction do
    begin
      AfterEdit := @HandleTransactionEdit;
      AfterDelete := @HandleTransactionDelete;
      AfterInsert := @HandleTransactionInsert;
      AfterPost := @HandleTransactionPost;
      AfterExecQuery := @HandleTransactionExecQuery;
    end;
    SelectSQL.Text := 'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,'+
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.F14, A.MyArray, A.'+
      'GRANTS, A."My Field" as MYFIELD1, A."MY Field" as MYFIELD2 From IBXTEST A';
    InsertSQL.Text :=
      'Insert Into IBXTEST(TABLEKEY, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, "f12", F13, F14, MyArray, '+
      ' GRANTS) Values(:TABLEKEY, :F1, :F2, :F3, :F4, :F5, :F6, :F7,'+
      ':F8, :F9, :F10, :F11, :F12, :F13, :F14, :MyArray, :GRANTS)';
    RefreshSQL.Text :=
      'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,' +
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.F14, A.MyArray, A.'+
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
        '  A.F14 = :F14,' +
        '  A.MyArray = :MyArray, ' +
        '  A."My Field"  = :MYFIELD1,'+
        '  A."MY Field" = :MYFIELD2,'+
        '  A.GRANTS = :GRANTS '+
        'Where A.TABLEKEY = :OLD_TABLEKEY';
    DeleteSQL.Text :=
      'Delete From IBXTEST A '+
      'Where A.TABLEKEY = :OLD_TABLEKEY';
    DataSetCloseAction := dcSaveChanges;
    AutoCommit := acDisabled;
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
var OldDefaultFormatSettings: TFormatSettings;
begin
  OldDefaultFormatSettings := DefaultFormatSettings;
  IBDatabase.CreateDatabase;
  try
    DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS.zzzz';
    IBTransaction.Active := true;
    FDataSet.Active := true;
    writeln(OutFile,'Add a record');
    FDataSet.Append;
    FDataSet.Post;
    writeln(OutFile,'Add and edit a record');
    FCreateArrayOnInsert := true;
    FDataSet.Append;
    FDataSet.Post;
    FDataSet.Edit;
    FDataSet.FieldByName('MYField1').AsString := 'My Field';
    FDataSet.FieldByName('MYFIELD2').AsString := 'MY Field';
    FDataSet.Post;
    IBTransaction.Commit;

    IBTransaction.Active := true;
    FDataSet.Active := true;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Delete a record');
    FDataSet.First;
    FDataSet.Delete;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Rollback Retaining');
    IBTransaction.RollbackRetaining;
    FDataSet.Active := false;
    FDataSet.Active := true;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Delete a record');
    FDataSet.First;
    FDataSet.Delete;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Rollback');
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    FDataSet.Active := true;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Commit Retaining');
    FDataSet.Append;
    FDataSet.Post;
    IBTransaction.CommitRetaining;
    FDataSet.Active := false;
    FDataSet.Active := true;
    PrintDataSet(FDataSet);
    writeln(OutFile,'Commit');
    IBTransaction.Commit;
    IBTransaction.Active := true;
    FDataSet.Active := true;
    PrintDataSet(FDataSet);
  finally
    IBDatabase.DropDatabase;
  end;

  writeln(Outfile,'Creating Database from SQL');
  IBDatabase.CreateDatabase('CREATE DATABASE ''' + Owner.GetNewDatabaseName +
                            ''' USER ''' + Owner.GetUserName +
                            ''' PASSWORD ''' + Owner.GetPassword + '''' +
                            ' DEFAULT CHARACTER SET UTF8');
  if IBDatabase.Connected then
  begin
    writeln(Outfile,'Database Name = ',IBDatabase.DatabaseName);
    try
      IBTransaction.Active := true;
      FDataSet.Active := true;
      writeln(OutFile,'Add a record');
      FDataSet.Append;
      FDataSet.Post;
      FDataSet.First;
      PrintDataSet(FDataSet);
    finally
      IBDatabase.DropDatabase;
    end;

  end
  else
    writeln(OutFile,'Create Database failed');
end;

initialization
  RegisterTest(TTest04);

end.

