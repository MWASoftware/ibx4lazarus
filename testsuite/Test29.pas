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
unit Test29;

{$mode objfpc}{$H+}

{Test 29: Journalling}

{ Description
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBJournal,
  IBCustomDataSet, IBUtils, IBExtract;

const
  aTestID    = '29';
  aTestTitle = 'IBJournal Tests';

type

{ TTest29 }

  TTest29 = class(TIBXTestBase)
  private
    FDataSet: TIBDataSet;
    FJournal: TIBJournal;
    FExtract: TIBExtract;
    FCreateArrayOnInsert: boolean;
    procedure HandleAfterInsert(DataSet: TDataSet);
    procedure HandleOnJournalEntry(Sender: TObject; aJnlEntry: PJnlEntry);
    procedure DoPlayback(dbDumpFile: string);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses DateUtils;

{ TTest29 }

procedure TTest29.HandleAfterInsert(DataSet: TDataSet);
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
    F := TFileStream.Create('resources/Test29.dat',fmOpenRead);
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
//    WriteArray(TIBArrayField(FieldByName('MyArray')).ArrayIntf);
  end;
end;

procedure TTest29.HandleOnJournalEntry(Sender: TObject; aJnlEntry: PJnlEntry);
begin
  writeln(OutFile,'Journal Entry Made');
  writeln(OutFile,IBFormatJnlEntry(aJnlEntry));
  writeln(OutFile);
end;

procedure TTest29.DoPlayback(dbDumpFile: string);
begin
  with TJournalPlayer.Create do
  try
    FJournal.ReplayJournal(FJournal.JournalFilePath);
    IBTransaction.Active := true;
    FExtract.ExtractObject(eoDatabase,'',[etData,etGrantsToUser]);
    FExtract.Items.SaveToFile(dbDumpFile);
    IBTransaction.Active := false;
  finally
    Free;
  end;
end;

procedure TTest29.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FDataSet := TIBDataSet.Create(Application);
  with FDataSet do
  begin
    Name := 'Dataset' + GetTestID;
    Database := IBDatabase;
    Transaction := IBTransaction;
    SelectSQL.Text := 'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,'+
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.F14, A.F15, A.MyArray, A.'+
      'GRANTS, A."My Field" as MYFIELD1, A."MY Field" as MYFIELD2 From IBXTEST A';
    InsertSQL.Text :=
      'Insert Into IBXTEST(TABLEKEY, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, "f12", F13, F14, MyArray, '+
      ' GRANTS) Values(:TABLEKEY, :F1, :F2, :F3, :F4, :F5, :F6, :F7,'+
      ':F8, :F9, :F10, :F11, :F12, :F13, :F14, :MyArray, :GRANTS) Returning MyArray, F15';
    RefreshSQL.Text :=
      'Select A.TABLEKEY, A.F1, A.F2, A.F3, A.F4, A.F5, A.F6,' +
      ' A.F7, A.F8, A.F9, A.F10, A.F11, A."f12", A.F13, A.F14, A.MyArray, A.'+
      'GRANTS, A."My Field" as MYFIELD1, A."MY Field" as MYFIELD2 From IBXTEST A '+
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
        'Where A.TABLEKEY = :OLD_TABLEKEY RETURNING A.MyArray, A.F15';
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
  FJournal := TIBJournal.Create(Application);
  with FJournal do
  begin
    Database := IBDatabase;
    ApplicationName := 'Test29';
    RetainJournal := true;
    OnJournalEntry := @HandleOnJournalEntry;
  end;
  FExtract := TIBExtract.Create(Application);
  FExtract.Database := IBDatabase;
  FExtract.Transaction := IBTransaction;
end;

function TTest29.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest29.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest29.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest29.RunTest(CharSet: AnsiString; SQLDialect: integer);
var OldDefaultFormatSettings: TFormatSettings;
    i: integer;
    dbDumpFile: string;
begin
  OldDefaultFormatSettings := DefaultFormatSettings;
  IBDatabase.CreateDatabase;
  try
   try
    DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS.zzzz';
    FJournal.Enabled := true;
    IBTransaction.Active := true;
    FDataSet.Active := true;
    ListFields(FDataset);
    writeln(OutFile,'Add a record');
    FDataSet.Append;
    FDataSet.Post;
    writeln(OutFile,'Add and edit a record');
    FCreateArrayOnInsert := true;
    FDataSet.Append;
    FDataSet.Post;
    FDataSet.Edit;
    FDataSet.FieldByName('F1').AsInteger := 199;
    FDataSet.FieldByName('MYField1').AsString := 'My Field';
    FDataSet.FieldByName('MYFIELD2').AsString := 'MY Field';
    PrintDataSetRow(FDataSet);
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
    IBTransaction.Active := false;
    PrintJournalTable(IBDatabase.Attachment);

    FJournal.Enabled := false;
    writeln(OutFile);
    {print out journal}
    writeln(OutFile,'Low Level Journal File Print out');
    PrintJournalFile(FJournal.JournalFilePath);
    writeln(OutFile);
    writeln(OutFile,'Print out Journal File using TIBJournal');
    with TJournalPlayer.Create do
    try
      LoadJournalFile(FJournal.JournalFilePath, FJournal.Database);
      for i := 0 to JnlEntryCount - 1 do
        writeln(OutFile,IBFormatJnlEntry(JnlEntry[i]));
        writeln(OutFile);
    finally
      Free;
    end;

    {Dump Database to text file}
    dbDumpFile := ChangeFileExt(GetOutFile,'.db1');
    IBTransaction.Active := true;
    {Drop journaling suuport before dumping database}
    IBDatabase.Attachment.ExecImmediate(IBTransaction.TransactionIntf,'Drop Table IBX$JOURNALS');
    IBDatabase.Attachment.ExecImmediate(IBTransaction.TransactionIntf,'Drop Sequence IBX$SESSIONS');
    IBTransaction.Commit;
    IBTransaction.Active := true;
    FExtract.ExtractObject(eoDatabase,'',[etData,etGrantsToUser]);
    FExtract.Items.SaveToFile(dbDumpFile);
    IBTransaction.Active := false;

   finally
     DefaultFormatSettings := OldDefaultFormatSettings;
     IBDatabase.DropDatabase;
   end;

   {now playback the log and restore the database}
   IBDatabase.CreateDatabase;
   try
     DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS.zzzz';
     DoPlayback(ChangeFileExt(dbDumpFile,'.db2'));
   finally
     DefaultFormatSettings := OldDefaultFormatSettings;
     IBDatabase.DropDatabase;
   end;

   writeln(OutFile);
   writeln(OutFile,'Comparing original database with restored database');
   CompareFiles(dbDumpFile, ChangeFileExt(dbDumpFile,'.db2'));
  except on E: Exception do
    begin
      writeln(OutFile,'Terminated with Exception: ' + E.Message);
      IBDatabase.ForceClose;
    end;
  end;
end;

initialization
  RegisterTest(TTest29);

end.

