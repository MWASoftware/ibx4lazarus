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
unit Test17;

{$mode objfpc}{$H+}

{Test 17: TIBDataSet Tests}

{ Description

  TIBDataset Insert/Update/Delete tests.
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBCustomDataSet;

const
  aTestID    = '17';
  aTestTitle = 'TIBDataset Tests';

type

{ TTest1 }

  { TTest17 }

  TTest17 = class(TIBXTestBase)
  private
    FIBDataSet1: TIBDataSet;
    FIBDataSet2: TIBDataSet;
    procedure HandleDeleteReturning(Sender: TObject; QryResults: IResults);
    procedure HandleBeforeOpen(DataSet: TDataSet);
    procedure HandleAfterOpen(DataSet: TDataSet);
    procedure HandleBeforeClose(DataSet: TDataSet);
    procedure HandleAfterClose(DataSet: TDataSet);
    procedure HandleBeforeInsert(DataSet: TDataSet);
    procedure HandleAfterInsert(DataSet: TDataSet);
    procedure HandleBeforeEdit(DataSet: TDataSet);
    procedure HandleAfterEdit(DataSet: TDataSet);
    procedure HandleBeforePost(DataSet: TDataSet);
    procedure HandleAfterPost(DataSet: TDataSet);
    procedure HandleBeforeCancel(DataSet: TDataSet);
    procedure HandleAfterCancel(DataSet: TDataSet);
    procedure HandleBeforeDelete(DataSet: TDataSet);
    procedure HandleAfterDelete(DataSet: TDataSet);
    procedure HandleBeforeScroll(DataSet: TDataSet);
    procedure HandleAfterScroll(DataSet: TDataSet);
    procedure HandleBeforeRefresh(DataSet: TDataSet);
    procedure HandleAfterRefresh(DataSet: TDataSet);
    procedure ValidatePostOK(Sender: TObject; var CancelPost: boolean);
    procedure ValidatePostCancel(Sender: TObject; var CancelPost: boolean);
    procedure HandlePostError(DataSet: TDataSet; E: EDatabaseError;
                     var DataAction: TDataAction);
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

procedure TTest17.HandleBeforeOpen(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeOpen: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterOpen(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterOpen: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeClose(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeClose: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterClose(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterClose: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeInsert(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeInsert: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterInsert(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterInsert: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeEdit(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeEdit: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterEdit(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterEdit: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforePost(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforePost: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterPost(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterPost: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeCancel(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeCancel: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterCancel(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterCancel: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeDelete(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeDelete: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterDelete(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterDelete: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeScroll(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeScroll: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterScroll(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterScroll: State = ',DataSet.State);
end;
procedure TTest17.HandleBeforeRefresh(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event BeforeRefresh: State = ',DataSet.State);
end;
procedure TTest17.HandleAfterRefresh(DataSet: TDataSet);
Begin
  writeln(Outfile,'Dataset Event AfterRefresh: State = ',DataSet.State);
end;

procedure TTest17.ValidatePostOK(Sender: TObject; var CancelPost: boolean);
begin
  writeln(Outfile,'Validate Post OK called');
  CancelPost := false;
end;

procedure TTest17.ValidatePostCancel(Sender: TObject; var CancelPost: boolean);
begin
  writeln(Outfile,'Validate Post Cancel called');
  CancelPost := true;
end;

procedure TTest17.HandlePostError(DataSet: TDataSet; E: EDatabaseError;
  var DataAction: TDataAction);
begin
  writeln(Outfile,'Post Error Called: ',E.Message);
  DataAction := daFail;
end;

procedure TTest17.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBDataSet1 := TIBDataSet.Create(Application);
  with FIBDataSet1 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    Unidirectional := false;
    SelectSQL.Add('Select * From IBDataSetTest');
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (:KeyField,:PlainText)');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
    GeneratorField.Field := 'KeyField';
    GeneratorField.Generator := 'AGENERATOR';
    GeneratorField.Increment := 1;
    OnPostError := @HandlePostError;
  end;
  FIBDataSet2 := TIBDataSet.Create(Application);
  with FIBDataSet2 do
  begin
    Database := IBDatabase;
    Transaction := IBTransaction;
    Unidirectional := false;
    SelectSQL.Add('Select * From IBDataSetTest');
    InsertSQL.Add('Insert into IBDataSetTest(KeyField,PlainText) Values (Gen_ID(AGenerator,1),:PlainText) Returning KeyField, TextAndKey');
    ModifySQL.Add('Update IBDataSetTest Set KeyField = :KeyField, PlainText = :PlainText Where KeyField = :Old_KeyField Returning TextAndKey,ServerSideText');
    DeleteSQL.Add('Delete from IBDataSetTest Where KeyField = :old_KeyField Returning KeyField');
    RefreshSQL.Add('Select * from IBDataSetTest Where KeyField = :KeyField');
    OnDeleteReturning := @HandleDeleteReturning;
    BeforeOpen := @HandleBeforeOpen;
    AfterOpen := @HandleAfterOpen;
    BeforeClose := @HandleBeforeClose;
    AfterClose := @HandleAfterClose;
    BeforeInsert := @HandleBeforeInsert;
    AfterInsert := @HandleAfterInsert;
    BeforeEdit := @HandleBeforeEdit;
    AfterEdit := @HandleAfterEdit;
    BeforePost := @HandleBeforePost;
    AfterPost := @HandleAfterPost;
    BeforeCancel := @HandleBeforeCancel;
    AfterCancel := @HandleAfterCancel;
    BeforeDelete := @HandleBeforeDelete;
    AfterDelete := @HandleAfterDelete;
    BeforeScroll := @HandleBeforeScroll;
    AfterScroll := @HandleAfterScroll;
    BeforeRefresh := @HandleBeforeRefresh;
    AfterRefresh := @HandleAfterRefresh;
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
var lastKey: integer;
    i: integer;
    B: TBookmark;
begin
  IBDatabase.CreateDatabase;
  try
    IBTransaction.Active := true;
    with FIBDataSet1 do
    begin
      Active := true;
      writeln(OutFile,'FIBDataSet1: Simple Append');
      Append;
//      writeln(outfile,'BOF = ',BOF,', EOF = ',EOF);
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
      writeln(Outfile,'Show whole Dataset');
      PrintDataSet(FIBDataSet1);
      writeln(OutFile,'Now delete the first row');
      First;
      Delete;
      PrintDataSet(FIBDataSet1);
      writeln(Outfile,'On Post KeyField Assignment');
      FIBDataSet1.GeneratorField.ApplyOnEvent := gaeOnPostRecord;
      Append;
      FieldByName('PlainText').AsString := 'On Post KeyField test';
      PrintDataSetRow(FIBDataSet1);
      Post;
      writeln(Outfile,'Row data after post');
      PrintDataSetRow(FIBDataSet1);
      FIBDataSet1.GeneratorField.ApplyOnEvent := gaeOnNewRecord; {restore}

      writeln(Outfile,'Catch a Post Error - duplicate key');
      lastkey := FieldByName('KeyField').AsInteger;
      Append;
      FieldByName('KeyField').AsInteger := lastkey;
      FieldByName('PlainText').AsString := 'On Post Error test';
      try
        Post;
      except on E: Exception do
        writeln(Outfile,'Exception handled: ',E.Message);
      end;

      IBTransaction.Rollback;
      IBTransaction.Active := true;
      DataSetCloseAction := dcSaveChanges;
      Active := true;
      writeln(OutFile,'FIBDataSet1: Simple Append with automatic posting on close');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet1);

      IBTransaction.Rollback;
      IBTransaction.Active := true;
      DataSetCloseAction := dcDiscardChanges;
      Active := true;
      writeln(OutFile,'FIBDataSet1: Simple Append with discard on close');
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet1);

      IBTransaction.Rollback;
      IBTransaction.Active := true;
      writeln(Outfile);
      writeln(Outfile,'Unidirectional editing');
      Unidirectional := true;
      Active := true;
      writeln(OutFile,'FIBDataSet1: Simple Append - unidirectional');
      Insert;
      writeln(outfile,'BOF = ',BOF,', EOF = ',EOF);
      FieldByName('PlainText').AsString := 'This is a test - unidirectional';
      PrintDataSetRow(FIBDataSet1);
      Post;
      writeln(outfile,'BOF = ',BOF,', EOF = ',EOF);
      PrintDataSetRow(FIBDataSet1);
      Refresh;
      writeln(OutFile,'After Refresh - unidirectional');
      PrintDataSetRow(FIBDataSet1);
      writeln(OutFile,' Record Count = ',FIBDataSet1.RecordCount);
      writeln(OutFile,'Insert and Update');
      Insert;
      FieldByName('PlainText').AsString := 'This is another test - unidirectional';
      Post;
      PrintDataSetRow(FIBDataSet1);
      Edit;
      FieldByName('PlainText').AsString := 'This is the update test - unidirectional';
      Post;
      PrintDataSetRow(FIBDataSet1);
      writeln(OutFile,'Now delete the first row - unidirectional with Record Count = ',FIBDataSet1.RecordCount);
      Active := false;
      Active := true;
      Delete;
      writeln(OutFile,'Show Current Row');
      PrintDataSetRow(FIBDataSet1);
      writeln(OutFile,' Record Count = ',FIBDataSet1.RecordCount);
      writeln(Outfile,'Ensure dataset saved to database');
      Active := false;
      Active := true;
      PrintDataSet(FIBDataSet1);

    end;
    writeln(Outfile,'==================================');
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    with FIBDataSet2 do
    try
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
      OnValidatePost := @ValidatePostOK;
      writeln(Outfile,'Validate Post OK');
      Append;
      FieldByName('PlainText').AsString := 'This is a validated Post';
      Post;
      PrintDataSetRow(FIBDataSet2);
      OnValidatePost := @ValidatePostCancel;
      writeln(Outfile,'Validate Post Cancel');
      Append;
      FieldByName('PlainText').AsString := 'This is a validated Post which should have been cancelled';
      Post;
      PrintDataSetRow(FIBDataSet2);
      OnValidatePost := nil;
      writeln(OutFile,'FIBDataSet2: Simple Append with Forced Refresh');
      ForcedRefresh := true;
      Append;
      FieldByName('PlainText').AsString := 'This is a test';
      Post;
      PrintDataSetRow(FIBDataSet2);
    except on E: Exception do
     writeln(Outfile,E.Message);
    end;
    IBTransaction.Rollback;
    IBTransaction.Active := true;
    with FIBDataSet1 do
    try
      Unidirectional := false;
      Active := true;
      writeln(outfile,'----------------------------------------------');
      writeln(OutFile,'FIBDataSet1: Insert at start');
      for i := 1 to 2 do
      begin
        Append;
        FieldByName('PlainText').AsString := 'Row ' + IntToStr(i);
        Post;
      end;
      First;
      Insert;
      FieldByName('PlainText').AsString := 'This is an insert test';
      Post;
      B := Bookmark;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Delete inserted row');
      Bookmark := B;
      Delete;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Repeat');
      First;
      Insert;
      FieldByName('PlainText').AsString := 'This is an insert test #1';
      Post;
      B := Bookmark;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Delete inserted row');
      Bookmark := B;
      Delete;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Insert/Delete after first row');
      Next;
      Insert;
      FieldByName('PlainText').AsString := 'This is an insert test #2';
      Post;
      B := Bookmark;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Delete inserted row');
      Bookmark := B;
      Delete;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Insert/Delete at last row');
      Last;
      Insert;
      FieldByName('PlainText').AsString := 'This is an insert test #3';
      Post;
      B := Bookmark;
      PrintDataSet(FIBDataSet1);
      writeln(outfile,'Delete inserted row');
      Bookmark := B;
      Delete;
      PrintDataSet(FIBDataSet1);
    except on E: Exception do
       writeln(Outfile,E.Message);
    end;

  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest17);

end.

