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
unit IBXTestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestApplication, CustApp, DB, IB, IBCustomDataSet, IBDatabase, IBQuery,
  ibxscript, IBDataOutput, IBSQL;

type

{ TIBXTestBase }

TIBXTestBase = class(TTestBase)
private
  FIBDatabase: TIBDatabase;
  FIBTransaction: TIBTransaction;
  FIBQuery: TIBQuery;
  FIBXScript: TIBXScript;
  FInitialising: boolean;
  FScriptFile: AnsiString;
  function GetRoleName: AnsiString;
  function GetScriptFile: AnsiString;
  procedure HandleCreateDatebase(Sender: TObject);
  procedure HandleDBFileName(Sender: TObject; var DatabaseFileName: string);
  procedure LogHandler(Sender: TObject; Msg: string);
  procedure ErrorLogHandler(Sender: TObject; Msg: string);
protected
  procedure ClientLibraryPathChanged; override;
  procedure CreateObjects(Application: TTestApplication); override;
  function GetFullTestID: string;
  function GetOutFile: string;
  function GetSSBackupFile: string;
  procedure InitialiseDatabase(aDatabase: TIBDatabase); virtual;
  procedure PrintDataSet(aDataSet: TDataSet);
  procedure PrintDataSetRow(aDataSet: TDataSet); overload;
  procedure PrintDataSetRow(aField: TField); overload;
  procedure PrintAffectedRows(query: TIBCustomDataSet); overload;
  procedure PrintAffectedRows(query: TIBSQL); overload;
  procedure ReadOnlyTransaction;
  procedure ReadWriteTransaction;
  procedure RunScript(aDatabase: TIBDatabase; aFileName: string);
  procedure ShowStrings(aCaption: string; List: TStrings);
  procedure WriteStrings(List: TStrings; limit: integer=0);
  procedure ExecuteSQL(SQL: string);
  procedure ShowFBVersion(attachment: IAttachment);
  procedure ShowBoolValue(aValue: integer; WhenTrue, WhenFalse: string);
  procedure ProcessResults; override;
public
  destructor Destroy; override;
  property IBDatabase: TIBDatabase read  FIBDatabase;
  property IBTransaction: TIBTransaction read FIBTransaction;
  property IBQuery: TIBQuery read FIBQuery;
  property RoleName: AnsiString read GetRoleName;
  property IBXScriptObj: TIBXScript read FIBXScript;
end;

implementation

uses Process, IBUtils;

const
  sqlScriptTemplate = 'resources/Test%s.sql';
  sqlCustomScriptTemplate = 'resources/Test%s.%d.sql';
  outFileTemplate   = 'Test%s.out';

{ TIBXTestBase }

procedure TIBXTestBase.HandleCreateDatebase(Sender: TObject);
begin
  if not FInitialising then
  begin
    FInitialising := true;
    try
      InitialiseDatabase(IBDatabase);
    finally
      FInitialising := false;
    end;
  end;
end;

function TIBXTestBase.GetRoleName: AnsiString;
begin
  if IBDatabase.Connected then
    Result := IBDatabase.Attachment.OpenCursorAtStart(IBTransaction.TransactionIntf,
                  'Select CURRENT_ROLE From RDB$Database',[])[0].AsString
  else
    Result := '';
end;

function TIBXTestBase.GetScriptFile: AnsiString;
begin
  FScriptFile := '';
  if IBDatabase.Attachment <> nil then
    FScriptFile := Format(sqlCustomScriptTemplate,[GetFullTestID,IBDatabase.Attachment.GetODSMajorVersion]);
  if not FileExists(FScriptFile) then
    FScriptFile := Format(sqlScriptTemplate,[GetFullTestID]);
  Result := FScriptFile;
end;

procedure TIBXTestBase.HandleDBFileName(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := IBDatabase.DatabaseName;
end;

procedure TIBXTestBase.LogHandler(Sender: TObject; Msg: string);
begin
   writeln(OutFile,Msg);
end;

procedure TIBXTestBase.ErrorLogHandler(Sender: TObject; Msg: string);
begin
  writeln(OutFile,Msg);
end;

procedure TIBXTestBase.ClientLibraryPathChanged;
begin
  inherited ClientLibraryPathChanged;
  FIBDatabase.FirebirdLibraryPathName := Owner.ClientLibraryPath;
end;

procedure TIBXTestBase.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  { In console Mode the application should own the database
    - ensures centralised exception handling }
  FIBDatabase := TIBDatabase.Create(Application);
  FIBDatabase.FirebirdLibraryPathName := Owner.ClientLibraryPath;
  FIBDatabase.LoginPrompt := false;
  FIBDatabase.Params.Add('user_name=' + Owner.GetUserName);
  FIBDatabase.Params.Add('password=' + Owner.GetPassword);
  FIBDatabase.Params.Add('lc_ctype=UTF8');
  FIBDatabase.OnCreateDatabase := @HandleCreateDatebase;
  FIBDatabase.Name := 'Test_Database_' + GetTestID;
  FIBTransaction := TIBTransaction.Create(Application);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBTransaction.Name := 'Test_Transaction_' + GetTestID;
  FIBQuery := TIBQuery.Create(Application);
  FIBQuery.Database := FIBDatabase;
  FIBXScript := TIBXScript.Create(Application);
  FIBXScript.Database := FIBDatabase;
  FIBXScript.Transaction := FIBTransaction;
  FIBXScript.OnOutputLog := @LogHandler;
  FIBXScript.OnErrorLog := @ErrorLogHandler;
  FIBXScript.DataOutputFormatter := TIBInsertStmtsOut.Create(Application);
  FIBXScript.OnCreateDatabase := @HandleDBFileName;
  FIBXScript.IgnoreCreateDatabase := FALSE;
end;

function TIBXTestBase.GetFullTestID: string;
begin
  Result := GetTestID;
  if Length(Result) = 1 then
    Result := '0' + Result;
end;

function TIBXTestBase.GetOutFile: string;
begin
  Result := Format(outFileTemplate,[GetFullTestID]);
end;

function TIBXTestBase.GetSSBackupFile: string;
begin
  Result := ChangeFileExt(Owner.GetBackupFileName,'.fbk');
end;

procedure TIBXTestBase.InitialiseDatabase(aDatabase: TIBDatabase);
var aFileName: string;
    {$IFDEF WINDOWS}
    F: text;
    line: AnsiString;
    {$ENDIF}
begin
  aFileName := GetScriptFile;
  if FileExists(aFileName) then
  begin
    writeln(OutFile,'Creating Database from ' + aFileName);
    writeln(OutFile);
    {$IFDEF WINDOWS}
    assignfile(F,aFileName);
    try
      Reset(F);
      readln(F,line);
      close(F);
      if Pos('link ',Line) = 1 then
         aFileName := ExtractFilePath(aFileName) + system.copy(Line,6,Length(Line)-5);
    except
      //do nothing
    end;
    {$ENDIF}
    RunScript(aDatabase,aFileName);
  end;
end;

procedure TIBXTestBase.PrintDataSet(aDataSet: TDataSet);
var rowno: integer;
begin
  if aDataSet.Name <> '' then
    writeln(OutFile,'Print Dataset for ',aDataSet.Name);
  aDataSet.First;
  rowno := 1;
  if aDataSet.EOF then
    writeln(OutFile,'Dataset Empty')
  else
  while not aDataSet.EOF do
  begin
    CheckSynchronize(100);
    writeln(OutFile,'Row No = ',rowno);
    PrintDataSetRow(aDataset);
    aDataSet.Next;
    Inc(rowno);
    writeln(OutFile);
  end;
  writeln(Outfile,'Rows printed = ',IntToStr(rowno-1));
  writeln(Outfile);
end;

procedure TIBXTestBase.PrintDataSetRow(aDataSet: TDataSet);
var i: integer;
begin
  for i := 0 to aDataSet.FieldCount - 1 do
    PrintDataSetRow(aDataSet.Fields[i]);
end;

procedure TIBXTestBase.PrintDataSetRow(aField: TField);
var s: AnsiString;
    dt: TDateTime;
begin
  if aField.IsNull then
    writeln(OutFile,aField.FieldName,' = NULL')
  else
  case aField.DataType of
  ftArray:
    begin
      if not aField.IsNull then
        WriteArray(TIBArrayField(aField).ArrayIntf);
    end;

  ftFloat:
    writeln(OutFile, aField.FieldName,' = ',FormatFloat('#,##0.000000000000',aField.AsFloat));

  ftLargeint:
    writeln(OutFile,aField.FieldName,' = ',aField.AsString);

  ftmemo, ftBlob:
    if TBlobField(aField).BlobType = ftMemo then
    begin
      s := aField.AsString;
      if FHexStrings then
      begin
        write(OutFile,aField.FieldName,' = ');
        PrintHexString(s);
        writeln(OutFile,' (Charset = ',TIBMemoField(aField).CharacterSetName, ' Codepage = ',StringCodePage(s),')');
      end
      else
      begin
        writeln(OutFile,aField.FieldName,' (Charset  = ',TIBMemoField(aField).CharacterSetName, ' Codepage = ',StringCodePage(s),')');
        writeln(OutFile);
        writeln(OutFile,s);
      end
    end
    else
      writeln(OutFile,aField.FieldName,' = (blob), Length = ',TBlobField(aField).BlobSize);

  ftString:
  begin
    s := aField.AsString;
    if FHexStrings then
    begin
      write(OutFile,aField.FieldName,' = ');
      PrintHexString(s);
      writeln(OutFile,' (Charset = ',TIBStringField(aField).CharacterSetName, ' Codepage = ',StringCodePage(s),')');
    end
    else
    if (aField is TIBStringField) and (TIBStringField(aField).CharacterSetName <> 'NONE') then
      writeln(OutFile,aField.FieldName,' = ',s,' (Charset = ',TIBStringField(aField).CharacterSetName, ' Codepage = ',StringCodePage(s),')')
    else
      writeln(OutFile,aField.FieldName,' = ',s);
  end;

  else
    writeln(OutFile,aField.FieldName,' = ',aField.AsString);
  end;
end;

procedure TIBXTestBase.PrintAffectedRows(query: TIBCustomDataSet);
var SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
begin
  if query.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount) then
  begin
    writeln(OutFile,'Selects = ',SelectCount);
    writeln(OutFile,'Inserts = ',InsertCount);
    writeln(OutFile,'Updates = ',UpdateCount);
    writeln(OutFile,'Deletes = ',DeleteCount);
  end;
end;

procedure TIBXTestBase.PrintAffectedRows(query: TIBSQL);
var SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
begin
  if query.statement.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount) then
  begin
    writeln(OutFile,'Selects = ',SelectCount);
    writeln(OutFile,'Inserts = ',InsertCount);
    writeln(OutFile,'Updates = ',UpdateCount);
    writeln(OutFile,'Deletes = ',DeleteCount);
  end;
end;

procedure TIBXTestBase.ReadOnlyTransaction;
begin
  FIBTransaction.Params.Clear;
  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');
  FIBTransaction.Params.Add('read');
end;


procedure TIBXTestBase.ReadWriteTransaction;
begin
  FIBTransaction.Params.Clear;
  FIBTransaction.Params.Add('concurrency');
  FIBTransaction.Params.Add('wait');
  FIBTransaction.Params.Add('write');
end;

procedure TIBXTestBase.RunScript(aDatabase: TIBDatabase; aFileName: string);
begin
  FIBXScript.Database := aDatabase;
  aDatabase.DefaultTransaction.Active := true;
  FIBXScript.Transaction := aDatabase.DefaultTransaction;
  FIBXScript.RunScript(aFileName);
end;

procedure TIBXTestBase.ShowStrings(aCaption: string; List: TStrings);
var s: string;
    i: integer;
begin
  s := aCaption + ': ';
  for i := 0 to List.Count - 1 do
  begin
    if i > 0 then
      s := s + ', ';
    s := s + List[i];
  end;
 writeln(OutFile,s);
end;

procedure TIBXTestBase.WriteStrings(List: TStrings; limit: integer);
var i: integer;
begin
 if Limit <= 0 then
   Limit := List.Count - 1;
  for i := 0 to limit do
    writeln(OutFile,List[i]);
  writeln(OutFile);
end;

procedure TIBXTestBase.ExecuteSQL(SQL: string);
begin
  FIBXScript.ExecSQLScript(SQL);
end;

procedure TIBXTestBase.ShowFBVersion(attachment: IAttachment);
var S: TStrings;
begin
  S := TStringList.Create;
  try
    attachment.getFBVersion(S);
    ShowStrings('FB Version',S);
  finally
    S.Free;
  end;
end;

procedure TIBXTestBase.ShowBoolValue(aValue: integer; WhenTrue,
  WhenFalse: string);
begin
 if aValue <> 0 then
   writeln(OutFile,WhenTrue)
 else
   writeln(OutFile,WhenFalse);
end;

procedure TIBXTestBase.ProcessResults;
var DiffExe: string;
    ResourceFile: string;
    S: TStrings;
    Results: string;
    ExitStatus: integer;
begin
   ResourceFile := FScriptFile;
   if FileExists(GetOutFile) and FileExists(ResourceFile) then
   begin
     DiffExe := GetEnvironmentVariable('DIFF');
     if DiffExe = '' then
       DiffExe := 'diff';
     S := TStringList.Create;
     try
       RunCommandInDir(GetCurrentDir,DiffExe ,[ResourceFile,GetOutFile],Results,ExitStatus);
       writeln(OutFile,'Run diff command returns ',ExitStatus);
       if Results <> '' then
       begin
         S.Text := Results;
         writeln(Outfile,'Output from diff command');
         WriteStrings(S);
       end;
     finally
       S.Free;
     end;
   end;
   IBDatabase.Connected := false;
end;

destructor TIBXTestBase.Destroy;
begin
  if IBDatabase <> nil then
    IBDatabase.Connected := false;
  inherited Destroy;
end;

end.

