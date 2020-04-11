unit IBXTestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestApplication, CustApp, DB, IB, IBCustomDataSet, IBDatabase, IBQuery,
  ibxscript, IBDataOutput;

type

{ TIBXTestBase }

TIBXTestBase = class(TTestBase)
private
  FIBDatabase: TIBDatabase;
  FIBTransaction: TIBTransaction;
  FIBQuery: TIBQuery;
  FIBXScript: TIBXScript;
  procedure HandleCreateDatebase(Sender: TObject);
  procedure HandleDBFileName(Sender: TObject; var DatabaseFileName: string);
  procedure LogHandler(Sender: TObject; Msg: string);
  procedure ErrorLogHandler(Sender: TObject; Msg: string);
protected
  procedure ClientLibraryPathChanged; override;
  procedure CreateObjects(Application: TTestApplication); override;
  procedure InitialiseDatabase(aDatabase: TIBDatabase); virtual;
  procedure PrintDataSet(aDataSet: TDataSet);
  procedure PrintDataSetRow(aField: TField);
  procedure PrintAffectedRows(query: TIBCustomDataSet);
  procedure ReadOnlyTransaction;
  procedure ReadWriteTransaction;
  procedure RunScript(aDatabase: TIBDatabase; aFileName: string);
  procedure ShowStrings(aCaption: string; List: TStrings);
  procedure WriteStrings(List: TStrings);
  procedure ExecuteSQL(SQL: string);
  procedure ShowFBVersion(attachment: IAttachment);
public
  property IBDatabase: TIBDatabase read  FIBDatabase;
  property IBTransaction: TIBTransaction read FIBTransaction;
  property IBQuery: TIBQuery read FIBQuery;
end;

implementation

{ TIBXTestBase }

procedure TIBXTestBase.HandleCreateDatebase(Sender: TObject);
begin
  InitialiseDatabase(IBDatabase);
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
  FIBXScript.IgnoreCreateDatabase := true;
end;

procedure TIBXTestBase.InitialiseDatabase(aDatabase: TIBDatabase);
var aFileName: string;
    aTestID: string;
begin
  aTestID := GetTestID;
  if Length(aTestID) = 1 then
    aTestID := '0' + aTestID;
  aFileName := Format('resources/Test%s.sql',[aTestID]);
  if FileExists(aFileName) then
  begin
    writeln(OutFile,'Creating Database from ' + aFileName);
    writeln(OutFile);
    RunScript(aDatabase,aFileName);
  end;
end;

procedure TIBXTestBase.PrintDataSet(aDataSet: TDataSet);
var i: integer;
    rowno: integer;
begin
  aDataSet.First;
  rowno := 1;
  if aDataSet.EOF then
    writeln(OutFile,'Dataset Empty')
  else
  while not aDataSet.EOF do
  begin
    writeln(OutFile,'Row No = ',rowno);
    for i := 0 to aDataSet.FieldCount - 1 do
      PrintDataSetRow(aDataSet.Fields[i]);
    aDataSet.Next;
    Inc(rowno);
    writeln(OutFile);
  end;
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
    writeln(OutFile, aField.FieldName,' = ',FormatFloat('#,##0.000',aField.AsFloat));

  ftLargeint:
    writeln(OutFile,aField.FieldName,' = ',aField.AsString);

  ftBlob:
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
    if TIBStringField(aField).CharacterSetName <> 'NONE' then
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
    writeln('Selects = ',SelectCount);
    writeln('Inserts = ',InsertCount);
    writeln('Updates = ',UpdateCount);
    writeln('Deletes = ',DeleteCount);
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

procedure TIBXTestBase.WriteStrings(List: TStrings);
var i: integer;
begin
  for i := 0 to List.Count - 1 do
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

end.

