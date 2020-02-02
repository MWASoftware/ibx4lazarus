unit IBXTestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestApplication, CustApp, DB, IBCustomDataSet, IBDatabase, IBQuery,
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
  procedure CreateObjects(Application: TCustomApplication); override;
  procedure InitialiseDatabase; virtual;
  procedure PrintDataSet(aDataSet: TIBCustomDataSet);
  procedure PrintDataSetRow(aField: TField);
  procedure PrintAffectedRows(query: TIBCustomDataSet);
  procedure ReadOnlyTransaction;
  procedure ReadWriteTransaction;
  procedure RunScript(aFileName: string);
  procedure ExecuteSQL(SQL: string);
public
  property IBDatabase: TIBDatabase read  FIBDatabase;
  property IBTransaction: TIBTransaction read FIBTransaction;
  property IBQuery: TIBQuery read FIBQuery;
end;

implementation

{ TIBXTestBase }

procedure TIBXTestBase.HandleCreateDatebase(Sender: TObject);
begin
  InitialiseDatabase;
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

procedure TIBXTestBase.CreateObjects(Application: TCustomApplication);
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
  FIBTransaction := TIBTransaction.Create(Application);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBDatabase.DefaultTransaction := FIBTransaction;
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

procedure TIBXTestBase.InitialiseDatabase;
begin
  //Do nothing
end;

procedure TIBXTestBase.PrintDataSet(aDataSet: TIBCustomDataSet);
var i: integer;
    rowno: integer;
begin
  aDataSet.First;
  rowno := 1;
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
    writeln(OutFile, aField.FieldName,' = ',FormatFloat('#,##0.00',aField.AsFloat));

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

procedure TIBXTestBase.RunScript(aFileName: string);
begin
  FIBXScript.RunScript(aFileName);
end;

procedure TIBXTestBase.ExecuteSQL(SQL: string);
begin
  FIBXScript.ExecSQLScript(SQL);
end;

end.

