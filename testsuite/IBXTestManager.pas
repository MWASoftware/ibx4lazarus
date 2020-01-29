unit IBXTestManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestManager, DB, IBCustomDataSet;

type

{ TIBXTestBase }

TIBXTestBase = class(TTestBase)
protected
  procedure PrintDataSet(aDataSet: TIBCustomDataSet);
  procedure PrintDataSetRow(aField: TField);
  procedure PrintAffectedRows(query: TIBCustomDataSet);
end;

implementation

{ TIBXTestBase }

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

end.

