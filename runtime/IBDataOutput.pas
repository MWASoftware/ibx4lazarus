(*
 *  IBX For Lazarus (Firebird Express)
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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBDataOutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBSQL, IBDatabase;

type

  { TIBCustomDataOutput }

  TIBCustomDataOutput = class(TComponent)
  private
    FIBSQL: TIBSQL;
    FIncludeHeader: Boolean;
    function GetDatabase: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetTransaction(AValue: TIBTransaction);
  protected
    procedure HeaderOut(var Data: TStrings); virtual;
    function FormatBlob(Field: ISQLData): string; virtual;
    procedure FormattedDataOut(var Data: TStrings); virtual; abstract;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader;
  public
    constructor Create(aOwner: TComponent); override;
    procedure DataOut(SelectQuery: string; var Data: TStrings);
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
  end;

  { TIBCSVDataOut }

  TIBCSVDataOut = class(TIBCustomDataOutput)
  private
    FQuoteChar: char;
  protected
    procedure HeaderOut(var Data: TStrings); override;
    procedure FormattedDataOut(var Data: TStrings); override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property IncludeHeader;
    property QuoteChar: char read FQuoteChar write FQuoteChar default '''';
  end;

  { TIBInsertStmtsOut }

  TIBInsertStmtsOut = class(TIBCustomDataOutput)
  private
    FIncludeBlobsAndArrays: boolean;
    procedure FormatBlob(Field: ISQLData; var Data: TStrings);
    procedure FormatArray(Field: ISQLData; var Data: TStrings);
  protected
    procedure FormattedDataOut(var Data: TStrings); override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property IncludeBlobsAndArrays: boolean read FIncludeBlobsAndArrays
                                    write FIncludeBlobsAndArrays default true;
  end;

implementation

uses IBUtils, FBMessages;

{ TIBInsertStmtsOut }

procedure TIBInsertStmtsOut.FormatBlob(Field: ISQLData; var Data: TStrings);

  function ToHex(aValue: byte): string;
  const
    HexChars: '0123456789ABCDEF';
  begin
    Result := HexChars[(byte and $F0) shr 8] +
              HexChars[(byte and $0F)];
  end;

var blob: string;
    i, j: integer;
    s: string;
begin
  Data.Add('<binary>');
  blob := Field.AsString; {get Blob as untyped string }
  i := 0;
  while i < Length(blob) do
  begin
    s := '';
    for j := 1 to 40 do
    begin
      if i = Length(blob) then
        break
      else
        s := HexChars(chr(blob[i]);
      inc(i);
    end;
    Data.Add(s);
  end;
  Data.Add('</binary>');
end;

procedure TIBInsertStmtsOut.FormatArray(Field: ISQLData; var Data: TStrings);
begin

end;

procedure TIBInsertStmtsOut.FormattedDataOut(var Data: TStrings);
const
  QuoteChar = '''';

var TableName, s: string;
    i,j: integer;
begin
  TableName := trim(FIBSQL.GetUniqueRelationName);
  if TableName = '' then
    IBError(ibxeUniqueRelationReqd,[nil]);

  s := 'INSERT INTO ' + QuoteIdentifier(Database.SQLDialect, TableName) + ' (';
  with FIBSQL do
  begin
    j := 0;
    for i := 0 to MetaData.Count - 1 do
    if IncludeBlobsAndArrays or
       ((MetaData[i].SQLTYPE <> SQL_BLOB) and (MetaData[i].SQLType <> SQL_ARRAY)) then
    begin
      if j <> 0 then s += ',';
      s += QuoteIdentifierIfNeeded(Database.SQLDialect,MetaData[i].getAliasName);
      Inc(j);
    end;
    s += ') VALUES(';

    j := 0;
    for i := 0 to Current.Count - 1 do
    if IncludeBlobsAndArrays or
       ((Current[i].SQLTYPE <> SQL_BLOB) and (Current[i].SQLType <> SQL_ARRAY)) then
    begin
      if j <> 0 then s += ',';
      case Current[i].SQLType of
      SQL_BLOB:
        if Current[i].SQLSubType = 1 then
          s += QuoteChar + Current[i].AsString + QuoteChar
        else
        begin
          Data.Add(s);
          s := '';
          FormatBlob(Current[i],Data);
        end;

      SQL_ARRAY:
        begin
          Data.Add(s);
          s := '';
          FormatArray(Current[i],Data);
        end;

      else
        s += Current[i].AsString;
      end;
      Inc(j);
    end;
  end;
  s += ');';
  Data.Add(s);
end;

constructor TIBInsertStmtsOut.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIncludeBlobsAndArrays := true;
end;

{ TIBCSVDataOut }

procedure TIBCSVDataOut.HeaderOut(var Data: TStrings);
var i: integer;
    s: string;
begin
  s := '';
  for i := 0 to FIBSQL.MetaData.Count - 1 do
  begin
    if i <> 0 then s += ',';
    s += FIBSQL.MetaData[i].getAliasName;
  end;
  Data.Add(s);
end;

procedure TIBCSVDataOut.FormattedDataOut(var Data: TStrings);
var i: integer;
    s: string;
begin
  s := '';
  with FIBSQL do
  begin
    for i := 0 to Current.Count - 1 do
    begin
      if i <> 0 then s += ',';
      if (Current[i].SQLType = SQL_BLOB) and (Current[i].SQLSubType <> 1) then
        s += '(blob')
      else
      if (Current[i].SQLType = SQL_VARYING) or (Current[i].SQLType = SQL_TEXT) or
          ((Current[i].SQLType = SQL_BLOB) and (Current[i].SQLSubType = 1)) then
        s += QuoteChar + Current[i].AsString + QuoteChar
      else
      s += Current[i].AsString;
    end;
  end;
  Data.Add(s);
end;

constructor TIBCSVDataOut.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuoteChar := '''';
end;

{ TIBCustomDataOutput }

function TIBCustomDataOutput.GetDatabase: TIBDatabase;
begin
  Result := FIBSQL.Database;
end;

function TIBCustomDataOutput.GetTransaction: TIBTransaction;
begin
  Result := FIBSQL.Transaction;
end;

procedure TIBCustomDataOutput.SetDatabase(AValue: TIBDatabase);
begin
  FIBSQL.Database := AValue;
end;

procedure TIBCustomDataOutput.SetTransaction(AValue: TIBTransaction);
begin
  FIBSQL.Transaction := AValue;
end;

procedure TIBCustomDataOutput.HeaderOut(var Data: TStrings);
begin
  //stub
end;

function TIBCustomDataOutput.FormatBlob(Field: ISQLData): string;
begin
  Result := '(blob)';
end;

constructor TIBCustomDataOutput.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIBSQL := TIBSQL.Create(self);
end;

procedure TIBCustomDataOutput.DataOut(SelectQuery: string; var Data: TStrings);
begin
  FIBSQL.SQL.Text := SelectQuery;
  FIBSQL.Prepare;
  if IncludeHeader then
    HeaderOut(Data);
  FIBSQL.ExecQuery;
  try
    while not FIBSQL.EOF do
    begin
      FormattedDataOut(Data);
      FIBSQL.Next;
    end;
  finally
    FIBSQL.Close;
  end;
end;

end.

