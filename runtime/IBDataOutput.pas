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
  Classes, SysUtils, IBSQL, IBDatabase, IB;

const
  MaxBlobText = 80;

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
    procedure FormattedDataOut(var Data: TStrings); virtual; abstract;
    procedure TrailerOut(var Data: TStrings); virtual;
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
    FInsertHeader: string;
    procedure FormatBlob(Field: ISQLData; var Data: TStrings);
    procedure FormatArray(ar: IArray; var Data: TStrings);
  protected
    procedure HeaderOut(var Data: TStrings); override;
    procedure FormattedDataOut(var Data: TStrings); override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property IncludeBlobsAndArrays: boolean read FIncludeBlobsAndArrays
                                    write FIncludeBlobsAndArrays default true;
  end;

  TAlignments = (taLeft, taCentre, taRight);

  { TIBBlockFormatOut }

  TIBBlockFormatOut = class(TIBCustomDataOutput)
  private
    FColWidths: array of integer;
    FRowWidth: integer;
    function DashedLine: string;
    function TextAlign(s: string; ColWidth: integer; alignment: TAlignments
      ): string;
  protected
    procedure HeaderOut(var Data: TStrings); override;
    procedure FormattedDataOut(var Data: TStrings); override;
    procedure TrailerOut(var Data: TStrings); override;
  end;

implementation

uses IBUtils, FBMessages, Math;

{ TIBBlockFormatOut }

function TIBBlockFormatOut.DashedLine: string;
var i: integer;
begin
  Setlength(Result,FRowWidth);
  for i := 1 to FRowWidth do
    Result[i] := '-';
end;

function TIBBlockFormatOut.TextAlign(s: string; ColWidth: integer;
  alignment: TAlignments): string;
begin
  SetLength(Result,ColWidth);
  FillChar(Result[1],ColWidth,' ');
  if Length(s) > ColWidth then
    s := LeftStr(s,ColWidth);
  case alignment of
  taLeft:
    Move(s[1],Result[1],Length(s));
  taCentre:
    Move(s[1],Result[(ColWidth - Length(s)) div 2],Length(s));
  taRight:
    Move(s[1],Result[ColWidth - Length(s)],Length(s));
  end;
end;

procedure TIBBlockFormatOut.HeaderOut(var Data: TStrings);
var i: integer;
    s: string;
begin
  with FIBSQL do
  begin
    {Calculate column Widths}
    SetLength(FColWidths,MetaData.Count);
    FRowWidth := 1; {assume leading '|'}
    for i := 0 to MetaData.Count - 1 do
    begin
      with MetaData[i] do
      case SQLType of
      SQL_VARYING, SQL_TEXT:
        FColWidths[i] := GetSize;

      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        FColWidths[i] := 18; {see http://www.freepascal.org/docs-html/rtl/sysutils/formatfloat.html}

      SQL_LONG:
        if Scale = 0 then
          FColWidths[i] := 12 {allow for minus sign}
        else
          FColWidths[i] := 13; {leave room for the decimal point}

      SQL_SHORT:
        if Scale = 0 then
          FColWidths[i] := 6 {allow for minus sign}
        else
          FColWidths[i] := 7; {leave room for the decimal point}

      SQL_INT64:
        if Scale = 0 then
          FColWidths[i] := 20 {allow for minus sign}
        else
          FColWidths[i] := 21; {leave room for the decimal point}

      SQL_TIMESTAMP:
        FColWidths[i] := 21;

      SQL_TYPE_DATE, SQL_TYPE_TIME:
        FColWidths[i] := 8;

      SQL_BLOB:
        if SQLSubType = 1 then
          FColWidths[i] := MaxBlobText
        else
          FColWidths[i] := length(SBlob);

      SQL_ARRAY:
        FColWidths[i] := length(SArray);

      SQL_BOOLEAN:
        FColWidths[i] := Max(Length(STrue),Length(SFalse));
      end;
      if FColWidths[i] < Length(Name) then
        FColWidths[i] := Length(Name);
      FRowWidth += FColWidths[i] + 1;
    end;

    {Now output the header}

    Data.Add(DashedLine);
    s := '|';
    for i := 0 to MetaData.Count - 1 do
      s += TextAlign(MetaData[i].Name,FColWidths[i],taCentre) + '|';
    Data.Add(s);
    Data.Add(DashedLine);
  end;
end;

procedure TIBBlockFormatOut.FormattedDataOut(var Data: TStrings);

  function TruncateTextBlob(textStr: string): string;
  begin
    if Length(textStr) > MaxBlobText then
      Result := LeftStr(textStr,MaxBlobText-3) + '...'
    else
      Result := textStr;
  end;

var i: integer;
    s: string;
begin
  s := '|';
  for i := 0 to FIBSQL.Current.Count - 1 do
  with FIBSQL.Current[i] do
  if IsNull then
    s += TextAlign('NULL',FColWidths[i],taCentre)
  else
  case SQLType of
  SQL_VARYING, SQL_TEXT,
  SQL_TIMESTAMP,SQL_TYPE_DATE, SQL_TYPE_TIME:
    s += TextAlign(AsString,FColWidths[i],taLeft);

  SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT,
  SQL_LONG, SQL_SHORT, SQL_INT64:
    s += TextAlign(AsString,FColWidths[i],taRight);

  SQL_BOOLEAN, SQL_ARRAY:
    s += TextAlign(AsString,FColWidths[i],taCentre);

  SQL_BLOB:
    if SQLSubType = 1 then
      s += TextAlign(TruncateTextBlob(AsString),FColWidths[i],taLeft)
    else
      s += TextAlign(sBlob,FColWidths[i],taCentre);
  end;
  s += '|';
  Data.Add(s);
  Data.Add(DashedLine);
end;

procedure TIBBlockFormatOut.TrailerOut(var Data: TStrings);
begin
  Data.Add(DashedLine);
end;

{ TIBInsertStmtsOut }

procedure TIBInsertStmtsOut.FormatBlob(Field: ISQLData; var Data: TStrings);

  function ToHex(aValue: byte): string;
  const
    HexChars = '0123456789ABCDEF';
  begin
    Result := HexChars[(aValue and $F0) shr 8] +
              HexChars[(aValue and $0F)];
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
        s += ToHex(byte(blob[i]));
      inc(i);
    end;
    Data.Add(s);
  end;
  Data.Add('</binary>');
end;

procedure TIBInsertStmtsOut.FormatArray(ar: IArray; var Data: TStrings);
var index: array of integer;

    procedure AddElements(dim: integer; indent:string = ' ');
    var i: integer;
        recurse: boolean;
    begin
      SetLength(index,dim+1);
      recurse := dim < ar.GetDimensions - 1;
      with ar.GetBounds[dim] do
      for i := LowerBound to UpperBound do
      begin
        index[dim] := i;
        if recurse then
        begin
          Data.Add(Format('%s<elt id="%d">',[indent,i]));
          AddElements(dim+1,indent + ' ');
          Data.Add('</elt>');
        end
        else
          Data.Add(Format('%s<elt id="%d">%s</elt>',[indent,i,ar.GetAsString(index)]));
      end;
    end;

var
    s: string;
begin
  s := Format('<array dim = "%d" sqltype = "%d" length = "%d"',
                              [ar.GetDimensions,ar.GetSQLType,ar.GetSize]);
  case ar.GetSQLType of
  SQL_DOUBLE, SQL_FLOAT, SQL_LONG, SQL_SHORT, SQL_D_FLOAT, SQL_INT64:
     s += Format('" scale = "%d"',[ ar.GetScale]);
  SQL_TEXT,
  SQL_VARYING:
    s += Format(' charset = "%s"',[FirebirdAPI.GetCharsetName(ar.GetCharSetID)]);
  end;
  s += '>';
  Data.Add(s);

  SetLength(index,0);
  AddElements(0);
  Data.Add('</array>');
end;

procedure TIBInsertStmtsOut.HeaderOut(var Data: TStrings);
var TableName: string;
    i,j: integer;
begin
  TableName := trim(FIBSQL.GetUniqueRelationName);
  if TableName = '' then
    IBError(ibxeUniqueRelationReqd,[nil]);

  Data.Add('');
  Data.Add('/* Inserting data into Table: ' + TableName + ' */');
  Data.Add('');

  FInsertHeader := 'INSERT INTO ' + QuoteIdentifier(Database.SQLDialect, TableName) + ' (';
  with FIBSQL do
  begin
    j := 0;
    for i := 0 to MetaData.Count - 1 do
    if IncludeBlobsAndArrays or
       ((MetaData[i].SQLTYPE <> SQL_BLOB) and (MetaData[i].SQLType <> SQL_ARRAY)) then
    begin
      if j <> 0 then FInsertHeader += ',';
      FInsertHeader += QuoteIdentifierIfNeeded(Database.SQLDialect,MetaData[i].getAliasName);
      Inc(j);
    end;
  end;
  FInsertHeader += ') VALUES(';
end;

procedure TIBInsertStmtsOut.FormattedDataOut(var Data: TStrings);
const
  QuoteChar = '''';

var s: string;
    i, j: integer;
    ar: IArray;
begin
  s := FInsertHeader;
  with FIBSQL do
  begin
    j := 0;
    for i := 0 to Current.Count - 1 do
    if IncludeBlobsAndArrays or
       ((Current[i].SQLTYPE <> SQL_BLOB) and (Current[i].SQLType <> SQL_ARRAY)) then
    begin
      if j <> 0 then s += ',';
      if Current[i].IsNull then
        s += 'NULL'
      else
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
          ar := Current[i].AsArray;
          if ar = nil then
           s += 'NULL'
          else
          begin
            writeln('Col = ',ar.GetColumnName);
            Data.Add(s);
            s := '';
            FormatArray(ar,Data);
          end;
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
        s += sBlob
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

procedure TIBCustomDataOutput.TrailerOut(var Data: TStrings);
begin
  //stub
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

