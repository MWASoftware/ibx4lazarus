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

  sTimeStampFormat = 'yyyy.mm.dd hh:nn:ss.zzz';
  sDateFormat      = 'yyyy.mm.dd';
  sTimeFormat      =  'hh:nn:ss.zzz';

type
  TPlanOptions = (poNoPlan,poIncludePlan, poPlanOnly);

  TAdd2Log = procedure(const Msg: string; IsError: boolean=true) of object;

  { TIBCustomDataOutput }

  TIBCustomDataOutput = class(TComponent)
  private
    FIBSQL: TIBSQL;
    FIncludeHeader: Boolean;
    FPlanOptions: TPlanOptions;
    FRowCount: integer;
    FShowPerformanceStats: boolean;
    function GetDatabase: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetTransaction(AValue: TIBTransaction);
  protected
    procedure HeaderOut(Add2Log: TAdd2Log); virtual;
    procedure FormattedDataOut(Add2Log: TAdd2Log); virtual; abstract;
    procedure TrailerOut(Add2Log: TAdd2Log); virtual;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default true;
  public
    constructor Create(aOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DataOut(SelectQuery: string; Add2Log: TAdd2Log);
    procedure SetCommand(command, aValue, stmt: string; var Done: boolean);
    class procedure ShowPerfStats(Statement: IStatement; Add2Log: TAdd2Log);
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
    property PlanOptions: TPlanOptions read FPlanOptions write FPlanOptions;
    property RowCount: integer read FRowCount write FRowCount;
    property ShowPerformanceStats: boolean read FShowPerformanceStats write FShowPerformanceStats;
 end;

  TDataOutputFormatter = class of TIBCustomDataOutput;

  { TIBCSVDataOut }

  TIBCSVDataOut = class(TIBCustomDataOutput)
  private
    FQuoteChar: char;
  protected
    procedure HeaderOut(Add2Log: TAdd2Log); override;
    procedure FormattedDataOut(Add2Log: TAdd2Log); override;
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
    procedure FormatBlob(Field: ISQLData; Add2Log: TAdd2Log);
    procedure FormatArray(ar: IArray; Add2Log: TAdd2Log);
  protected
    procedure HeaderOut(Add2Log: TAdd2Log); override;
    procedure FormattedDataOut(Add2Log: TAdd2Log); override;
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
    procedure HeaderOut(Add2Log: TAdd2Log); override;
    procedure FormattedDataOut(Add2Log: TAdd2Log); override;
    procedure TrailerOut(Add2Log: TAdd2Log); override;
  published
    property IncludeHeader;
  end;

implementation

uses IBUtils, FBMessages, Math, IBXScript;

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
    Move(s[1],Result[(ColWidth - Length(s)) div 2 + 1],Length(s));
  taRight:
    Move(s[1],Result[ColWidth - Length(s) + 1],Length(s));
  end;
end;

procedure TIBBlockFormatOut.HeaderOut(Add2Log: TAdd2Log);
var i: integer;
    s: string;
begin
  with FIBSQL do
  begin
    {Calculate column Widths}
    SetLength(FColWidths,MetaData.Count);
    FRowWidth := 1; {assume leading '|'}
    for i := 0 to MetaData.Count - 1 do
    with MetaData[i] do
    begin
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
        FColWidths[i] := 23;

      SQL_TYPE_DATE:
        FColWidths[i] := 10;

      SQL_TYPE_TIME:
        FColWidths[i] := 12;

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

    Add2Log(DashedLine);
    s := '|';
    for i := 0 to MetaData.Count - 1 do
      s += TextAlign(MetaData[i].Name,FColWidths[i],taCentre) + '|';
    Add2Log(s);
    Add2Log(DashedLine);
  end;
end;

procedure TIBBlockFormatOut.FormattedDataOut(Add2Log: TAdd2Log);

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
  begin
    if IsNull then
      s += TextAlign('NULL',FColWidths[i],taCentre)
    else
    case SQLType of
    SQL_VARYING, SQL_TEXT:
      s += TextAlign(AsString,FColWidths[i],taLeft);

    SQL_TIMESTAMP:
      s += TextAlign(FormatDateTime(sTimeStampFormat,AsDateTime),FColWidths[i],taLeft);

    SQL_TYPE_DATE:
      s += TextAlign(FormatDateTime(sDateFormat,AsDateTime),FColWidths[i],taLeft);

    SQL_TYPE_TIME:
      s += TextAlign(FormatDateTime(sTimeFormat,AsDateTime),FColWidths[i],taLeft);

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
  end;
  Add2Log(s);
  Add2Log(DashedLine);
end;

procedure TIBBlockFormatOut.TrailerOut(Add2Log: TAdd2Log);
begin
  Add2Log(DashedLine);
end;

{ TIBInsertStmtsOut }

procedure TIBInsertStmtsOut.FormatBlob(Field: ISQLData; Add2Log: TAdd2Log);

  function ToHex(aValue: byte): string;
  const
    HexChars: array [0..15] of char = '0123456789ABCDEF';
  begin
    Result := HexChars[aValue shr 4] +
               HexChars[(aValue and $0F)];
  end;

var blob: string;
    i, j: integer;
    s: string;
begin
  Add2Log(Format('<binary subtype="%d">',[Field.getSubtype]));
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
    Add2Log(s);
  end;
  Add2Log('</binary>');
end;

procedure TIBInsertStmtsOut.FormatArray(ar: IArray; Add2Log: TAdd2Log);
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
          Add2Log(Format('%s<elt id="%d">',[indent,i]));
          AddElements(dim+1,indent + ' ');
          Add2Log('</elt>');
        end
        else
          Add2Log(Format('%s<elt ix="%d">%s</elt>',[indent,i,ar.GetAsString(index)]));
      end;
    end;

var
    s: string;
    bounds: TArrayBounds;
    i: integer;
    boundsList: string;
begin
  s := Format('<array dim = "%d" sqltype = "%d" length = "%d"',
                              [ar.GetDimensions,ar.GetSQLType,ar.GetSize]);
  case ar.GetSQLType of
  SQL_DOUBLE, SQL_FLOAT, SQL_LONG, SQL_SHORT, SQL_D_FLOAT, SQL_INT64:
     s += Format(' scale = "%d"',[ ar.GetScale]);
  SQL_TEXT,
  SQL_VARYING:
    s += Format(' charset = "%s"',[FirebirdAPI.GetCharsetName(ar.GetCharSetID)]);
  end;
  bounds := ar.GetBounds;
  boundsList := '';
  for i := 0 to length(bounds) - 1 do
  begin
    if i <> 0 then boundsList += ',';
    boundsList += Format('%d:%d',[bounds[i].LowerBound,bounds[i].UpperBound]);
  end;
  s += Format(' bounds="%s"',[boundsList]);
  s += '>';
  Add2Log(s);

  SetLength(index,0);
  AddElements(0);
  Add2Log('</array>');
end;

procedure TIBInsertStmtsOut.HeaderOut(Add2Log: TAdd2Log);
var TableName: string;
    i,j: integer;
begin
  TableName := trim(FIBSQL.GetUniqueRelationName);
  if TableName = '' then
    IBError(ibxeUniqueRelationReqd,[nil]);

  Add2Log('');
  Add2Log('/* Inserting data into Table: ' + TableName + ' */');
  Add2Log('');

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

procedure TIBInsertStmtsOut.FormattedDataOut(Add2Log: TAdd2Log);
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
          s += QuoteChar + SQLSafeString(Current[i].AsString) + QuoteChar
        else
        begin
          Add2Log(s);
          s := '';
          FormatBlob(Current[i],Add2Log);
        end;

      SQL_ARRAY:
        begin
          ar := Current[i].AsArray;
          if ar = nil then
           s += 'NULL'
          else
          begin
            Add2Log(s);
            s := '';
            FormatArray(ar,Add2Log);
          end;
        end;

      SQL_TEXT, SQL_VARYING:
        s += QuoteChar + SQLSafeString(Current[i].AsString) + QuoteChar;

      SQL_TIMESTAMP:
        s += QuoteChar + FormatDateTime(sTimeStampFormat,Current[i].AsDateTime) + QuoteChar;

      SQL_TYPE_DATE:
        s += QuoteChar + FormatDateTime(sDateFormat,Current[i].AsDateTime) + QuoteChar;

      SQL_TYPE_TIME:
        s += QuoteChar + FormatDateTime(sTimeFormat,Current[i].AsDateTime) + QuoteChar;

      else
        s += Current[i].AsString;
      end;
      Inc(j);
    end;
  end;
  s += ');';
  Add2Log(s);
end;

constructor TIBInsertStmtsOut.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIncludeBlobsAndArrays := true;
end;

{ TIBCSVDataOut }

procedure TIBCSVDataOut.HeaderOut(Add2Log: TAdd2Log);
var i: integer;
    s: string;
begin
  s := '';
  for i := 0 to FIBSQL.MetaData.Count - 1 do
  begin
    if i <> 0 then s += ',';
    s += FIBSQL.MetaData[i].getAliasName;
  end;
  Add2Log(s);
end;

procedure TIBCSVDataOut.FormattedDataOut(Add2Log: TAdd2Log);
var i: integer;
    s: string;
begin
  s := '';
  with FIBSQL do
  begin
    for i := 0 to Current.Count - 1 do
    with Current[i] do
    begin
      if i <> 0 then s += ',';
      case SQLType of
      SQL_BLOB:
        if SQLSubType <> 1 then
          s += sBlob
        else
          s += QuoteChar + Current[i].AsString + QuoteChar;

      SQL_VARYING,SQL_TEXT,
      SQL_TIMESTAMP,SQL_TYPE_DATE,SQL_TYPE_TIME:
        s += QuoteChar + Current[i].AsString + QuoteChar;

      else
        s += Current[i].AsString;
      end;
    end;
  end;
  Add2Log(s);
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

procedure TIBCustomDataOutput.HeaderOut(Add2Log: TAdd2Log);
begin
  //stub
end;

procedure TIBCustomDataOutput.TrailerOut(Add2Log: TAdd2Log);
begin
  //stub
end;

constructor TIBCustomDataOutput.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIBSQL := TIBSQL.Create(self);
  FIncludeHeader := true;
end;

procedure TIBCustomDataOutput.Assign(Source: TPersistent);
begin
  if Source is TIBCustomDataOutput then
  begin
    IncludeHeader := TIBCustomDataOutput(Source).IncludeHeader;
    RowCount := TIBCustomDataOutput(Source).RowCount;
    ShowPerformanceStats := TIBCustomDataOutput(Source).ShowPerformanceStats;
    PlanOptions := TIBCustomDataOutput(Source).PlanOptions;
  end;
end;

procedure TIBCustomDataOutput.DataOut(SelectQuery: string; Add2Log: TAdd2Log);
var Count: integer;
begin
  FIBSQL.SQL.Text := SelectQuery;
  FIBSQL.Prepare;
  FIBSQL.Statement.EnableStatistics(ShowPerformanceStats);
  if PlanOptions <> poNoPlan then
    Add2Log(FIBSQL.Plan);
  if PlanOptions = poPlanOnly then
    Exit;

  if IncludeHeader then
    HeaderOut(Add2Log);
  Count := 0;
  FIBSQL.ExecQuery;
  try
    while (not FIBSQL.EOF) and ((FRowCount = 0) or (Count < FRowCount)) do
    begin
      FormattedDataOut(Add2Log);
      FIBSQL.Next;
      Inc(Count);
    end;
    ShowPerfStats(FIBSQL.Statement,Add2Log);
  finally
    FIBSQL.Close;
  end;
end;

procedure TIBCustomDataOutput.SetCommand(command, aValue, stmt: string;
  var Done: boolean);

  function Toggle(aValue: string): boolean;
  begin
    aValue := AnsiUpperCase(aValue);
    if aValue = 'ON' then
      Result := true
    else
    if aValue = 'OFF' then
      Result := false
    else
      raise Exception.CreateFmt(sInvalidSetStatement, [command,stmt]);
  end;

begin
  done := true;
  if command = 'HEADING' then
    FIncludeHeader := ((aValue = '') and not FIncludeHeader) or
                      ((aValue <> '') and Toggle(aValue))
  else
  if command = 'ROWCOUNT' then
    FRowCount := StrToInt(aValue)
  else
  if command = 'PLAN' then
  begin
    if aValue = '' then
    begin
      if FPlanOptions <>  poIncludePlan then
        FPlanOptions := poIncludePlan
      else
        FPlanOptions := poNoPlan;
    end
    else
    if Toggle(aValue) then
      FPlanOptions := poIncludePlan
    else
      FPlanOptions := poNoPlan;
  end
  else
  if command = 'PLANONLY' then
  begin
    if aValue = '' then
    begin
      if FPlanOptions <>  poPlanOnly then
        FPlanOptions := poPlanOnly
      else
        FPlanOptions := poNoPlan;
    end
    else
    if Toggle(aValue) then
      FPlanOptions := poPlanOnly
    else
    if FPlanOptions <> poIncludePlan then
      FPlanOptions := poNoPlan;
  end
  else
    done := false;
end;

class procedure TIBCustomDataOutput.ShowPerfStats(Statement: IStatement;
  Add2Log: TAdd2Log);
var stats: TPerfCounters;
begin
  if Statement.GetPerfStatistics(stats) then
  begin
    Add2Log(Format('Current memory = %d',[stats[psCurrentMemory]]));
    Add2Log(Format('Delta memory = %d',[stats[psDeltaMemory]]));
    Add2Log(Format('Max memory = %d',[stats[psMaxMemory]]));
    Add2Log('Elapsed time= ' + FormatFloat('#0.000',stats[psRealTime]/1000) +' sec');
    Add2Log('Cpu = ' + FormatFloat('#0.000',stats[psUserTime]/1000) + ' sec');
    Add2Log(Format('Buffers = %d',[stats[psBuffers]]));
    Add2Log(Format('Reads = %d',[stats[psReads]]));
    Add2Log(Format('Writes = %d',[stats[psWrites]]));
    Add2Log(Format('Fetches = %d',[stats[psFetches]]));
 end;
end;

end.

