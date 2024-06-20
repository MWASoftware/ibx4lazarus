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
 *  The Original Code is (C) 2017 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBDataOutput;

{$mode objfpc}{$H+}
{$codepage UTF8}

(*
     These are helper components, primarily for use with TIBXScript, but which are
     also used by TIBExtact (for formatting data as SQL Insert statements). Their
     purpose is to execute SQL SELECT statements and to format the results of the
     query. Data Output Formatters are currently available for:

     Block Format Output 	(TIBBlockFormatOut)
     CSV Format 	        (TIBCSVDataOut)
     SQL Insert Statements 	(TIBInsertStmtsOut).
  *)
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
  TOnFormatTextString = procedure(sender: TObject; var TextString: string) of object;

  { TIBCustomDataOutput }

  TIBCustomDataOutput = class(TComponent)
  private
    FDateFormat: string;
    FIBSQL: TIBSQL;
    FIncludeHeader: Boolean;
    FOnFormatTextString: TOnFormatTextString;
    FPlanOptions: TPlanOptions;
    FRowCount: integer;
    FShowPerformanceStats: boolean;
    FTimeFormat: string;
    FTimestampFormat: string;
    function GetDatabase: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetTransaction(AValue: TIBTransaction);
  protected
    procedure HeaderOut(Add2Log: TAdd2Log); virtual;
    procedure FormattedDataOut(Add2Log: TAdd2Log); virtual; abstract;
    procedure TrailerOut(Add2Log: TAdd2Log); virtual;
    function FormatTimestamp(aValue: ISQLData): string;
    function FormatDate(aValue: ISQLData): string;
    function FormatTime(aValue: ISQLData): string;
    function FormatTextString(aValue: string): string;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default true;
    property OnFormatTextString: TOnFormatTextString read FOnFormatTextString write FOnFormatTextString;
  public
    constructor Create(aOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function DataOut(SelectQuery: string; Add2Log: TAdd2Log): boolean;
    procedure SetCommand(command, aValue, stmt: string; var Done: boolean); virtual;
    class procedure ShowPerfStats(Statement: IStatement; Add2Log: TAdd2Log); overload;
    class procedure ShowPerfStats(stats: TPerfCounters; Add2Log: TAdd2Log); overload;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
    property PlanOptions: TPlanOptions read FPlanOptions write FPlanOptions;
    property RowCount: integer read FRowCount write FRowCount;
    property ShowPerformanceStats: boolean read FShowPerformanceStats write FShowPerformanceStats;
    property TimestampFormat: string read FTimestampFormat write FTimestampFormat;
    property DateFormat: string read FDateFormat write FDateFormat;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
 end;

  TDataOutputFormatter = class of TIBCustomDataOutput;

  { TIBCSVDataOut }

  TIBCSVDataOut = class(TIBCustomDataOutput)
  private
    FFieldSeparator: string;
    FHeaderSeparator: string;
    FQuoteChar: char;
    FQuoteStrings: boolean;
  protected
    procedure HeaderOut(Add2Log: TAdd2Log); override;
    procedure FormattedDataOut(Add2Log: TAdd2Log); override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property IncludeHeader;
    property FieldSeparator: string read FFieldSeparator write FFieldSeparator;
    property HeaderSeparator: string read FHeaderSeparator write FHeaderSeparator;
    property QuoteStrings: boolean read FQuoteStrings write FQuoteStrings default true;
    property QuoteChar: char read FQuoteChar write FQuoteChar default '''';
    property OnFormatTextString;
  end;

  { TIBInsertStmtsOut }

  TIBInsertStmtsOut = class(TIBCustomDataOutput)
  private
    FIncludeBlobsAndArrays: boolean;
    FInsertHeader: string;
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
    property OnFormatTextString;
  end;

implementation

uses IBUtils, IBMessages, Math, IBXScript;

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
        if TimestampFormat = '' then  {Default format}
          FColWidths[i] := GetDateTimeStrLength(dfTimestamp)
        else
          FColWidths[i] := Length(TimestampFormat);

      SQL_TYPE_DATE:
        if DateFormat = '' then {Default format}
          FColWidths[i] := GetDateTimeStrLength(dfDateTime)
        else
          FColWidths[i] := Length(DateFormat);

      SQL_TYPE_TIME:
        if TimeFormat = '' then {Default format}
          FColWidths[i] := GetDateTimeStrLength(dfTime)
        else
          FColWidths[i] := Length(TimeFormat);

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

    Add2Log(DashedLine,false);
    s := '|';
    for i := 0 to MetaData.Count - 1 do
      s += TextAlign(MetaData[i].Name,FColWidths[i],taCentre) + '|';
    Add2Log(s,false);
    Add2Log(DashedLine,false);
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
  with FIBSQL do
  begin
    if Current[i].IsNull then
      s += TextAlign('NULL',FColWidths[i],taCentre)
    else
    case Current[i].SQLType of
    SQL_VARYING, SQL_TEXT:
      s += TextAlign(FormatTextString(Current[i].AsString),FColWidths[i],taLeft);

    SQL_TIMESTAMP:
      s += TextAlign(FormatTimeStamp(Current[i]),FColWidths[i],taLeft);

    SQL_TYPE_DATE:
      s += TextAlign(FormatDate(Current[i]),FColWidths[i],taLeft);

    SQL_TYPE_TIME:
      s += TextAlign(FormatTime(Current[i]),FColWidths[i],taLeft);

    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT,
    SQL_LONG, SQL_SHORT, SQL_INT64:
      s += TextAlign(Current[i].AsString,FColWidths[i],taRight);

    SQL_BOOLEAN, SQL_ARRAY:
      s += TextAlign(Current[i].AsString,FColWidths[i],taCentre);

    SQL_BLOB:
      if Current[i].SQLSubType = 1 then
        s += TextAlign(TruncateTextBlob(Current[i].AsString),FColWidths[i],taLeft)
      else
        s += TextAlign(sBlob,FColWidths[i],taCentre);
    end;
    s += '|';
  end;
  Add2Log(s,false);
  Add2Log(DashedLine,false);
end;

procedure TIBBlockFormatOut.TrailerOut(Add2Log: TAdd2Log);
begin
  Add2Log(DashedLine,false);
end;

{ TIBInsertStmtsOut }

procedure TIBInsertStmtsOut.HeaderOut(Add2Log: TAdd2Log);
var TableName: string;
    i,j: integer;
begin
  TableName := trim(FIBSQL.GetUniqueRelationName);
  if TableName = '' then
    IBError(ibxeUniqueRelationReqd,[nil]);

  Add2Log('',false);
  Add2Log('/* Inserting data into Table: ' + TableName + ' */',false);
  Add2Log('',false);

  FInsertHeader := 'INSERT INTO ' + QuoteIdentifierIfNeeded(Database.SQLDialect, TableName) + ' (';
  with FIBSQL do
  begin
    j := 0;
    for i := 0 to MetaData.Count - 1 do
    if IncludeBlobsAndArrays or
       ((MetaData[i].SQLTYPE <> SQL_BLOB) and (MetaData[i].SQLType <> SQL_ARRAY)) then
    begin
      if j <> 0 then FInsertHeader += ',';
      FInsertHeader += QuoteIdentifierIfNeeded(Database.SQLDialect,Trim(MetaData[i].getAliasName));
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
        begin
          if Current[i].getCharSetID = 1 {octets} then
            s += Format('x''%s''',[StringToHex(Current[i].AsString)])
          else
            s += QuoteChar + SQLSafeString(Current[i].AsString) + QuoteChar
        end
        else
          s += TSQLXMLReader.FormatBlob(Current[i]);

      SQL_ARRAY:
        begin
          ar := Current[i].AsArray;
          if ar = nil then
            s += 'NULL'
          else
            s += TSQLXMLReader.FormatArray(ar);
        end;

      SQL_TEXT, SQL_VARYING:
        if Current[i].getCharSetID = 1 {octets} then
          s += Format('x''%s''',[StringToHex(Current[i].AsString)])
        else
          s += QuoteChar + SQLSafeString(Current[i].AsString) + QuoteChar;

      SQL_TIMESTAMP:
        s += QuoteChar + FormatTimeStamp(Current[i]) + QuoteChar;

      SQL_TYPE_DATE:
        s += QuoteChar + FormatDate(Current[i]) + QuoteChar;

      SQL_TYPE_TIME:
        s += QuoteChar + FormatTime(Current[i]) + QuoteChar;

      else
        s += Current[i].AsString;
      end;
      Inc(j);
    end;
  end;
  s += ');';
  Add2Log(s,false);
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
    if i <> 0 then s += HeaderSeparator;
    s += FIBSQL.MetaData[i].getAliasName;
  end;
  Add2Log(s,false);
end;

procedure TIBCSVDataOut.FormattedDataOut(Add2Log: TAdd2Log);

  function GetQuoteChar: string;
  begin
    if QuoteStrings then
      Result := QuoteChar
    else
      Result := '';
  end;

var i: integer;
    s: string;
begin
  s := '';
  with FIBSQL do
  begin
    for i := 0 to Current.Count - 1 do
    with Current[i] do
    begin
      if i <> 0 then s += FieldSeparator;
      case SQLType of
      SQL_BLOB:
        if SQLSubType <> 1 then
          s += sBlob
        else
          s += GetQuoteChar + Current[i].AsString + GetQuoteChar;

      SQL_VARYING,SQL_TEXT:
        s += GetQuoteChar + FormatTextString(Current[i].AsString) + GetQuoteChar;

      SQL_TIMESTAMP:
        s += GetQuoteChar + FormatTimeStamp(Current[i]) + GetQuoteChar;

      SQL_TYPE_DATE:
        s += GetQuoteChar + FormatDate(Current[i]) + GetQuoteChar;

      SQL_TYPE_TIME:
        s += GetQuoteChar + FormatTime(Current[i]) + GetQuoteChar;

      else
        s += Current[i].AsString;
      end;
    end;
  end;
  Add2Log(s,false);
end;

constructor TIBCSVDataOut.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuoteStrings := true;
  FQuoteChar := '''';
  FTimestampFormat := '';
  FDateFormat := '';
  FTimestampFormat := '';
  FFieldSeparator := ',';
  FHeaderSeparator := ',';
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

function TIBCustomDataOutput.FormatTimestamp(aValue: ISQLData): string;
begin
  if TimeStampFormat <> '' then
    Result := FormatDateTime(TimeStampFormat,aValue.AsDateTime)
  else
    Result := aValue.AsString;
end;

function TIBCustomDataOutput.FormatDate(aValue: ISQLData): string;
begin
  if DateFormat <> '' then
    Result := FormatDateTime(DateFormat,aValue.AsDateTime)
  else
    Result := aValue.AsString;
end;

function TIBCustomDataOutput.FormatTime(aValue: ISQLData): string;
begin
  if TimeFormat <> '' then
    Result := FormatDateTime(TimeFormat,aValue.AsDateTime)
  else
    Result := aValue.AsString;
end;

function TIBCustomDataOutput.FormatTextString(aValue: string): string;
begin
  Result := aValue;
  if assigned(FOnFormatTextString) then
    OnFormatTextString(self,Result);
end;

constructor TIBCustomDataOutput.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIBSQL := TIBSQL.Create(self);
  FIncludeHeader := true;
  FTimestampFormat := sTimestampFormat;
  FDateFormat := sDateFormat;
  FTimeFormat := sTimeFormat;
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

function TIBCustomDataOutput.DataOut(SelectQuery: string; Add2Log: TAdd2Log
  ): boolean;
var Count: integer;
begin
  FIBSQL.SQL.Text := SelectQuery;
  FIBSQL.Prepare;
  FIBSQL.Statement.EnableStatistics(ShowPerformanceStats);
  if PlanOptions <> poNoPlan then
    Add2Log(FIBSQL.Plan,false);
  if PlanOptions = poPlanOnly then
    Exit;

  Count := 0;
  FIBSQL.ExecQuery;
  try
    if IncludeHeader and not FIBSQL.EOF then
      HeaderOut(Add2Log);
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
  Result := Count > 0;
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
var stats : TPerfCounters;
begin
  if Statement.GetPerfStatistics(stats) then
    ShowPerfStats(stats,Add2Log);
end;

class procedure TIBCustomDataOutput.ShowPerfStats(stats: TPerfCounters;
  Add2Log : TAdd2Log);
var LargeCompFormat: string;
    ThreeSigPlacesFormat: string;
begin
  LargeCompFormat := '#' + DefaultFormatSettings.ThousandSeparator + '##0';
  ThreeSigPlacesFormat := '#0' + DefaultFormatSettings.DecimalSeparator + '000';
  Add2Log('Current memory = ' + FormatFloat(LargeCompFormat,stats[psCurrentMemory]));
  Add2Log('Delta memory = ' + FormatFloat(LargeCompFormat,stats[psDeltaMemory]));
  Add2Log('Max memory = ' + FormatFloat(LargeCompFormat,stats[psMaxMemory]));
  Add2Log('Elapsed time= ' + FormatFloat(ThreeSigPlacesFormat,stats[psRealTime]/1000) +' sec');
  Add2Log('Cpu = ' + FormatFloat(ThreeSigPlacesFormat,stats[psUserTime]/1000) + ' clock ticks');
  Add2Log('Buffers = ' + FormatFloat('#0',stats[psBuffers]));
  Add2Log('Reads = ' + FormatFloat('#0',stats[psReads]));
  Add2Log('Writes = ' + FormatFloat('#0',stats[psWrites]));
  Add2Log('Fetches = ' + FormatFloat('#0',stats[psFetches]));
end;

end.

