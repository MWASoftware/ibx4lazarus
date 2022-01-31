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
 *  The Original Code is (C) 2015-2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit IBUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBCustomDataSet, DB, IB, IBDatabase, IBExternals, IBMessages;

type

  TOnApplyUpdates = procedure(Sender: TObject; UpdateKind: TUpdateKind; Params: ISQLParams) of object;

  { TIBUpdate}

  TIBUpdate = class(TIBDataSetUpdateObject)
  private
    FDataSet: TIBCustomDataSet;
    FDummySQL: TStrings;
    FOnApplyUpdates: TOnApplyUpdates;
  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetDataSet: TIBCustomDataSet; override;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); override;
    procedure Apply(UpdateKind: TUpdateKind; buff: PChar); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataSet;
  published
    property OnApplyUpdates: TOnApplyUpdates read FOnApplyUpdates write FOnApplyUpdates;
 end;


implementation

uses variants, FmtBCD, DateUtils, FBNumeric;

type

  { TParamListIntf }

  TParamListIntf = class(TInterfacedObject,ISQLParams)
  private
    type TParamRec = record
      Name: string;
      Value: variant;
      Modified: boolean;
      TimeZoneID: TFBTimeZoneID;
      DataSet: TDataSet;
    end;
  private
    FDatabase: TIBDatabase;
    FModified: boolean;
    FParams: array of TParamRec;
    procedure SetParam(index: integer; aValue: variant);
    procedure SetTimeZoneID(index: integer; aValue: TFBTimeZoneID);
  public
    constructor Create(aFields: TFields; aDatabase: TIBDatabase);
    destructor Destroy; override;
    property Database: TIBDatabase read FDatabase;
  public
    {ISQLParams}
    function getCount: integer;
    function getSQLParam(index: integer): ISQLParam;
    function ParamExists(Idx: AnsiString): boolean;
    function ByName(Idx: AnsiString): ISQLParam ;
    function GetModified: Boolean;
    function GetHasCaseSensitiveParams: Boolean;
    function GetStatement: IStatement;
    function GetTransaction: ITransaction;
    function GetAttachment: IAttachment;
    procedure Clear;
  end;

  { TParamIntf }

  TParamIntf = class(TInterfacedObject,ISQLParam)
  private
    FIndex: integer;
    FOwner: TParamListIntf;
    function GetDataSet: TDataSet;
  public
    constructor Create(aOwner: TParamListIntf; aIndex: integer);
    function getColMetadata: IParamMetaData;
    function GetIndex: integer;
    function GetSQLType: cardinal;
    function GetSQLTypeName: AnsiString;
    function getSubtype: integer;
    function getName: AnsiString;
    function getScale: integer;
    function getCharSetID: cardinal;
    function getCodePage: TSystemCodePage;
    function getIsNullable: boolean;
    function GetSize: cardinal;
    function GetAsBoolean: boolean;
    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime; overload;
    procedure GetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID); overload;
    procedure GetAsDateTime(var aDateTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString); overload;
    procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime); overload;
    procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString; OnDate: TDateTime); overload;
    procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID); overload;
    procedure GetAsTime(var aTime: TDateTime; var dstOffset: smallint; var aTimezone: AnsiString); overload;
    function GetAsUTCDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Float;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: short;
    function GetAsString: AnsiString;
    function GetIsNull: boolean;
    function GetAsVariant: Variant;
    function GetAsBlob: IBlob;
    function GetAsArray: IArray;
    function GetAsBCD: tBCD;
    function GetAsNumeric: IFBNumeric;
    function GetStatement: IStatement;
    function GetTransaction: ITransaction;
    function GetAttachment: IAttachment;
    procedure Clear;
    function GetModified: boolean;
    procedure SetAsBoolean(AValue: boolean);
    procedure SetAsCurrency(aValue: Currency);
    procedure SetAsInt64(aValue: Int64);
    procedure SetAsDate(aValue: TDateTime);
    procedure SetAsLong(aValue: Long);
    procedure SetAsTime(aValue: TDateTime); overload;
    procedure SetAsTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsTime(aValue: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsTime(aValue: TDateTime; OnDate: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsDateTime(aValue: TDateTime); overload;
    procedure SetAsDateTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
    procedure SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString); overload;
    procedure SetAsUTCDateTime(aUTCTime: TDateTime);
    procedure SetAsDouble(aValue: Double);
    procedure SetAsFloat(aValue: Float);
    procedure SetAsPointer(aValue: Pointer);
    procedure SetAsShort(aValue: Short);
    procedure SetAsString(aValue: AnsiString);
    procedure SetAsVariant(aValue: Variant);
    procedure SetIsNull(aValue: Boolean);
    procedure SetAsBlob(aValue: IBlob);
    procedure SetAsArray(anArray: IArray);
    procedure SetAsQuad(aValue: TISC_QUAD);
    procedure SetCharSetID(aValue: cardinal);
    procedure SetAsBcd(aValue: tBCD);
    procedure SetAsNumeric(Value: IFBNumeric);
  end;

{ TParamIntf }

function TParamIntf.GetDataSet: TDataSet;
begin
  Result := FOwner.FParams[FIndex].DataSet;
end;

constructor TParamIntf.Create(aOwner: TParamListIntf; aIndex: integer);
begin
  FOwner := aOwner;
  FIndex := aIndex;
end;

function TParamIntf.getColMetadata: IParamMetaData;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetIndex: integer;
begin
  Result := Findex;
end;

function TParamIntf.GetSQLType: cardinal;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetSQLTypeName: AnsiString;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.getSubtype: integer;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.getName: AnsiString;
begin
  Result := FOwner.FParams[FIndex].Name;
end;

function TParamIntf.getScale: integer;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.getCharSetID: cardinal;
var id: integer;
begin
   FOwner.Database.Attachment.CodePage2CharSetID(StringCodePage(FOwner.FParams[FIndex].Value),id);
   Result := id;
end;

function TParamIntf.getCodePage: TSystemCodePage;
begin
  Result := StringCodePage(FOwner.FParams[FIndex].Value);
end;

function TParamIntf.getIsNullable: boolean;
begin
  Result := true;
end;

function TParamIntf.GetSize: cardinal;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAsBoolean: boolean;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsCurrency: Currency;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsInt64: Int64;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsDateTime: TDateTime;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

procedure TParamIntf.GetAsDateTime(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimezoneID: TFBTimeZoneID);
begin
  with FOwner.FParams[FIndex] do
  if VarIsArray(Value) then
  begin
    aDateTime := Value[0];
    dstOffset := Value[1];
    if VarType(Value[2]) in [varSmallint, varInteger, varByte, varWord, varShortInt] then
      aTimezoneID := Value[2]
    else
      aTimeZoneID := FOwner.DataBase.attachment.GetTimeZoneServices.TimeZoneName2TimeZoneID(Value[2]);
  end
  else
  begin
    aDateTime := FOwner.FParams[FIndex].Value;
    dstOffset := 0;
    aTimeZoneID := TimeZoneID_GMT;
  end;
end;

procedure TParamIntf.GetAsDateTime(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimezone: AnsiString);
begin
  with FOwner.FParams[FIndex] do
  if VarIsArray(Value) then
  begin
    aDateTime := Value[0];
    dstOffset := Value[1];
    if VarType(Value[2]) in [varSmallint, varInteger, varByte, varWord, varShortInt] then
      aTimeZone := FOwner.DataBase.attachment.GetTimeZoneServices.TimeZoneID2TimeZoneName(Value[2])
    else
      aTimezone := Value[2];
  end
  else
  begin
    aDateTime := FOwner.FParams[FIndex].Value;
    dstOffset := 0;
    aTimeZone := 'GMT';
  end;
end;

procedure TParamIntf.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID; OnDate: TDateTime);
var LocalTime: TDateTime;
begin
  with FOwner.FParams[FIndex] do
  if VarIsArray(Value) then
  begin
    LocalTime := OnDate + TimeOf(Value[0]);
    dstOffset := Value[1];
    if VarType(Value[2]) in [varSmallint, varInteger, varByte, varWord, varShortInt] then
      aTimezoneID := Value[2]
    else
      aTimeZoneID := FOwner.DataBase.attachment.GetTimeZoneServices.TimeZoneName2TimeZoneID(Value[2]);
    aTime := TimeOf(FOwner.DataBase.attachment.GetTimeZoneServices.GMTToLocalTime(IncMinute(LocalTime,-dstOffset),aTimeZoneID))
  end
  else
  begin
    aTime := FOwner.FParams[FIndex].Value;
    dstOffset := 0;
    aTimeZoneID := TimeZoneID_GMT;
  end;
end;

procedure TParamIntf.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezone: AnsiString; OnDate: TDateTime);
var LocalTime: TDateTime;
begin
  with FOwner.FParams[FIndex] do
  if VarIsArray(Value) then
  begin
    LocalTime := OnDate + TimeOf(Value[0]);
    dstOffset := Value[1];
    if VarType(Value[2]) in [varSmallint, varInteger, varByte, varWord, varShortInt] then
      aTimeZone := FOwner.DataBase.attachment.GetTimeZoneServices.TimeZoneID2TimeZoneName(Value[2])
    else
      aTimezone := Value[2];
    aTime := TimeOf(FOwner.DataBase.attachment.GetTimeZoneServices.GMTToLocalTime(IncMinute(LocalTime,-dstOffset),aTimeZone))
  end
  else
  begin
    aTime := FOwner.FParams[FIndex].Value;
    dstOffset := 0;
    aTimeZone := 'GMT';
  end;
end;

procedure TParamIntf.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezoneID: TFBTimeZoneID);
begin
  GetAsTime(aTime,dstOffset,aTimeZoneID,(GetDataSet as TIBCustomDataSet).DefaultTZDate);
end;

procedure TParamIntf.GetAsTime(var aTime: TDateTime; var dstOffset: smallint;
  var aTimezone: AnsiString);
begin
  GetAsTime(aTime,dstOffset,aTimeZone,(GetDataSet as TIBCustomDataSet).DefaultTZDate);
end;

function TParamIntf.GetAsUTCDateTime: TDateTime;
begin
  with FOwner.FParams[FIndex] do
  if VarIsArray(Value) then
    Result := IncMinute(Value[0],-Value[1])
  else
    Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsDouble: Double;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsFloat: Float;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsLong: Long;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsPointer: Pointer;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAsQuad: TISC_QUAD;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAsShort: short;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsString: AnsiString;
var v: variant;
begin
  v := FOwner.FParams[FIndex].Value;
  Case varType(v) of
      varEmpty,
      varNull:
          Result := '';
      varShortInt,
      varSmallint,
      varInteger,
      varInt64,
      varByte,
      varWord,
      varDecimal,
      varLongWord,
      varQWord,
      varSingle:
          Result := IntToStr(v);
      varCurrency,
      varDouble:
          Result := FloatToStr(v);
      varDate:
          Result := DateTimeToStr(v);
      varStrArg,
      varString:
          Result := v;
      varBoolean:
          if v then
            Result := 'true'
          else
            Result := 'false';
      varVariant:
          Result := v;
   else
     Result := v;
   end;
end;

function TParamIntf.GetIsNull: boolean;
begin
  Result := VarIsNull(FOwner.FParams[FIndex].Value);
end;

function TParamIntf.GetAsVariant: Variant;
begin
  Result := FOwner.FParams[FIndex].Value;
end;

function TParamIntf.GetAsBlob: IBlob;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAsArray: IArray;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAsBCD: tBCD;
begin
  Result := VarToBCD(FOwner.FParams[FIndex].Value);
end;

function TParamIntf.GetAsNumeric: IFBNumeric;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetStatement: IStatement;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetTransaction: ITransaction;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamIntf.GetAttachment: IAttachment;
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.Clear;
begin
  FOwner.SetParam(FIndex,NULL);
end;

function TParamIntf.GetModified: boolean;
begin
  Result := FOwner.FParams[FIndex].Modified;
end;

procedure TParamIntf.SetAsBoolean(AValue: boolean);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsCurrency(aValue: Currency);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsInt64(aValue: Int64);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsDate(aValue: TDateTime);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsLong(aValue: Long);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsTime(aValue: TDateTime);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID);
begin
  SetAsTime(aValue,(GetDataSet as TIBCustomDataSet).DefaultTZDate,aTimeZoneID);
end;

procedure TParamIntf.SetAsTime(aValue: TDateTime; aTimeZone: AnsiString);
begin
  SetAsTime(aValue,(GetDataSet as TIBCustomDataSet).DefaultTZDate,aTimeZone);
end;

procedure TParamIntf.SetAsTime(aValue: TDateTime; OnDate: TDateTime;
  aTimeZoneID: TFBTimeZoneID);
var dstOffset: smallint;
begin
  aValue := TimeOf(aValue);
  dstOffset := FOwner.Database.Attachment.GetTimeZoneServices.GetEffectiveOffsetMins(OnDate + aValue,aTimeZoneID);
  FOwner.SetParam(FIndex,VarArrayOf([aValue,dstOffset,aTimeZoneID]));
end;

procedure TParamIntf.SetAsTime(aValue: TDateTime; OnDate: TDateTime;
  aTimeZone: AnsiString);
var dstOffset: smallint;
begin
  aValue := TimeOf(aValue);
  dstOffset := FOwner.Database.Attachment.GetTimeZoneServices.GetEffectiveOffsetMins(OnDate + aValue,aTimeZone);
  FOwner.SetParam(FIndex,VarArrayOf([aValue,dstOffset,aTimeZone]));
end;

procedure TParamIntf.SetAsDateTime(aValue: TDateTime);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsDateTime(aValue: TDateTime; aTimeZoneID: TFBTimeZoneID
  );
var dstOffset: smallint;
begin
  with FOwner.DataBase.attachment.GetTimeZoneServices do
  begin
    dstOffset := GetEffectiveOffsetMins(aValue,aTimeZoneID);
    FOwner.SetParam(FIndex,VarArrayOf([aValue,aTimeZoneID]));
  end;
end;

procedure TParamIntf.SetAsDateTime(aValue: TDateTime; aTimeZone: AnsiString);
var dstOffset: smallint;
begin
  with FOwner.DataBase.attachment.GetTimeZoneServices do
  begin
    dstOffset := GetEffectiveOffsetMins(aValue,aTimeZone);
    FOwner.SetParam(FIndex,VarArrayOf([aValue,aTimeZone]));
  end;
end;

procedure TParamIntf.SetAsUTCDateTime(aUTCTime: TDateTime);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.SetAsDouble(aValue: Double);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsFloat(aValue: Float);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsPointer(aValue: Pointer);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.SetAsShort(aValue: Short);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsString(aValue: AnsiString);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetAsVariant(aValue: Variant);
begin
  FOwner.SetParam(FIndex,AValue);
end;

procedure TParamIntf.SetIsNull(aValue: Boolean);
begin
  if aValue then
    FOwner.SetParam(FIndex,NULL)
end;

procedure TParamIntf.SetAsBlob(aValue: IBlob);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.SetAsArray(anArray: IArray);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.SetAsQuad(aValue: TISC_QUAD);
begin
  IBError(ibxeNotSupported,[]);
end;

procedure TParamIntf.SetCharSetID(aValue: cardinal);
var s: RawByteString;
    codepage: TSystemCodePage;
    str: string;
begin
  str := FOwner.FParams[FIndex].Value;
  s := str;
  if FOwner.Database.Attachment.CharSetID2CodePage(aValue,codepage) then
    SetCodePage(s,codepage,codepage <> cp_none);
end;

procedure TParamIntf.SetAsBcd(aValue: tBCD);
begin
  FOwner.SetParam(FIndex,VarFmtBCDCreate(AValue));
end;

procedure TParamIntf.SetAsNumeric(Value: IFBNumeric);
begin
  IBError(ibxeNotSupported,[]);
end;

{ TParamListIntf }

procedure TParamListIntf.SetParam(index: integer; aValue: variant);
begin
  FParams[index].Value := aValue;
  FParams[index].Modified := true;
  FParams[index].TimeZoneID := TimeZoneID_GMT;
  FModified := true;
end;

procedure TParamListIntf.SetTimeZoneID(index: integer; aValue: TFBTimeZoneID);
begin
  if FParams[index].Modified then
    FParams[index].TimeZoneID := aValue;
end;

constructor TParamListIntf.Create(aFields: TFields; aDatabase: TIBDatabase);
var i,j: integer;
begin
  inherited Create;
  FDatabase := aDatabase;
  SetLength(FParams,aFields.Count*2);
  j := 0;
  {set up both current and "OLD" parameters from Field Names}
  for i := 0 to aFields.Count - 1 do
  if aFields[i].FieldKind = fkData then
  begin
    FParams[j].Name := aFields[i].FieldName;
    FParams[j].Value := NULL;
    FParams[j].Modified := false;
    FParams[j].DataSet := aFields[i].DataSet;
    Inc(j);
    FParams[j].Name := 'OLD_' + aFields[i].FieldName;
    FParams[j].Value := NULL;
    FParams[j].Modified := false;
    FParams[j].DataSet := aFields[i].DataSet;
    Inc(j);
  end;
  SetLength(FParams,j);
end;

destructor TParamListIntf.Destroy;
begin
  SetLength(FParams,0);
  inherited Destroy;
end;

function TParamListIntf.getCount: integer;
begin
  Result := Length(FParams);
end;

function TParamListIntf.getSQLParam(index: integer): ISQLParam;
begin
  if (index < 0) or (index >= getCount) then
    IBError(ibxeInvalidColumnIndex,[nil]);
  Result := TParamIntf.Create(self,index);
end;

function TParamListIntf.ParamExists(Idx: AnsiString): boolean;
begin
  Result := ByName(Idx) <> nil;
end;

function TParamListIntf.ByName(Idx: AnsiString): ISQLParam;
var i: integer;
begin
  Result := nil;
  for i := 0 to getCount - 1 do
    if CompareText(FParams[i].Name,Idx) = 0 then
    begin
      Result := getSQLParam(i);
      Exit;
    end;
end;

function TParamListIntf.GetModified: Boolean;
begin
  Result := FModified;
end;

function TParamListIntf.GetHasCaseSensitiveParams: Boolean;
begin
  Result := false;
end;

function TParamListIntf.GetStatement: IStatement;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamListIntf.GetTransaction: ITransaction;
begin
  IBError(ibxeNotSupported,[]);
end;

function TParamListIntf.GetAttachment: IAttachment;
begin
  Result := Database.Attachment;
end;

procedure TParamListIntf.Clear;
var i: integer;
begin
  for i := 0 to getCount - 1 do
    getSQLParam(i).Clear;
end;

{ TIBUpdate }

function TIBUpdate.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FDummySQL; {non empty result}
end;

function TIBUpdate.GetDataSet: TIBCustomDataSet;
begin
  Result := FDataSet;
end;

procedure TIBUpdate.SetDataSet(ADataSet: TIBCustomDataSet);
begin
  FDataSet := ADataset;
end;

procedure TIBUpdate.Apply(UpdateKind: TUpdateKind; buff: PChar);
var Params: ISQLParams;
begin
  Params := TParamListIntf.Create(Dataset.Fields,(DataSet.Database as TIBDatabase));
  InternalSetParams(Params,buff);
  if assigned(FOnApplyUpdates) then
    OnApplyUpdates(self,UpdateKind,Params);
end;

constructor TIBUpdate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDummySQL := TStringList.Create;
  FDummySQL.Text := '*';
end;

destructor TIBUpdate.Destroy;
begin
  if assigned(FDummySQL) then FDummySQL.Free;
  inherited Destroy;
end;

end.

