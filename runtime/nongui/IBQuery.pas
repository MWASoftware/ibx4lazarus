{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2018                                               }
{                                                                        }
{************************************************************************}

unit IBQuery;

interface

{$Mode Delphi}

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
 SysUtils, Classes,  Db, IB, IBCustomDataSet, IBSQL;

type

{ TIBQuery }

  TIBQuery = class(TIBParserDataSet)
  private
    FSQL: TStrings;
    FPrepared: Boolean;
    FParams: TParams;
    FText: string;
    FSQLUpdating: boolean;
    FInQueryChanged: boolean;
    function GetRowsAffected: Integer;
    function ParseSQL(ParamList: TParams; SQL: string; DoClear: boolean): string;
    procedure PrepareSQL;
    procedure QueryChanged(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure SetPrepared(Value: Boolean);
    procedure SetPrepare(Value: Boolean);
    procedure WriteParamData(Writer: TWriter);
    function GetStmtHandle: IStatement;
    procedure UpdateSQL;

  protected
    { IProviderSupport }
(*    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;  *)

    procedure DefineProperties(Filer: TFiler); override;
    procedure InitFieldDefs; override;
    procedure InternalOpen; override;
    procedure Disconnect; override;
    function GetParamsCount: Word;
    function GenerateQueryForLiveUpdate : Boolean;
    procedure SetFiltered(Value: Boolean); override;
    procedure SQLChanged(Sender: TObject); override;
    procedure SQLChanging(Sender: TObject); override;

    {IDynamicSQLParam}
    function GetParamValue(ParamName: string): variant; override;
    procedure SetParamValue(ParamName: string; ParamValue: variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BatchInput(InputObject: TIBBatchInput);
    procedure BatchOutput(OutputObject: TIBBatchOutput);
    procedure ExecSQL;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList);(* override;*)
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    procedure ResetParser; override;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property ParamCount: Word read GetParamsCount;
    property StmtHandle: IStatement read GetStmtHandle;
    property StatementType;
    property Text: string read FText;
    property RowsAffected: Integer read GetRowsAffected;
 //   property Params: TParams read FParams write SetParamsList;
    property BaseSQLSelect;

  published
    property Active;
    property AutoCommit;
    property BufferChunks;
    property BufferChunksInFirstBlock;
    property CachedUpdates;
    property CaseSensitiveParameterNames;
    property DataSource read GetDataSource write SetDataSource;
    property EnableStatistics;
    property GenerateParamNames;
 //   property Constraints stored ConstraintsStored;
    property GeneratorField;
    property MasterDetailDelay;
    property ParamCheck;
    property SQL: TStrings read FSQL write SetQuery;
    property Params: TParams read FParams write SetParamsList;
    property UniDirectional default False;
    property UpdateObject;
    property Filtered;
    property Filter;
    property SQLFiltered;
    property SQLFilterParams;
    property DataSetCloseAction;
    property TZTextOption;
    property DefaultTZDate;

    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;
    property OnFilterRecord;
    property OnValidatePost;
    property OnDeleteReturning;
end;

implementation

uses IBMessages, IBUtils;

{ TIBQuery }

constructor TIBQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create(Self);
  ParamCheck := True;
end;

destructor TIBQuery.Destroy;
begin
  Destroying;
  Disconnect;
  SQL.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TIBQuery.InitFieldDefs;
begin
  inherited InitFieldDefs;
end;

procedure TIBQuery.InternalOpen;
begin
  ActivateConnection();
  ActivateTransaction;
  QSelect.GenerateParamNames := GenerateParamNames;
  SetPrepared(True);
  if DataSource <> nil then
    SetParamsFromCursor;
  SetParams;
  inherited InternalOpen;
end;

procedure TIBQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TIBQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TIBQuery.Prepare;
begin
  SetPrepared(True);
end;

procedure TIBQuery.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TIBQuery.ResetParser;
begin
  inherited ResetParser;
  UpdateSQL;
end;

procedure TIBQuery.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    Disconnect;
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

procedure TIBQuery.QueryChanged(Sender: TObject);
begin
  if FInQueryChanged then Exit;
  FInQueryChanged := true;
  try
    if not (csReading in ComponentState) then
    begin
      Disconnect;
      if csDesigning in ComponentState then
        FText := ParseSQL(FParams,SQL.Text, true)
      else
        FText := SQL.Text;
      DataEvent(dePropertyChange, 0);
    end else
      FText := ParseSQL(FParams,SQL.Text, true);

    if not FSQLUpdating then
    begin
      Prepared := false;
      SelectSQL.Assign(SQL);
    end;
  finally
    FInQueryChanged := false;
  end;
end;

procedure TIBQuery.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

function TIBQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

procedure TIBQuery.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
  {The following results in a stream read error with nested frames. Hence commented out until
   someone fixes the LCL }
{    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TIBQuery(Filer.Ancestor).FParams) else}
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;


procedure TIBQuery.ReadParamData(Reader: TReader);
begin
  FParams.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TIBQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TIBQuery.SetPrepared(Value: Boolean);
begin
  CheckDatasetClosed;
  if Value <> Prepared then
  begin
    if Value then
    begin
      if Length(Text) > 1 then PrepareSQL
      else IBError(ibxeEmptySQLStatement, [nil]);
    end
    else
    begin
      InternalUnPrepare;
      FParams.Clear;
    end;
    FPrepared := Value;
  end;
end;

procedure TIBQuery.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
  Field: TField;

  procedure CheckRequiredParams;
  var
    I: Integer;
  begin
    for I := 0 to FParams.Count - 1 do
    with FParams[I] do
      if not Bound then
        IBError(ibxeRequiredParamNotSet, [FParams[I].Name]);
  end;

begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
      if not FParams[I].Bound then
      begin
        Field := DataSet.FindField(FParams[I].Name);
        if assigned(Field) then
        begin
            FParams[I].AssignField(Field);
            FParams[I].Bound := False;
        end;
      end;
    end
    else
      CheckRequiredParams;
  end
  else
    CheckRequiredParams;
end;


function TIBQuery.ParamByName(const Value: string): TParam;
var i: integer;
begin
  if not Prepared then
    Prepare;
  Result := nil;
  if not CaseSensitiveParameterNames then
    Result := FParams.ParamByName(Value)
  else
  begin
    i := FParams.Count - 1;
    while (Result = nil) and (i >= 0) do
      if CompareStr(Value,FParams.Items[i].Name) = 0 then
        Result := FParams.Items[i]
      else
        Dec(i);
    if Result = nil then
      IBError(ibxeParameterNameNotFound,[Value]);
  end;
end;

procedure TIBQuery.BatchInput(InputObject: TIBBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TIBQuery.BatchOutput(OutputObject: TIBBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TIBQuery.ExecSQL;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if SQL.Count <= 0 then
    IBError(ibxeEmptySQLStatement, [nil]);

  ActivateConnection();
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then SetParamsFromCursor;
    if FParams.Count > 0 then SetParams;
    InternalExecQuery;
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

procedure TIBQuery.SetParams;

var
i : integer;
Buffer: Pointer;
SQLParam: ISQLParam;

begin
  for I := 0 to FParams.Count - 1 do
  begin
    SQLParam :=  SQLParams.ByName(Params[i].Name);
    if Params[i].IsNull then
      SQLParam.IsNull := True
    else begin
      SQLParam.IsNull := False;
      case Params[i].DataType of
        ftBytes:
        begin
          GetMem(Buffer,Params[i].GetDataSize);
          try
            Params[i].GetData(Buffer);
            SQLParam.AsPointer := Buffer;
          finally
            FreeMem(Buffer);
          end;
        end;
        ftString:
          SQLParam.AsString := Params[i].AsString;
        ftBoolean:
          SQLParam.AsBoolean := Params[i].AsBoolean;
        ftSmallint, ftWord:
          SQLParam.AsShort := Params[i].AsSmallInt;
        ftInteger:
          SQLParam.AsLong := Params[i].AsInteger;
        ftLargeInt:
          SQLParam.AsInt64 := Params[i].AsLargeInt;
        ftFloat:
         SQLParam.AsDouble := Params[i].AsFloat;
        ftBCD, ftCurrency:
          SQLParam.AsCurrency := Params[i].AsCurrency;
        ftDate:
          SQLParam.AsDate := Params[i].AsDateTime;
        ftTime:
          SQLParam.AsTime := Params[i].AsDateTime;
        ftDateTime:
          SQLParam.AsDateTime := Params[i].AsDateTime;
        ftBlob, ftMemo:
          SQLParam.AsString := Params[i].AsString;
        else
          IBError(ibxeNotSupported, [nil]);
      end;
    end;
  end;
end;

procedure TIBQuery.PrepareSQL;
var List: TParams;
begin
  QSelect.GenerateParamNames := GenerateParamNames;
  InternalPrepare;
  UpdateSQL;
  if ParamCheck  then
  begin
    List := TParams.Create(Self);
    try
      FText := ParseSQL(List,SQL.Text, True);
      List.AssignValues(FParams);
      FParams.Clear;
      FParams.Assign(List);
    finally
      List.Free;
    end;
  end;
end;


function TIBQuery.GetRowsAffected: Integer;
begin
  Result := -1;
  if Prepared then
   Result := QSelect.RowsAffected
end;

function TIBQuery.ParseSQL(ParamList: TParams; SQL: string; DoClear: boolean
  ): string;
var ParamNames: TStrings;
    i: integer;
begin
  ParamNames := TStringList.Create;
  try
    Result := TSQLParamProcessor.Execute(SQL,GenerateParamNames,ParamNames);
    if DoClear then ParamList.Clear;
    for i := 0 to ParamNames.Count - 1 do
    begin
      if not CaseSensitiveParameterNames then
        ParamNames[i] := AnsiUpperCase(ParamNames[i]);
      if ParamList.FindParam(ParamNames[i]) = nil then
        ParamList.CreateParam(ftUnknown, ParamNames[i], ptInput);
    end;
  finally
    ParamNames.Free
  end;
end;


procedure TIBQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if (Field <> nil) then
      List.Add(Field);
    Result := Field <> nil;
  end;

var
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

function TIBQuery.GetStmtHandle: IStatement;
begin
  Result := SelectStmtHandle;
end;

procedure TIBQuery.UpdateSQL;
begin
  if not FSQLUpdating and not FInQueryChanged and (SQL.Text <> SelectSQL.Text) then
  begin
    FSQLUpdating := true;
    try
      SQL.Text := SelectSQL.Text;
    finally
      FSQLUpdating := false
    end;
  end;
end;

function TIBQuery.GenerateQueryForLiveUpdate : Boolean;
begin
  Result := False;
end;

procedure TIBQuery.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

procedure TIBQuery.SQLChanged(Sender: TObject);
begin
  inherited SQLChanged(Sender);
  UpdateSQL;
end;

procedure TIBQuery.SQLChanging(Sender: TObject);
begin
  inherited SQLChanging(Sender);
  Prepared := false;
end;

function TIBQuery.GetParamValue(ParamName : string) : variant;
begin
  Result := ParamByName(ParamName).Value;
end;

procedure TIBQuery.SetParamValue(ParamName : string; ParamValue : variant);
begin
 ParamByName(ParamName).Value := ParamValue;
end;

{ TIBQuery IProviderSupport }
 (*
function TIBQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

procedure TIBQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;

function TIBQuery.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

procedure TIBQuery.PSExecute;
begin
  ExecSQL;
end;

procedure TIBQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;
 *)
end.

