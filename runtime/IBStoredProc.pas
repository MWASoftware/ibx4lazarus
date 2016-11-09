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
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBStoredProc;

{$Mode Delphi}

{$codepage UTF8}

interface

uses SysUtils, Classes, DB, IB, IBDatabase, IBCustomDataSet,
     IBSQL, IBUtils;
     
{ TIBStoredProc }
type

  TIBStoredProc = class(TIBCustomDataSet)
  private
    FStmtHandle: IStatement;
    FProcName: string;
    FParams: TParams;
    FPrepared: Boolean;
    FNameList: TStrings;
    procedure SetParamsList(Value: TParams);
    procedure FreeStatement;
    function GetStoredProcedureNames: TStrings;
    procedure GetStoredProcedureNamesFromServer;
    procedure CreateParamDesc;
    procedure SetParams;
    procedure SetParamsFromCursor;
    procedure GenerateSQL;
    procedure FetchDataIntoOutputParams;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

  protected

    procedure DefineProperties(Filer: TFiler); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure InitFieldDefs; override;
    function GetParamsCount: Word;
    procedure SetPrepared(Value: Boolean);
    procedure SetPrepare(Value: Boolean);
    procedure SetProcName(Value: string);
    procedure Disconnect; override;
    procedure InternalOpen; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyParams(Value: TParams);
    procedure ExecProc;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    property ParamCount: Word read GetParamsCount;
    property StmtHandle: IStatement read FStmtHandle;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property StoredProcedureNames: TStrings read GetStoredProcedureNames;

  published
    property StoredProcName: string read FProcName write SetProcName;
    property Params: TParams read FParams write SetParamsList;
    property Filtered;

    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;
    property OnFilterRecord;
  end;

implementation

 uses  FBMessages;

{ TIBStoredProc }

constructor TIBStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TParams.Create (self);
  FNameList := TStringList.Create;
end;

destructor TIBStoredProc.Destroy;
begin
  Destroying;
  Disconnect;
  if assigned (FParams) then FParams.Free;
  if assigned(FNameList) then FNameList.Destroy;
  inherited Destroy;
end;

procedure TIBStoredProc.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TIBStoredProc.ExecProc;
var
  DidActivate: Boolean;
begin
  CheckInActive;
  if StoredProcName = '' then
    IBError(ibxeNoStoredProcName, [nil]);
  ActivateConnection;
  DidActivate := ActivateTransaction;
  try
    SetPrepared(True);
    if DataSource <> nil then SetParamsFromCursor;
    if FParams.Count > 0 then SetParams;
    InternalExecQuery;
    FetchDataIntoOutputParams;
  finally
    if DidActivate then
      DeactivateTransaction;
  end;
end;

procedure TIBStoredProc.SetProcName(Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if Value <> FProcName then
    begin
      FProcName := Value;
      FreeStatement;
      FParams.Clear;
      if (Value <> '') and
        (Database <> nil) then
        GenerateSQL;
    end;
  end else begin
    FProcName := Value;
  if (Value <> '') and
    (Database <> nil) then
    GenerateSQL;
  end;
end;

function TIBStoredProc.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

procedure TIBStoredProc.SetFiltered(Value: Boolean);
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

 procedure TIBStoredProc.InitFieldDefs;
begin
  if SelectSQL.Text = '' then
     GenerateSQL;
  inherited InitFieldDefs;
end;

procedure TIBStoredProc.GenerateSQL;

var Params: TStringList;

  function FormatParameter(Dialect: Integer; Value: String): String;
  var j: integer;
  begin
    Value := Trim(Value);
    if Dialect = 1 then
       Result := AnsiUpperCase(Value)
    else
    begin
      j := 1;
      Value := Space2Underscore(AnsiUpperCase(Value));
      Result := Value;
      while Params.IndexOf(Result) <> -1 do
      begin
        Result := Value + IntToStr(j);
        Inc(j)
      end;
      Params.Add(Result)
    end;
  end;

var
  Query : TIBSQL;
  input : string;
begin
  input := '';
  if FProcName = '' then
     IBError(ibxeNoStoredProcName,[nil]);
  ActivateConnection;
  Database.InternalTransaction.StartTransaction;
  Params := TStringList.Create;
  Query := TIBSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    Query.SQL.Text := 'SELECT RDB$PARAMETER_NAME,  RDB$PARAMETER_TYPE ' + {do not localize}
                       'FROM RDB$PROCEDURE_PARAMETERS ' + {do not localize}
                       'WHERE RDB$PROCEDURE_NAME = ' + {do not localize}
                       '''' + FormatIdentifierValue(Database.SQLDialect, FProcName) + '''' +
                       ' ORDER BY RDB$PARAMETER_NUMBER'; {do not localize}
    Query.Prepare;
    Query.GoToFirstRecordOnExecute := False;
    Query.ExecQuery;
    while (not Query.EOF) and Query.Next do begin
      if (Query.FieldByName('RDB$PARAMETER_TYPE').AsInteger = 0) then begin {do not localize}
        if (input <> '') then
          input := input + ', :' +
            FormatParameter(Database.SQLDialect, Query.Current.ByName('RDB$PARAMETER_NAME').AsString) else {do not localize}
          input := ':' +
            FormatParameter(Database.SQLDialect, Query.Current.ByName('RDB$PARAMETER_NAME').AsString); {do not localize}
      end
    end;
    SelectSQL.Text := 'Execute Procedure ' + {do not localize}
                FormatParameter(Database.SQLDialect, FProcName) + ' ' + input;
 {   writeln(SelectSQL.Text);}
  finally
    Query.Free;
    Params.Free;
    Database.InternalTransaction.Commit;
  end;
end;

procedure TIBStoredProc.CreateParamDesc;
var
  i : integer;
  DataType : TFieldType;
begin
  DataType := ftUnknown;
  for i := 0 to QSelect.MetaData.Count - 1 do begin
  case QSelect.MetaData[i].SQLtype of
    SQL_TYPE_DATE: DataType := ftDate;
    SQL_TYPE_TIME: DataType := ftTime;
    SQL_TIMESTAMP: DataType := ftDateTime;
    SQL_SHORT:
      if QSelect.MetaData[i].getScale = 0 then
        DataType := ftSmallInt
      else
        DataType := ftBCD;
    SQL_LONG:
      if QSelect.MetaData[i].getScale = 0 then
        DataType := ftInteger
      else if QSelect.MetaData[i].getScale >= -4 then
        DataType := ftBCD
      else
        DataType := ftFloat;
    SQL_INT64:
      if QSelect.MetaData[i].getScale = 0 then
        DataType := ftLargeInt
      else if QSelect.MetaData[i].getScale >= -4 then
        DataType := ftBCD
      else
        DataType := ftFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
    SQL_BOOLEAN:
      DataType := ftBoolean;
    SQL_TEXT: DataType := ftString;
    SQL_VARYING:
      if QSelect.MetaData[i].GetSize < 1024 then
        DataType := ftString
      else DataType := ftBlob;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
    end;
    FParams.CreateParam(DataType, Trim(QSelect.MetaData[i].Name), ptOutput);
  end;

  DataType := ftUnknown;
  for i := 0 to QSelect.Params.Count - 1 do begin
  case QSelect.Params[i].GetSQLtype of
    SQL_TYPE_DATE: DataType := ftDate;
    SQL_TYPE_TIME: DataType := ftTime;
    SQL_TIMESTAMP: DataType := ftDateTime;
    SQL_SHORT:
      if QSelect.Params[i].getScale = 0 then
        DataType := ftSmallInt
      else
        DataType := ftBCD;
    SQL_LONG:
      if QSelect.Params[i].getScale = 0 then
        DataType := ftInteger
      else if QSelect.Params[i].getScale >= -4 then
        DataType := ftBCD
      else DataType := ftFloat;
    SQL_INT64:
      if QSelect.Params[i].getScale = 0 then
        DataType := ftLargeInt
      else if QSelect.Params[i].getScale >= -4 then
        DataType := ftBCD
      else DataType := ftFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT: DataType := ftFloat;
    SQL_BOOLEAN:
      DataType := ftBoolean;
    SQL_TEXT: DataType := ftString;
    SQL_VARYING:
      if QSelect.Params[i].GetSize < 1024 then
        DataType := ftString
      else DataType := ftBlob;
    SQL_BLOB, SQL_ARRAY, SQL_QUAD: DataType := ftBlob;
    end;
    FParams.CreateParam(DataType, Trim(QSelect.Params[i].Name), ptInput);
  end;
end;

procedure TIBStoredProc.SetPrepared(Value: Boolean);
begin
  if Prepared <> Value then
  begin
    if Value then
      try
        if SelectSQL.Text = '' then GenerateSQL;
        InternalPrepare;
        if FParams.Count = 0 then CreateParamDesc;
        FPrepared := True;
      except
        FreeStatement;
        raise;
      end
    else FreeStatement;
  end;

end;

procedure TIBStoredProc.Prepare;
begin
  SetPrepared(True);
end;

procedure TIBStoredProc.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TIBStoredProc.FreeStatement;
begin
  InternalUnPrepare;
  FPrepared := False;
end;

procedure TIBStoredProc.SetPrepare(Value: Boolean);
begin
  if Value then Prepare
  else UnPrepare;
end;

procedure TIBStoredProc.CopyParams(Value: TParams);
begin
  if not Prepared and (FParams.Count = 0) then
  try
    Prepare;
    Value.Assign(FParams);
  finally
    UnPrepare;
  end else
    Value.Assign(FParams);
end;

procedure TIBStoredProc.SetParamsList(Value: TParams);
begin
  CheckInactive;
  if Prepared then
  begin
    SetPrepared(False);
    FParams.Assign(Value);
    SetPrepared(True);
  end else
    FParams.Assign(Value);
end;

function TIBStoredProc.ParamByName(const Value: string): TParam;
begin
  Prepare;
  Result := FParams.ParamByName(Value);
end;

function TIBStoredProc.GetStoredProcedureNames: TStrings;
begin
  FNameList.clear;
  GetStoredProcedureNamesFromServer;
  Result := FNameList;
end;

procedure TIBStoredProc.GetStoredProcedureNamesFromServer;
var
  Query : TIBSQL;
begin
  if not (csReading in ComponentState) then begin
    ActivateConnection;
    Database.InternalTransaction.StartTransaction;
    Query := TIBSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := DataBase;
      Query.Transaction := Database.InternalTransaction;
      Query.SQL.Text := 'Select RDB$PROCEDURE_NAME from RDB$PROCEDURES'; {do not localize}
      Query.Prepare;
      Query.ExecQuery;
      while (not Query.EOF) and Query.Next do
        FNameList.Add(TrimRight(Query.Current.ByName('RDB$PROCEDURE_NAME').AsString)); {do not localize}
    finally
      Query.Free;
      Database.InternalTransaction.Commit;
    end;
  end;
end;

procedure TIBStoredProc.SetParams;
var
i : integer;
j: integer;
begin
  i := 0;
  for j := 0 to FParams.Count - 1 do
  begin
    if (Params[j].ParamType <> ptInput) then
      continue;
    if not Params[j].Bound then
      IBError(ibxeRequiredParamNotSet, [Params[j].Name]);
    if Params[j].IsNull then
      SQLParams[i].IsNull := True
    else begin
      SQLParams[i].IsNull := False;
      case Params[j].DataType of
        ftString:
          SQLParams[i].AsString := Params[j].AsString;
        ftSmallint, ftWord:
          SQLParams[i].AsShort := Params[j].AsSmallInt;
        ftBoolean:
           SQLParams[i].AsBoolean := Params[j].AsBoolean;
        ftInteger:
          SQLParams[i].AsLong := Params[j].AsInteger;
        ftLargeInt:
          SQLParams[i].AsInt64 := Params[j].AsLargeInt;
        ftFloat, ftCurrency:
         SQLParams[i].AsDouble := Params[j].AsFloat;
        ftBCD:
          SQLParams[i].AsCurrency := Params[j].AsCurrency;
        ftDate:
          SQLParams[i].AsDate := Params[j].AsDateTime;
        ftTime:
          SQLParams[i].AsTime := Params[j].AsDateTime;
        ftDateTime:
          SQLParams[i].AsDateTime := Params[j].AsDateTime;
        ftBlob, ftMemo:
          SQLParams[i].AsString := Params[j].AsString;
        else
          IBError(ibxeNotSupported, [nil]);
      end;
    end;
    Inc(i);
  end;
end;

procedure TIBStoredProc.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if (not Bound) and
            ((ParamType = ptInput) or (ParamType =  ptInputOutput)) then
            AssignField(DataSet.FieldByName(Name));
    end;
  end;
end;

procedure TIBStoredProc.FetchDataIntoOutputParams;
var
i,j : Integer;
begin
  j := 0;
  for i := 0 to FParams.Count - 1 do
    with Params[I] do
      if ParamType = ptOutput then begin
         Value := QSelect.Fields[j].Value;
         Inc(j);
      end;
end;

procedure TIBStoredProc.InternalOpen;
begin
  IBError(ibxeIsAExecuteProcedure,[nil]);
end;

procedure TIBStoredProc.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TIBStoredProc(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TIBStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TIBStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

end.
