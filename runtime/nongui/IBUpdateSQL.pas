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

unit IBUpdateSQL;

{$Mode Delphi}

interface

uses SysUtils, Classes, DB, IB, IBCustomDataSet, IBSQL;

type
{ TIBUpdateSQL }

  TIBUpdateSQL = class(TIBDataSetUpdateObject)
  private
    FLastUpdateKind :TUpdateKind;
    FQueries: array[TUpdateKind] of TIBSQL;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TIBSQL;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    procedure InternalPrepare(UpdateKind: TUpdateKind);
    procedure SQLChanged(Sender: TObject);
    procedure Apply(UpdateKind: TUpdateKind; buff: TRecordBuffer); override;
    procedure ExecSQL(UpdateKind: TUpdateKind; buff: TRecordBuffer);
    procedure SetDataSet(AValue : TIBCustomDataSet); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
      DeleteCount: integer): boolean; override;
    procedure RegisterQueries; override;
    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TIBSQL read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

implementation

uses Variants, IBBufferedCursors;

{ TIBUpdateSQL }

constructor TIBUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
    TStringList(FSQLText[UpdateKind]).OnChanging := SQLChanging;
  end;
end;

destructor TIBUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(DataSet) and (DataSet.UpdateObject = Self) then
    DataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

function TIBUpdateSQL.GetRowsAffected(var SelectCount, InsertCount,
                                 UpdateCount, DeleteCount: integer): boolean;
begin
  if Query[FLastUpdateKind].Statement <> nil  then
    Result := Query[FLastUpdateKind].Statement.GetRowsAffected(SelectCount, InsertCount,  UpdateCount, DeleteCount)
  else
    Result := inherited;
end;

procedure TIBUpdateSQL.RegisterQueries;
var
  UpdateKind: TUpdateKind;
begin
  inherited RegisterQueries;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  case UpdateKind of
    ukInsert:
      RegisterQuery(rqInsert,Query[UpdateKind]);
    ukModify:
      RegisterQuery(rqModify,Query[UpdateKind]);
    ukDelete:
      RegisterQuery(rqDelete,Query[UpdateKind]);
  end;
end;

procedure TIBUpdateSQL.ExecSQL(UpdateKind: TUpdateKind; buff: TRecordBuffer);
begin
  InternalPrepare(UpdateKind);
  with Query[UpdateKind] do
  begin
    ExecQuery;
//    if RowsAffected <> 1 then IBError(ibxeUpdateFailed, [nil]);
// Commented out in release 1.2
    if FieldCount > 0 then  {Has RETURNING Clause}
      UpdateRecordFromQuery(UpdateKind,Current,Buff);
  end;
end;

function TIBUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TIBSQL;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TIBSQL.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    FQueries[UpdateKind].Database := DataSet.DataBase;
    FQueries[UpdateKind].Transaction := DataSet.Transaction;
  end;
  Result := FQueries[UpdateKind];
end;

function TIBUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TIBUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

procedure TIBUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TIBUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

procedure TIBUpdateSQL.SetDataSet(AValue : TIBCustomDataSet);
var i: TUpdateKind;
begin
  if Dataset = AValue then Exit;

  inherited SetDataSet(AValue);
  for i := low(TUpdateKind) to high(TUpdateKind) do
    if assigned(FQueries[i]) then
    begin
      if assigned(DataSet) then
      begin
        FQueries[i].Database := DataSet.Database;
        FQueries[i].Transaction := DataSet.Transaction;
      end
      else
      begin
        FQueries[i].Database := nil;
        FQueries[i].Transaction := nil;
      end;
    end;
end;

procedure TIBUpdateSQL.InternalPrepare(UpdateKind: TUpdateKind);
begin
  with Query[UpdateKind] do
  begin
    with Transaction do
      if not InTransaction then StartTransaction;
    if not Prepared then Prepare;
  end;
end;

procedure TIBUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      Break;
    end;
end;

procedure TIBUpdateSQL.Apply(UpdateKind: TUpdateKind; buff: TRecordBuffer);
begin
  if not Assigned(DataSet) then Exit;
  InternalPrepare(UpdateKind);
  InternalSetParams(Query[UpdateKind].Params,buff);
  ExecSQL(UpdateKind,buff);
  FLastUpdateKind := UpdateKind;
end;

end.
