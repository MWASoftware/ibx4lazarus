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

unit IBUpdateSQL;

{$Mode Delphi}

interface

uses SysUtils, Classes, DB, IB, IBCustomDataSet, IBSQL;

type
{ TIBUpdateSQL }

  TIBUpdateSQL = class(TIBDataSetUpdateObject)
  private
    FDataSet: TIBCustomDataSet;
    FQueries: array[TUpdateKind] of TIBSQL;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TIBSQL;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  protected
    procedure InternalPrepare(UpdateKind: TUpdateKind);
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetDataSet: TIBCustomDataSet; override;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); override;
    procedure SQLChanged(Sender: TObject);
    procedure Apply(UpdateKind: TUpdateKind; buff: PChar); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TIBSQL read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

implementation

uses Variants;

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
  end;
end;

destructor TIBUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

procedure TIBUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
  InternalPrepare(UpdateKind);
  with Query[UpdateKind] do
  begin
    ExecQuery;
//    if RowsAffected <> 1 then IBError(ibxeUpdateFailed, [nil]);
// Commented out in release 1.2
  end;
end;

function TIBUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TIBSQL;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TIBSQL.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    if (FDataSet is TIBCustomDataSet) then
    begin
      FQueries[UpdateKind].Database := TIBCustomDataSet(FDataSet).DataBase;
      FQueries[UpdateKind].Transaction := TIBCustomDataSet(FDataSet).Transaction;
    end;
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

function TIBUpdateSQL.GetDataSet: TIBCustomDataSet;
begin
  Result := FDataSet;
end;

procedure TIBUpdateSQL.SetDataSet(ADataSet: TIBCustomDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TIBUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TIBUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
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

procedure TIBUpdateSQL.Apply(UpdateKind: TUpdateKind; buff: PChar);
begin
  if not Assigned(FDataSet) then Exit;
  InternalPrepare(UpdateKind);
  InternalSetParams(Query[UpdateKind],buff);
  ExecSQL(UpdateKind);
end;

end.