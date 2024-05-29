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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DB, DBTreeView, IBDynamicInterfaces;

type
  {
    TIBTreeView is intended to be a data aware descendent of TCustomTreeView and used to display
    hierarchically structured data in a natural manner. Nodes can be deleted, moved
    and added to the tree and each change is reflected in the underlying dataset. The
    Node text can similarly be edited.
  }

  TIBTreeView = class;
  TIBTreeNode = TDBTreeNode;

  TIBTreeView = class(TDBTreeView,IDynamicSQLComponent)
  private
    { Private declarations }
    FCurDataset: TDataSet; {reference to Dataset on last call to DataSetChanged}
    FOnSetParams : TOnSetParams;
    FOnUpdateSQL : TOnUpdateSQL;
   protected
     procedure DataSourceChanged; override;
     procedure RefreshDataset; override;
     {IDynamicSQLComponent}
     procedure UpdateSQL(SQLEditor: IDynamicSQLEditor);
     procedure SetParams(SQLParamProvider: IDynamicSQLParam);
  public
    { Public declarations }
    destructor Destroy; override;
  published
    property OnUpdateSQL: TOnUpdateSQL read FOnUpdateSQL write FOnUpdateSQL;
    property OnSetParams: TOnSetParams read FOnSetParams write FOnSetParams;
  end;

function StrIntListToVar(s: string): TVariantArray;
function VarToStrIntList(a: TVariantArray): string;

implementation

function StrIntListToVar(s: string): TVariantArray;
begin
  Result := DBTreeView.StrIntListToVar(s);
end;

function VarToStrIntList(a: TVariantArray): string;
begin
  Result := DBTreeView.VarToStrIntList(a);
end;



procedure TIBTreeView.DataSourceChanged;
begin
  if FCurDataset <> DataSet then
  begin
    if FCurDataset <> nil then
      (FCurDataset as IDynamicSQLDataset).UnRegisterDynamicComponent(self);
    FCurDataset := nil;
    if ProvidesIDynamicSQLDataset(DataSet,false) then
    begin
      (DataSet as IDynamicSQLDataset).RegisterDynamicComponent(self);
      FCurDataset := DataSet;
    end;
  end;
end;

procedure TIBTreeView.RefreshDataset;
begin
  DataSet.Active := false;
  DataSet.Active := true;
end;

procedure TIBTreeView.UpdateSQL(SQLEditor : IDynamicSQLEditor);
begin
  with SQLEditor do
  if not assigned(FExpandNode) and assigned(FUpdateNode)  then {Scrolling dataset}
    Add2WhereClause(GetRelationNameQualifier + '"' + KeyField + '" = :IBX_KEY_VALUE')
  else
  if (Items.Count = 0) then
    {Need to Load Root Nodes}
    Add2WhereClause(GetRelationNameQualifier + '"' + ParentField + '" is null')
  else
  if assigned(FExpandNode) then
  begin
    Add2WhereClause(GetRelationNameQualifier + '"' + ParentField + '" = :IBX_PARENT_VALUE');
    Add2WhereClause(GetRelationNameQualifier + '"' + KeyField + '" = :IBX_PARENT_VALUE',true);
  end;
  if assigned(FOnUpdateSQL) then
    OnUpdateSQL(self,SQLEditor);
end;

procedure TIBTreeView.SetParams(SQLParamProvider : IDynamicSQLParam);
begin
  with SQLParamProvider do
  if not assigned(FExpandNode) and assigned(FUpdateNode)  then {Scrolling dataset}
    SetParamValue('IBX_KEY_VALUE',FUpdateNode.KeyValue)
  else
  if assigned(FExpandNode) then
    SetParamValue('IBX_PARENT_VALUE',TDBTreeNode(FExpandNode).KeyValue);

  if assigned(FOnSetParams) then
    OnSetParams(self, SQLParamProvider);
end;

destructor TIBTreeView.Destroy;
begin
  if DataSet <> nil then
    (DataSet as IDynamicSQLDataset).UnRegisterDynamicComponent(self);
  inherited Destroy;
end;

end.
