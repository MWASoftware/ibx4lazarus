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
  DB, DBTreeView, IBSQLParser, IBCustomDataSet;

type
  {
    TIBTreeView is intended to be a data aware descendent of TCustomTreeView and used to display
    hierarchically structured data in a natural manner. Nodes can be deleted, moved
    and added to the tree and each change is reflected in the underlying dataset. The
    Node text can similarly be edited.
  }

  TIBTreeView = class;

  { TIBTreeViewControlLink }

  TIBTreeViewControlLink = class(TIBControlLink)
  private
    FOwner: TIBTreeView;
  protected
    procedure UpdateSQL(Sender: TObject); override;
    procedure UpdateParams(Sender: TObject); override;
  public
    constructor Create(AOwner: TIBTreeView);
  end;

  TIBTreeView = class(TDBTreeView)
  private
    { Private declarations }
    FIBTreeViewControlLink: TIBTreeViewControlLink;
    procedure UpdateParams(Sender: TObject; Parser: TSelectSQLParser);
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
   protected
     procedure DataSourceChanged; override;
     procedure RefreshDataset; override;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses IBQuery;


{ TIBTreeViewControlLink }

constructor TIBTreeViewControlLink.Create(AOwner: TIBTreeView);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TIBTreeViewControlLink.UpdateParams(Sender: TObject);
begin
  FOwner.UpdateParams(self,TIBParserDataSet(Sender).Parser)
end;

procedure TIBTreeViewControlLink.UpdateSQL(Sender: TObject);
begin
  FOwner.UpdateSQL(self,TIBParserDataSet(Sender).Parser)
end;


procedure TIBTreeView.UpdateParams(Sender: TObject; Parser: TSelectSQLParser);
begin
  if not assigned(FExpandNode) and assigned(FUpdateNode)  then {Scrolling dataset}
   begin
     if DataSource.DataSet is TIBQuery then
       TIBQuery(DataSource.DataSet).ParamByName('IBX_KEY_VALUE').Value :=
         FUpdateNode.KeyValue
     else
     if DataSource.DataSet is TIBDataSet then
       TIBDataSet(DataSource.DataSet).ParamByName('IBX_KEY_VALUE').Value :=
         FUpdateNode.KeyValue
   end
  else
  if assigned(FExpandNode) then
  begin
    if DataSource.DataSet is TIBQuery then
      TIBQuery(DataSource.DataSet).ParamByName('IBX_PARENT_VALUE').Value :=
        TDBTreeNode(FExpandNode).KeyValue
    else
    if DataSource.DataSet is TIBDataSet then
      TIBDataSet(DataSource.DataSet).ParamByName('IBX_PARENT_VALUE').Value :=
        TDBTreeNode(FExpandNode).KeyValue
  end;
end;

procedure TIBTreeView.UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
begin
    if not assigned(FExpandNode) and assigned(FUpdateNode)  then {Scrolling dataset}
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + KeyField + '" = :IBX_KEY_VALUE')
    else
    if (Items.Count = 0) then
      {Need to Load Root Nodes}
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + ParentField + '" is null')
    else
    if assigned(FExpandNode) then
    begin
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + ParentField + '" = :IBX_PARENT_VALUE');
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + KeyField + '" = :IBX_PARENT_VALUE',true);
    end;
end;

procedure TIBTreeView.DataSourceChanged;
begin
    if assigned(DataSource) and (DataSource.DataSet <> nil) and (DataSource.DataSet is TIBParserDataset) then
      FIBTreeViewControllink.IBDataSet := TIBCustomDataSet(DataSource.DataSet)
    else
      FIBTreeViewControllink.IBDataSet := nil;
end;

procedure TIBTreeView.RefreshDataset;
begin
  DataSet.Active := false;
  DataSet.Active := true;
end;

constructor TIBTreeView.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FIBTreeViewControlLink := TIBTreeViewControlLink.Create(self);
end;

destructor TIBTreeView.Destroy;
begin
  if assigned(FIBTreeViewControlLink) then FIBTreeViewControlLink.Free;
  inherited Destroy;
end;

end.
