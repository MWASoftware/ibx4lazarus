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
  DB, IBSQLParser, IBCustomDataSet;

type
  {
    TIBTreeView is intended to be a data aware descendent of TCustomTreeView and used to display
    hierarchically structured data in a natural manner. Nodes can be deleted, moved
    and added to the tree and each change is reflected in the underlying dataset. The
    Node text can similarly be edited.
  }

  TVariantArray = array of variant;

  TIBTreeView = class;

  { TIBTreeViewDatalink }

  TIBTreeViewDatalink = class(TDataLink)
  private
    FOwner: TIBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TIBTreeView);
  end;

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

  { TIBTreeNode }

  TIBTreeNode = class(TTreeNode)
  private
    FKeyValue: variant;
  public
    procedure DeleteAll;
    property KeyValue: variant read FKeyValue;
  end;

  TIBTreeView = class(TCustomTreeView)
  private
    { Private declarations }
    FDataLink: TIBTreeViewDatalink;
    FIBTreeViewControlLink: TIBTreeViewControlLink;
    FHasChildField: string;
    FKeyField: string;
    FTextField: string;
    FParentField: string;
    FExpandNode: TTreeNode;
    FNoAddNodeToDataset: boolean;
    FRelationName: string;
    FUpdateNode: TIBTreeNode;
    FModifiedNode: TIBTreeNode;
    FUpdating: boolean;
    FLocatingNode: boolean;
    FLastSelected: TVariantArray;
    procedure ActiveChanged(Sender: TObject);
    procedure AddNodes;
    procedure DataSetChanged(Sender: TObject);
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetRelationNameQualifier: string;
    function GetSelectedKeyValue: variant;
    procedure IBControlLinkChanged;
    procedure NodeMoved(Node: TTreeNode);
    procedure NodeUpdated(Node: TTreeNode);
    procedure RecordChanged(Sender: TObject; Field: TField);
    procedure SetHasChildField(AValue: string);
    procedure SetKeyField(AValue: string);
    procedure SetTextField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetParentField(AValue: string);
    function ScrollToNode(Node: TIBTreeNode): boolean;
    procedure UpdateData(Sender: TObject);
    procedure UpdateParams(Sender: TObject; Parser: TSelectSQLParser);
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
   protected
    { Protected declarations }
     procedure Added(Node: TTreeNode); override;
     procedure Delete(Node: TTreeNode); override;
     procedure Change(Node: TTreeNode); override;
     function CreateNode: TTreeNode; override;
     function CanEdit(Node: TTreeNode): Boolean; override;
     procedure Expand(Node: TTreeNode); override;
     procedure Loaded; override;
     procedure NodeChanged(Node: TTreeNode; ChangeEvent: TTreeNodeChangeReason); override;
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
     procedure Reinitialise;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    function FindNode(KeyValuePath: array of variant; SelectNode: boolean): TIBTreeNode; overload;
    function FindNode(KeyValue: variant): TIBTreeNode; overload;
    function GetNodePath(Node: TTreeNode): TVariantArray;
    property DataSet: TDataSet read GetDataSet;
    property SelectedKeyValue: variant read GetSelectedKeyValue;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property TextField: string read FTextField write SetTextField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultItemHeight;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExpandSignColor;
    property ExpandSignType;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property HasChildField: string read FHasChildField write SetHasChildField;
    property KeyField: string read FKeyField write SetKeyField;
    property MultiSelect;
    property MultiSelectStyle;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentField: string read FParentField write SetParentField;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RelationName: string read FRelationName write FRelationName;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomCreateItem;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEditingEnd;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnNodeChanged;
    property OnSelectionChanged;
    property OnShowHint;
    //property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Options;
    property Items;
    property TreeLineColor;
    property TreeLinePenStyle;
  end;

  function StrIntListToVar(s: string): TVariantArray;
  function VarToStrIntList(a: TVariantArray): string;

implementation

uses IBQuery,Variants;

function StrIntListToVar(s: string): TVariantArray;
var i, idx: integer;
    List: TStringList;
begin
  List := TStringList.Create;
  try
    idx := 1;
    List.Clear;
    while idx <= Length(s) do
       List.Add(ExtractFieldName(s,idx));

    Setlength(Result,List.Count);
    for i := 0 to List.Count - 1 do
        Result[i] := StrToInt(List[i])
  finally
    List.Free
  end;
end;

function VarToStrIntList(a: TVariantArray): string;
var i: integer;
begin
  for i := 0 to Length(a) - 1 do
      if VarIsOrdinal(a[i]) then
      begin
        if i = 0 then
           Result := IntToStr(a[i])
        else
          Result := Result + ';' + IntToStr(a[i])
      end
      else
        raise Exception.Create('Ordinal Type Expected when converting to integer string');
end;

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

{ TIBTreeNode }

procedure TIBTreeNode.DeleteAll;
var Node, NextNode: TTreeNode;
begin
    Expand(true);
    Node := GetFirstChild;
    while Node <> nil do
    begin
      NextNode := Node.GetNextSibling;
      TIBTreeNode(Node).DeleteAll;
      Node := NextNode;
    end;
    Delete
end;

{ TIBTreeView }

procedure TIBTreeView.ActiveChanged(Sender: TObject);
var AtTopLevel: boolean;
begin
  if (csDesigning in ComponentState) then Exit;
  IBControlLinkChanged;
  if assigned(DataSet) and not DataSet.Active then
  begin
    if not assigned(FExpandNode) and not assigned(FUpdateNode) then {must really be closing}
      Reinitialise
  end
  else
  begin
    AtTopLevel := Items.TopLvlCount = 0;
    AddNodes;
    if not FLocatingNode and (Selected = nil) and (Items.TopLvlCount > 0) then
    begin
      if Length(FLastSelected) > 0 then
         Selected := FindNode(FLastSelected,true)
      else
        Selected := Items.TopLvlItems[0];
    end
  end
end;

procedure TIBTreeView.AddNodes;
var Node: TTreeNode;
    ChildCount: integer;
begin
  if assigned(FExpandNode) or (Items.Count = 0) then
  begin
    ChildCount := 0;
    FNoAddNodeToDataset := true;
    try
      DataSet.First;
      while not DataSet.EOF do
      begin
        Node := Items.AddChild(FExpandNode,DataSet.FieldByName(TextField).AsString);
        TIBTreeNode(Node).FKeyValue := DataSet.FieldByName(KeyField).AsVariant;
        Node.HasChildren := (HasChildField = '') or (DataSet.FieldByName(HasChildField).AsInteger <> 0);
        Inc(ChildCount);
        DataSet.Next
      end;
    finally
      FNoAddNodeToDataset := false
    end;
    if assigned(FExpandNode) then
      FExpandNode.HasChildren := ChildCount > 0;
    FExpandNode := nil
  end
end;

procedure TIBTreeView.DataSetChanged(Sender: TObject);
begin
//  Reinitialise
end;

function TIBTreeView.GetDataSet: TDataSet;
begin
  Result := FDataLink.DataSet
end;

function TIBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource
end;

function TIBTreeView.GetRelationNameQualifier: string;
begin
  if FRelationName <> '' then
     Result := FRelationName + '.'
  else
    Result := ''
end;

function TIBTreeView.GetSelectedKeyValue: variant;
begin
  Result := NULL;
  if assigned(Selected) and (Selected is TIBTreeNode) then
     Result := TIBTreeNode(Selected).KeyValue
end;

procedure TIBTreeView.NodeMoved(Node: TTreeNode);
begin
  {Need to update Parent}
  if ScrollToNode(TIBTreeNode(Node))  then
  begin
      FDataLink.Edit;
      FModifiedNode := TIBTreeNode(Node)
  end;
end;

procedure TIBTreeView.NodeUpdated(Node: TTreeNode);
begin
  {Need to Update List Field}
  if ScrollToNode(TIBTreeNode(Node)) then
  begin
    FDataLink.Edit;
    FModifiedNode := TIBTreeNode(Node);
    FDataLink.UpdateRecord
  end;
end;

procedure TIBTreeView.RecordChanged(Sender: TObject; Field: TField);
var Node: TIBTreeNode;
    Destination: TIBTreeNode;
begin
  if DataSet.State = dsInsert then Exit;

  if assigned(Field) and (Field.FieldName = TextField) then
  begin
    Node := FindNode(DataSet.FieldByName(KeyField).AsVariant);
    if assigned(Node) then
    begin
      FUpdating := true;
      try
        Node.Text := Field.Text
      finally
        FUpdating := false
      end;
    end;
  end
  else
  if assigned(Field) and (Field.FieldName = ParentField) then
  begin
    Node := FindNode(DataSet.FieldByName(KeyField).AsVariant);
    if assigned(Node)  then
    begin
      if DataSet.FieldByName(ParentField).IsNull then
         Destination := nil
      else
        Destination := FindNode(DataSet.FieldByName(ParentField).AsVariant);

      if (Destination = nil) or (Destination = Node.Parent) then Exit;

      FUpdating := true;
      try
        Node.MoveTo(Destination,naAddChild);
      finally
        FUpdating := false
      end;
    end;
  end
end;

procedure TIBTreeView.SetHasChildField(AValue: string);
begin
  if FHasChildField = AValue then Exit;
  FHasChildField := AValue;
  Reinitialise
end;

procedure TIBTreeView.SetKeyField(AValue: string);
begin
  if FKeyField = AValue then Exit;
  FKeyField := AValue;
  Reinitialise
end;

procedure TIBTreeView.SetTextField(AValue: string);
begin
  if FTextField = AValue then Exit;
  FTextField := AValue;
  Reinitialise
end;

procedure TIBTreeView.SetDataSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  IBControlLinkChanged;
end;

procedure TIBTreeView.SetParentField(AValue: string);
begin
  if FParentField = AValue then Exit;
  FParentField := AValue;
  Reinitialise
end;

function TIBTreeView.ScrollToNode(Node: TIBTreeNode): boolean;
begin
  Result :=  assigned(DataSet) and DataSet.Active and assigned(Node) and not varIsNull(Node.KeyValue);
  if Result then
  begin
    if DataSet.Active and (DataSet.RecordCount > 0)
         and (Node.KeyValue = DataSet.FieldByName(KeyField).AsVariant) then Exit;

    FUpdateNode := Node;
    try
      DataSet.Active := false;
      DataSet.Active := true;
    finally
      FUpdateNode := nil
    end;
    Result := DataSet.FieldByName(KeyField).AsVariant = Node.KeyValue
  end;
end;

procedure TIBTreeView.UpdateData(Sender: TObject);
begin
  if assigned(FModifiedNode) then
  begin
    DataSet.FieldByName(TextField).AsString := FModifiedNode.Text;
    if FModifiedNode.Parent = nil then
      DataSet.FieldByName(ParentField).Clear
    else
      DataSet.FieldByName(ParentField).AsVariant := TIBTreeNode(FModifiedNode.Parent).KeyValue;
    FModifiedNode := nil
  end
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
        TIBTreeNode(FExpandNode).KeyValue
    else
    if DataSource.DataSet is TIBDataSet then
      TIBDataSet(DataSource.DataSet).ParamByName('IBX_PARENT_VALUE').Value :=
        TIBTreeNode(FExpandNode).KeyValue
  end;
end;

procedure TIBTreeView.UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
begin
    if not assigned(FExpandNode) and assigned(FUpdateNode)  then {Scrolling dataset}
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + FKeyField + '" = :IBX_KEY_VALUE')
    else
    if (Items.Count = 0) then
      {Need to Load Root Nodes}
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + FParentField + '" is null')
    else
    if assigned(FExpandNode) then
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + FParentField + '" = :IBX_PARENT_VALUE');
end;

procedure TIBTreeView.Added(Node: TTreeNode);
begin
  if assigned(DataSet) and DataSet.Active and not FNoAddNodeToDataset then
  begin
    DataSet.Append;
    TIBTreeNode(Node).FKeyValue := DataSet.FieldByName(KeyField).AsVariant;
    if (Node.Text = '') and not DataSet.FieldByName(TextField).IsNull then
       Node.Text := DataSet.FieldByName(TextField).AsString;
    FModifiedNode := TIBTreeNode(Node);
    FDataLink.UpdateRecord
  end;
  inherited Added(Node);
end;

procedure TIBTreeView.Delete(Node: TTreeNode);
begin
  if not (tvsUpdating in States) {TreeNodes being cleared}
     and not (tvsManualNotify in States) {Tree Collapse with node delete}
     and ScrollToNode(TIBTreeNode(Node)) then
     DataSet.Delete;
  inherited Delete(Node);
end;

procedure TIBTreeView.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  ScrollToNode(TIBTreeNode(Node));
end;

function TIBTreeView.CreateNode: TTreeNode;
var
  NewNodeClass: TTreeNodeClass;
begin
  Result := nil;
  if Assigned(OnCustomCreateItem) then
    OnCustomCreateItem(Self, Result);
  if Result = nil then
  begin
    NewNodeClass:=TIBTreeNode;
    if Assigned(OnCreateNodeClass) then
      OnCreateNodeClass(Self,NewNodeClass);
    Result := NewNodeClass.Create(Items);
  end;
end;

function TIBTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node)
              and assigned(DataSet) and not DataSet.FieldByName(TextField).ReadOnly
end;

procedure TIBTreeView.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
  if Node.HasChildren and assigned(DataSet) and (Node.GetFirstChild = nil) then
  begin
    FExpandNode := Node;
    DataSet.Active := false;
    DataSet.Active := true;
    if assigned(Selected) then
      ScrollToNode(TIBTreeNode(Selected))
  end;
end;

procedure TIBTreeView.IBControlLinkChanged;
begin
  if assigned(DataSource) and (DataSource.DataSet <> nil) and (DataSource.DataSet is TIBParserDataset) then
    FIBTreeViewControllink.IBDataSet := TIBCustomDataSet(DataSource.DataSet)
  else
    FIBTreeViewControllink.IBDataSet := nil;
end;

procedure TIBTreeView.Loaded;
begin
  inherited Loaded;
  IBControlLinkChanged;
  Reinitialise
end;

procedure TIBTreeView.NodeChanged(Node: TTreeNode;
  ChangeEvent: TTreeNodeChangeReason);
begin
  inherited NodeChanged(Node, ChangeEvent);
  if not FNoAddNodeToDataset and not FUpdating then
  case ChangeEvent of
  ncTextChanged:
    NodeUpdated(Node);
  ncParentChanged:
    NodeMoved(Node);
  end;
end;

procedure TIBTreeView.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and (AComponent = DataSource) then
     DataSource := nil;
end;

procedure TIBTreeView.Reinitialise;
begin
  if [csDesigning,csLoading] * ComponentState <> [] then Exit;
  FLastSelected := GetNodePath(Selected);
  Items.Clear;
end;

constructor TIBTreeView.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TIBTreeViewDatalink.Create(self);
  FIBTreeViewControlLink := TIBTreeViewControlLink.Create(self);
end;

destructor TIBTreeView.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  if assigned(FIBTreeViewControlLink) then FIBTreeViewControlLink.Free;
  inherited Destroy;
end;

function TIBTreeView.FindNode(KeyValuePath: array of variant;
  SelectNode: boolean): TIBTreeNode;
var Node: TTreeNode;
    i,j: integer;
begin
  Result := nil;
  FLocatingNode := true;
  try
   for j := 0 to Items.TopLvlCount - 1 do
   begin
    Node := Items.TopLvlItems[j];
    i := 0;
    Node.Expand(false);
    while assigned(Node)  do
    begin
      if TIBTreeNode(Node).KeyValue = KeyValuePath[i] then
      begin
        Inc(i);
        if i = Length(KeyValuePath) then
        begin
          Result := TIBTreeNode(Node);
          if SelectNode then
             Selected := Node;
          Exit
        end
        else
        begin
          Node.Expand(false);
          Node := Node.GetFirstChild;
        end
      end
      else
        Node := Node.GetNextSibling
    end
   end
  finally
    FLocatingNode := false
  end
end;

function TIBTreeView.FindNode(KeyValue: variant): TIBTreeNode;
var i: integer;
begin
  Result := nil;
  if (Selected <> nil) and (TIBTreeNode(Selected).KeyValue = KeyValue) then
     Result := TIBTreeNode(Selected)
  else
  {Find it the hard way}
  begin
    FullExpand;
    for i := 0 to Items.Count -1 do
      if TIBTreeNode(Items[i]).KeyValue = KeyValue then
      begin
        Result := TIBTreeNode(Items[i])
      end;
  end;
end;

function TIBTreeView.GetNodePath(Node: TTreeNode): TVariantArray;
var aParent: TTreeNode;
    i: integer;
begin
  if not assigned(Node) or not (Node is TIBTreeNode) then
     SetLength(Result,0)
  else
  begin
    {Count length of Path}
    i := 1;
    aParent := Node.Parent;
    while (aParent <> nil) do
    begin
        Inc(i);
        aParent := aParent.Parent
    end;

    {Save Path}
    Setlength(Result,i);
    while i > 0 do
    begin
      Dec(i);
      Result[i] := TIBTreeNode(Node).KeyValue;
      Node := Node.Parent
    end;
  end;
end;

{ TIBTreeViewDatalink }

procedure TIBTreeViewDatalink.ActiveChanged;
begin
  FOwner.ActiveChanged(self)
end;

procedure TIBTreeViewDatalink.DataSetChanged;
begin
  FOwner.DataSetChanged(self)
end;

procedure TIBTreeViewDatalink.RecordChanged(Field: TField);
begin
  FOwner.RecordChanged(self,Field);
end;

procedure TIBTreeViewDatalink.UpdateData;
begin
  FOwner.UpdateData(self)
end;

constructor TIBTreeViewDatalink.Create(AOwner: TIBTreeView);
begin
  inherited Create;
  FOwner := AOwner
end;

end.
