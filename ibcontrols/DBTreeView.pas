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
unit DBTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DB;

type
  {
    TDBTreeView is intended to be a data aware descendent of TCustomTreeView and used to display
    hierarchically structured data in a natural manner. Nodes can be deleted, moved
    and added to the tree and each change is reflected in the underlying dataset. The
    Node text can similarly be edited.
  }

  TVariantArray = array of variant;

  TDBTreeView = class;

  { TDBTreeViewDatalink }

  TDBTreeViewDatalink = class(TDataLink)
  private
    FOwner: TDBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TDBTreeView);
  end;

  { TDBTreeNode }

  TDBTreeNode = class(TTreeNode)
  private
    FKeyValue: variant;
  public
    constructor Create(AnOwner: TTreeNodes); override;
    procedure DeleteAll;
    property KeyValue: variant read FKeyValue;
  end;

  TDBTreeView = class(TCustomTreeView)
  private
    { Private declarations }
    FDataLink: TDBTreeViewDatalink;
    FHasChildField: string;
    FImageIndexField: string;
    FKeyField: string;
    FSelectedIndexField: string;
    FTextField: string;
    FParentField: string;
    FNoAddNodeToDataset: boolean;
    FRelationName: string;
    FModifiedNode: TDBTreeNode;
    FLastSelected: TVariantArray;
    procedure ActiveChanged(Sender: TObject);
    procedure AddNodes;
    procedure DataSetChanged(Sender: TObject);
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetSelectedKeyValue: variant;
    procedure NodeMoved(Node: TTreeNode);
    procedure RecordChanged(Sender: TObject; Field: TField);
    procedure SetHasChildField(AValue: string);
    procedure SetImageIndexField(AValue: string);
    procedure SetKeyField(AValue: string);
    procedure SetSelectedIndexField(AValue: string);
    procedure SetTextField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetParentField(AValue: string);
    procedure UpdateData(Sender: TObject);
   protected
     { Protected declarations }
     FExpandNode: TTreeNode;
     FUpdateNode: TDBTreeNode;
     FUpdating: boolean;
     FLocatingNode: boolean;
     procedure Added(Node: TTreeNode); override;
     procedure DataSourceChanged; virtual; abstract;
     procedure DoOnDataInit; virtual;
     procedure Delete(Node: TTreeNode); override;
     procedure Change(Node: TTreeNode); override;
     function CreateNode: TTreeNode; override;
     function CanEdit(Node: TTreeNode): Boolean; override;
     procedure Expand(Node: TTreeNode); override;
     function GetRelationNameQualifier: string;
     procedure Loaded; override;
     procedure NodeChanged(Node: TTreeNode; ChangeEvent: TTreeNodeChangeReason); override;
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
     procedure NodeUpdated(Node: TTreeNode); virtual;
     procedure Reinitialise;
     procedure RefreshDataset; virtual; abstract;
     function ScrollToNode(Node: TDBTreeNode): boolean; virtual;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    function FindNode(KeyValuePath: TVariantArray; SelectNode: boolean): TDBTreeNode; overload;
    function FindNode(KeyValue: variant): TDBTreeNode; overload;
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
    property ImageIndexField: string read FImageIndexField write SetImageIndexField;
    property SelectedIndexField: string read FSelectedIndexField write SetSelectedIndexField;
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

uses Variants;

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

{ TDBTreeNode }

constructor TDBTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);
  FKeyValue := NULL;
end;

procedure TDBTreeNode.DeleteAll;
var Node, NextNode: TTreeNode;
begin
    Expand(true);
    Node := GetFirstChild;
    while Node <> nil do
    begin
      NextNode := Node.GetNextSibling;
      TDBTreeNode(Node).DeleteAll;
      Node := NextNode;
    end;
    Delete
end;

{ TDBTreeView }

procedure TDBTreeView.ActiveChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) then Exit;
  DataSourceChanged;
  if assigned(DataSet) and not DataSet.Active then
  begin
    if not assigned(FExpandNode) and not assigned(FUpdateNode) then {must really be closing}
      Reinitialise
  end
  else
  begin
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

procedure TDBTreeView.AddNodes;
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
        if (FExpandNode = nil) or (TDBTreeNode(FExpandNode).KeyValue <> DataSet.FieldByName(KeyField).AsVariant) then
        begin
          Node := Items.AddChild(FExpandNode,DataSet.FieldByName(TextField).AsString);
          Node.HasChildren := (HasChildField = '') or (DataSet.FieldByName(HasChildField).AsInteger <> 0);
          Inc(ChildCount);
        end;
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

procedure TDBTreeView.DataSetChanged(Sender: TObject);
begin
//  Do nothing;
end;

function TDBTreeView.GetDataSet: TDataSet;
begin
  Result := FDataLink.DataSet
end;

function TDBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource
end;

function TDBTreeView.GetRelationNameQualifier: string;
begin
  if FRelationName <> '' then
     Result := FRelationName + '.'
  else
    Result := ''
end;

function TDBTreeView.GetSelectedKeyValue: variant;
begin
  Result := NULL;
  if assigned(Selected) and (Selected is TDBTreeNode) then
     Result := TDBTreeNode(Selected).KeyValue
end;

procedure TDBTreeView.NodeMoved(Node: TTreeNode);
begin
  {Need to update Parent}
  if ScrollToNode(TDBTreeNode(Node))  then
  begin
      FDataLink.Edit;
      FModifiedNode := TDBTreeNode(Node)
  end;
end;

procedure TDBTreeView.NodeUpdated(Node: TTreeNode);
begin
  {Need to Update List Field}
  if ScrollToNode(TDBTreeNode(Node)) then
  begin
    FDataLink.Edit;
    FModifiedNode := TDBTreeNode(Node);
    FDataLink.UpdateRecord
  end;
end;

procedure TDBTreeView.RecordChanged(Sender: TObject; Field: TField);
var Node: TDBTreeNode;
    Destination: TDBTreeNode;
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
  if assigned(Field) and (Field.FieldName = ImageIndexField) then
  begin
    Node := FindNode(DataSet.FieldByName(KeyField).AsVariant);
    if assigned(Node) then
    begin
      FUpdating := true;
      try
        Node.ImageIndex := Field.AsInteger
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

procedure TDBTreeView.SetHasChildField(AValue: string);
begin
  if FHasChildField = AValue then Exit;
  FHasChildField := AValue;
  Reinitialise
end;

procedure TDBTreeView.SetImageIndexField(AValue: string);
begin
  if FImageIndexField = AValue then Exit;
  FImageIndexField := AValue;
  Reinitialise
end;

procedure TDBTreeView.SetKeyField(AValue: string);
begin
  if FKeyField = AValue then Exit;
  FKeyField := AValue;
  Reinitialise
end;

procedure TDBTreeView.SetSelectedIndexField(AValue: string);
begin
  if FSelectedIndexField = AValue then Exit;
  FSelectedIndexField := AValue;
  Reinitialise;
end;

procedure TDBTreeView.SetTextField(AValue: string);
begin
  if FTextField = AValue then Exit;
  FTextField := AValue;
  Reinitialise
end;

procedure TDBTreeView.SetDataSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  DataSourceChanged;
end;

procedure TDBTreeView.SetParentField(AValue: string);
begin
  if FParentField = AValue then Exit;
  FParentField := AValue;
  Reinitialise
end;

function TDBTreeView.ScrollToNode(Node: TDBTreeNode): boolean;
begin
  Result :=  assigned(DataSet) and DataSet.Active and assigned(Node) and not varIsNull(Node.KeyValue);
  if Result then
  begin
    if DataSet.Active and (DataSet.RecordCount > 0)
         and DataSet.Locate(KeyField,Node.KeyValue,[]) then Exit;

    FUpdateNode := Node;
    try
      RefreshDataset;
    finally
      FUpdateNode := nil
    end;
    Result := DataSet.FieldByName(KeyField).AsVariant = Node.KeyValue
  end;
end;

procedure TDBTreeView.UpdateData(Sender: TObject);
begin
  if assigned(FModifiedNode) then
  begin
    DataSet.FieldByName(TextField).AsString := FModifiedNode.Text;
    if FModifiedNode.Parent = nil then
      DataSet.FieldByName(ParentField).Clear
    else
      DataSet.FieldByName(ParentField).AsVariant := TDBTreeNode(FModifiedNode.Parent).KeyValue;
    FModifiedNode := nil
  end
end;

procedure TDBTreeView.Added(Node: TTreeNode);
begin
  if assigned(DataSet) and DataSet.Active then
  begin
    if not FNoAddNodeToDataset then
    begin
      DataSet.Append;
      if (Node.Text = '') and not DataSet.FieldByName(TextField).IsNull then
         Node.Text := DataSet.FieldByName(TextField).AsString;
      FModifiedNode := TDBTreeNode(Node);
      FDataLink.UpdateRecord;
    end;
    DoOnDataInit;
    TDBTreeNode(Node).FKeyValue := DataSet.FieldByName(KeyField).AsVariant;
    if ImageIndexField <> '' then
      Node.ImageIndex := DataSet.FieldByName(ImageIndexField).AsInteger;
    if SelectedIndexField <> '' then
      Node.SelectedIndex := DataSet.FieldByName(SelectedIndexField).AsInteger;
  end;
  inherited Added(Node);
end;

procedure TDBTreeView.DoOnDataInit;
begin
  //Do nothing
end;

procedure TDBTreeView.Delete(Node: TTreeNode);
begin
  if not (tvsUpdating in States) {TreeNodes being cleared}
     and not (tvsManualNotify in States) {Tree Collapse with node delete}
     and ScrollToNode(TDBTreeNode(Node)) then
     DataSet.Delete;
  inherited Delete(Node);
end;

procedure TDBTreeView.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  ScrollToNode(TDBTreeNode(Node));
end;

function TDBTreeView.CreateNode: TTreeNode;
var
  NewNodeClass: TTreeNodeClass;
begin
  Result := nil;
  if Assigned(OnCustomCreateItem) then
    OnCustomCreateItem(Self, Result);
  if Result = nil then
  begin
    NewNodeClass:=TDBTreeNode;
    if Assigned(OnCreateNodeClass) then
      OnCreateNodeClass(Self,NewNodeClass);
    Result := NewNodeClass.Create(Items);
  end;
end;

function TDBTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node)
              and assigned(DataSet) and not DataSet.FieldByName(TextField).ReadOnly
end;

procedure TDBTreeView.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
  if Node.HasChildren and assigned(DataSet) and (Node.GetFirstChild = nil) then
  begin
    FExpandNode := Node;
    RefreshDataset;
    Selected := Node;
  end;
end;

procedure TDBTreeView.Loaded;
begin
  inherited Loaded;
  DataSourceChanged;
  Reinitialise
end;

procedure TDBTreeView.NodeChanged(Node: TTreeNode;
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

procedure TDBTreeView.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and (AComponent = DataSource) then
     DataSource := nil;
end;

procedure TDBTreeView.Reinitialise;
begin
  if [csDesigning,csLoading] * ComponentState <> [] then Exit;
  FLastSelected := GetNodePath(Selected);
  Items.Clear;
end;

constructor TDBTreeView.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TDBTreeViewDatalink.Create(self);
end;

destructor TDBTreeView.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  inherited Destroy;
end;

function TDBTreeView.FindNode(KeyValuePath: TVariantArray; SelectNode: boolean
  ): TDBTreeNode;
var Node: TTreeNode;
    i,j: integer;
begin
  Result := nil;
  if Length(KeyValuePath) = 0 then Exit;

  FLocatingNode := true;
  try
   for j := 0 to Items.TopLvlCount - 1 do
   begin
    Node := Items.TopLvlItems[j];
    i := 0;
    Node.Expand(false);
    while assigned(Node)  do
    begin
      if not VarIsNull(TDBTreeNode(Node).KeyValue) and
                        (TDBTreeNode(Node).KeyValue = KeyValuePath[i]) then
      begin
        Inc(i);
        if i = Length(KeyValuePath) then
        begin
          Result := TDBTreeNode(Node);
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

function TDBTreeView.FindNode(KeyValue: variant): TDBTreeNode;
var i: integer;
begin
  Result := nil;
  if (Selected <> nil) and (TDBTreeNode(Selected).KeyValue = KeyValue) then
     Result := TDBTreeNode(Selected)
  else
  {Find it the hard way}
  begin
    FullExpand;
    for i := 0 to Items.Count -1 do
      if TDBTreeNode(Items[i]).KeyValue = KeyValue then
      begin
        Result := TDBTreeNode(Items[i])
      end;
  end;
end;

function TDBTreeView.GetNodePath(Node: TTreeNode): TVariantArray;
var aParent: TTreeNode;
    i: integer;
begin
  if not assigned(Node) or not (Node is TDBTreeNode) then
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
      Result[i] := TDBTreeNode(Node).KeyValue;
      Node := Node.Parent
    end;
  end;
end;

{ TDBTreeViewDatalink }

procedure TDBTreeViewDatalink.ActiveChanged;
begin
  FOwner.ActiveChanged(self)
end;

procedure TDBTreeViewDatalink.DataSetChanged;
begin
  FOwner.DataSetChanged(self)
end;

procedure TDBTreeViewDatalink.RecordChanged(Field: TField);
begin
  FOwner.RecordChanged(self,Field);
end;

procedure TDBTreeViewDatalink.UpdateData;
begin
  FOwner.UpdateData(self)
end;

constructor TDBTreeViewDatalink.Create(AOwner: TDBTreeView);
begin
  inherited Create;
  FOwner := AOwner
end;

end.
