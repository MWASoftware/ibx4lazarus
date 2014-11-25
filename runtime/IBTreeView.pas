unit IBTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DB, IBSQLParserUnit;

type
  TVariantArray = array of variant;

  TIBTreeView = class;

  { TIBTreeViewDatalink }

  TIBTreeViewDatalink = class(TDataLink)
  private
    FOwner: TIBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
    procedure DataSetChanged; override;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TIBTreeView);
  end;

  TIBTreeView = class(TCustomTreeView)
  private
    { Private declarations }
    FDataLink: TIBTreeViewDatalink;
    FHasChildField: string;
    FKeyField: string;
    FListField: string;
    FParentField: string;
    FExpandNode: TTreeNode;
    FRootNode: TTreeNode;
    FRootNodeTitle: string;
    procedure ActiveChanged(Sender: TObject);
    procedure AddNodes;
    procedure DataSetChanged(Sender: TObject);
    function GetDataSet: TDataSet;
    function GetListSource: TDataSource;
    function GetSelectedKeyValue: variant;
    procedure SetHasChildField(AValue: string);
    procedure SetKeyField(AValue: string);
    procedure SetListField(AValue: string);
    procedure SetListSource(AValue: TDataSource);
    procedure SetParentField(AValue: string);
    procedure SetRootNodeTitle(AValue: string);
    procedure UpdateData(Sender: TObject);
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
   protected
    { Protected declarations }
     function CanEdit(Node: TTreeNode): Boolean; override;
     procedure Delete(Node: TTreeNode); override;
     procedure EditorEditingDone(Sender: TObject); override;
     procedure Expand(Node: TTreeNode); override;
     procedure Loaded; override;
     procedure Reinitialise;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    function FindNode(KeyValuePath: array of variant; SelectNode: boolean): TTreeNode;
    function GetSelectNodePath: TVariantArray;
    property DataSet: TDataSet read GetDataSet;
    property RootNode: TTreeNode read FRootNode;
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
    property ListField: string read FListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property MultiSelect;
    property MultiSelectStyle;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentField: string read FParentField write SetParentField;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RootNodeTitle: string read FRootNodeTitle write SetRootNodeTitle;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
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

uses IBQuery,IBCustomDataSet, Variants;

type

  { TNodeInfo }

  TNodeInfo = class
  private
    FKeyValue: variant;
  public
    property KeyValue: variant read FKeyValue;
    constructor Create(aKeyValue: variant);
  end;

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

{ TIBTreeView }

procedure TIBTreeView.ActiveChanged(Sender: TObject);
begin
  if assigned(DataSet) and not DataSet.Active then
  begin
    if not assigned(FExpandNode) then {must really be closing}
      Reinitialise
  end
  else
    AddNodes;
end;

procedure TIBTreeView.AddNodes;
var Node: TTreeNode;
begin
  DataSet.First;
  if assigned(FExpandNode) then
  begin
    while not DataSet.EOF do
    begin
      Node := Items.AddChild(FExpandNode,DataSet.FieldByName(ListField).AsString);
      Node.Data := TNodeInfo.Create(DataSet.FieldByName(KeyField).AsVariant);
      Node.HasChildren := DataSet.FieldByName(HasChildField).AsInteger <> 0;
      DataSet.Next
    end;
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

function TIBTreeView.GetListSource: TDataSource;
begin
  Result := FDataLink.DataSource
end;

function TIBTreeView.GetSelectedKeyValue: variant;
begin
  Result := NULL;
  if assigned(Selected) and assigned(Selected.Data) then
     Result := TNodeInfo(Selected.Data).KeyValue
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

procedure TIBTreeView.SetListField(AValue: string);
begin
  if FListField = AValue then Exit;
  FListField := AValue;
  Reinitialise
end;

procedure TIBTreeView.SetListSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue
end;

procedure TIBTreeView.SetParentField(AValue: string);
begin
  if FParentField = AValue then Exit;
  FParentField := AValue;
  Reinitialise
end;

procedure TIBTreeView.SetRootNodeTitle(AValue: string);
begin
  if FRootNodeTitle = AValue then Exit;
  FRootNodeTitle := AValue;
  Reinitialise
end;

procedure TIBTreeView.UpdateData(Sender: TObject);
begin
  if assigned(Selected) then
    DataSet.FieldByName(ListField).AsString := Selected.Text;
end;

procedure TIBTreeView.UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
begin
  if not assigned(FExpandNode) then Exit; {avoid exception}
  Parser.ResetWhereClause;
  if not assigned(FExpandNode) and assigned(Selected) then {Scrolling dataset}
  begin
    Parser.Add2WhereClause(FKeyField + ' = :IBX_KEY_VALUE');
    if (Sender as TDataLink).DataSet is TIBQuery then
      TIBQuery((Sender as TDataLink).DataSet).ParamByName('IBX_KEY_VALUE').Value :=
        TNodeInfo(Selected.Data).KeyValue
    else
    if (Sender as TDataLink).DataSet is TIBDataSet then
      TIBDataSet((Sender as TDataLink).DataSet).ParamByName('IBX_KEY_VALUE').Value :=
        TNodeInfo(Selected.Data).KeyValue
  end
  else
  if FExpandNode = FRootNode then
    {Need to Load Root Nodes}
    Parser.Add2WhereClause(FParentField + ' is null')
  else
  begin
    Parser.Add2WhereClause(FParentField + ' = :IBX_PARENT_VALUE');
    if (Sender as TDataLink).DataSet is TIBQuery then
      TIBQuery((Sender as TDataLink).DataSet).ParamByName('IBX_PARENT_VALUE').Value :=
        TNodeInfo(FExpandNode.Data).KeyValue
    else
    if (Sender as TDataLink).DataSet is TIBDataSet then
      TIBDataSet((Sender as TDataLink).DataSet).ParamByName('IBX_PARENT_VALUE').Value :=
        TNodeInfo(FExpandNode.Data).KeyValue
  end;
end;

function TIBTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node) and (Node <> RootNode)
              and assigned(DataSet) and not DataSet.FieldByName(ListField).ReadOnly
end;

procedure TIBTreeView.Delete(Node: TTreeNode);
begin
  inherited Delete(Node);
  if assigned(Node.Data) and (TObject(Node.Data) is TNodeInfo) then
    TNodeInfo(Node.Data).Free
end;

procedure TIBTreeView.EditorEditingDone(Sender: TObject);
begin
  {Ideally, we woold have done these checks before editing began
   as well as put the dataset in the correct mode - but "BeginEditing"
   is not virtual....}
  if assigned(DataSet) then
  begin
    {Scroll the DataSet to the edited item}
    DataSet.Active := false;
    DataSet.Active := true;
    if DataSet.RecordCount > 1 then
       raise Exception.Create('Record Key is ambiguous')
  end;
  inherited EditorEditingDone(Sender);
  {Now Update the DataSet}
  if assigned(DataSet) and assigned(Selected) then
  begin
    if not assigned(Selected.Data) then
    begin
       DataSet.Append;
       Selected.Data := TNodeInfo.Create(DataSet.FieldByName(KeyField).AsVariant)
    end
    else
      DataSet.Edit;
    try
      DataSet.Post
    except
      DataSet.Cancel;
      raise
    end
  end
end;

procedure TIBTreeView.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
  if Node.HasChildren and assigned(DataSet) and (Node.GetFirstChild = nil) then
  begin
    FExpandNode := Node;
    DataSet.Active := false;
    DataSet.Active := true
  end;
end;

procedure TIBTreeView.Loaded;
begin
  inherited Loaded;
  Reinitialise
end;

procedure TIBTreeView.Reinitialise;
begin
  if csLoading in ComponentState then Exit;
  Items.Clear;
  FRootNode := nil;
  if assigned(DataSet) then
     DataSet.Active := false;
  FRootNode := Items.Add(nil,FRootNodeTitle);
  FRootNode.HasChildren := assigned(DataSet)
end;

constructor TIBTreeView.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TIBTreeViewDatalink.Create(self);
end;

destructor TIBTreeView.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  inherited Destroy;
end;

function TIBTreeView.FindNode(KeyValuePath: array of variant;
  SelectNode: boolean): TTreeNode;
var Node: TTreeNode;
    i: integer;
begin
  Result := nil;
  Node := FRootNode;
  i := 0;
  Node.Expand(false);
  Node := Node.GetFirstChild;
  while assigned(Node) and assigned(Node.Data) do
  begin
      if TNodeInfo(Node.Data).KeyValue = KeyValuePath[i] then
      begin
        Inc(i);
        if i = Length(KeyValuePath) then
        begin
          Result := Node;
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
end;

function TIBTreeView.GetSelectNodePath: TVariantArray;
var Node: TTreeNode;
    i: integer;
begin
  if not assigned(Selected) then
     SetLength(Result,0)
  else
  begin
    {Count length of Path}
    i := 1;
    Node := Selected.Parent;
    while (Node <> nil) and (Node.Data <> nil) do
    begin
        Inc(i);
        Node := Node.Parent
    end;

    {Save Path}
    Setlength(Result,i);
    Node := Selected;
    while i > 0 do
    begin
      Dec(i);
      Result[i] := TNodeInfo(Node.Data).KeyValue;
      Node := Node.Parent
    end;
  end;
end;

{ TNodeInfo }

constructor TNodeInfo.Create(aKeyValue: variant);
begin
  FKeyValue := aKeyValue
end;

{ TIBTreeViewDatalink }

procedure TIBTreeViewDatalink.ActiveChanged;
begin
  FOwner.ActiveChanged(self)
end;

procedure TIBTreeViewDatalink.DataEvent(Event: TDataEvent; Info: Ptrint);
begin
  if (Event = deCheckBrowseMode) and (Info = 1) and not DataSet.Active then
  begin
    if (DataSet is TIBDataSet) then
      FOwner.UpdateSQL(self,TIBDataSet(DataSet).Parser)
    else
    if (DataSet is TIBQuery) then
      FOwner.UpdateSQL(self,TIBQuery(DataSet).Parser)
  end
  else
    inherited DataEvent(Event, Info);
end;

procedure TIBTreeViewDatalink.DataSetChanged;
begin
  FOwner.DataSetChanged(self)
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
