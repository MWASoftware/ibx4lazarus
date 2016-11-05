unit IBArrayGrid;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  Db, DBCtrls, IBCustomDataSet, IB;

type

  { TIBArrayGrid }

  TIBArrayGrid = class(TCustomStringGrid)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FArray: IArray;
    FActive: boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure LoadGridData(ArrayDimensions: integer; ArrayBounds: TArrayBounds);
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetReadOnly(AValue: Boolean);
    procedure UpdateLayout;
  protected
    { Protected declarations }
    function  EditorIsReadOnly: boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEditText(aCol, aRow: Longint; const aValue: string); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ArrayIntf: IArray read FArray;
    property DataSet: TDataSet read GetDataSet;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property ColCount;
    property Color;
    property ColumnClickSorts;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property MouseWheelOption;
    property Options;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RangeSelectMode;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabAdvance;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;

    property OnBeforeSelection;
    property OnChangeBounds;
    property OnCheckboxToggled;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnDrawCell;
    property OnButtonClick;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellHint;
    property OnGetCheckboxState;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnHeaderSizing;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnResize;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetCheckboxState;
    property OnSetEditText;
    property OnShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;
    property OnValidateEntry;
  end;


implementation

resourcestring
  sArrayDimensionsOutofRange = 'Array Dimensions (%d) out of range';
  sNotAnArrayField        = '%s is not an Array Field';

{ TIBArrayGrid }

procedure TIBArrayGrid.ActiveChange(Sender: TObject);
begin
  try
    if (DataSet <> nil) and DataSet.Active then
    begin
      FActive := true;
      if not (Field is TIBArrayField) then
        raise Exception.CreateFmt(sNotAnArrayField,[Field.Name]);
      UpdateLayout;
      DataChange(Sender);
    end
    else
    begin
      FActive := false;
      FArray := nil;
      Clean;
    end;
  except
    FActive := false;
    raise;
  end;
end;

procedure TIBArrayGrid.DataChange(Sender: TObject);
begin
  if (DataSet <> nil) and DataSet.Active and FActive then
  with TIBArrayField(Field) do
  begin
    FArray := ArrayIntf;
    LoadGridData(ArrayDimensions,ArrayBounds);
  end;
end;

function TIBArrayGrid.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TIBArrayGrid.GetDataSet: TDataSet;
begin
  Result := FDataLink.DataSet;
end;

function TIBArrayGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TIBArrayGrid.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TIBArrayGrid.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TIBArrayGrid.LoadGridData(ArrayDimensions: integer;
  ArrayBounds: TArrayBounds);
var i, j, k, l: integer;
begin
  if FArray = nil then Exit;
  case ArrayDimensions of
  1:
    begin
      if RowCount <> 1 then
         raise Exception.CreateFmt(sArrayDimensionsOutofRange,[ArrayDimensions]);

      with ArrayBounds[0] do
      for i := LowerBound to UpperBound do
        if (i - LowerBound >= 0) and (i - LowerBound < ColCount) then
          Cells[i - LowerBound,0] := FArray.GetAsString([i]);
    end;

  2:
    begin
      with ArrayBounds[0] do
      for i := LowerBound to UpperBound do
      begin
        k := i - LowerBound + FixedCols;
        if (k >= 0) and (k < ColCount) then
        begin
          with ArrayBounds[1] do
          for j := LowerBound to UpperBound do
          begin
            l := j - LowerBound + FixedRows;
            if ( l >= 0) and (l < RowCount) then
              Cells[k,l] := FArray.GetAsString([i,j]);
          end;
        end;
      end;
    end;

  else
     raise Exception.CreateFmt(sArrayDimensionsOutofRange,[ArrayDimensions]);
  end;
end;

procedure TIBArrayGrid.SetDataField(AValue: string);
begin
  FDataLink.FieldName := AValue;
  if csDesigning in ComponentState then
    UpdateLayout;
end;

procedure TIBArrayGrid.SetDataSource(AValue: TDataSource);
begin
  if FDataLink.DataSource = AValue then exit;
  if FDataLink.DataSource <> nil then
    FDataLink.DataSource.RemoveFreeNotification(self);
  FDataLink.DataSource := AValue;
  if FDataLink.DataSource <> nil then
    FDataLink.DataSource.FreeNotification(self);
  if csDesigning in ComponentState then
    UpdateLayout;
end;

procedure TIBArrayGrid.SetReadOnly(AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

procedure TIBArrayGrid.UpdateLayout;
var i: integer;
begin
  if csLoading in ComponentState then Exit;
  if (DataSource <> nil) and (DataSet <> nil) and (DataField <> '') then
  try
    for i := 0 to DataSet.FieldDefs.Count - 1 do
    if (DataSet.FieldDefs[i] <> nil) and (DataSet.FieldDefs[i].Name = DataField)
       and (DataSet.FieldDefs[i] is TIBFieldDef) and (DataSet.FieldDefs[i].DataType = ftArray) then
    with TIBFieldDef(DataSet.FieldDefs[i]) do
    begin
      case ArrayDimensions of
      1:
        RowCount := 1 + FixedRows;

      2:
        with ArrayBounds[1] do
          RowCount := UpperBound - LowerBound + 1 + FixedRows;

      else
        raise Exception.CreateFmt(sArrayDimensionsOutofRange,[ArrayDimensions]);
      end;
      with ArrayBounds[0] do
        ColCount := UpperBound - LowerBound + 1 + FixedCols;
      Exit;
    end;
    raise Exception.CreateFmt(sNotAnArrayField,[DataField]);
  except
    DataField := '';
    raise;
  end;
end;

function TIBArrayGrid.EditorIsReadOnly: boolean;
begin
  Result := FActive and inherited EditorIsReadOnly;

  if not Result then
  begin
    if assigned(Field) then
    begin
      // if field can't be modified, it's assumed readonly
      result := not Field.CanModify;

      // if it's not readonly and is not already editing, start editing.
       if not result and not FDatalink.Editing then
         Result := not FDataLink.Edit;
     end
     else
       result := true;  // field is nil so it's readonly
  end;
end;

procedure TIBArrayGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource:=nil;
  end;
end;

procedure TIBArrayGrid.SetEditText(aCol, aRow: Longint; const aValue: string);
var k, l: integer;
begin
  inherited SetEditText(aCol, aRow, aValue);
  if not EditorIsReadOnly then
  begin
    FDataLink.Modified;
    if FArray = nil then Exit;
    with TIBArrayField(Field) do
    begin
      k := aCol + ArrayBounds[0].LowerBound - FixedCols;
      if ArrayDimensions = 1 then
        FArray.SetAsString([k],aValue)
      else
      begin
        l := aRow + ArrayBounds[1].LowerBound - FixedRows;
        FArray.SetAsString([k,l],aValue);
      end;
    end;
  end;
end;

constructor TIBArrayGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnActiveChange := @ActiveChange;
end;

destructor TIBArrayGrid.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  inherited Destroy;
end;

end.
