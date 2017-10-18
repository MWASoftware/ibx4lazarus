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
unit IBArrayGrid;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  Db, DBCtrls, IBCustomDataSet, IB;

type

(*
 This is a data aware control derived from TCustomStringGrid and which may be used
 to display/edit the contents of a one or two dimensional Firebird array.

 Firebird Arrays are defined in the same way as any other database column e.g.

  Alter Table MyData (
    ...
    MyArray VarChar(16) [0:16, -1:7] Character Set UTF8
  );

  An array may have a different set of values for each row. In the above example,
  a two dimensional array of strings is defined. The first index may vary from
  0 to 16 and the second from -1 to 7.

  IBX defines the TField descendent 'TIBArrayField' and this may be used to access
  the array element in each row of a query/table when using TQuery, TIBDataSet or
  TIBTable. The SQL used to select/update/insert tables with array data is the
  same as for any other SQL type. In any given row, the array field may be null or
  have a set of values. Note that individual array elements cannot themselves be null.

  TIBArrayGrid is a visual control that can be linked to a TIBArrayField and used
  to display/edit the contents of a one or two dimensional Firebird array.  It may be
  found in the “Firebird Data Controls” palette.

  To use a TIBArrayGrid, simply drop it onto a form and set the DataSource property
  to the source dataset and the DataField property to the name of an array field.
  The grid should then be automatically sized to match the dimensions of the array.
  Note that the array bounds can be refreshed at any time in the IDE, by right clicking
  on the control and selecting "Update Layout" from the pop up menu.

  At runtime, the TIBArrayGrid will always display/edit the value of the array element
  in the current row. If this element is null then the array is empty. However,
  data can be inserted into the array. When the row is posted, the field will be
  set to the new/updated array.

  Properties
  ==========

  Most TIBArrayGrid properties are the same as for TStringGrid. The following
  are specific to TIBArrayGrid. Note that you cannot set the Row or column counts
  directly as these are always set to match the array field.

  Public:
    ArrayIntf:                 Provides direct access to the array itself.
    DataSet:                   The DataSet provided by the DataSource (read only).
    Field:                     The source field

  Published:
    DataField:                 name of array column.
    DataSource:                The data source providing the source table.
    ReadOnly:                  Set to true to prevent editing
    ColumnLabels:              A string list that provides the labels for each
                               column in the grid. Provide one line per column.
                               If non empty then a column label row is created.
    ColumnLabelAlignment:      Sets the text alignment for column Labels
    ColumnLabelFont:           Sets the font used for column labels
    RowLabels:                 A string list that provides the labels for each
                               row in the grid. Provide one line per row.
                               If non empty then a row label row is created.
    RowLabelAlignment:         Sets the text alignment for row Labels
    RowLabelFont:              Sets the font used for row labels
    RowLabelColumnWidth:       Width of the Fixed Column used for row labels.
    TextAlignment:             Alignment of all cells other that those containing
                               labels.
 *)

  { TIBArrayGrid }

  TIBArrayGrid = class(TCustomStringGrid)
  private
    { Private declarations }
    FColumnLabelAlignment: TAlignment;
    FColumnLabelFont: TFont;
    FDataLink: TFieldDataLink;
    FArray: IArray;
    FActive: boolean;
    FRowLabelAlignment: TAlignment;
    FRowLabelColumnWidth: integer;
    FRowLabelFont: TFont;
    FRowLabels: TStrings;
    FColumnLabels: TStrings;
    FTextAlignment: TAlignment;
    FTextChanged: boolean;
    procedure ActiveChange(Sender: TObject);
    procedure ColumnLabelChanged(Sender: TObject);
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure LoadGridData(ArrayDimensions: integer; ArrayBounds: TArrayBounds);
    procedure ReadColCount(Reader: TReader);
    procedure ReadRowCount(Reader: TReader);
    procedure RowLabelChanged(Sender: TObject);
    procedure SetColumnLabelAlignment(AValue: TAlignment);
    procedure SetColumnLabelFont(AValue: TFont);
    procedure SetColumnLabels(AValue: TStrings);
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetRowLabelAlignment(AValue: TAlignment);
    procedure SetRowLabelColumnWidth(AValue: integer);
    procedure SetRowLabelFont(AValue: TFont);
    procedure SetRowLabels(AValue: TStrings);
    procedure UpdateLabels;
    procedure UpdateData(Sender: TObject);
    procedure WriteColCount(Writer: TWriter);
    procedure WriteRowCount(Writer: TWriter);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefineCellsProperty(Filer: TFiler); override;
    procedure DrawCellText(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState; aText: String); override;
    procedure EditorHide; override;
    function  EditorIsReadOnly: boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetSizes; override;
    procedure SetEditText(aCol, aRow: Longint; const aValue: string); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateLayout;
    property ArrayIntf: IArray read FArray;
    property DataSet: TDataSet read GetDataSet;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ColumnLabelAlignment: TAlignment read FColumnLabelAlignment
                        write SetColumnLabelAlignment default taCenter;
    property ColumnLabels: TStrings read FColumnLabels write SetColumnLabels;
    property ColumnLabelFont: TFont read FColumnLabelFont write SetColumnLabelFont;
    property RowLabels: TStrings read FRowLabels write SetRowLabels;
    property RowLabelAlignment: TAlignment read FRowLabelAlignment
                                      write SetRowLabelAlignment default taLeftJustify;
    property RowLabelFont: TFont read FRowLabelFont write SetRowLabelFont;
    property RowLabelColumnWidth: integer read FRowLabelColumnWidth write SetRowLabelColumnWidth;
    property TextAlignment:TAlignment read FTextAlignment
                                      write FTextAlignment default taLeftJustify;
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
      if Field = nil then
        raise Exception.CreateFmt(sNotAnArrayField,['Unknown']);
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

procedure TIBArrayGrid.ColumnLabelChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  if FColumnLabels.Count > 0 then
  begin
    FixedRows := 1;
    RowCount := RowCount + FixedRows;
  end
  else
  begin
    RowCount := RowCount - FixedRows;
    FixedRows := 0;
  end;
  UpdateLabels;
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
  if (FArray = nil) or (FDataLink.Editing and not FArray.IsEmpty)
     then Exit;
  case ArrayDimensions of
  1:
    begin
      if RowCount - FixedRows  <> 1 then
         raise Exception.CreateFmt(sArrayDimensionsOutofRange,[ArrayDimensions]);

      with ArrayBounds[0] do
      for i := LowerBound to UpperBound do
        if (i - LowerBound >= 0) and (i - LowerBound < ColCount) then
          Cells[i - LowerBound,FixedRows] := FArray.GetAsString([i]);
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

procedure TIBArrayGrid.ReadColCount(Reader: TReader);
begin
  ColCount := Reader.ReadInteger;
end;

procedure TIBArrayGrid.ReadRowCount(Reader: TReader);
begin
  RowCount := Reader.ReadInteger;
end;

procedure TIBArrayGrid.RowLabelChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  if FRowLabels.Count > 0 then
  begin
    FixedCols := 1;
    ColCount := ColCount + FixedCols;
  end
  else
  begin
    if ColCount >= FixedCols then
      ColCount := ColCount - FixedCols;
    FixedCols := 0;
  end;
  UpdateLabels;
end;

procedure TIBArrayGrid.SetColumnLabelAlignment(AValue: TAlignment);
begin
  if FColumnLabelAlignment = AValue then Exit;
  FColumnLabelAlignment := AValue;
  UpdateLabels;
end;

procedure TIBArrayGrid.SetColumnLabelFont(AValue: TFont);
begin
  if FColumnLabelFont = AValue then Exit;
  FColumnLabelFont.Assign(AValue);
  Invalidate;
end;

procedure TIBArrayGrid.SetColumnLabels(AValue: TStrings);
begin
  if FColumnLabels <> AValue then
    FColumnLabels.Assign(AValue);
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

procedure TIBArrayGrid.SetRowLabelAlignment(AValue: TAlignment);
begin
  if FRowLabelAlignment = AValue then Exit;
  FRowLabelAlignment := AValue;
  UpdateLabels;
end;

procedure TIBArrayGrid.SetRowLabelColumnWidth(AValue: integer);
begin
  if FRowLabelColumnWidth = AValue then Exit;
  FRowLabelColumnWidth := AValue;
  if (csLoading in ComponentState) or (FixedCols = 0) then Exit;
  ColWidths[0] := AValue;
  Invalidate;
end;

procedure TIBArrayGrid.SetRowLabelFont(AValue: TFont);
begin
  if FRowLabelFont = AValue then Exit;
  FRowLabelFont.Assign(AValue);
  Invalidate;
end;

procedure TIBArrayGrid.SetRowLabels(AValue: TStrings);
begin
  if FRowLabels <> AValue then
    FRowLabels.Assign(AValue);
end;

procedure TIBArrayGrid.UpdateLabels;
var i: integer;
begin
  Clean;
  for i := 0 to FColumnLabels.Count - 1 do
    if i < ColCount - FixedCols then
      Cells[i+FixedCols,0] := FColumnLabels[i];

  for i := 0 to FRowLabels.Count - 1 do
    if i < RowCount - FixedRows then
      Cells[0,i+FixedRows] := FRowLabels[i];
end;

procedure TIBArrayGrid.UpdateData(Sender: TObject);
begin
  EditorHide;
end;

procedure TIBArrayGrid.UpdateLayout;
var i: integer;
begin
  if csLoading in ComponentState then Exit;
  if (DataSource <> nil) and (DataSet <> nil) and (DataField <> '') then
  try
    ResetDefaultColWidths;
    DataSet.FieldDefs.Update;
    if DataSet.FieldDefs.Count > 0 then
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
      UpdateLabels;
      Exit;
    end;
    raise Exception.CreateFmt(sNotAnArrayField,[DataField]);
  except
    DataField := '';
    raise;
  end;
end;

procedure TIBArrayGrid.WriteColCount(Writer: TWriter);
begin
  Writer.WriteInteger(ColCount);
end;

procedure TIBArrayGrid.WriteRowCount(Writer: TWriter);
begin
  Writer.WriteInteger(RowCount);
end;

procedure TIBArrayGrid.DefineProperties(Filer: TFiler);
begin
  with Filer do
  begin
    DefineProperty('ColCount',  @ReadColCount,  @WriteColCount,  true);
    DefineProperty('RowCount',  @ReadRowCount,  @WriteRowCount,  true);
  end;
  inherited DefineProperties(Filer);
end;

procedure TIBArrayGrid.DefineCellsProperty(Filer: TFiler);
begin
  //Do Nothing
end;

procedure TIBArrayGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState; aText: String);
var Style: TTextStyle;
    oldAlignment: TAlignment;
begin
  Style := Canvas.TextStyle;
  oldAlignment := Style.Alignment;
  if (aRow < FixedRows) then
  begin
    Style.Alignment := ColumnLabelAlignment;
    Canvas.Font.Assign(ColumnLabelFont);
  end
  else
  if aCol < FixedCols then
  begin
    Style.Alignment := RowLabelAlignment;
    Canvas.Font.Assign(RowLabelFont);
  end
  else
    Style.Alignment := TextAlignment;
  Canvas.TextStyle := Style;
  try
    inherited DrawCellText(aCol, aRow, aRect, aState, aText);
  finally
    Style.Alignment := oldAlignment;
    Canvas.TextStyle := Style;
  end;
end;

procedure TIBArrayGrid.EditorHide;
var k, l: integer;
begin
  inherited EditorHide;
  try
    if not FTextChanged or (FArray = nil) then Exit;

    with TIBArrayField(Field) do
    begin
      k := Col + ArrayBounds[0].LowerBound - FixedCols;
      if ArrayDimensions = 1 then
      try
        FArray.SetAsString([k],Cells[Col,Row])
      except
        Cells[Col,Row] := FArray.GetAsString([k]);
        raise;
      end
      else
      try
        l := Row + ArrayBounds[1].LowerBound - FixedRows;
        FArray.SetAsString([k,l],Cells[Col,Row]);
      except
        Cells[Col,Row] := FArray.GetAsString([k,l]);
        raise;
      end;
  end;
  finally
    FTextChanged := false;
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

procedure TIBArrayGrid.Loaded;
begin
  inherited Loaded;
  RowLabelChanged(nil);
  ColumnLabelChanged(nil);
  UpdateLabels;
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

procedure TIBArrayGrid.ResetSizes;
begin
  inherited ResetSizes;
  if FixedCols > 0 then
  begin
    ColWidths[0] := RowLabelColumnWidth;
    VisualChange;
  end;
end;

procedure TIBArrayGrid.SetEditText(aCol, aRow: Longint; const aValue: string);
begin
  inherited SetEditText(aCol, aRow, aValue);
  if not EditorIsReadOnly then
  begin
    FDataLink.Modified;
    FTextChanged := true;
  end;
end;

constructor TIBArrayGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnUpdateData := @UpdateData;
  FRowLabels := TStringList.Create;
  TStringList(FRowLabels).OnChange := @RowLabelChanged;
  FColumnLabels := TStringList.Create;
  TStringList(FColumnLabels).OnChange := @ColumnLabelChanged;
  FixedRows := 0;
  FixedCols := 0;
  FColumnLabelAlignment := taCenter;
  FTextAlignment := taLeftJustify;
  FRowLabelAlignment := taLeftJustify;
  FRowLabelFont := TFont.Create;
  FColumnLabelFont := TFont.Create;
  FRowLabelColumnWidth := DefaultColWidth;
  Options := Options + [goEditing];
end;

destructor TIBArrayGrid.Destroy;
begin
  if assigned(FColumnLabelFont) then FColumnLabelFont.Free;
  if assigned(FRowLabelFont) then FRowLabelFont.Free;
  if assigned(FColumnLabels) then FColumnLabels.Free;
  if assigned(FRowLabels) then FRowLabels.Free;
  if assigned(FDataLink) then FDataLink.Free;
  inherited Destroy;
end;

end.
