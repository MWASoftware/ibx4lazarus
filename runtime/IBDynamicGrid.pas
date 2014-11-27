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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBDynamicGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids, DB,
  IBSqlParserUnit, Grids, IBLookupComboEditBox, LMessages;

type
  {
  TIBDynamicGrid is a TDBGrid descendent that provides for:
   - automatic resizing of selected columns to fill the available row length
   - automatic positioning and sizing of a "totals" control, typically at the
     column footer, on a per column basis.
   - DataSet resorting on header row click, sorting the dataset by the selected column.
     A second click on the same header cell reversed the sort order.
   - Reselection of the same row following resorting.
   - A new cell editor that provides the same functionality as TIBLookupComboEditBox.
     Its properties are specified on a per column basis and allows for one or more
     columns to have their values selected from a list provided by a dataset.
     Autocomplete and autoinsert are also available. The existing picklist editor
     is unaffected by the extension.
  }

  TIBDynamicGrid = class;

  TOnUpdateSortOrder = procedure (Sender: TObject; ColIndex: integer; var OrderBy: string) of Object;

  { TDynamicGridDataLink }

  TDynamicGridDataLink = class(TDataLink)
  private
    FOwner: TIBDynamicGrid;
  protected
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(AOwner: TIBDynamicGrid);
  end;

  { TDBDynamicGridColumn }

  TDBDynamicGridColumn = class(TColumn)
  private
    FAutoSizeColumn: boolean;
    FColumnTotalsControl: TControl;
    FDesignWidth: integer;
    function GetWidth: integer;
    procedure SetWidth(AValue: integer);
  public
    property DesignWidth: integer read FDesignWidth;
  published
    property ColumnTotalsControl: TControl read FColumnTotalsControl write FColumnTotalsControl;
    property AutoSizeColumn: boolean read FAutoSizeColumn write FAutoSizeColumn;
    property Width: integer read GetWidth write SetWidth;
  end;

  { TIBDynamicGridColumn }

  TIBDynamicGridColumn = class(TDBDynamicGridColumn)
  private
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FCaseSensitiveMatch: boolean;
    FInitialSortColumn: boolean;
    FKeyField: string;
    FKeyPressInterval: integer;
    FListField: string;
    FListSource: TDataSource;
    FOnCustomInsert: TCustomInsert;
    procedure SetInitialSortColumn(AValue: boolean);
  public
    constructor Create(ACollection: TCollection); override;
  published
    property InitialSortColumn: boolean read FInitialSortColumn write SetInitialSortColumn;
    property ListSource: TDataSource read FListSource write FListSource;
    property KeyField: string read FKeyField write FKeyField;
    property ListField: string read FListField write FListField;
    property AutoInsert: boolean read FAutoInsert write FAutoInsert default true;
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default true;
    property CaseSensitiveMatch: boolean read FCaseSensitiveMatch write FCaseSensitiveMatch;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 500;
    property OnCustomInsert: TCustomInsert read FOnCustomInsert write FOnCustomInsert;
  end;

  { TDBLookupCellEditor }

  TDBLookupCellEditor = class(TIBLookupComboEditBox)
  private
    FGrid: TCustomGrid;
    FCol,FRow: Integer;
  protected
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure Select; override;
    procedure Change; override;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
  public
    procedure EditingDone; override;
    property BorderStyle;
    property OnEditingDone;
  end;

  TDBDynamicGrid = class(TDBGrid)
  private
    { Private declarations }
    FResizing: boolean;
    procedure DoGridResize;
    procedure PositionTotals;
  protected
    procedure Loaded; override;
    procedure DoOnResize; override;
    function CreateColumns: TGridColumns; override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
  public
    constructor Create(TheComponent: TComponent); override;
 end;

  { TIBDynamicGrid }

  TIBDynamicGrid = class(TDBDynamicGrid)
  private
    { Private declarations }
    FAllowColumnSort: boolean;
    FDataLink: TDynamicGridDataLink;
    FOnUpdateSortOrder: TOnUpdateSortOrder;
    FDefaultPositionAtEnd: boolean;
    FDescending: boolean;
    FColHeaderClick: boolean;
    FLastColIndex: integer;
    FIndexFieldNames: string;
    FIndexFieldsList: TStringList;
    FBookmark: array of variant;
    FDBLookupCellEditor: TDBLookupCellEditor;
    FActive: boolean;
    procedure ColumnHeaderClick(Index: integer);
    function GetDataSource: TDataSource;
    function GetEditorBorderStyle: TBorderStyle;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetEditorBorderStyle(AValue: TBorderStyle);
    procedure ProcessColumns;
    procedure SetIndexFieldNames(AValue: string);
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
    procedure UpdateSortColumn(Sender: TObject);
    procedure DataSetScrolled(Sender: TObject);
    procedure RestorePosition(Data: PtrInt);
    procedure DoReOpen(Data: PtrInt);
  protected
    { Protected declarations }
    procedure Loaded; override;
    function  CreateColumns: TGridColumns; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure LinkActive(Value: Boolean); override;
    function  GetDefaultEditor(Column: Integer): TWinControl; override;
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string); override;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AllowColumnSort: boolean read FAllowColumnSort write FAllowColumnSort default true;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Descending: boolean read FDescending write FDescending;
    property EditorBorderStyle: TBorderStyle read GetEditorBorderStyle write SetEditorBorderStyle;
    property DefaultPositionAtEnd: boolean read  FDefaultPositionAtEnd write FDefaultPositionAtEnd;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
    property OnUpdateSortOrder: TOnUpdateSortOrder read FOnUpdateSortOrder write FOnUpdateSortOrder;
 end;

implementation

uses Math, IBQuery, IBCustomDataSet, StdCtrls, LCLType;

{ TDBDynamicGrid }

procedure TDBDynamicGrid.DoGridResize;
var ColSum: integer;
    ResizeColCount: integer;
    I: integer;
    adjustment: integer;
    n: integer;
begin
  if Columns.Count = 0 then Exit;

  FResizing := true;
  try
    ColSum := 0;
    for I := 0 to  ColCount - 1 do
       ColSum := ColSum + ColWidths[I];

    if Colsum <> ClientWidth then
    begin
      ResizeColCount := 0;
      for I := 0 to Columns.Count -1 do
        if TDBDynamicGridColumn(Columns[I]).AutoSizeColumn then
        begin
          Inc(ResizeColCount);
          Colsum := Colsum + TDBDynamicGridColumn(Columns[I]).DesignWidth - Columns[I].Width;
          Columns[I].Width := TDBDynamicGridColumn(Columns[I]).DesignWidth;
        end;

        if Colsum < ClientWidth then
        begin
          adjustment := (ClientWidth - ColSum) div ResizeColCount;
          n := (ClientWidth - ColSum) mod ResizeColCount;

          for I := 0 to Columns.Count -1 do
            if TDBDynamicGridColumn(Columns[I]).AutoSizeColumn then
            begin
              if I = 0 then
                Columns[I].Width := Columns[I].Width + adjustment + n
              else
                Columns[I].Width := Columns[I].Width + adjustment;
            end;
        end;
    end;
    PositionTotals
  finally
    FResizing := false
  end;
end;

procedure TDBDynamicGrid.PositionTotals;
var I: integer;
    acol: TDBDynamicGridColumn;
    LPos: integer;
begin
  LPos := Left;
  for I := 0 to FirstGridColumn - 1  do
    LPos := LPos + ColWidths[I];

  for I := 0 to Columns.Count - 1 do
  begin
    acol := TDBDynamicGridColumn(Columns[I]);
    if assigned(acol.FColumnTotalsControl) then
    begin
      acol.FColumnTotalsControl.AutoSize :=  false;
      acol.FColumnTotalsControl.Left := LPos;
      acol.FColumnTotalsControl.Width := acol.Width
    end;
    LPos := LPos + acol.Width;
  end;
end;

procedure TDBDynamicGrid.Loaded;
begin
  inherited Loaded;
  DoGridResize
end;

procedure TDBDynamicGrid.DoOnResize;
begin
  inherited DoOnResize;
  DoGridResize
end;

function TDBDynamicGrid.CreateColumns: TGridColumns;
begin
  result := TDBGridColumns.Create(Self, TDBDynamicGridColumn);
end;

procedure TDBDynamicGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
begin
  inherited HeaderSized(IsColumn, Index);
  PositionTotals
end;

constructor TDBDynamicGrid.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  ScrollBars := ssAutoVertical;
end;

{ TDBDynamicGridColumn }

procedure TDBDynamicGridColumn.SetWidth(AValue: integer);
begin
  if Width = AValue then Exit;
  inherited Width := AValue;
  if not TDBDynamicGrid(Grid).FResizing then
    FDesignWidth := Width
end;

function TDBDynamicGridColumn.GetWidth: integer;
begin
  Result := inherited Width
end;

{ TDBLookupCellEditor }

procedure TDBLookupCellEditor.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.msg=LM_KILLFOCUS then begin
    if HWND(TheMessage.WParam) = HWND(Handle) then begin
      // lost the focus but it returns to ourselves
      // eat the message.
      TheMessage.Result := 0;
      exit;
    end;
  end;
  inherited WndProc(TheMessage);
end;

procedure TDBLookupCellEditor.Select;
begin
  if FGrid<>nil then
    (FGrid as TIBDynamicGrid).EditorTextChanged(FCol, FRow, Text);
  inherited Select;
end;

procedure TDBLookupCellEditor.Change;
begin
  if FGrid<>nil then
    (FGrid as TIBDynamicGrid).EditorTextChanged(FCol, FRow, Text);
  inherited Change;
end;

procedure TDBLookupCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  CheckAndInsert;
  Msg.Col := FCol;
  Msg.Row := FRow;
  Msg.Value:=Text;
end;

procedure TDBLookupCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TDBLookupCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text := Msg.Value;
  SelStart := Length(Text);
end;

procedure TDBLookupCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol := Msg.Col;
  FRow := Msg.Row;
end;

procedure TDBLookupCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid := FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

procedure TDBLookupCellEditor.EditingDone;
begin
  inherited EditingDone;
  if FGrid<>nil then
    FGrid.EditingDone;
end;

{ TIBDynamicGridColumn }

procedure TIBDynamicGridColumn.SetInitialSortColumn(AValue: boolean);
begin
  if FInitialSortColumn = AValue then Exit;
  FInitialSortColumn := AValue;
  (Grid as TIBDynamicGrid).UpdateSortColumn(self)
end;

constructor TIBDynamicGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAutoInsert := true;
  FAutoComplete := true;
  FKeyPressInterval := 500
end;

{ TDynamicGridDataLink }

procedure TDynamicGridDataLink.DataEvent(Event: TDataEvent; Info: Ptrint);
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

procedure TDynamicGridDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited DataSetScrolled(Distance);
  FOwner.DataSetScrolled(self)
end;

constructor TDynamicGridDataLink.Create(AOwner: TIBDynamicGrid);
begin
  inherited Create;
  FOwner := AOwner
end;


{ TIBDynamicGrid }

procedure TIBDynamicGrid.ColumnHeaderClick(Index: integer);
begin
  FColHeaderClick := true;
  try
    if Index = FLastColIndex then
      FDescending := not FDescending;
    FLastColIndex := Index;
    if assigned(DataSource) and assigned(DataSource.DataSet) and DataSource.DataSet.Active then
    begin
      DataSource.DataSet.Active := false;
      Application.QueueAsyncCall(@DoReopen,0)
    end;
  finally
    FColHeaderClick := false
  end;
end;

function TIBDynamicGrid.GetDataSource: TDataSource;
begin
  Result := inherited DataSource
end;

function TIBDynamicGrid.GetEditorBorderStyle: TBorderStyle;
begin
  if Editor = FDBLookupCellEditor then
     Result := FDBLookupCellEditor.BorderStyle
  else
    Result := inherited EditorBorderStyle
end;

procedure TIBDynamicGrid.SetDataSource(AValue: TDataSource);
begin
  inherited DataSource := AValue;
  FDataLink.DataSource := AValue;
end;

procedure TIBDynamicGrid.SetEditorBorderStyle(AValue: TBorderStyle);
begin
  inherited EditorBorderStyle := AValue;
  if FDBLookupCellEditor.BorderStyle <> AValue then
  begin
    FDBLookupCellEditor.BorderStyle := AValue;
    if (Editor = FDBLookupCellEditor) and EditorMode then
      EditorWidthChanged(Col,FDBLookupCellEditor.Width);
  end;
end;

procedure TIBDynamicGrid.ProcessColumns;
var i: integer;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    if TIBDynamicGridColumn(columns[i]).InitialSortColumn then
      FLastColIndex := i
  end
end;

procedure TIBDynamicGrid.SetIndexFieldNames(AValue: string);
var idx: integer;
begin
  if FIndexFieldNames = AValue then Exit;
  FIndexFieldNames := AValue;
  idx := 1;
  FIndexFieldsList.Clear;
  while idx <= Length(AValue) do
        FIndexFieldsList.Add(ExtractFieldName(AValue,idx));
end;

procedure TIBDynamicGrid.UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
var OrderBy: string;
begin
    if Sender = TObject(FDataLink) then
    begin
      if Descending then
        Parser.OrderByClause := '"' + Columns[FLastColIndex].FieldName + '" desc'
      else
        Parser.OrderByClause := '"' + Columns[FLastColIndex].FieldName + '" asc';

      if assigned(FOnUpdateSortOrder) then
      begin
        OrderBy := Parser.OrderByClause;
        OnUpdateSortOrder(self,FLastColIndex,OrderBy);
        Parser.OrderByClause := OrderBy
      end
    end;
end;

procedure TIBDynamicGrid.UpdateSortColumn(Sender: TObject);
var i: integer;
begin
  if Sender is TIBDynamicGridColumn then
  begin
    for i := 0 to Columns.Count -1 do
      if TObject(Columns[i]) <> Sender then
         TIBDynamicGridColumn(Columns[i]).InitialSortColumn := false
  end

end;

procedure TIBDynamicGrid.DataSetScrolled(Sender: TObject);
var i: integer;
    F: TField;
begin
    SetLength(FBookmark,FIndexFieldsList.Count);
    for i := 0 to FIndexFieldsList.Count - 1 do
    begin
      F := DataSource.DataSet.FindField(FIndexFieldsList[i]);
      if assigned(F) then
         FBookmark[i] := F.AsVariant;
    end;
end;

procedure TIBDynamicGrid.RestorePosition(Data: PtrInt);
begin
  if assigned(DataSource) and assigned(DataSource.DataSet) and DataSource.DataSet.Active then
  begin
    if Length(FBookmark) > 0 then
      DataSource.DataSet.Locate(FIndexFieldNames,FBookmark,[])
    else
    if FDefaultPositionAtEnd then
       DataSource.DataSet.Last
  end;
end;

procedure TIBDynamicGrid.DoReOpen(Data: PtrInt);
begin
  DataSource.DataSet.Active := true;
end;

procedure TIBDynamicGrid.Loaded;
begin
  inherited Loaded;
  ProcessColumns;
end;

function TIBDynamicGrid.CreateColumns: TGridColumns;
begin
  result := TDBGridColumns.Create(Self, TIBDynamicGridColumn);
end;

procedure TIBDynamicGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Coord: TGridCoord;
    function PtInRect(const Rect : TRect;const p : TPoint) : Boolean;

    begin
      PtInRect:=(p.y>=Rect.Top) and
                (p.y<Rect.Bottom) and
                (p.x>=Rect.Left) and
                (p.x<Rect.Right);
    end;
begin
  if (Editor is TDBLookupCellEditor) and Editor.Visible
             and not PtInRect(Editor.BoundsRect,Point(X,Y)) then
     Editor.Perform(CM_EXIT,0,0);  {Do insert new value if necessary}
  inherited MouseDown(Button, Shift, X, Y);
  Coord := MouseCoord(X,Y);
  if AllowColumnSort and
   (Coord.Y = 0) and (MouseCoord(X+5,Y).X = Coord.X) {not on boundary}
                   and (MouseCoord(X-5,Y).X = Coord.X) then
    ColumnHeaderClick(Coord.X-1);
end;

procedure TIBDynamicGrid.LinkActive(Value: Boolean);
begin
  inherited LinkActive(Value);
  if (FActive <> Value) and Value then
    Application.QueueAsyncCall(@RestorePosition,0);
  FActive := Value
end;

function TIBDynamicGrid.GetDefaultEditor(Column: Integer): TWinControl;
var C: TIBDynamicGridColumn;
    bs: TColumnButtonStyle;
begin
  result := nil;
  if EditingAllowed(Col) then
  begin
    C := ColumnFromGridColumn(Column) as TIBDynamicGridColumn;
    if C <> nil then
    begin
      bs := C.ButtonStyle;
      if (bs = cbsPickList) and assigned(C.ListSource) then
      begin
         FDBLookupCellEditor.ListSource := C.ListSource;
         FDBLookupCellEditor.KeyField := C.KeyField;
         FDBLookupCellEditor.ListField := C.ListField;
         FDBLookupCellEditor.AutoInsert := C.AutoInsert;
         FDBLookupCellEditor.AutoComplete := C.AutoComplete;
         FDBLookupCellEditor.CaseSensitiveMatch := C.CaseSensitiveMatch;
         FDBLookupCellEditor.KeyPressInterval := C.KeyPressInterval;
         FDBLookupCellEditor.OnCustomInsert := C.OnCustomInsert;
         Result := FDBLookupCellEditor;
      end
      else
        Result := inherited GetDefaultEditor(Column);
    end
  end
end;

procedure TIBDynamicGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin
  inherited EditorTextChanged(aCol, aRow, aText);
end;

constructor TIBDynamicGrid.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FAllowColumnSort := true;
  FDataLink := TDynamicGridDataLink.Create(self);
  FIndexFieldsList := TStringList.Create;
  FIndexFieldsList.Delimiter := ';';
  FIndexFieldsList.StrictDelimiter := true;
  FDBLookupCellEditor := TDBLookupCellEditor.Create(nil);
  FDBLookupCellEditor.Name := 'DBLookupCellEditor';
  FDBLookupCellEditor.Visible := False;
  FDBLookupCellEditor.AutoSize := false;
end;

destructor TIBDynamicGrid.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  if assigned(FIndexFieldsList) then FIndexFieldsList.Free;
  if assigned(FDBLookupCellEditor) then FDBLookupCellEditor.Free;
  inherited Destroy;
end;

end.