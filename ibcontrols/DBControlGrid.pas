{
 /***************************************************************************
                               DBControlGrid.pas
                               -----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Mar 8 2015


 ***************************************************************************/

 Unashameably hacked from DBGrid.Pas (Copyright (C) 2003  Jesus Reyes Aguilar.)
 by Tony Whyman (tony@mwasoftware.co.uk) .Additional source code is
 Copyright (c) McCallum Whyman Associates Ltd (trading as MWA Software) 2015.

 This unit defines TDBControlGrid: a lookalike rather than a clone for the Delphi
 TDBCrtlGrid. TDBControlGrid is a single column grid that replicates a TWinControl
 - typically a TPanel or a TFrame in each row. Each row corresponding to the rows
 of the linked DataSource. Any data aware control on the replicated (e.g.) TPanel
 will then appear to have the appropriate value for the row.

 The replicated control is not part of this control but must be added by the
 programmer at design time, and linked to the "DrawPanel" property.

 Rows can be edited, inserted (append only) or deleted.

 Distributed and licensed under the Library GNU General Public License
 see https://www.gnu.org/licenses/lgpl.html with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

}
unit DBControlGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, DB, Grids, DBGrids, Graphics, StdCtrls,
  LMessages;

{
  The TRowCache is where we keep track of the DataSet and cache images of each row.
  TDBControlGrid is really a slight of hand. Only the active record is shown in
  the panel and the others are cached and displayed as images.

  The image cache is indexed by TDataSet.RecNo and accessed by current active
  record number (the data being displayed on the panel) and row offset from this
  record number.

  This is efficient but gives us a problem as the TDataSet model does not remove
  deleted records. Instead it simply marks them as deleted. Likewise, we need to
  keep track of deleted rows and skip over them when accessing the cache.

  When alternate row colours are in use, the cache is also used to keep track of the
  correct row colour as we must similarly ignore delete rows when calculating the
  correct colour. Odd and Even row numbers is not good enough here.
}

type
  TRowCacheState = (rcEmpty,rcPresent,rcDeleted);
  TRowDetails = record
    FState: TRowCacheState;
    FAlternateColor: boolean;
    FBitmap: TBitmap;
  end;

  { TRowCache }

  TRowCache = class
  private
    FAltColorStartNormal: boolean;
    FHeight: integer;
    FList: array of TRowDetails;
    FUseAlternateColors: boolean;
    FWidth: integer;
    procedure FreeImages(Reset: boolean);
    function GetAlternateColor(RecNo: integer): boolean;
    function Render(Control: TWinControl): TBitmap;
    procedure ExtendCache(aMaxIndex: integer);
    procedure OnWidthChange(Sender: TObject);
    procedure SetHeight(AValue: integer);
    procedure SetUseAlternateColors(AValue: boolean);
    procedure SetWidth(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCache;
    function Add2Cache(RecNo: Longint; Control: TWinControl): TBitmap;
    function GetRowImage(RecNo, Offset: integer): TBitmap;
    procedure InvalidateRowImage(RecNo: integer);
    function IsEmpty(RecNo: integer): boolean;
    procedure MarkAsDeleted(RecNo: integer);
    property AlternateColor[RecNo: integer]: boolean read GetAlternateColor;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property AltColorStartNormal: boolean read FAltColorStartNormal write FAltColorStartNormal;
    property UseAlternateColors: boolean read FUseAlternateColors write SetUseAlternateColors;
  end;

  { TDBControlGridDataLink }

  TDBControlGridDataLink = class(TComponentDataLink)
  private
    FOnCheckBrowseMode: TDataSetNotifyEvent;
  protected
    procedure CheckBrowseMode; override;
  public
    property OnCheckBrowseMode: TDataSetNotifyEvent read FOnCheckBrowseMode write FOnCheckBrowseMode;
  end;

  TKeyDownHandler = procedure (Sender: TObject; var Key: Word;  Shift: TShiftState; var Done: boolean) of object;

  TPanelGridOption = (dgpIndicator,dgpDisableInsert,dgpCancelOnExit);
  TPanelGridOptions = set of TPanelGridOption;

  { TDBControlGrid }

  TDBControlGrid = class(TCustomGrid)
  private
    { Private declarations }
    FDataLink: TDBControlGridDataLink;
    FDefaultPositionAtEnd: boolean;
    FDrawPanel: TWinControl;
    FDrawingActiveRecord: boolean;
    FOldPosition: Integer;
    FOnKeyDownHander: TKeyDownHandler;
    FOptions: TPanelGridOptions;
    FWeHaveFocus: boolean;
    FRowCache: TRowCache;
    FDrawRow: integer;          {The current row in the draw panel}
    FSelectedRow: integer;      {The row containing the current selection}
    FSelectedRecNo: integer;    {The DataSet RecNo for the current row}
    FRequiredRecNo: integer;    {Used after a big jump and is the dataset recno
                                 that we want to end up with}
    FInCacheRefresh: boolean;       {Cache refresh in progress during paint}
    FCacheRefreshQueued: boolean;   {cache refresh requested during wmpaint}
    FModified: boolean;
    FLastRecordCount: integer;

    {Used to pass mouse clicks to panel when focused row changes}
    FLastMouse: TPoint;
    FLastMouseButton: TMouseButton;
    FLastMouseShiftState: TShiftState;

    function ActiveControl: TControl;
    procedure EmptyGrid;
    function GetDataSource: TDataSource;
    function GetRecordCount: Integer;
    procedure GetScrollbarParams(out aRange, aPage, aPos: Integer);
    function  GridCanModify: boolean;
    procedure DoDrawRow(aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure DoMoveRecord(Data: PtrInt);
    procedure DoSelectNext(Data: PtrInt);
    procedure DoScrollDataSet(Data: PtrInt);
    procedure DoSetupDrawPanel(Data: PtrInt);
    procedure DoSendMouseClicks(Data: PtrInt);
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnRecordChanged(Field:TField);
    procedure OnCheckBrowseMode(aDataSet: TDataSet);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnDrawPanelResize(Sender: TObject);
    procedure OnEditingChanged(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnLayoutChanged(aDataSet: TDataSet);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure OnUpdateData(aDataSet: TDataSet);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDrawPanel(AValue: TWinControl);
    procedure SetOptions(AValue: TPanelGridOptions);
    procedure SetupDrawPanel(aRow: integer);
    function  UpdateGridCounts: Integer;
    procedure UpdateBufferCount;
    procedure UpdateDrawPanelBounds(aRow: integer);
    procedure UpdateScrollbarRange;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    function  ISEOF: boolean;
    function  ValidDataSet: boolean;
    function  InsertCancelable: boolean;
  protected
    { Protected declarations }
    function  GetBufferCount: integer; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoGridResize;
    procedure DoOnResize; override;
    procedure DrawAllRows; override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawIndicator(ACanvas: TCanvas; aRow: integer; R: TRect; Opt: TDataSetState; MultiSel: boolean); virtual;
    procedure GridMouseWheel(shift: TShiftState; Delta: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MoveSelection; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure ResetSizes; override;
    procedure SetColor(Value: TColor); override;
    procedure UpdateActive; virtual;
    procedure UpdateData; virtual;
    procedure UpdateShowing; override;
    procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage,aPos: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseToRecordOffset(const x, y: Integer; out RecordOffset: Integer
      ): TGridZone;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Datalink: TDBControlGridDataLink read FDatalink;
  published
    { Published declarations }
    property Align;
    property AlternateColor;
    property AltColorStartNormal;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property Color;
    property Constraints;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultPositionAtEnd: boolean read  FDefaultPositionAtEnd write FDefaultPositionAtEnd;
    property DragCursor;
    property DragMode;
    property DrawPanel: TWinControl read FDrawPanel write SetDrawPanel;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property Flat;
    property Font;
    property Options: TPanelGridOptions read FOptions write SetOptions;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    property PopupMenu;
    property Scrollbars default ssAutoVertical;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseXORFeatures;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDownHander: TKeyDownHandler read FOnKeyDownHander write FOnKeyDownHander;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPrepareCanvas;
    property OnStartDrag;
    property OnUTF8KeyPress;
end;

implementation

uses LCLType, Math, LCLIntf, Forms, LCLMessageGlue, EditBtn, MaskEdit;

{ TDBControlGridDataLink }

procedure TDBControlGridDataLink.CheckBrowseMode;
begin
  inherited CheckBrowseMode;
  if assigned(FOnCheckBrowseMode) then
    OnCheckBrowseMode(DataSet);
end;

{ TRowCache }

function TRowCache.Render(Control: TWinControl): TBitmap;
var Container: TBitmap;
begin
  Container := TBitmap.Create;
  try
    Container.SetSize(Control.Width,Control.Height);
    Control.PaintTo(Container.Canvas,0,0);
  except
    Container.Free;
    raise
  end;
  Result := Container;
end;

procedure TRowCache.FreeImages(Reset: boolean);
var i: integer;
    altColor: boolean;
begin
  altColor := not AltColorStartNormal;
  for i := 0 to Length(FList) - 1 do
    begin
      if (FList[i].FState <> rcEmpty) and (FList[i].FBitmap <> nil) then
        begin
         FList[i].FBitmap.Free;
         FList[i].FBitmap := nil;
        end;
      if Reset or (FList[i].FState = rcPresent) then
        FList[i].FState := rcEmpty;
      if FList[i].FState <> rcDeleted then
      begin
        FList[i].FAlternateColor := altColor;
        altColor := not altColor;
      end;
    end;
end;

function TRowCache.GetAlternateColor(RecNo: integer): boolean;
begin
  ExtendCache(RecNo);
  Dec(RecNo);
  if (RecNo >= 0) and (RecNo < Length(FList)) then
    Result := FList[RecNo].FAlternateColor
  else
    Result := false;
end;

procedure TRowCache.ExtendCache(aMaxIndex: integer);
var i: integer;
    StartIndex: integer;
    altColor: boolean;
begin
  if aMaxIndex > Length(FList) then
  begin
    aMaxIndex := aMaxIndex + 10;
    StartIndex := Length(FList);
    SetLength(FList,aMaxIndex);
    if not UseAlternateColors then
      altColor := false
    else
    if StartIndex = 0 then
       altColor := not AltColorStartNormal
    else
      altColor := not FList[StartIndex-1].FAlternateColor;

    for i := StartIndex to Length(FList) - 1 do
    begin
      FList[i].FState := rcEmpty;
      FList[i].FBitmap := nil;
      FList[i].FAlternateColor := altColor;
      if UseAlternateColors then
        altColor := not altColor;
    end;
  end;
end;

procedure TRowCache.OnWidthChange(Sender: TObject);
begin
  FreeImages(false);
end;

procedure TRowCache.SetHeight(AValue: integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  FreeImages(false);
end;

procedure TRowCache.SetUseAlternateColors(AValue: boolean);
begin
  if FUseAlternateColors = AValue then Exit;
  FUseAlternateColors := AValue;
  FreeImages(false);
end;

procedure TRowCache.SetWidth(AValue: integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  FreeImages(false);
end;

constructor TRowCache.Create;
begin
  SetLength(FList,0);
end;

destructor TRowCache.Destroy;
begin
  ClearCache;
  inherited Destroy;
end;

procedure TRowCache.ClearCache;
begin
  FreeImages(true);
  SetLength(FList,0);
end;

function TRowCache.Add2Cache(RecNo: Longint; Control: TWinControl): TBitmap;
var i: integer;
begin
  Dec(RecNo); {Adust to zero base}
  ExtendCache(RecNo + 1);
  FList[RecNo].FState := rcPresent;
  if FList[RecNo].FBitmap <> nil then
    FList[RecNo].FBitmap.Free;
  FList[RecNo].FBitmap := Render(Control);
  Result := FList[RecNo].FBitmap;
end;

function TRowCache.GetRowImage(RecNo, Offset: integer): TBitmap;
begin
  Result := nil;
  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then
    Exit;

  if Offset >= 0 then
  repeat
    while (RecNo < Length(FList)) and (FList[RecNo].FState = rcDeleted) do
      Inc(RecNo);

    if RecNo >= Length(FList) then
       Exit;

    if Offset = 0 then
    begin
      if FList[RecNo].FState = rcPresent then
         Result := FList[RecNo].FBitmap;
      Exit;
    end;
    Inc(RecNo);
    Dec(Offset);
  until false
  else
  repeat
    Inc(Offset);
    Dec(RecNo);
    while (RecNo > 0) and (FList[RecNo].FState = rcDeleted) do
      Dec(RecNo);

    if RecNo < 0 then
       Exit;

    if Offset = 0 then
    begin
      if FList[RecNo].FState = rcPresent then
         Result := FList[RecNo].FBitmap;
      Exit;
    end;
  until false;
end;

procedure TRowCache.InvalidateRowImage(RecNo: integer);
begin
  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then
    Exit;

  if FList[RecNo].FState = rcPresent then
  begin
    FList[RecNo].FBitmap.Free;
    FList[RecNo].FBitmap := nil;
    FList[RecNo].FState := rcEmpty;
  end;
end;

function TRowCache.IsEmpty(RecNo: integer): boolean;
begin
  Dec(RecNo);
  Result := (RecNo < 0) or (RecNo >= Length(FList)) or (FList[RecNo].FState = rcEmpty);
end;

procedure TRowCache.MarkAsDeleted(RecNo: integer);
var altColor: boolean;
    i: integer;
begin
  Dec(RecNo); {adjust to zero base}
  if (RecNo < 0) or (RecNo >= Length(FList)) then
    Exit;

  FList[RecNo].FState := rcDeleted;
  if not UseAlternateColors then
    Exit;

  {Reset Alternate Colours}

  if RecNo = 0 then
     altColor := not AltColorStartNormal
  else
    altColor := not FList[RecNo-1].FAlternateColor;

  for i := RecNo + 1 to Length(FList) - 1 do
  begin
    if FList[i].FState <> rcDeleted then
    begin
      FList[i].FAlternateColor := altColor;
      altColor := not altColor;
      if FList[i].FState = rcPresent then
      begin
        FList[i].FBitmap.Free;
        FList[i].FState := rcEmpty;
      end;
    end;
  end;
end;

{ TDBControlGrid }

function TDBControlGrid.ActiveControl: TControl;
var AParent: TWinControl;
begin
  Result := nil;
  AParent := Parent;
  while (AParent <> nil) and  not (AParent is TCustomForm) do
    AParent := AParent.Parent;
  if (AParent <> nil) and (AParent is TCustomForm)then
      Result := TCustomForm(AParent).ActiveControl;
end;

procedure TDBControlGrid.EmptyGrid;
var
  OldFixedRows: Integer;
begin
  OldFixedRows := FixedRows;
  Clear;
  FRowCache.ClearCache;
  RowCount := OldFixedRows + 1;
  if dgpIndicator in FOptions then
    ColWidths[0]:=12;
  if assigned(FDrawPanel) then
    FDrawPanel.Visible := false;
end;

function TDBControlGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

function TDBControlGrid.GetRecordCount: Integer;
begin
  if assigned(FDataLink.DataSet) then
    result := FDataLink.DataSet.RecordCount
  else
    result := 0;
end;

procedure TDBControlGrid.GetScrollbarParams(out aRange, aPage, aPos: Integer);
begin
  if (FDatalink<>nil) and (FDataLink.DataSet <> nil) and FDatalink.Active then begin
    if FDatalink.dataset.IsSequenced then begin
      aRange := GetRecordCount + VisibleRowCount - 1;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      if FDatalink.BOF then aPos := 0 else
      if FDatalink.EOF then aPos := aRange
      else
        aPos := FDataLink.DataSet.RecNo - 1; // RecNo is 1 based
      if aPos<0 then aPos:=0;
    end else begin
      aRange := 6;
      aPage := 2;
      if FDatalink.EOF then aPos := 4 else
      if FDatalink.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;
end;

function TDBControlGrid.GridCanModify: boolean;
begin
  result := not FDataLink.ReadOnly
    and ValidDataSet and FDatalink.DataSet.CanModify;
end;

procedure TDBControlGrid.DoDrawRow(aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var CachedRow: TBitmap;
begin
  CachedRow := FRowCache.GetRowImage(FSelectedRecNo,aRow-FDrawRow);
  {if the row is in the cache then draw it - otherwise schedule a cache refresh cycle}
  if CachedRow = nil then
  begin
    if not FCacheRefreshQueued  then
    begin
      FCacheRefreshQueued := true;
      Application.QueueAsyncCall(@DoMoveRecord,PtrInt(aRow));
    end
  end
  else
     Canvas.Draw(aRect.Left,aRect.Top,CachedRow)
end;

procedure TDBControlGrid.DoMoveRecord(Data: PtrInt);
var aRow: integer;
begin
  if AppDestroying in Application.Flags then Exit;

  FCacheRefreshQueued := false;
  aRow := integer(Data);
  FInCacheRefresh := true;
  if assigned(FDataLink.DataSet) then
    FDatalink.DataSet.MoveBy(aRow - FDrawRow);
end;

procedure TDBControlGrid.DoSetupDrawPanel(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;
  SetupDrawPanel(FDrawRow);
end;

procedure TDBControlGrid.DoSendMouseClicks(Data: PtrInt);
var P: TPoint;
    Control: TControl;
begin
  if AppDestroying in Application.Flags then Exit;

  if assigned(FDrawPanel) and (FLastMouse.X <> 0) then
  begin
    P := ClientToScreen(FLastMouse);
    Control :=  FindControlAtPosition(P,false);
    if (Control <> nil) and (Control is TWinControl) then
      TWinControl(Control).SetFocus
    else
      Control := FDrawPanel;

    P := Control.ScreenToClient(P);

    LCLSendMouseDownMsg(Control,P.X,P.Y,FLastMouseButton,FLastMouseShiftState);
    LCLSendMouseUpMsg(Control,P.X,P.Y, FLastMouseButton,FLastMouseShiftState);

  end;
  FLastMouse.X := 0;
end;

procedure TDBControlGrid.KeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Done: boolean;
    AControl: TControl;
begin
  if Visible and assigned(FDrawPanel) and FDrawPanel.Visible and FWeHaveFocus then
  begin
    AControl := ActiveControl;
    if (AControl <> nil) and (AControl is TCustomComboBox)
                         and ((Key in [VK_UP,VK_DOWN]) or
                         (TCustomComboBox(AControl).DroppedDown and (Key = VK_RETURN)) or
                         ((TCustomComboBox(AControl).Text <> '') and (Key =  VK_ESCAPE))) then
      Exit; {ignore these keys if we are in a  combobox}

    if (AControl <> nil) and (AControl is TCustomMemo)
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN]) then Exit; {Ignore Return in a CustomMemo}

    if (AControl <> nil) and (AControl is TCustomGrid)
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN,VK_TAB]) then Exit; {Ignore Return in a CustomMemo}

    if (AControl <> nil) and ((AControl is TDateEdit) or (AControl is TCustomMaskedit))
                         and (Key in [VK_RETURN,VK_UP,VK_DOWN,
                               VK_ESCAPE,VK_LEFT,VK_RIGHT]) then Exit; {Ignore Return in a CustomMemo}
    Done := false;
    if assigned(FOnKeyDownHander) then
      OnKeyDownHander(Sender,Key,Shift,Done);
    if Done then Exit;

    KeyDown(Key,Shift)
  end;
end;

procedure TDBControlGrid.OnRecordChanged(Field: TField);
begin
  UpdateActive
end;

procedure TDBControlGrid.OnCheckBrowseMode(aDataSet: TDataSet);
var RecNo: integer;
begin
  if assigned(FDrawPanel) and (aDataSet.RecNo > 0)
      and (FModified or (FRowCache.IsEmpty(aDataSet.RecNo))) then
  begin
    RecNo := aDataSet.RecNo;
    Application.ProcessMessages;
    if RecNo = aDataSet.RecNo then   {Guard against sudden changes}
      FRowCache.Add2Cache(RecNo,FDrawPanel);
  end;
end;

procedure TDBControlGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  if aDataSet.State = dsBrowse then
  begin
    if GetRecordCount = 0 then
    begin
      {Must be closed/reopened}
      FRowCache.ClearCache;
      FSelectedRow := 0;
    end
    else
    if FLastRecordCount >  GetRecordCount then
    begin
      {must be delete}
      FRowCache.MarkAsDeleted(FSelectedRecNo);
      Dec(FSelectedRow);
    end;
    LayoutChanged;
  end;
  FLastRecordCount := GetRecordCount;
  if aDataSet.State = dsInsert then
  begin
    FRequiredRecNo := aDataSet.RecNo + 1;
    Application.QueueAsyncCall(@DoSelectNext,0);
  end;
  UpdateActive
end;

procedure TDBControlGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  LinkActive(true);
  UpdateActive;
end;

procedure TDBControlGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  LinkActive(false);
end;

procedure TDBControlGrid.OnDrawPanelResize(Sender: TObject);
begin
  FRowCache.Height := FDrawPanel.Height;
  DefaultRowHeight := FDrawPanel.Height;
end;

procedure TDBControlGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  FModified := true;
end;

procedure TDBControlGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  LinkActive(False);
end;

procedure TDBControlGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  LinkActive(False);
end;

procedure TDBControlGrid.OnLayoutChanged(aDataSet: TDataSet);
begin
  LayoutChanged;
end;

procedure TDBControlGrid.OnNewDataSet(aDataSet: TDataset);
begin
  LinkActive(True);
  UpdateActive;
end;

procedure TDBControlGrid.OnDataSetScrolled(aDataSet: TDataSet; Distance: Integer);
begin
  UpdateScrollBarRange;
  if Distance <> 0 then
  begin
    FDrawRow := FixedRows + FDataLink.ActiveRecord;

    if not FInCacheRefresh then
    begin
      Row := FDrawRow;
      FSelectedRow := FDrawRow;
      FSelectedRecNo := aDataSet.RecNo;
      SetupDrawPanel(FDrawRow);
    end
    else
      Application.QueueAsyncCall(@DoSetupDrawPanel,0);
   end
  else
     UpdateActive;
end;

procedure TDBControlGrid.OnUpdateData(aDataSet: TDataSet);
begin
  UpdateData;
end;

procedure TDBControlGrid.SetDataSource(AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TDBControlGrid.SetDrawPanel(AValue: TWinControl);
var theForm: TWinControl;
begin
  if FDrawPanel = AValue then Exit;
  if FDrawPanel <> nil then
  begin
     RemoveFreeNotification(FDrawPanel);
     FDrawPanel.RemoveAllHandlersOfObject(self);
     theForm := Parent;
     while not ((theForm is TCustomForm) or (theForm is TCustomFrame))
                           and (theForm.Parent <> nil) do
       theForm := theForm.Parent;
     FDrawPanel.Parent := theForm;
  end;
  FRowCache.ClearCache;
  try
    FDrawPanel := AValue;
    if assigned(FDrawPanel) then
    begin
      FDrawPanel.Parent := self;
      DefaultRowHeight := FDrawPanel.Height;
      if csDesigning in ComponentState then
        UpdateDrawPanelBounds(0)
      else
       FDrawPanel.Visible := false;
      FRowCache.Height := FDrawPanel.Height;
      FRowCache.Width := FDrawPanel.Width;
      FDrawPanel.AddHandlerOnResize(@OnDrawPanelResize);
      FreeNotification(FDrawPanel);
    end;
  except
    FDrawPanel := nil;
    raise;
  end;
end;

procedure TDBControlGrid.SetOptions(AValue: TPanelGridOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  if dgpIndicator in FOptions then
  begin
    FixedCols := 1;
    ColWidths[0] := 12
  end
  else
    FixedCols := 0;
end;

procedure TDBControlGrid.SetupDrawPanel(aRow: integer);
begin
  if FDrawPanel = nil then Exit;
  if ValidDataSet and FRowCache.AlternateColor[FDataLink.DataSet.RecNo] then
    FDrawPanel.Color := AlternateColor
  else
    FDrawPanel.Color := self.Color;
  FDrawPanel.Visible := true;
  UpdateDrawPanelBounds(aRow);         {Position Draw Panel over expanded Row}
  Invalidate;
end;

function TDBControlGrid.UpdateGridCounts: Integer;
var
  RecCount: Integer;
  FRCount, FCCount: Integer;
begin
  BeginUpdate;
  try
    FRCount := 0;
    if dgpIndicator in FOptions then
       FCCount := 1
    else
      FCCount := 0;
      if FDataLink.Active then begin
        UpdateBufferCount;
        RecCount := FDataLink.RecordCount;
        if RecCount<1 then
          RecCount := 1;
      end else begin
        RecCount := 0;
        if FRCount=0 then
          // need to be large enough to hold indicator
          // if there is one, and if there are no titles
          RecCount := FCCount;
      end;

      Inc(RecCount, FRCount);

      RowCount := RecCount;
      FixedRows := FRCount;
      Result := RowCount ;
  finally
    EndUpdate;
  end;
end;

procedure TDBControlGrid.UpdateBufferCount;
var
  BCount: Integer;
begin
  if FDataLink.Active then begin
    BCount := GetBufferCount;
    if BCount<1 then
      BCount := 1;
    FDataLink.BufferCount:= BCount;
  end;
end;

procedure TDBControlGrid.UpdateDrawPanelBounds(aRow: integer);
var R: TRect;
begin
  R := Rect(0,0,0,0);
  if assigned(FDrawPanel) and
   (aRow >= 0) and (aRow < RowCount) then
  begin
    // Upper and Lower bounds for this row
    ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);
    //Bounds for visible Column
    ColRowToOffSet(True,True,ColCount-1,R.Left,R.RIght);
    FDrawPanel.BoundsRect := R;
  end;
end;

procedure TDBControlGrid.UpdateScrollbarRange;
var
  aRange, aPage, aPos: Integer;
  ScrollInfo: TScrollInfo;
begin

  if not HandleAllocated then exit;


  GetScrollBarParams(aRange, aPage, aPos);

  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);

  {TODO: try to move this out}
  {$ifdef WINDOWS}
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.ntrackPos := 0;
  {$else}
  ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
  //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
  ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
  {$endif}
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := aRange;
  ScrollInfo.nPos := Min(aPos,aRange-aPage);
  ScrollInfo.nPage := aPage;
  // the redraw argument of SetScrollInfo means under gtk
  // if the scrollbar is visible or not, in windows it
  // seems to mean if the scrollbar is redrawn or not
  // to reflect the scrollbar changes made
  SetScrollInfo(Handle, SB_VERT, ScrollInfo,
    (ScrollBars in [ssBoth, ssVertical]) or
    ((Scrollbars in [ssAutoVertical, ssAutoBoth]) and (aRange>aPAge))
  );
  FOldPosition := aPos;
end;

procedure TDBControlGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos, aRange, aPage: Integer;
  DeltaRec: integer;

  function MaxPos: Integer;
  begin
    if IsSeq then
      result := GetRecordCount - 1
    else
      result := 4;
  end;

  procedure DsMoveBy(Delta: Integer);
  begin
    FDataLink.DataSet.MoveBy(Delta);
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  procedure DsGoto(BOF: boolean);
  begin
    if BOF then FDatalink.DataSet.First
    else        FDataLink.DataSet.Last;
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  function DsPos: boolean;
  begin
    result := false;
    aPos := Message.Pos;
    if aPos=FOldPosition then begin
      result := true;
      exit;
    end;
    if aPos>=MaxPos then
      dsGoto(False)
    else if aPos<=0 then
      dsGoto(True)
    else if IsSeq then
      FDatalink.DataSet.RecNo := aPos + 1
    else begin
      DeltaRec := Message.Pos - FOldPosition;
      if DeltaRec=0 then begin
        result := true;
        exit
      end
      else if DeltaRec<-1 then
        DsMoveBy(-VisibleRowCount)
      else if DeltaRec>1 then
        DsMoveBy(VisibleRowCount)
      else
        DsMoveBy(DeltaRec);
    end;
  end;

begin
  if not FDatalink.Active or not assigned(FDataLink.DataSet) then exit;

  IsSeq := FDatalink.DataSet.IsSequenced and not FDataLink.DataSet.Filtered;
  case Message.ScrollCode of
    SB_TOP:
      DsGoto(True);
    SB_BOTTOM:
      DsGoto(False);
    SB_PAGEUP:
      DsMoveBy(-VisibleRowCount);
    SB_LINEUP:
      DsMoveBy(-1);
    SB_LINEDOWN:
      DsMoveBy(1);
    SB_PAGEDOWN:
      DsMoveBy(VisibleRowCount);
    SB_THUMBPOSITION:
      if DsPos then
        exit;
    SB_THUMBTRACK:
        if not (FDatalink.DataSet.IsSequenced) or DsPos then
        begin
          exit;
        end;
    else begin
      Exit;
    end;
  end;

  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;  end;

function TDBControlGrid.ISEOF: boolean;
begin
  with FDatalink do
    result := ValidDataSet and DataSet.EOF;
end;

function TDBControlGrid.ValidDataSet: boolean;
begin
   result := FDatalink.Active And (FDatalink.DataSet<>nil)
end;

function TDBControlGrid.InsertCancelable: boolean;
begin
  Result := ValidDataSet;
  if Result then
  with FDatalink.DataSet do
    Result := (State=dsInsert) and not Modified ;
end;

function TDBControlGrid.GetBufferCount: integer;
begin
  Result := ClientHeight div DefaultRowHeight;
end;

procedure TDBControlGrid.DoEnter;
begin
  inherited DoEnter;
  FWeHaveFocus := true;
end;

procedure TDBControlGrid.DoExit;
begin
  FWeHaveFocus := false;
  if ValidDataSet and (dgpCancelOnExit in Options) and
    InsertCancelable then
  begin
    FDataLink.DataSet.Cancel;
  end;
  inherited DoExit;
end;

procedure TDBControlGrid.DoGridResize;
begin
    if Columns.Count = 0 then Exit;

    if ColCount > 1 then
      Columns[0].Width := ClientWidth - ColWidths[0]
    else
      Columns[0].Width := ClientWidth;

    FRowCache.Width := Columns[0].Width;
    UpdateDrawPanelBounds(Row);
end;

procedure TDBControlGrid.DoOnResize;
begin
  inherited DoOnResize;
  DoGridResize;
end;

procedure TDBControlGrid.DoScrollDataSet(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;
  FDataLink.DataSet.MoveBy(integer(Data) - FDataLink.DataSet.RecNo);
end;

procedure TDBControlGrid.DoSelectNext(Data: PtrInt);
begin
  FDataLink.DataSet.MoveBy(1);
end;

procedure TDBControlGrid.DrawAllRows;
begin
  inherited DrawAllRows;
  if  ValidDataSet and FDatalink.DataSet.Active then
  begin
    if FInCacheRefresh and not FCacheRefreshQueued then
    {We are at the end of a cache refresh cycle}
    begin
      if FRequiredRecNo > 0  then
      begin
        if FRequiredRecNo <> FDataLink.DataSet.RecNo then
          Application.QueueAsyncCall(@DoScrollDataSet,FRequiredRecNo);
        FRequiredRecNo := 0;
      end
      else
      if FDrawRow <> FSelectedRow then
        Application.QueueAsyncCall(@DoMoveRecord,FSelectedRow);
    end;
    FInCacheRefresh := false;
  end;
end;

procedure TDBControlGrid.DrawRow(ARow: Integer);
begin
  if (ARow>=FixedRows) and FDataLink.Active then
    FDrawingActiveRecord := (ARow = FDrawRow)
  else
    FDrawingActiveRecord := False;
  inherited DrawRow(ARow);
end;

procedure TDBControlGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

function GetDatasetState: TDataSetState;
begin
  if ValidDataSet then
    result := FDataLink.DataSet.State
  else
    result := dsInactive;
end;

var
  DataCol: Integer;
begin
  PrepareCanvas(aCol, aRow, aState);

  if aCol < FixedCols then
     DrawIndicator(Canvas,aRow, aRect,GetDataSetState,false)
  else
  if (FDrawPanel = nil) or not FDataLink.Active then
    DrawFillRect(Canvas,aRect)
  else
  if not FDrawingActiveRecord and FDataLink.Active then
      DoDrawRow(aRow,aRect,aState);
  {if we are drawing the active record then this is rendered by the Draw Panel
   i.e. a child control - so we need do nothing here}

  DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TDBControlGrid.DrawIndicator(ACanvas: TCanvas; aRow: integer;
  R: TRect; Opt: TDataSetState; MultiSel: boolean);
var
  dx,dy, x, y: Integer;
  procedure CenterY;
  begin
    y := R.Top + (R.Bottom-R.Top) div 2;
  end;
  procedure CenterX;
  begin
    X := R.Left + (R.Right-R.Left) div 2;
  end;
  procedure DrawEdit(clr: Tcolor);
  begin
    ACanvas.Pen.Color := clr;
    CenterY;
    CenterX;
    ACanvas.MoveTo(X-2, Y-Dy);
    ACanvas.LineTo(X+3, Y-Dy);
    ACanvas.MoveTo(X, Y-Dy);
    ACanvas.LineTo(X, Y+Dy);
    ACanvas.MoveTo(X-2, Y+Dy);
    ACanvas.LineTo(X+3, Y+Dy);
  end;
  procedure DrawBrowse;
  begin
    ACanvas.Brush.Color:=clBlack;
    ACanvas.Pen.Color:=clBlack;
    CenterY;
    x:= R.Left+3;
    if MultiSel then begin
      if BiDiMode = bdRightToLeft then begin
        ACanvas.Polyline([point(x+dx,y-dy),  point(x,y),point(x+dx,y+dy), point(x+dx,y+dy-1)]);
        ACanvas.Polyline([point(x+dx,y-dy+1),  point(x+1,y),point(x+dx,y+dy-1), point(x+dx,y+dy-2)]);
        CenterX;
        Dec(X,3);
        ACanvas.Ellipse(Rect(X+dx-2,Y-2,X+dx+2,Y+2));
      end else begin
        ACanvas.Polyline([point(x,y-dy),  point(x+dx,y),point(x,y+dy), point(x,y+dy-1)]);
        ACanvas.Polyline([point(x,y-dy+1),point(x+dx-1,y),point(x, y+dy-1), point(x,y+dy-2)]);
        CenterX;
        Dec(X,3);
        ACanvas.Ellipse(Rect(X-2,Y-2,X+2,Y+2));
      end;
    end else begin
      if BiDiMode = bdRightToLeft then
        ACanvas.Polygon([point(x,y),point(x+dx,y-dy),point(x+dx, y+dy),point(x,y)])
      else
        ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
    end;
  end;

begin
  ACanvas.Brush.Color := FixedColor;
  ACanvas.FillRect(R);
  if aRow <> Row then Exit;

  dx := 6;
  dy := 6;
  case Opt of
    dsBrowse:
      DrawBrowse;
    dsEdit:
      if FDrawingActiveRecord then
        DrawEdit(clBlack)
      else
        DrawBrowse;
    dsInsert:
      if FDrawingActiveRecord then
        DrawEdit(clGreen)
      else
        DrawBrowse;
    else
    if MultiSel then begin
      ACanvas.Brush.Color:=clBlack;
      ACanvas.Pen.Color:=clBlack;
      CenterX;
      CenterY;
      ACanvas.Ellipse(Rect(X-3,Y-3,X+3,Y+3));
    end;
  end;            end;

procedure TDBControlGrid.GridMouseWheel(shift: TShiftState; Delta: Integer);
begin
  inherited GridMouseWheel(shift, Delta);
  self.SetFocus;
  if ValidDataSet then
    FDataLink.DataSet.MoveBy(Delta);
end;

procedure TDBControlGrid.KeyDown(var Key: Word; Shift: TShiftState);
type
  TOperation=(opMoveBy,opCancel,opAppend,opInsert,opDelete);
var
  DeltaCol,DeltaRow: Integer;

  procedure DoOnKeyDown;
  begin
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
  end;

 procedure DoOperation(AOper: TOperation; Arg: Integer = 0);
  begin
    self.SetFocus;
    case AOper of
      opMoveBy:
        FDatalink.DataSet.MoveBy(Arg);
      opCancel:
        begin
          FDatalink.Dataset.Cancel;
        end;
      opAppend:
        FDatalink.Dataset.Append;
      opInsert:
        FDatalink.Dataset.Insert;
      opDelete:
        FDatalink.Dataset.Delete;
    end;
  end;

  function doVKDown: boolean;
  begin
    if InsertCancelable then
    begin
      if IsEOF then
        result:=true
      else begin
        doOperation(opCancel);
        result := false;
      end;
    end else begin
      result:=false;
      doOperation(opMoveBy, 1);
      if GridCanModify and FDataLink.EOF then begin
        if not (dgpDisableInsert in Options) then
          doOperation(opAppend);
      end
    end;
  end;

  function DoVKUP: boolean;
  begin
    if InsertCancelable then
      doOperation(opCancel)
    else begin
      doOperation(opMoveBy, -1);
    end;
    result := FDatalink.DataSet.BOF;
  end;



begin
  case Key of
    VK_DOWN:
      begin
        DoOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKDown;
          Key := 0;
        end;
      end;

    VK_UP:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKUp;
          key := 0;
         end;
      end;

    VK_NEXT:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, VisibleRowCount);
          Key := 0;
        end;
      end;

    VK_PRIOR:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, -VisibleRowCount);
          key := 0;
        end;
      end;

    VK_ESCAPE:
      begin
        doOnKeyDown;
        if ValidDataSet then
           doOperation(opCancel);
      end;

    VK_HOME:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataSet then
        begin
            if ssCTRL in Shift then
            begin
              FDataLink.DataSet.First;
              Key:=0;
            end;
        end;
      end;

    VK_END:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if ValidDataSet then
          begin
            if ssCTRL in shift then
            begin
              FDatalink.DataSet.Last;
              Key:=0;
            end;
          end;
        end;
      end;

  end;
end;

procedure TDBControlGrid.LinkActive(Value: Boolean);
begin
  if not Value then
  begin
    FRowCache.ClearCache;
    FInCacheRefresh := false;
    FCacheRefreshQueued := false;
    Row := FixedRows;
    FDrawingActiveRecord := false;
    FSelectedRecNo := 0;
    FSelectedRow := 0;
    FRequiredRecNo := 0;
  end;
  FRowCache.UseAlternateColors := AlternateColor <> Color;
  FRowCache.AltColorStartNormal := AltColorStartNormal;
  FLastRecordCount := 0;
  LayoutChanged;
  if Value then
  begin
    { The problem being solved here is that TDataSet does not readily tell us
      when a record is deleted. We get a DataSetChanged event - but this can
      occur for many reasons. Only by monitoring the record count accurately
      can be determine when a record is deleted. To do this we need to scroll
      the whole dataset to the end when the dataset is activated. Not desirable
      with large datasets - but a fix to TDataSet is needed to avoid this.
    }
    FDataLink.DataSet.DisableControls;
    try
      FDataLink.DataSet.Last;
      FLastRecordCount := FDataLink.DataSet.RecordCount;
      if not FDefaultPositionAtEnd then
        FDataLink.DataSet.First;
      FRequiredRecNo := FDataLink.DataSet.RecNo;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

procedure TDBControlGrid.LayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  BeginUpdate;
  try
    if UpdateGridCounts=0 then
      EmptyGrid;
  finally
    EndUpdate;
  end;
  UpdateScrollbarRange;
end;

procedure TDBControlGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
  procedure doMouseDown;
  begin
//    if not Focused then
//      SetFocus;
    if assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
  end;
  procedure doInherited;
  begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
  procedure doMoveBy;
  begin
    FDatalink.DataSet.MoveBy(P.Y - Row);
  end;
  procedure doMoveToColumn;
  begin
    Col := P.X;
  end;
  procedure DoCancel;
  begin
    FDatalink.Dataset.cancel;
  end;
begin
  if (csDesigning in componentState) or not ValidDataSet then begin
    exit;
  end;
  self.SetFocus;

{  if not MouseButtonAllowed(Button) then begin
    doInherited;
    exit;
  end;}

  Gz:=MouseToGridZone(X,Y);
  CacheMouseDown(X,Y);
  case Gz of
    gzInvalid:
      doMouseDown;

    gzFixedCells, gzFixedCols:
      doInherited;
    else
      begin

        P:=MouseToCell(Point(X,Y));
        if Gz=gzFixedRows then
          P.X := Col;

        if P.Y=Row then begin
          //doAcceptValue;

          if not (ssCtrl in Shift) then
          begin
            if gz=gzFixedRows then
              doMouseDown
            else
              doInherited;
          end;

        end else begin
          doMouseDown;
          if ValidDataSet then begin
            if InsertCancelable and IsEOF then
              doCancel;
            doMoveBy;
          end;
        end;
      end;
  end;
end;

procedure TDBControlGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FLastMouse.X := X;
  FLastMouse.Y := Y;
  FLastMouseButton := Button;
  FLastMouseShiftState := Shift;
  Application.QueueAsyncCall(@DoSendMouseClicks,0);
end;

procedure TDBControlGrid.MoveSelection;
begin
  inherited MoveSelection;
  InvalidateRow(Row);
end;

procedure TDBControlGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (AComponent = FDrawPanel) then FDrawPanel := nil;
end;

procedure TDBControlGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState
  );
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if gdFixed in aState then
  begin
    if gdHot in aState then
      Canvas.Brush.Color := FixedHotColor
    else
      Canvas.Brush.Color := GetColumnColor(aCol, gdFixed in AState);
  end;

  if (not FDatalink.Active) and ((gdSelected in aState) or (gdFocused in aState)) then
    Canvas.Brush.Color := Self.Color;

end;

procedure TDBControlGrid.ResetSizes;
begin
  LayoutChanged;
  inherited ResetSizes;
  DoGridResize;
end;

procedure TDBControlGrid.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  if (csDesigning in ComponentState) and assigned(FDrawPaneL) then
     FDrawPanel.Color := Value;
end;

procedure TDBControlGrid.UpdateActive;
var
  PrevRow: Integer;
begin
  if (csDestroying in ComponentState) or
    (FDatalink=nil) or (not FDatalink.Active) or
    (FDatalink.ActiveRecord<0) then
    exit;

  FDrawRow := FixedRows + FDataLink.ActiveRecord;
  FSelectedRecNo := FDataLink.DataSet.RecNo;
  PrevRow := Row;
  Row := FDrawRow;
  if not FInCacheRefresh then
  begin
    FSelectedRow := FDrawRow;
    if FDatalink.DataSet.State <> dsInsert then
      FRowCache.InvalidateRowImage(FSelectedRecNo);
  end;
  InvalidateRow(PrevRow);
  SetupDrawPanel(FDrawRow);
end;

procedure TDBControlGrid.UpdateData;
begin
  FModified := false;
end;

procedure TDBControlGrid.UpdateShowing;
begin
  inherited UpdateShowing;
  DoGridResize
end;

procedure TDBControlGrid.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  UpdateScrollbarRange;
end;

constructor TDBControlGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TDBControlGridDataLink.Create;//(Self);
  FRowCache := TRowCache.Create;
  FDataLink.OnRecordChanged:=@OnRecordChanged;
  FDataLink.OnDatasetChanged:=@OnDataSetChanged;
  FDataLink.OnDataSetOpen:=@OnDataSetOpen;
  FDataLink.OnDataSetClose:=@OnDataSetClose;
  FDataLink.OnNewDataSet:=@OnNewDataSet;
  FDataLink.OnInvalidDataSet:=@OnInvalidDataset;
  FDataLink.OnInvalidDataSource:=@OnInvalidDataSource;
  FDataLink.OnDataSetScrolled:=@OnDataSetScrolled;
  FDataLink.OnLayoutChanged:=@OnLayoutChanged;
  FDataLink.OnEditingChanged:=@OnEditingChanged;
  FDataLink.OnUpdateData:=@OnUpdateData;
  FDataLink.OnCheckBrowseMode := @OnCheckBrowseMode;
  FDataLink.VisualControl:= True;
  ScrollBars := ssAutoVertical;
  FOptions := [dgpIndicator];
  FixedCols := 1;
  ColCount := 1;
  FixedRows := 0;
  RowCount := 1;
  ColWidths[0] := 12;
  Columns.Add.ReadOnly := true; {Add Dummy Column for Panel}
  DoGridResize;
  if not (csDesigning in ComponentState) then
    Application.AddOnKeyDownBeforeHandler(@KeyDownHandler,false);
end;

destructor TDBControlGrid.Destroy;
begin
  if assigned(FDataLink) then
  begin
    FDataLink.OnDataSetChanged:=nil;
    FDataLink.OnRecordChanged:=nil;
    FDataLink.Free;
  end;
  if assigned(FRowCache) then FRowCache.Free;
  inherited Destroy;
end;

function TDBControlGrid.MouseToRecordOffset(const x, y: Integer;
  out RecordOffset: Integer): TGridZone;
var
  aCol,aRow: Integer;
begin
  Result := MouseToGridZone(x, y);

  RecordOffset := 0;

  if (Result=gzInvalid) or (Result=gzFixedCells) then
    exit;

  MouseToCell(x, y, aCol, aRow);

  if (Result=gzFixedRows) or (Result=gzNormal) then
    RecordOffset := aRow - Row;

  if (Result=gzFixedCols) or (Result=gzNormal) then begin
    aRow := ColumnIndexFromGridColumn(aCol);
  end;
end;

function TDBControlGrid.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil)
            and DataLink.ExecuteAction(AAction);
end;

function TDBControlGrid.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil)
            and DataLink.UpdateAction(AAction);
end;

end.

