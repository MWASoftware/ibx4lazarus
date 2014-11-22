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
unit LookupIBComboBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBCtrls, DB, ExtCtrls;

type

  TCustomInsert = procedure(Sender: TObject; aText: string; var KeyValue: variant) of object;

  { TLookupDBComboBox }

  TLookupDBComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FLookup: TDBLookup;
    function GetKeyField: string;
    function GetKeyValue: variant;
    function GetListField: string;
    function GetListFieldIndex: Integer;
    function GetListSource: TDataSource;
    function GetLookupCache: boolean;
    function GetNullValueKey: TShortCut;
    procedure SetKeyField(const AValue: string);
    procedure SetKeyValue(const AValue: variant);
    procedure SetListField(const AValue: string);
    procedure SetListFieldIndex(const AValue: Integer);
    procedure SetListSource(const AValue: TDataSource);
    procedure SetLookupCache(const AValue: boolean);
    procedure SetNullValueKey(const AValue: TShortCut);
  protected
    { Protected declarations }
     procedure InitializeWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property KeyValue: variant read GetKeyValue write SetKeyValue;
    property Text;
    property ItemIndex;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoDropDown;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property KeyField: string read GetKeyField write SetKeyField;
    property ListField: string read GetListField write SetListField;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property LookupCache: boolean read GetLookupCache  write SetLookupCache default true;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey default 0;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelect;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
end;

  { TLookupIBComboBox }

  TLookupIBComboBox = class(TLookupDBComboBox)
  private
    { Private declarations }
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FCaseSensitiveMatch: boolean;
    FKeyPressInterval: integer;
    FOnCustomInsert: TCustomInsert;
    FTimer: TTimer;
    FFiltered: boolean;
    FOriginalTextValue: string;
    procedure HandleTimer(Sender: TObject);
    procedure ResetParser;
    procedure UpdateList;
    procedure HandleEnter(Data: PtrInt);
  protected
    procedure CheckAndInsert;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
  published
    property AutoInsert: boolean read FAutoInsert write FAutoInsert default true;
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default true;
    property CaseSensitiveMatch: boolean read FCaseSensitiveMatch write FCaseSensitiveMatch;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 500;
    property OnCustomInsert: TCustomInsert read FOnCustomInsert write FOnCustomInsert;
  end;

implementation

uses IBQuery, IBCustomDataSet, LCLType, Variants;

{ TLookupIBComboBox }

procedure TLookupIBComboBox.CheckAndInsert;
var newid: variant;
    CurText: string;
begin
  if AutoComplete and (Text <> '') and assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active and (ListSource.DataSet.RecordCount = 0) then
  begin
    {New Value}
    CurText := Text;
    if assigned(FOnCustomInsert) then
    begin
      newid := Null;
      OnCustomInsert(self,Text,newid);
      if varIsNull(newid) then
         Text := FOriginalTextValue
      else
        Text := CurText;
      ResetParser
    end
    else
    if AutoInsert then
    begin
      ListSource.DataSet.DisableControls;
      try
        ResetParser;
        ListSource.DataSet.Append;
        try
          ListSource.DataSet.FieldByName(ListField).AsString := CurText;
          ListSource.DataSet.Post;
          Text := CurText;
       except
          ListSource.DataSet.Cancel;
          Text := FOriginalTextValue;
          raise
       end;
      finally
        ListSource.DataSet.EnableControls
      end;
    end;
  end;
end;

procedure TLookupIBComboBox.HandleTimer(Sender: TObject);
var ActiveState: boolean;
begin
  FTimer.Interval := 0;
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) and
     ListSource.DataSet.Active then
  begin
    TIBDataSet(ListSource.DataSet).Parser.ResetWhereClause;
    if CaseSensitiveMatch then
      TIBDataSet(ListSource.DataSet).Parser.Add2WhereClause(ListField + ' Like ''' + Text + '%''')
    else
      TIBDataSet(ListSource.DataSet).Parser.Add2WhereClause('Upper(' + ListField + ') Like Upper(''' + Text + '%'')');
    UpdateList;
    FFiltered := true
  end;
end;

procedure TLookupIBComboBox.ResetParser;
begin
  if FFiltered and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) and
     ListSource.DataSet.Active then
  begin
    TIBDataSet(ListSource.DataSet).Parser.ResetWhereClause;
    UpdateList;
    FFiltered := false
  end;
end;

procedure TLookupIBComboBox.UpdateList;
var CurSelLength: integer;
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) then
  begin
         ListSource.DataSet.Active := false;
         ListSource.DataSet.Active :=  true;
         if Focused then
         begin
           CurSelLength := length(Text);
           if ListSource.DataSet.Active and (ListSource.DataSet.RecordCount > 0) then
           begin
             Text := ListSource.DataSet.FieldByName(ListField).AsString;
             SelStart := CurSelLength ;
             SelLength := Length(Text) - CurSelLength
           end;
         end;
  end;
end;

procedure TLookupIBComboBox.HandleEnter(Data: PtrInt);
begin
  SelectAll
end;

procedure TLookupIBComboBox.DoEnter;
begin
  inherited DoEnter;
  FOriginalTextValue:= Text;
  ResetParser;
  Application.QueueAsyncCall(@HandleEnter,0);
end;

procedure TLookupIBComboBox.DoExit;
begin
  inherited DoExit;
  FTimer.Interval := 0;
end;

procedure TLookupIBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
   inherited KeyDown(Key, Shift);
  if Key = VK_RETURN then
     EditingDone
  else
  if Key = VK_ESCAPE then
  begin
    ResetParser;
    Text := FOriginalTextValue;
  end;
end;

procedure TLookupIBComboBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if AutoComplete and not (Word(Key)  in [VK_RETURN,VK_ESCAPE]) then
    FTimer.Interval := FKeyPressInterval
end;

constructor TLookupIBComboBox.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FKeyPressInterval := 500;
  FAutoInsert := true;
  FAutoComplete := true;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 0;
  FTimer.OnTimer := @HandleTimer;
end;

destructor TLookupIBComboBox.Destroy;
begin
  if assigned(FTimer) then FTimer.Free;
  inherited Destroy;
end;

procedure TLookupIBComboBox.EditingDone;
begin
  CheckAndInsert;
  inherited EditingDone;
end;

{ TLookupDBComboBox }

function TLookupDBComboBox.GetKeyField: string;
begin
  Result := FLookup.KeyField;
end;

function TLookupDBComboBox.GetKeyValue: variant;
begin
  result := FLookup.GetKeyValue(ItemIndex);
end;

function TLookupDBComboBox.GetListField: string;
begin
  Result := FLookup.ListField;
end;

function TLookupDBComboBox.GetListFieldIndex: Integer;
begin
  Result := FLookup.ListFieldIndex;
end;

function TLookupDBComboBox.GetListSource: TDataSource;
begin
  Result := FLookup.ListSource;
end;

function TLookupDBComboBox.GetLookupCache: boolean;
begin
  Result := FLookup.LookupCache;
end;

function TLookupDBComboBox.GetNullValueKey: TShortCut;
begin
  result := FLookup.NullValueKey;
end;

procedure TLookupDBComboBox.SetKeyField(const AValue: string);
begin
  FLookup.KeyField := AValue;
end;

procedure TLookupDBComboBox.SetKeyValue(const AValue: variant);
begin
  ItemIndex := FLookup.GetKeyIndex(AValue);
end;

procedure TLookupDBComboBox.SetListField(const AValue: string);
begin
  FLookup.ListField:= AValue;
end;

procedure TLookupDBComboBox.SetListFieldIndex(const AValue: Integer);
begin
   FLookup.ListFieldIndex:= AValue;
end;

procedure TLookupDBComboBox.SetListSource(const AValue: TDataSource);
begin
  FLookup.ListSource:= AValue;
end;

procedure TLookupDBComboBox.SetLookupCache(const AValue: boolean);
begin
  FLookup.LookupCache := AValue
end;

procedure TLookupDBComboBox.SetNullValueKey(const AValue: TShortCut);
begin
  FLookup.NullValueKey := AValue;
end;

procedure TLookupDBComboBox.InitializeWnd;
begin
  inherited InitializeWnd;
  FLookup.Initialize(nil, Items);
end;

constructor TLookupDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FLookup:= TDBLookup.Create(Self);
  FLookup.LookupCache := true;
end;

destructor TLookupDBComboBox.Destroy;
begin
  if assigned(FLookup) then FLookup.Free;
  inherited Destroy;
end;

end.
