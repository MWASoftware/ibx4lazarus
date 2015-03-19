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
unit IBLookupComboEditBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls, IBSQLParser, DB, StdCtrls;

type

  {TIBLookupComboEditBox is a TDBLookupComboBox descendent that implements "autocomplete"
   of typed in text and "autoinsert" of new entries. Autocomplete uses SQL manipulation
   to revise the available list and restrict it to items that are prefixed by the
   typed text (either case sensitive or case insenstive). Autoinsert allows a
   newly typed entry to be added to the list dataset and included in the available
   list items.    }

  TAutoInsert = procedure(Sender: TObject; aText: string; var NewKeyValue: variant) of object;
  TCanAutoInsert = procedure (Sender: TObject; aText: string; var Accept: boolean) of object;

  TIBLookupComboEditBox = class;

  { TIBLookupComboDataLink }

  TIBLookupComboDataLink = class(TDataLink)
  private
    FOwner: TIBLookupComboEditBox;
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TIBLookupComboEditBox);
  end;


  { TIBLookupComboEditBox }

  TIBLookupComboEditBox = class(TDBLookupComboBox)
  private
    FCanAutoInsert: TCanAutoInsert;
    { Private declarations }
    FDataLink: TIBLookupComboDataLink;
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FKeyPressInterval: integer;
    FOnCanAutoInsert: TCanAutoInsert;
    FRelationName: string;
    FTimer: TTimer;
    FFiltered: boolean;
    FOnAutoInsert: TAutoInsert;
    FOriginalTextValue: string;
    FUpdating: boolean;
    FInserting: boolean;
    FExiting: boolean;
    FLastKeyValue: variant;
    procedure DoActiveChanged(Data: PtrInt);
    function GetAutoCompleteText: TComboBoxAutoCompleteText;
    function GetListSource: TDataSource;
    function GetRelationNameQualifier: string;
    procedure HandleTimer(Sender: TObject);
    procedure ResetParser;
    procedure RecordChanged(Sender: TObject; aField: TField);
    procedure SetAutoCompleteText(AValue: TComboBoxAutoCompleteText);
    procedure SetListSource(AValue: TDataSource);
    procedure UpdateList;
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
    procedure HandleEnter(Data: PtrInt);
    procedure UpdateLinkData(Sender: TObject);
  protected
    { Protected declarations }
    procedure ActiveChanged(Sender: TObject);
    procedure CheckAndInsert;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetItemIndex(const Val: integer); override;
    procedure UpdateShowing; override;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
  published
    { Published declarations }
    property AutoInsert: boolean read FAutoInsert write FAutoInsert;
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default true;
    property AutoCompleteText: TComboBoxAutoCompleteText read GetAutoCompleteText
             write SetAutoCompleteText;
    property ItemHeight;
    property ItemWidth;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 500;
    property RelationName: string read FRelationName write FRelationName;
    property OnAutoInsert: TAutoInsert read FOnAutoInsert write FOnAutoInsert;
    property OnCanAutoInsert: TCanAutoInsert read FOnCanAutoInsert write FOnCanAutoInsert;
  end;


implementation

uses IBQuery, IBCustomDataSet, LCLType, Variants, LCLProc;

{ TIBLookupComboDataLink }

procedure TIBLookupComboDataLink.ActiveChanged;
begin
  FOwner.ActiveChanged(self)
end;

procedure TIBLookupComboDataLink.DataEvent(Event: TDataEvent; Info: Ptrint);
begin
  {If we are not visible then avoid unnecessary work}
  if not FOwner.Showing then Exit;

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

procedure TIBLookupComboDataLink.RecordChanged(Field: TField);
begin
  FOwner.RecordChanged(self,Field);
end;

procedure TIBLookupComboDataLink.UpdateData;
begin
  FOwner.UpdateLinkData(self)
end;

constructor TIBLookupComboDataLink.Create(AOwner: TIBLookupComboEditBox);
begin
  inherited Create;
  FOwner := AOwner
end;

{ TIBLookupComboEditBox }

procedure TIBLookupComboEditBox.HandleTimer(Sender: TObject);
var ActiveState: boolean;
begin
  FTimer.Interval := 0;
  FFiltered := Text <> '';
  UpdateList
end;

function TIBLookupComboEditBox.GetListSource: TDataSource;
begin
  Result := inherited ListSource;
end;

function TIBLookupComboEditBox.GetRelationNameQualifier: string;
begin
  if FRelationName <> '' then
     Result := FRelationName + '.'
  else
    Result := ''
end;

procedure TIBLookupComboEditBox.ActiveChanged(Sender: TObject);
begin
  if not FInserting and not FUpdating then
     Application.QueueAsyncCall(@DoActiveChanged,0);
end;

procedure TIBLookupComboEditBox.DoActiveChanged(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;

  if (DataSource = nil) and assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active   then
  begin
    begin
      if varIsNull(FLastKeyValue) and (ItemIndex = -1) then
        KeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant
      else
      begin
        KeyValue := FLastKeyValue;
        UpdateData(self); {Force auto scroll}
        if varIsNull(KeyValue) then {Value not present}
          Text := ListSource.DataSet.FieldByName(ListField).AsString
      end;
    end;
  end
  else
  if (DataSource <> nil) and  assigned(DataSource.DataSet) and
                   (DataSource.DataSet.Active) and (DataField <> '') then
  begin
    ResetParser;
    KeyValue := Field.AsVariant;
  end
  else
    Text := '';
  FOriginalTextValue := Text;
end;

function TIBLookupComboEditBox.GetAutoCompleteText: TComboBoxAutoCompleteText;
begin
  Result := inherited AutoCompleteText;
  if AutoComplete then
     Result := Result + [cbactEnabled]
end;

procedure TIBLookupComboEditBox.ResetParser;
var curKeyValue: variant;
begin
  if FFiltered then
  begin
    FFiltered := false;
    curKeyValue := KeyValue;
    Text := ''; {Ensure full list}
    UpdateList;
    KeyValue := curKeyValue;
    UpdateData(self); {Force Scroll}
  end;
end;

procedure TIBLookupComboEditBox.RecordChanged(Sender: TObject; aField: TField);
begin
  {Make sure that we are in sync with other data controls}
  if DataSource = nil then
  begin
    KeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant;
    if VarIsNull(KeyValue) then {Probable deletion}
    begin
      UpdateList;
      KeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant;
    end;
  end;
end;

procedure TIBLookupComboEditBox.SetAutoCompleteText(
  AValue: TComboBoxAutoCompleteText);
begin
  if AValue <> AutoCompleteText then
  begin
    FAutoComplete := cbactEnabled in AValue;
    inherited AutoCompleteText := AValue - [cbactEnabled]
  end;
end;

procedure TIBLookupComboEditBox.SetListSource(AValue: TDataSource);
begin
  if AValue <> inherited ListSource then
  begin
    FDataLink.DataSource := AValue;
    inherited ListSource := AValue;
  end;
end;

procedure TIBLookupComboEditBox.UpdateList;
{ Note: Algorithm taken from TCustomComboBox.KeyUp but modified to use the
  ListSource DataSet as the source for the autocomplete text. It also runs
  after a delay rather than immediately on keyup
}
var
  iSelStart: Integer; // char position
  sCompleteText, sPrefixText, sResultText: string;
  curText: string;
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet)
     and ListSource.DataSet.Active then
  begin
    FUpdating := true;
    try
         iSelStart := SelStart;//Capture original cursor position
         if ((iSelStart < UTF8Length(Text)) and
           (cbactEndOfLineComplete in AutoCompleteText)) then
                Exit;
         curText := Text;
         sPrefixText := UTF8Copy(Text, 1, iSelStart);
         ListSource.DataSet.Active := false;
         ListSource.DataSet.Active :=  true;
         Text := curText;
         if not FExiting and Focused and (Text <> '')then
         begin
           if ListSource.DataSet.Active and (ListSource.DataSet.RecordCount > 0) then
           begin
             sCompleteText := ListSource.DataSet.FieldByName(ListField).AsString;
             if (sCompleteText <> Text) then
             begin
               sResultText := sCompleteText;
               if ((cbactEndOfLineComplete in AutoCompleteText) and
                         (cbactRetainPrefixCase in AutoCompleteText)) then
               begin//Retain Prefix Character cases
                 UTF8Delete(sResultText, 1, iSelStart);
                 UTF8Insert(sPrefixText, sResultText, 1);
               end;
               Text := sResultText;
               SelStart := iSelStart;
               SelLength := UTF8Length(Text);
             end;
           end;
         end;
    finally
      FUpdating := false
    end;
  end;
end;

procedure TIBLookupComboEditBox.UpdateSQL(Sender: TObject;
  Parser: TSelectSQLParser);
var FieldPosition: integer;
begin
  if FFiltered then
  begin
    if cbactSearchCaseSensitive in AutoCompleteText then
      Parser.Add2WhereClause(GetRelationNameQualifier + '"' + ListField + '" Like ''' + Text + '%''')
    else
      Parser.Add2WhereClause(GetRelationNameQualifier + 'Upper("' + ListField + '") Like Upper(''' + Text + '%'')');

  end;
  if cbactSearchAscending in AutoCompleteText then
  begin
    FieldPosition := Parser.GetFieldPosition(ListField);
    if FieldPosition = 0 then Exit;

    Parser.OrderByClause := IntToStr(FieldPosition) + ' ascending';
  end;
end;

procedure TIBLookupComboEditBox.HandleEnter(Data: PtrInt);
begin
  SelectAll
end;

procedure TIBLookupComboEditBox.UpdateLinkData(Sender: TObject);
begin
  if FInserting then
    ListSource.DataSet.FieldByName(ListField).AsString := Text
end;

procedure TIBLookupComboEditBox.CheckAndInsert;
var Accept: boolean;
    NewKeyValue: variant;
begin
  if AutoInsert and (Text <> '') and assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active and (ListSource.DataSet.RecordCount = 0) then
  try
    {Is it OK to insert a new list member?}
    Accept := true;
    if assigned(FOnCanAutoInsert) then
       OnCanAutoInsert(self,Text,Accept);
    if not Accept then
    begin
      ResetParser;
      Text := FOriginalTextValue;
      SelectAll;
      Exit;
    end;

    FInserting := true;
    try
      {New Value}
      FFiltered := false;
      if assigned(FOnAutoInsert) then
      begin
        {In an OnAutoInsert handler, the client is expected to insert the new
         row into the List DataSet and to set the KeyValue property to the
         value of the primary key of the new row.}
        OnAutoInsert(self,Text,NewKeyValue);
      end
      else
      begin
        ListSource.DataSet.Append;
        {The new KeyValue should be determined by an external generator or
         in the "OnInsert" handler. If it is the same as the ListField, then
         it will be set from the UpdateLinkData method}
        try
          ListSource.DataSet.Post;
        except
          ListSource.DataSet.Cancel;
          raise;
        end;
        NewKeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant;
      end;
      Text := ''; {Ensure full list}
      UpdateList;
      KeyValue := NewKeyValue;
      UpdateData(nil); {Force sync with DataField}
    finally
      FInserting := false
    end;
  except
    Text := FOriginalTextValue;
    ResetParser;
    raise;
  end;
end;

procedure TIBLookupComboEditBox.DoEnter;
begin
  inherited DoEnter;
  FOriginalTextValue:= Text;
  ResetParser;
  Application.QueueAsyncCall(@HandleEnter,0);
end;

procedure TIBLookupComboEditBox.DoExit;
begin
  FExiting := true;
  try
    CheckAndInsert;
    ResetParser;
    FTimer.Interval := 0;
  finally
    FExiting := false;
  end;
  inherited DoExit;
end;

procedure TIBLookupComboEditBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_RETURN then
     EditingDone
  else
  if Key = VK_ESCAPE then
  begin
    SelStart := UTF8Length(Text);      {Ensure end of line selection}
    ResetParser;
    Text := FOriginalTextValue;
    SelectAll;
  end
  else
  if (IsEditableTextKey(Key) or (Key = VK_BACK))
     and AutoComplete and (Style <> csDropDownList) and
     (not (cbactEndOfLineComplete in AutoCompleteText) or (SelStart = UTF8Length(Text))) then
    FTimer.Interval := FKeyPressInterval
  else
    FTimer.Interval := 0
end;

procedure TIBLookupComboEditBox.SetItemIndex(const Val: integer);
begin
  inherited SetItemIndex(Val);
  FLastKeyValue := KeyValue;
end;

procedure TIBLookupComboEditBox.UpdateShowing;
begin
  inherited UpdateShowing;
  if Showing then {Ensure up-to-date as we were ignoring any changes}
    ActiveChanged(nil);
end;

constructor TIBLookupComboEditBox.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TIBLookupComboDataLink.Create(self);
  FKeyPressInterval := 500;
  FAutoComplete := true;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 0;
  FTimer.OnTimer := @HandleTimer;
  FLastKeyValue := NULL;
end;

destructor TIBLookupComboEditBox.Destroy;
begin
  if assigned(FDataLink) then FDataLink.Free;
  if assigned(FTimer) then FTimer.Free;
  inherited Destroy;
end;

procedure TIBLookupComboEditBox.EditingDone;
begin
  CheckAndInsert;
  inherited EditingDone;
end;

end.
