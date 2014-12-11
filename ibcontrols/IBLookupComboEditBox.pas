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

  TAutoInsert = procedure(Sender: TObject; aText: string) of object;
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
    FTimer: TTimer;
    FFiltered: boolean;
    FOnAutoInsert: TAutoInsert;
    FOriginalTextValue: string;
    FUpdating: boolean;
    FInserting: boolean;
    FLastKeyValue: variant;
    procedure ActiveChanged(Sender: TObject);
    function GetAutoCompleteText: TComboBoxAutoCompleteText;
    function GetListSource: TDataSource;
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
    procedure CheckAndInsert;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetItemIndex(const Val: integer); override;
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
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 500;
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

procedure TIBLookupComboEditBox.ActiveChanged(Sender: TObject);
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and ListSource.DataSet.Active
     and (FInserting or not FUpdating)  then
  begin
    begin
      if varIsNull(FLastKeyValue) and (ItemIndex = -1) then
        Text := ListSource.DataSet.FieldByName(ListField).AsString
      else
      begin
        KeyValue := FLastKeyValue;
        UpdateData(self); {Force auto scroll}
        if varIsNull(KeyValue) then {Value not present}
          Text := ListSource.DataSet.FieldByName(ListField).AsString
      end;
    end;
  end;
end;

function TIBLookupComboEditBox.GetAutoCompleteText: TComboBoxAutoCompleteText;
begin
  Result := inherited AutoCompleteText;
  if AutoComplete then
     Result := Result + [cbactEnabled]
end;

procedure TIBLookupComboEditBox.ResetParser;
begin
  if FFiltered then
  begin
    FFiltered := false;
    UpdateList;
    UpdateData(self); {Force Scroll}
  end;
end;

procedure TIBLookupComboEditBox.RecordChanged(Sender: TObject; aField: TField);
begin
  {Make sure that we are in sync with other data controls}
  if DataSource = nil then
    KeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant
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
  inherited ListSource := AValue;
  FDataLink.DataSource := AValue
end;

procedure TIBLookupComboEditBox.UpdateList;
{ Note: Algorithm taken from TCustomComboBox.KeyUp but modified to use the
  ListSource DataSet as the source for the autocomplete text. It also runs
  after a delay rather than immediately on keyup
}
var
  iSelStart: Integer; // char position
  sCompleteText, sPrefixText, sResultText: string;
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet)
     and ListSource.DataSet.Active then
  begin
    FUpdating := true;
    try
         ListSource.DataSet.Active := false;
         ListSource.DataSet.Active :=  true;
         if Focused and (Text <> '')then
         begin
           if ListSource.DataSet.Active and (ListSource.DataSet.RecordCount > 0) then
           begin
             iSelStart := SelStart;//Capture original cursor position
             sPrefixText := UTF8Copy(Text, 1, iSelStart);
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
begin
  if FFiltered then
  begin
    if cbactSearchCaseSensitive in AutoCompleteText then
      Parser.Add2WhereClause('"' + ListField + '" Like ''' + Text + '%''')
    else
      Parser.Add2WhereClause('Upper("' + ListField + '") Like Upper(''' + Text + '%'')');

  end;
  if cbactSearchAscending in AutoCompleteText then
     Parser.OrderByClause := '"' + ListField + '" ascending';
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
begin
  if AutoInsert and (Text <> '') and assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active and (ListSource.DataSet.RecordCount = 0) then
  begin
    {Is it OK to insert a new list member?}
    Accept := true;
    if assigned(FOnCanAutoInsert) then
       OnCanAutoInsert(self,Text,Accept);
    if not Accept then Exit;

    FInserting := true;
    try
      {New Value}
      if assigned(FOnAutoInsert) then
        OnAutoInsert(self,Text)
      else
        ListSource.DataSet.Append;
      FLastKeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant;
      UpdateList;
    finally
      FInserting := false
    end
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
  inherited DoExit;
  CheckAndInsert;
  ResetParser;
  FTimer.Interval := 0;
end;

procedure TIBLookupComboEditBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_RETURN then
     EditingDone
  else
  if Key = VK_ESCAPE then
  begin
    ResetParser;
    Text := FOriginalTextValue;
  end
  else
  if IsEditableTextKey(Key)  and AutoComplete and (Style <> csDropDownList) and
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
