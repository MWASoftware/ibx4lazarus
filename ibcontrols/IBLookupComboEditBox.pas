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
  Classes, SysUtils, LCLType, LResources, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ExtCtrls,  DB, StdCtrls,  LCLVersion, IBDynamicInterfaces;

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
    {$if lcl_fullversion < 2000000}
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
    {$endif}
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TIBLookupComboEditBox);
  end;

  { TIBLookupComboEditBox }

  TIBLookupComboEditBox = class(TDBLookupComboBox,IDynamicSQLComponent)
  private
    { Private declarations }
    FDataLink: TIBLookupComboDataLink;
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FKeyPressInterval: integer;
    FOnCanAutoInsert: TCanAutoInsert;
    FOnSetParams : TOnSetParams;
    FOnUpdateSQL : TOnUpdateSQL;
    FRelationName: string;
    FTimer: TTimer;
    FFiltered: boolean;
    FOnAutoInsert: TAutoInsert;
    FOriginalTextValue: string;
    FUpdating: boolean;
    FInserting: boolean;
    FExiting: boolean;
    FForceAutoComplete: boolean;
    FInCheckAndInsert: boolean;
    FLastKeyValue: variant;
    FCurText: string;
    FModified: boolean;
    FCurListDataset: TDataset;
    procedure DoActiveChanged(Data: PtrInt);
    function GetAutoCompleteText: TComboBoxAutoCompleteText;
    function GetListDatset : TDataset;
    function GetListSource: TDataSource;
    function GetRelationNameQualifier: string;
    procedure HandleTimer(Sender: TObject);
    procedure ListDatasetChanged;
    procedure ResetParser;
    procedure RecordChanged(Sender: TObject; aField: TField);
    procedure SetAutoCompleteText(AValue: TComboBoxAutoCompleteText);
    procedure SetListSource(AValue: TDataSource);
    procedure UpdateList;
    procedure HandleEnter(Data: PtrInt);
    procedure UpdateLinkData(Sender: TObject);
    procedure ValidateListField;
  protected
    { Protected declarations }
    procedure ActiveChanged(Sender: TObject);
    procedure CheckAndInsert;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$if lcl_fullversion >= 2000002}
    {Deferred update changes in Lazarus 2.0 stop the combo box working when
     the datasource is nil. We thus have to reverse out the changes :(}
    function DoEdit: boolean; override;
    procedure Change; override;
    procedure CloseUp; override;
    procedure Select; override;
    {$ifend}
    {$if lcl_fullversion = 2000002}
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    {$ifend}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetItemIndex(const Val: integer); override;
    procedure UpdateShowing; override;
    procedure UpdateData(Sender: TObject); override;
    {IDynamicSQLComponent}
    procedure UpdateSQL(SQLEditor: IDynamicSQLEditor);
    procedure SetParams(SQLParamProvider: IDynamicSQLParam);
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property ListDataSet: TDataset read GetListDatset;
  published
    { Published declarations }
    property AutoInsert: boolean read FAutoInsert write FAutoInsert;
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default true;
    property AutoCompleteText: TComboBoxAutoCompleteText read GetAutoCompleteText
             write SetAutoCompleteText;
    property ItemHeight;
    property ItemWidth;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 200;
    property RelationName: string read FRelationName write FRelationName;
    property OnAutoInsert: TAutoInsert read FOnAutoInsert write FOnAutoInsert;
    property OnCanAutoInsert: TCanAutoInsert read FOnCanAutoInsert write FOnCanAutoInsert;
    property OnUpdateSQL: TOnUpdateSQL read FOnUpdateSQL write FOnUpdateSQL;
    property OnSetParams: TOnSetParams read FOnSetParams write FOnSetParams;
  end;


implementation

uses Variants, LCLProc, LazUTF8;

resourcestring
  ibxeListFieldNotFound = 'ListField Name is not a valid dataset column name (%s)';


{ TIBLookupComboDataLink }

procedure TIBLookupComboDataLink.ActiveChanged;
begin
  FOwner.ActiveChanged(self)
end;

{$if lcl_fullversion < 2000000}
procedure TIBLookupComboDataLink.DataEvent(Event: TDataEvent; Info: Ptrint);
begin
  inherited DataEvent(Event, Info);
  if Event = deLayoutChange then
   FOwner.LookupCache := FOwner.LookupCache; {sneaky way of calling UpdateLookup}
end;
{$endif}

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
begin
  FTimer.Interval := 0;
  FFiltered := Text <> '';
  UpdateList
end;

procedure TIBLookupComboEditBox.ListDatasetChanged;
begin
  if FCurListDataset <> ListDataSet then
  begin
    if (FCurListDataset <> nil) and (FCurListDataset is IDynamicSQLDataset) then
      (FCurListDataset as IDynamicSQLDataset).UnRegisterDynamicComponent(self);
    if (ListDataSet <> nil) and (ListDataSet is IDynamicSQLDataset) then
    with ListDataSet as IDynamicSQLDataset do
    if [dcUpdateWhereClause,dcChangeDatasetOrder] <= GetCapabilities then
      RegisterDynamicComponent(self);
    FCurListDataset := ListDataSet;
  end;
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
  ListDatasetChanged;
end;

procedure TIBLookupComboEditBox.DoActiveChanged(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;

  if (DataSource = nil) and assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active   then
  begin
    begin
      ValidateListField;
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

function TIBLookupComboEditBox.GetListDatset : TDataset;
begin
  if ListSource = nil then
    Result := nil
  else
    Result := ListSource.Dataset;
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
    ListDatasetChanged;
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
begin
  if assigned(ListSource) and assigned(ListSource.DataSet)
     and ListSource.DataSet.Active then
  begin
    FCurText := Text;
    FUpdating := true;
    try
         iSelStart := SelStart;//Capture original cursor position
         if ((iSelStart < UTF8Length(Text)) and
           (cbactEndOfLineComplete in AutoCompleteText)) then
                Exit;
         sPrefixText := UTF8Copy(Text, 1, iSelStart);
         ListSource.DataSet.Active := false;
         ListSource.DataSet.Active :=  true;
         Text := FCurText;
         if not FExiting and (FForceAutoComplete or Focused) and (FCurText <> '')then
         begin
           if ListSource.DataSet.Active and (ListSource.DataSet.RecordCount > 0) then
           begin
             sCompleteText := ListSource.DataSet.FieldByName(ListField).AsString;
             if (sCompleteText <> FCurText) then
             begin
               KeyValue := ListSource.DataSet.FieldByName(KeyField).AsVariant;
               sResultText := sCompleteText;
               if ((cbactEndOfLineComplete in AutoCompleteText) and
                         (cbactRetainPrefixCase in AutoCompleteText)) then
               begin//Retain Prefix Character cases
                 UTF8Delete(sResultText, 1, iSelStart);
                 UTF8Insert(sPrefixText, sResultText, 1);
               end;
               Text := sResultText;
             end;
             SelStart := iSelStart;
             SelLength := UTF8Length(Text) - iSelStart;
           end
           else
           begin
             SelStart := iSelStart;
             SelLength := 0;
           end;
         end;
    finally
      FUpdating := false
    end;
    FModified := true;
  end;
end;

procedure TIBLookupComboEditBox.HandleEnter(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;
   SelectAll
end;

procedure TIBLookupComboEditBox.UpdateLinkData(Sender: TObject);
begin
  if FInserting then
    ListSource.DataSet.FieldByName(ListField).AsString := Text
end;

{Check to ensure that ListField exists and convert to upper case if necessary}

procedure TIBLookupComboEditBox.ValidateListField;
var  FieldNames: TStringList;
begin
  if (ListSource = nil) or (ListSource.DataSet = nil) then Exit;
  FieldNames := TStringList.Create;
  try
    FieldNames.CaseSensitive := true;
    FieldNames.Sorted := true;
    FieldNames.Duplicates := dupError;
    ListSource.DataSet.GetFieldNames(FieldNames);
    if FieldNames.IndexOf(ListField) = -1 then {not found}
    begin
      if FieldNames.IndexOf(AnsiUpperCase(ListField)) <> - 1 then {normalise to upper case}
        ListField := AnsiUpperCase(ListField)
      else
        raise Exception.CreateFmt(ibxeListFieldNotFound,[ListField])
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TIBLookupComboEditBox.CheckAndInsert;
var Accept: boolean;
    NewKeyValue: variant;
begin
  if FInCheckAndInsert then Exit;
  FInCheckAndInsert := true;
  try
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
  finally
    FInCheckAndInsert := false
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
  if FTimer.Interval <> 0 then
    HandleTimer(nil);
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
  if Key = VK_ESCAPE then
  begin
    SelStart := UTF8Length(Text);      {Ensure end of line selection}
    ResetParser;
    Text := FOriginalTextValue;
    SelectAll;
  end
  else
  if AutoComplete and (Style <> csDropDownList) then
  begin
    if (Key = VK_BACK) or (Key = VK_DELETE) then
    begin
      if SelStart = 0 then
      begin
        SelStart := UTF8Length(Text);
        SelLength := 0;
      end;
      FTimer.Interval := 0;
    end
    else
    if IsEditableTextKey(Key) and
     (not(cbactEndOfLineComplete in AutoCompleteText) or (SelStart = UTF8Length(Text))) then
    begin
      FTimer.Interval := 0;
      FTimer.Interval := FKeyPressInterval;
    end;
  end;
end;

procedure TIBLookupComboEditBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then
    ListSource := nil;
end;

procedure TIBLookupComboEditBox.SetItemIndex(const Val: integer);
begin
  if Val > 0 then
    FCurText := '';
  inherited SetItemIndex(Val);
  FLastKeyValue := KeyValue;
end;

procedure TIBLookupComboEditBox.UpdateShowing;
begin
  inherited UpdateShowing;
  if Showing then {Ensure up-to-date as we were ignoring any changes}
    ActiveChanged(nil);
end;

procedure TIBLookupComboEditBox.UpdateData(Sender: TObject);
begin
  inherited UpdateData(Sender);
  if FCurText <> '' then
    Text := FCurText + Text;
  FModified := false;
end;

procedure TIBLookupComboEditBox.UpdateSQL(SQLEditor : IDynamicSQLEditor);
var FilterText: string;
begin
  if FFiltered then
  begin
    if FUpdating then
      FilterText := FCurText
    else
      FilterText := Text;

    with SQLEditor do
    begin
      if cbactSearchCaseSensitive in AutoCompleteText then
        Add2WhereClause(GetRelationNameQualifier + QuoteIdentifierIfNeeded(ListField) + ' Like ''' +
                                    SQLSafeString(FilterText) + '%''')
      else
        Add2WhereClause('Upper(' + GetRelationNameQualifier + QuoteIdentifierIfNeeded(ListField) + ') Like Upper(''' +
                                    SQLSafeString(FilterText) + '%'')');

      if cbactSearchAscending in AutoCompleteText then
        Orderby(ListField,true);
    end;
  end;
  if assigned(FOnUpdateSQL) then
    OnUpdateSQL(self,SQLEditor);
end;

procedure TIBLookupComboEditBox.SetParams(SQLParamProvider : IDynamicSQLParam);
begin
  if assigned(FOnSetParams) then
    OnSetParams(self, SQLParamProvider);
end;


{Workarounds due to bugs in various Lazarus 2.0 release candidates}
{$if lcl_fullversion >= 2000002}
type

  { THackedCustomComboBox }

  THackedCustomComboBox = class(TCustomComboBox)
  private
    procedure CallChange;
    procedure CallUTF8KeyPress(var UTF8Key: TUTF8Char);
  end;

{ THackedCustomComboBox }

procedure THackedCustomComboBox.CallChange;
begin
  inherited Change;
end;

procedure THackedCustomComboBox.CallUTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
end;

procedure TIBLookupComboEditBox.Change;
begin
  if DataSource = nil then
    THackedCustomComboBox(self).CallChange
  else
    inherited Change;
end;

procedure TIBLookupComboEditBox.CloseUp;
begin
  inherited DoEdit;
  inherited CloseUp;
  EditingDone;
end;

procedure TIBLookupComboEditBox.Select;
begin
  inherited Select;
  if DataSource = nil then
    inherited DoEdit;
end;

function TIBLookupComboEditBox.DoEdit: boolean;
begin
  {DoEdit will swallow characters if no editable Field. Hence, to enabled
   writing we must avoid calling the inherited method.}
  if DataSource = nil then
    Result := true
  else
    Result := inherited DoEdit;
end;
{$ifend}

{$if lcl_fullversion = 2000002}
procedure TIBLookupComboEditBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  if DataSource = nil then
    THackedCustomComboBox(self).CallUTF8KeyPress(UTF8Key)
  else
    inherited;
end;
{$ifend}


constructor TIBLookupComboEditBox.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TIBLookupComboDataLink.Create(self);
  FKeyPressInterval := 200;
  FAutoComplete := true;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 0;
  FTimer.OnTimer := @HandleTimer;
  FLastKeyValue := NULL;
end;

destructor TIBLookupComboEditBox.Destroy;
begin
  if ListDataset <> nil then
    (ListDataset as IDynamicSQLDataset).UnRegisterDynamicComponent(self);
  if assigned(FDataLink) then FDataLink.Free;
  if assigned(FTimer) then FTimer.Free;
  Application.RemoveAsyncCalls(self);
  inherited Destroy;
end;

procedure TIBLookupComboEditBox.EditingDone;
begin
  FForceAutoComplete := true;
  try
  if FTimer.Interval <> 0 then
    HandleTimer(nil);
  finally
    FForceAutoComplete := false;
  end;
  CheckAndInsert;
  FCurText := '';
  if FModified then
    Change; {ensure Update}
  inherited EditingDone;
end;

end.
