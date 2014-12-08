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
  ExtCtrls, IBSQLParser, DB;

type

  {TIBLookupComboEditBox is a TDBLookupComboBox descendent that implements "autocomplete"
   of typed in text and "autoinsert" of new entries. Autocomplete uses SQL manipulation
   to revise the available list and restrict it to items that are prefixed by the
   typed text (either case sensitive or case insenstive). Autoinsert allows a
   newly typed entry to be added to the list dataset and included in the available
   list items.    }

  TCustomInsert = procedure(Sender: TObject; aText: string; var KeyValue: variant) of object;

  TIBLookupComboEditBox = class;

  { TIBLookupComboDataLink }

  TIBLookupComboDataLink = class(TDataLink)
  private
    FOwner: TIBLookupComboEditBox;
  protected
    procedure DataEvent(Event: TDataEvent; Info: Ptrint); override;
  public
    constructor Create(AOwner: TIBLookupComboEditBox);
  end;


  { TIBLookupComboEditBox }

  TIBLookupComboEditBox = class(TDBLookupComboBox)
  private
    { Private declarations }
    FDataLink: TIBLookupComboDataLink;
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FCaseSensitiveMatch: boolean;
    FKeyPressInterval: integer;
    FTimer: TTimer;
    FFiltered: boolean;
    FOnCustomInsert: TCustomInsert;
    FOriginalTextValue: string;
    function GetListSource: TDataSource;
    procedure HandleTimer(Sender: TObject);
    procedure ResetParser;
    procedure SetListSource(AValue: TDataSource);
    procedure UpdateList;
    procedure UpdateSQL(Sender: TObject; Parser: TSelectSQLParser);
    procedure HandleEnter(Data: PtrInt);
  protected
    { Protected declarations }
    procedure CheckAndInsert;
    procedure DataChange(Sender: TObject); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
  published
    { Published declarations }
    property AutoInsert: boolean read FAutoInsert write FAutoInsert default true;
    property AutoComplete: boolean read FAutoComplete write FAutoComplete default true;
    property CaseSensitiveMatch: boolean read FCaseSensitiveMatch write FCaseSensitiveMatch;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyPressInterval: integer read FKeyPressInterval write FKeyPressInterval default 500;
    property OnCustomInsert: TCustomInsert read FOnCustomInsert write FOnCustomInsert;
  end;


implementation

uses IBQuery, IBCustomDataSet, LCLType, Variants, LCLProc;

{ TIBLookupComboDataLink }

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

procedure TIBLookupComboEditBox.ResetParser;
begin
  if FFiltered and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) and
     ListSource.DataSet.Active then
  begin
    TIBDataSet(ListSource.DataSet).Parser.Reset;
    UpdateList;
    FFiltered := false
  end;
end;

procedure TIBLookupComboEditBox.SetListSource(AValue: TDataSource);
begin
  inherited ListSource := AValue;
  FDataLink.DataSource := AValue
end;

procedure TIBLookupComboEditBox.UpdateList;
var CurSelLength: integer;
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) then
  begin
         ListSource.DataSet.Active := false;
         ListSource.DataSet.Active :=  true;
         if Focused and (Text <> '')then
         begin
           CurSelLength := UTF8Length(Text);
           if ListSource.DataSet.Active and (ListSource.DataSet.RecordCount > 0) then
           begin
             Text := ListSource.DataSet.FieldByName(ListField).AsString;
             SelStart := CurSelLength ;
             SelLength := UTF8Length(Text) - CurSelLength
           end;
         end;
  end;
end;

procedure TIBLookupComboEditBox.UpdateSQL(Sender: TObject;
  Parser: TSelectSQLParser);
begin
  if FFiltered then
  begin
    if CaseSensitiveMatch then
      Parser.Add2WhereClause(ListField + ' Like ''' + Text + '%''')
    else
      Parser.Add2WhereClause('Upper(' + ListField + ') Like Upper(''' + Text + '%'')');
  end;
end;

procedure TIBLookupComboEditBox.HandleEnter(Data: PtrInt);
begin
  SelectAll
end;

procedure TIBLookupComboEditBox.CheckAndInsert;
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
         CurText := FOriginalTextValue
    end
    else
    if AutoInsert then
    begin
      ListSource.DataSet.DisableControls;
      try
        ListSource.DataSet.Append;
        try
          ListSource.DataSet.FieldByName(ListField).AsString := CurText;
          ListSource.DataSet.Post;
       except
          ListSource.DataSet.Cancel;
          Text := FOriginalTextValue;
          raise
       end;
      finally
        ListSource.DataSet.EnableControls
      end;
    end;
    ResetParser; {Closes ListSource DataSet}
    ListSource.DataSet.Active := true;
    Text := CurText;
    UpdateData(nil);
  end;
end;

procedure TIBLookupComboEditBox.DataChange(Sender: TObject);
begin
  inherited DataChange(Sender);
  ResetParser;
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
  if AutoComplete  then
    FTimer.Interval := FKeyPressInterval
end;

constructor TIBLookupComboEditBox.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FDataLink := TIBLookupComboDataLink.Create(self);
  FKeyPressInterval := 500;
  FAutoInsert := true;
  FAutoComplete := true;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 0;
  FTimer.OnTimer := @HandleTimer;
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
