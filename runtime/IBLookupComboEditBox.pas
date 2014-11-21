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
  ExtCtrls, IBSQLParserUnit, DB;

type

  TIBLookupComboEditBox = class;

  TCustomInsert = procedure(Sender: TObject; aText: string; var KeyValue: variant) of object;

  { TLookupDataLink }

  { TIBLookupComboEditBox }

  TIBLookupComboEditBox = class(TDBLookupComboBox)
  private
    FAutoComplete: boolean;
    FAutoInsert: boolean;
    FCaseSensitiveMatch: boolean;
    { Private declarations }
     FTimer: TTimer;
     FFiltered: boolean;
     FOnCustomInsert: TCustomInsert;
     FOriginalTextValue: string;
     procedure Handletimer(Sender: TObject);
     procedure ResetParser;
     procedure UpdateList;
     procedure HandleEnter(Data: PtrInt);
  protected
    { Protected declarations }
    procedure DataChange(Sender: TObject); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
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
    property OnCustomInsert: TCustomInsert read FOnCustomInsert write FOnCustomInsert;
  end;


implementation

uses IBQuery, IBCustomDataSet, LCLType, Variants;

{ TIBLookupComboEditBox }

procedure TIBLookupComboEditBox.Handletimer(Sender: TObject);
var ActiveState: boolean;
begin
  FTimer.Interval := 0;
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) then
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

procedure TIBLookupComboEditBox.ResetParser;
begin
  if FFiltered and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) then
  begin
    TIBDataSet(ListSource.DataSet).Parser.ResetWhereClause;
    UpdateList;
    FFiltered := false
  end;
end;

procedure TIBLookupComboEditBox.UpdateList;
var CurSelLength: integer;
begin
  if assigned(ListSource) and assigned(ListSource.DataSet) and (ListSource.DataSet is TIBCustomDataSet) then
  begin
       if ListSource.DataSet.Active then
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
       end
  end;
end;

procedure TIBLookupComboEditBox.HandleEnter(Data: PtrInt);
begin
  SelectAll
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
  FTimer.Interval := 0;
end;

procedure TIBLookupComboEditBox.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TIBLookupComboEditBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if AutoComplete and not (Word(Key)  in [VK_RETURN,VK_ESCAPE]) then
    FTimer.Interval := 500
end;

constructor TIBLookupComboEditBox.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FAutoInsert := true;
  FAutoComplete := true;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 0;
  FTimer.OnTimer := @HandleTimer;
end;

destructor TIBLookupComboEditBox.Destroy;
begin
  if assigned(FTimer) then FTimer.Free;
  inherited Destroy;
end;

procedure TIBLookupComboEditBox.EditingDone;
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
    UpdateData(nil);
  end;
  inherited EditingDone;
end;

end.
