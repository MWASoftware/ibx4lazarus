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
unit dbFieldListPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, DB, PropEdits;

type

  { TIndexFieldNamesProperty }

  TIndexFieldNamesProperty =  class(TStringProperty)
  protected
    function GetFieldDefs: TFieldDefs; virtual;
    function GetIndexFieldNames: string; virtual; abstract;
    procedure SetIndexFieldNames(const Value: string); virtual; abstract;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
end;

  { TFieldListEditor }

  TFieldListEditor = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    AvailableFields: TListBox;
    SelectedFields: TListBox;
    SelectButton: TSpeedButton;
    DeselectButton: TSpeedButton;
    FSelectedFields: TStringList;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure DeselectButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SelectButtonClick(Sender: TObject);
  private
    { private declarations }
    FFieldDefs: TFieldDefs;
    FIndexFieldNames: string;
    procedure ExtractNames(List: TStringList; Names: string);
  public
    { public declarations }
    constructor Create(TheComponent: TComponent); override;
    destructor Destroy; override;
    property FieldDefs: TFieldDefs read FFieldDefs write FFieldDefs;
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
  end;

var
  FieldListEditor: TFieldListEditor;

function EditIndexFieldList(var aIndexFieldNames: string; aFieldDefs: TFieldDefs): boolean;

implementation

function EditIndexFieldList(var aIndexFieldNames: string; aFieldDefs: TFieldDefs
  ): boolean;
begin
  with TFieldListEditor.Create(Application) do
  try
    IndexFieldNames := aIndexFieldNames;
    FieldDefs := aFieldDefs;
    Result := ShowModal = mrOK;
    if Result then
      aIndexFieldNames := IndexFieldNames
  finally
    Free
  end;
end;

{$R *.lfm}

{ TIndexFieldNamesProperty }

function TIndexFieldNamesProperty.GetFieldDefs: TFieldDefs;
begin
  Result := nil
end;

procedure TIndexFieldNamesProperty.Edit;
var aIndexFieldNames: string;
begin
  aIndexFieldNames := IndexFieldNames;
  if EditIndexFieldList(aIndexFieldNames,FieldDefs) then
  begin
    IndexFieldNames := aIndexFieldNames;
    Modified
  end
end;

function TIndexFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TFieldListEditor }

procedure TFieldListEditor.ApplicationProperties1Idle(Sender: TObject;
  var Done: Boolean);
begin
  DeselectButton.Enabled :=  SelectedFields.ItemIndex >= 0;
  SelectButton.Enabled :=  AvailableFields.ItemIndex >= 0;
end;

procedure TFieldListEditor.DeselectButtonClick(Sender: TObject);
begin
  AvailableFields.Items.Add(SelectedFields.Items[SelectedFields.ItemIndex]);
  SelectedFields.Items.Delete(SelectedFields.ItemIndex);
  SelectedFields.ItemIndex := -1
end;

procedure TFieldListEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var I: integer;
begin
  if assigned(FieldDefs) and (ModalResult = mrOK) then
  begin
      FIndexFieldNames := '';
      for i := 0 to SelectedFields.Count - 1 do
        if i = 0 then
          FIndexFieldNames := SelectedFields.Items[0]
        else
          FIndexFieldNames := FIndexFieldNames + ';' + SelectedFields.items[i]
  end;
end;

procedure TFieldListEditor.FormShow(Sender: TObject);
var i: integer;
begin
  FSelectedFields.Clear;
  AvailableFields.Items.Clear;
  if not assigned(FieldDefs) then Exit;

  ExtractNames(FSelectedFields, FIndexFieldNames);
  FieldDefs.Update;
  for i := FSelectedFields.Count - 1 downto 0 do
      if FieldDefs.Find(FSelectedFields[i]) = nil then
         FSelectedFields.Delete(i);

  SelectedFields.Items.Assign(FSelectedFields);
  SelectedFields.ItemIndex := -1;

  for i := 0 to FieldDefs.Count - 1 do
    if SelectedFields.Items.IndexOf(FieldDefs.Items[i].Name) = -1 then
       AvailableFields.Items.Add(FieldDefs.Items[i].Name);
  AvailableFields.ItemIndex := -1
end;

procedure TFieldListEditor.SelectButtonClick(Sender: TObject);
begin
  SelectedFields.Items.Add(AvailableFields.Items[AvailableFields.ItemIndex]);
  AvailableFields.Items.Delete(AvailableFields.ItemIndex);
  AvailableFields.ItemIndex := -1
end;

procedure TFieldListEditor.ExtractNames(List: TStringList; Names: string);
var idx: integer;
begin
 idx := 1;
 List.Clear;
 while idx <= Length(Names) do
       List.Add(ExtractFieldName(Names,idx));
end;

constructor TFieldListEditor.Create(TheComponent: TComponent);
begin
  inherited Create(TheComponent);
  FSelectedFields := TStringList.Create;
  FSelectedFields.Delimiter := ';';
  FSelectedFields.StrictDelimiter := true
end;

destructor TFieldListEditor.Destroy;
begin
 if assigned(FSelectedFields) then FSelectedFields.Free;
  inherited Destroy;
end;

end.

