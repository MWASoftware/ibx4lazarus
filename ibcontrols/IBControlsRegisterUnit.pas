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
 *  The Original Code is (C) 2011-2024 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBControlsRegisterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, DB, ComponentEditors, PropEdits, DBPropEdits, FieldsEditor,
  dbFieldListPropEditor;

type

  { TIBArrayGridEditor }

  TIBArrayGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TDBDynamicGridFieldProperty }

  TDBDynamicGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TDBLookupPropertiesGridFieldProperty }

  TDBLookupPropertiesGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TIBTreeViewFieldProperty }

  TIBTreeViewFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TIBDynamicGridIndexNamesProperty }

  TIBDynamicGridIndexNamesProperty = class(TIndexFieldNamesProperty)
  protected
    function GetFieldDefs: TFieldDefs; override;
    function GetIndexFieldNames: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
  end;

procedure Register;

implementation

uses IBDynamicGrid, IBLookupComboEditBox, IBTreeView, DBControlGrid, IBArrayGrid,
  LResources;

const
  PaletteName = 'Dynamic Database Controls';   {do not localize}
  PACKAGE_VERSION = '1.0.0';

resourcestring
  SIBControlsVersion = 'Dynamic DB Controls for Lazarus ' + PACKAGE_VERSION;
  SIBUpdateLayout = 'Update Layout';

procedure LoadDataSourceFields(DataSource: TDataSource; List: TStrings);
var
  DataSet: TDataSet;
  i: Integer;
begin
  if Assigned(DataSource) then
  begin
    DataSet := DataSource.DataSet;
    if Assigned(DataSet) then
    begin
      if DataSet.Fields.Count > 0 then
        DataSet.GetFieldNames(List)
      else
      begin
        DataSet.FieldDefs.Update;
        for i := 0 to DataSet.FieldDefs.Count - 1 do
          List.Add(DataSet.FieldDefs[i].Name);
      end;
    end;
  end;
end;

{ TIBArrayGridEditor }

procedure TIBArrayGridEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index)
  else
  case Index of
    0: TIBArrayGrid(Component).UpdateLayout;
  end;
end;

function TIBArrayGridEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SIBUpdateLayout;
      1 : Result := SIBControlsVersion ;
    end;
  end;
end;

function TIBArrayGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TDBLookupPropertiesGridFieldProperty }

procedure TDBLookupPropertiesGridFieldProperty.FillValues(
  const Values: TStringList);
var
  P: TDBLookupProperties;
begin
  P :=TDBLookupProperties(GetComponent(0));
  if not (P is TDBLookupProperties) then exit;
  LoadDataSourceFields(TIBDynamicGrid(P.Owner.Grid).DataSource, Values);
end;

{ TIBTreeViewFieldProperty }

procedure TIBTreeViewFieldProperty.FillValues(const Values: TStringList);
var ListSource: TDataSource;
begin
  ListSource :=  TIBTreeView(GetComponent(0)).DataSource;
  LoadDataSourceFields(ListSource, Values);
end;

{ TIBDynamicGridIndexNamesProperty }

function TIBDynamicGridIndexNamesProperty.GetFieldDefs: TFieldDefs;
var Grid: TIBDynamicGrid;
begin
  Result := nil;
  Grid := TIBDynamicGrid(GetComponent(0));
  if assigned(Grid.DataSource) and assigned(Grid.DataSource.DataSet) then
     Result := Grid.DataSource.DataSet.FieldDefs
end;

function TIBDynamicGridIndexNamesProperty.GetIndexFieldNames: string;
var Grid: TIBDynamicGrid;
begin
  Grid := TIBDynamicGrid(GetComponent(0));
  Result := Grid.IndexFieldNames
end;

procedure TIBDynamicGridIndexNamesProperty.SetIndexFieldNames(
  const Value: string);
var Grid: TIBDynamicGrid;
begin
  Grid := TIBDynamicGrid(GetComponent(0));
  Grid.IndexFieldNames := Value
end;

{ TDBDynamicGridFieldProperty }

procedure TDBDynamicGridFieldProperty.FillValues(const Values: TStringList);
var
  P: TDBLookupProperties;
begin
  P :=TDBLookupProperties(GetComponent(0));
  if not (P is TDBLookupProperties) then exit;
  LoadDataSourceFields(P.ListSource, Values);
end;

procedure Register;
begin
  RegisterComponents(PaletteName,[TIBLookupComboEditBox,TIBDynamicGrid,TIBTreeView,TDBControlGrid, TIBArrayGrid]);
  RegisterComponentEditor(TIBArrayGrid, TIBArrayGridEditor);

  RegisterPropertyEditor(TypeInfo(string), TDBLookupProperties, 'KeyField', TDBDynamicGridFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupProperties, 'ListField', TDBDynamicGridFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBDynamicGrid, 'IndexFieldNames', TIBDynamicGridIndexNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupProperties, 'DataFieldName', TDBLookupPropertiesGridFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'KeyField', TIBTreeViewFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'TextField', TIBTreeViewFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'ParentField', TIBTreeViewFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'HasChildField', TIBTreeViewFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'ImageIndexField', TIBTreeViewFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTreeView, 'SelectedIndexField', TIBTreeViewFieldProperty);

end;

initialization

{$I ibcontrols.lrs}

end.

