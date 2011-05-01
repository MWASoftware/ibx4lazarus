{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{************************************************************************}

unit IBDBReg;

{$MODE Delphi}

(*
 * Compiler defines
 *)
{$A+}                           (* Aligned records: On *)
{$B-}                           (* Short circuit boolean expressions: Off *)
{$G+}                           (* Imported data: On *)
{$H+}                           (* Huge Strings: On *)
{$J-}                           (* Modification of Typed Constants: Off *)
{$M+}                           (* Generate run-time type information: On *)
{$O+}                           (* Optimization: On *)
{$Q-}                           (* Overflow checks: Off *)
{$R-}                           (* Range checks: Off *)
{$T+}                           (* Typed address: On *)
{$U+}                           (* Pentim-safe FDIVs: On *)
{$W-}                           (* Always generate stack frames: Off *)
{$X+}                           (* Extended syntax: On *)
{$Z1}                           (* Minimum Enumeration Size: 1 Byte *)

interface

uses {Windows,} SysUtils, Classes, Graphics, Dialogs, Controls, Forms, TypInfo,
     DB, IBTable, IBDatabase,  IBEventsEditor,  LazarusPackageIntf,
      IBUpdateSQL, IBXConst, ComponentEditors, PropEdits, DBPropEdits, FieldsEditor;

type

{ TIBFileNameProperty
  Property editor the DataBase Name property.  Brings up the Open dialog }

  TIBFileNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TIBNameProperty
  }
  TIBNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TIBStoredProcNameProperty
    Editor for the TIBStoredProc.StoredProcName property.  Displays a drop-down list of all
    the StoredProcedures in the Database.}
  TIBStoredProcNameProperty = class(TIBNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TIBTableNameProperty
    Editor for the TIBTable.TableName property.  Displays a drop-down list of all
    the Tables in the Database.}
  TIBTableNameProperty = class(TIBNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TIBIndexFieldNamesProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TIBIndexNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{ TIBDatabaseEditor }

  TIBDatabaseEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TIBTransactionEditor }

  TIBTransactionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TIBQueryEditor }

  TIBQueryEditor = class(TFieldsComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TIBStoredProcEditor }

  TIBStoredProcEditor = class(TFieldsComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TIBDataSetEditor }

  TIBDataSetEditor = class(TFieldsComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TIBUpdateSQLEditor }

  TIBUpdateSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
   function GetVerbCount: Integer; override;
  end;

{ TIBSQLEditor }

  TIBSQLEditor  = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
   function GetVerbCount: Integer; override;
  end;

{ TIBServiceEditor}

  TIBServiceEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
   function GetVerbCount: Integer; override;
  end;

  TIBStoredProcParamsProperty = class(TCollectionPropertyEditor)
  public
    procedure Edit; override;
  end;
 (*
  TIBTableFieldLinkProperty = class(TFieldLinkProperty)
  private
    FTable: TIBTable;
  protected
    function GetIndexFieldNames: string; override;
    function GetMasterFields: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
    procedure SetMasterFields(const Value: string); override;
  public
    procedure Edit; override;
  end;
*)
{ TSQLPropertyEditor }

  TSQLPropertyEditor = class(TStringsPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TIBQuerySQLProperty }

  TIBQuerySQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{TIBSQLSQLPropertyEditor }

  TIBSQLSQLPropertyEditor = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBDatasetSQLProperty }

  TIBDatasetSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBSQLProperty }

  TIBSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TUpdateSQLPropertyEditor }

   TUpdateSQLPropertyEditor = class(TSQLPropertyEditor)
   protected
     FIBUpdateSQL: TIBUpdateSQL;
     FDatabase: TIBDatabase;
     function GetObjects: boolean;
   end;

{ TIBUpdateSQLProperty }

  TIBUpdateSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBRefreshSQLProperty }

  TIBRefreshSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBInsertSQLProperty }

  TIBInsertSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBDeleteSQLProperty }

  TIBDeleteSQLProperty = class(TSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

  { TIBUpdateSQLUpdateProperty }

  TIBUpdateSQLUpdateProperty = class(TUpdateSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBUpdateSQLRefreshSQLProperty }

  TIBUpdateSQLRefreshSQLProperty = class(TUpdateSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TIBUpdateSQLInsertSQLProperty }

  TIBUpdateSQLInsertSQLProperty = class(TUpdateSQLPropertyEditor)
  public
    procedure Edit; override;
  end;

  { TIBUpdateSQLDeleteProperty }

  TIBUpdateSQLDeleteProperty = class(TUpdateSQLPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TIBEventListProperty }

  TIBEventListProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{TIBGeneratorProperty}

  TIBGeneratorProperty = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses IB, IBQuery, IBStoredProc, IBCustomDataSet,
     IBIntf, IBSQL, IBSQLMonitor, IBDatabaseInfo, IBEvents,
     IBServices, IBDatabaseEdit, IBTransactionEdit,
     IBBatchMove, DBLoginDlg, IBExtract,LResources, IBSelectSQLEditor,
     IBModifySQLEditor,IBDeleteSQLEditor,IBRefreshSQLEditor,
     IBInsertSQLEditor, IBGeneratorEditor, IBUpdateSQLEditor, IBDataSetEditor,
     IBSQLEditor, ibserviceeditor;



procedure Register;
begin
  RegisterNoIcon([TIBStringField, TIBBCDField]);
  RegisterComponents(IBPalette1, [ TIBQuery, TIBDataSet,
   TIBDatabase, TIBTransaction, TIBUpdateSQL, TIBEvents,
     TIBSQL, TIBDatabaseInfo, TIBSQLMonitor,
       TIBStoredProc,TIBBatchMove,  TIBTable,TIBExtract]);
  if (TryIBLoad) and IBServiceAPIPresent  then
    RegisterComponents(IBPalette2, [TIBConfigService, TIBBackupService,
      TIBRestoreService, TIBValidationService, TIBStatisticalService,
      TIBLogService, TIBSecurityService, TIBServerProperties]);
  RegisterPropertyEditor(TypeInfo(TIBFileName), TIBDatabase, 'DatabaseName', TIBFileNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TIBStoredProc, 'StoredProcName', TIBStoredProcNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TParams), TIBStoredProc, 'Params', TIBStoredProcParamsProperty);
  RegisterPropertyEditor(TypeInfo(string), TIBTable, 'TableName', TIBTableNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TIBTable, 'IndexName', TIBIndexNameProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(string), TIBTable, 'IndexFieldNames', TIBIndexFieldNamesProperty); {do not localize}
//  RegisterPropertyEditor(TypeInfo(string), TIBTable, 'MasterFields', TIBTableFieldLinkProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBQuery, 'SQL', TIBQuerySQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBDataSet, 'SelectSQL', TIBDatasetSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBDataSet, 'ModifySQL', TIBUpdateSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBDataSet, 'InsertSQL', TIBInsertSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBDataSet, 'RefreshSQL', TIBRefreshSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBDataSet, 'DeleteSQL', TIBDeleteSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBSQL, 'SQL', TIBSQLSQLPropertyEditor); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBUpdateSQL, 'RefreshSQL', TIBUpdateSQLRefreshSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBUpdateSQL, 'ModifySQL', TIBUpdateSQLUpdateProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBUpdateSQL, 'InsertSQL', TIBUpdateSQLInsertSQLProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBUpdateSQL, 'DeleteSQL', TIBUpdateSQLDeleteProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TStrings), TIBEvents, 'Events', TIBEventListProperty); {do not localize}
  RegisterPropertyEditor(TypeInfo(TPersistent), TIBDataSet, 'GeneratorField', TIBGeneratorProperty);  {do not localize}
  RegisterPropertyEditor(TypeInfo(TPersistent), TIBQuery, 'GeneratorField', TIBGeneratorProperty);  {do not localize}

  RegisterComponentEditor(TIBDatabase, TIBDatabaseEditor);
  RegisterComponentEditor(TIBTransaction, TIBTransactionEditor);
  RegisterComponentEditor(TIBUpdateSQL, TIBUpdateSQLEditor);
  RegisterComponentEditor(TIBDataSet, TIBDataSetEditor);
  RegisterComponentEditor(TIBQuery, TIBQueryEditor);
  RegisterComponentEditor(TIBStoredProc, TIBStoredProcEditor);
  RegisterComponentEditor(TIBSQL, TIBSQLEditor);
  RegisterComponentEditor(TIBCustomService, TIBServiceEditor);
end;

{ TIBServiceEditor }

procedure TIBServiceEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0 : if ibserviceeditor.EditIBService(TIBCustomService(Component)) then Designer.Modified;
    end;
  end;
end;

function TIBServiceEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SIBServiceEditor;
      1 : Result := SInterbaseExpressVersion;
    end;
  end;
end;

function TIBServiceEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TIBFileNameProperty }
procedure TIBFileNameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
    try
      InitialDir := ExtractFilePath(GetStrValue);
      Filter := 'Database Files|*.gdb'; {do not localize}
      if Execute then
        SetStrValue(FileName);
    finally
      Free
    end;
end;

function TIBFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TIBNameProperty }

function TIBNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

{ TIBStoredProcNameProperty }

procedure TIBStoredProcNameProperty.GetValues(Proc: TGetStrProc);
var
   StoredProc : TIBStoredProc;
   i : integer;
begin
    StoredProc := GetComponent(0) as TIBStoredProc;
    with StoredProc do
      for I := 0 to StoredProcedureNames.Count - 1 do
        Proc (StoredProcedureNames[i]);
end;

{ TIBTableNameProperty }

procedure TIBTableNameProperty.GetValues(Proc: TGetStrProc);
var
   TableName : TIBTable;
   i : integer;
begin
  TableName := GetComponent(0) as TIBTable;
  with TableName do
    for I := 0 to TableNames.Count - 1 do
      Proc (TableNames[i]);
end;

{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ Utility Functions }

function GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetPropertyValue(DataSet, 'IndexDefs') as TIndexDefs; {do not localize}
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

{ TIBIndexFieldNamesProperty }

procedure TIBIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  I: Integer;
  IndexDefs: TIndexDefs;
begin
  IndexDefs := GetIndexDefs(GetComponent(0));
  for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if (Options * [ixExpression, ixDescending] = []) and (Fields <> '') then
        List.Add(Fields);
end;


{ TIBIndexNameProperty }

procedure TIBIndexNameProperty.GetValueList(List: TStrings);
begin
  GetIndexDefs(GetComponent(0)).GetItemNames(List);
end;

{ TSQLPropertyEditor }

function TSQLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect,paSubProperties];
end;

{ TIBQuerySQLProperty }

procedure TIBQuerySQLProperty.Edit;
var
  Query: TIBQuery;
begin
  Query := GetComponent(0) as TIBQuery;
  if Assigned(Query.Database) and
    IBSelectSQLEditor.EditSQL(Query.Database,Query.Transaction,Query.SQL) then Modified;
end;

{ TIBDatasetSQLProperty }

procedure TIBDatasetSQLProperty.Edit;
var
  IBDataset: TIBDataset;
begin
  IBDataset := GetComponent(0) as TIBDataset;
  if Assigned(IBDataSet.Database) and
     IBSelectSQLEditor.EditSQL(IBDataSet.Database,IBDataSet.Transaction,IBDataSet.SelectSQL) then Modified;
end;

{ TIBSQLProperty }

procedure TIBSQLProperty.Edit;
var
  IBSQL: TIBSQL;
begin
  IBSQL := GetComponent(0) as TIBSQL;
  if IBSelectSQLEditor.EditSQL(IBSQL.Database,IBSQL.Transaction,IBSQL.SQL) then Modified;
end;

{ TIBUpdateSQLEditor }

procedure TIBUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  if IBUpdateSQLEditor.EditIBUpdateSQL(TIBUpdateSQL(Component)) then Modified;
end;

function TIBUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := SIBUpdateSQLEditor;
    1: Result := SInterbaseExpressVersion;
  end;
end;

function TIBUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result :=  2;
end;

{ TIBDataSetEditor }

procedure TIBDataSetEditor.ExecuteVerb(Index: Integer);
var
  IBDataset: TIBDataset;
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0:
        if IBDataSetEditor.EditIBDataSet(TIBDataSet(Component)) then
          Designer.Modified;
      1: (Component as TIBDataSet).ExecSQL;
    end;
  end;
end;

function TIBDataSetEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SIBDataSetEditor;
      1: Result := SExecute;
      2: Result := SInterbaseExpressVersion;
    end;
  end;
end;

function TIBDataSetEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 3;
end;

{ TIBEventListProperty }

function TIBEventListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect,paSubProperties];
end;

procedure TIBEventListProperty.Edit;
var
  Events: TStrings;
  IBEvents: TIBEvents;
begin
  IBEvents := GetComponent(0) as TIBEvents;
  Events := TStringList.Create;
  try
    Events.Assign( IBEvents.Events);
    if EditAlerterEvents( Events) then
    begin
      IBEvents.Events.Assign(Events);
      Modified
    end;
  finally
    Events.Free;
  end;
end;

{ TIBDatabaseEditor }
procedure TIBDatabaseEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0 : if EditIBDatabase(TIBDatabase(Component)) then Designer.Modified;
    end;
  end;
end;

function TIBDatabaseEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SIBDatabaseEditor;
      1 : Result := SInterbaseExpressVersion;
    end;
  end;
end;

function TIBDatabaseEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TIBTransactionEditor }

procedure TIBTransactionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditIBTransaction(TIBTransaction(Component)) then Designer.Modified;
  end;
end;

function TIBTransactionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SIBTransactionEditor;
    1: Result := SInterbaseExpressVersion;
  end;
end;

function TIBTransactionEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TIBQueryEditor }

procedure TIBQueryEditor.ExecuteVerb(Index: Integer);
var
  Query: TIBQuery;
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index) else
  begin
    Query := Component as TIBQuery;
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Query.ExecSQL;
      1: if ibselectsqleditor.EditSQL(Query.Database,Query.Transaction,Query.SQL) then Designer.Modified;
    end;
  end;
end;

function TIBQueryEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SExecute;
      1: Result := SEditSQL;
      2: Result := SInterbaseExpressVersion;
    end;
  end;
end;

function TIBQueryEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 3;
end;

{ TIBStoredProcEditor }

procedure TIBStoredProcEditor.ExecuteVerb(Index: Integer);
begin
  if Index < inherited GetVerbCount then
    inherited ExecuteVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    if Index = 0 then (Component as TIBStoredProc).ExecProc;
  end;
end;

function TIBStoredProcEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index) else
  begin
    Dec(Index, inherited GetVerbCount);
    case Index of
      0: Result := SExecute;
      1: Result := SInterbaseExpressVersion;
    end;
  end;
end;

function TIBStoredProcEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TIBStoredProcParamsProperty }

procedure TIBStoredProcParamsProperty.Edit;
var
  StoredProc: TIBStoredProc;
  Params: TParams;
begin
  StoredProc := (GetComponent(0) as TIBStoredProc);
  Params := TParams.Create(nil);
  try
    StoredProc.CopyParams(Params);
  finally
    Params.Free;
  end;
  inherited Edit;
end;
(*
{ TIBTableFieldLinkProperty }

procedure TIBTableFieldLinkProperty.Edit;
begin
  FTable := DataSet as TIBTable;
  inherited Edit;
end;

function TIBTableFieldLinkProperty.GetIndexFieldNames: string;
begin
  Result := FTable.IndexFieldNames;
end;

function TIBTableFieldLinkProperty.GetMasterFields: string;
begin
  Result := FTable.MasterFields;
end;

procedure TIBTableFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TIBTableFieldLinkProperty.SetMasterFields(const Value: string);
begin
  FTable.MasterFields := Value;
end;*)

{ TIBUpdateSQLProperty }

procedure TIBUpdateSQLProperty.Edit;
var
  IBDataset: TIBDataset;
begin
  IBDataset := GetComponent(0) as TIBDataset;
  if Assigned(IBDataSet.Database) and
    IBModifySQLEditor.EditSQL(IBDataSet.Database,IBDataSet.Transaction,IBDataSet.ModifySQL) then Modified;
end;

{ TIBUpdateSQLUpdateProperty }

procedure TIBUpdateSQLUpdateProperty.Edit;
begin
  if not GetObjects then
    ShowMessage('Not Linked to an IBQuery object or Database not assigned')
  else
  if assigned(FDatabase) and
      IBModifySQLEditor.EditSQL(FDatabase,FDatabase.DefaultTransaction,FIBUpdateSQL.ModifySQL) then Modified;
end;

{ TIBRefreshSQLProperty }

procedure TIBRefreshSQLProperty.Edit;
var
  IBDataset: TIBDataset;
begin
  IBDataset := GetComponent(0) as TIBDataset;
  if Assigned(IBDataSet.Database) and
    IBRefreshSQLEditor.EditSQL(IBDataSet.Database,IBDataSet.Transaction,IBDataSet.RefreshSQL) then Modified;
end;

{ TIBUpdateSQLRefreshSQLProperty }

procedure TIBUpdateSQLRefreshSQLProperty.Edit;
begin
  if not GetObjects then
    ShowMessage('Not Linked to an IBQuery object or Database not assigned')
  else
  if IBRefreshSQLEditor.EditSQL(FDatabase,FDatabase.DefaultTransaction,FIBUpdateSQL.RefreshSQL) then Modified;
end;

{ TIBDeleteSQLProperty }

procedure TIBDeleteSQLProperty.Edit;
var
  IBDataset: TIBDataset;
begin
  IBDataset := GetComponent(0) as TIBDataset;
  if Assigned(IBDataSet.Database) and
    IBDeleteSQLEditor.EditSQL(IBDataSet.Database,IBDataSet.Transaction,IBDataSet.DeleteSQL) then Modified;
end;

{ TIBUpdateSQLDeleteProperty }

function TIBUpdateSQLDeleteProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes;
end;

procedure TIBUpdateSQLDeleteProperty.Edit;
begin
  if not GetObjects then
    ShowMessage('Not Linked to an IBQuery object or Database not assigned')
  else
  if IBDeleteSQLEditor.EditSQL(FDatabase,FDatabase.DefaultTransaction,FIBUpdateSQL.DeleteSQL) then Modified;
end;

{ TUpdateSQLPropertyEditor }

function TUpdateSQLPropertyEditor.GetObjects: boolean;
begin
  Result := false;
  FIBUpdateSQL := GetComponent(0) as TIBUpdateSQL;
  if not assigned(FIBUpdateSQL) or not assigned(FIBUpdateSQL.DataSet) then
    Exit;
  if FIBUpdateSQL.DataSet is TIBQuery then
  begin
    FDatabase := (FIBUpdateSQL.DataSet as TIBQuery).Database;
    Result := true
  end;
end;

{ TIBInsertSQLProperty }

procedure TIBInsertSQLProperty.Edit;
var
  IBDataset: TIBDataset;
begin
  IBDataset := GetComponent(0) as TIBDataset;
  if Assigned(IBDataSet.Database) and
    IBInsertSQLEditor.EditSQL(IBDataSet.Database,IBDataSet.Transaction,IBDataSet.InsertSQL) then Modified;
end;

{ TIBUpdateSQLInsertSQLProperty }

procedure TIBUpdateSQLInsertSQLProperty.Edit;
begin
  if not GetObjects then
    ShowMessage('Not Linked to an IBQuery object or Database not assigned')
  else
  if assigned(FDatabase) and
    IBInsertSQLEditor.EditSQL(FDatabase,FDatabase.DefaultTransaction,FIBUpdateSQL.InsertSQL) then Modified;
end;

{ TIBGeneratorProperty }

function TIBGeneratorProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paDialog] - [paMultiSelect,paValueList];
end;

procedure TIBGeneratorProperty.Edit;
begin
  if IBGeneratorEditor.EditGenerator(GetPersistentReference as TIBGenerator) then Modified;
end;

{ TIBSQLEditor }

procedure TIBSQLEditor.ExecuteVerb(Index: Integer);
begin
  if IBSQLEditor.EditIBSQL(TIBSQL(Component)) then Modified;
end;

function TIBSQLEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := SIBSQLEditor;
    1: Result := SInterbaseExpressVersion;
  end;
end;

function TIBSQLEditor.GetVerbCount: Integer;
begin
  Result:= 2
end;

{ TIBSQLSQLPropertyEditor }

procedure TIBSQLSQLPropertyEditor.Edit;
var
  IBSQL: TIBSQL;
begin
  IBSQL := GetComponent(0) as TIBSQL;
  if IBSQLEditor.EditIBSQL(IBSQL) then Modified;
end;

initialization
  {$I IBDBReg.lrs}
end.
