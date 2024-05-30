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
 *  The Original Code is (C) 2024 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBDynamicInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils, DB;

{$I IBDynamicInterfaces.inc}

function ProvidesIDynamicSQLDataset(Dataset: TDataset; RaiseException:boolean=true): boolean;
function ProvidesIArrayFieldDef(aFieldDef: TFieldDef; RaiseException:boolean=true): boolean;
function ProvidesIArrayField(aField: TField; RaiseException:boolean=true): boolean;

implementation

resourcestring
  sNoIDynamicSQLDataset = 'Dataset (%s) does not provide the IDynamicSQLDataset interface';
  sNoIArrayFieldDef = 'FieldDef  (%s) does not provide the IArrayFieldDef interface';
  sNoIArrayField = 'Field (%s) does not provide the IArrayField interface';

function ProvidesIDynamicSQLDataset(Dataset : TDataset; RaiseException : boolean
  ) : boolean;
var obj: pointer;
begin
  Result := false;
  if Dataset <> nil then
  begin
    DataSet.GetInterface(IDynamicSQLDataset,obj);
    Result := obj <> nil;
    if not Result and RaiseException then
      raise Exception.CreateFmt(sNoIDynamicSQLDataset,[Dataset.Name])
  end
end;

function ProvidesIArrayFieldDef(aFieldDef: TFieldDef; RaiseException : boolean
  ) : boolean;
var obj: pointer;
begin
  Result := false;
  if (aFieldDef <> nil) and (aFieldDef.DataType = ftArray) then
  begin
    aFieldDef.GetInterface(IArrayFieldDef,obj);
    Result := obj <> nil;
    if not Result and RaiseException then
      raise Exception.CreateFmt(sNoIArrayFieldDef,[aFieldDef.Name])
  end
end;

function ProvidesIArrayField(aField : TField; RaiseException : boolean
  ) : boolean;
var obj: pointer;
begin
  Result := false;
  if aField <> nil then
  begin
    aField.GetInterface(IArrayField,obj);
    Result := obj <> nil;
    if not Result and RaiseException then
      raise Exception.CreateFmt(sNoIArrayField,[aField.Name])
  end
end;


end.

