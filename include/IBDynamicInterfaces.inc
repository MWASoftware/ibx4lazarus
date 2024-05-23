resourcestring
  sNoIDynamicSQLDataset = 'Dataset (%s) does not provide the IDynamicSQLDataset interface';
  sNoIDynamicSQLComponent = 'Component (%s) does not provide the IDynamicSQLComponent interface';
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

function ProvidesIDynamicSQLComponent(aComponent: TComponent; RaiseException:boolean): boolean;
var obj: pointer;
begin
  Result := false;
  if aComponent <> nil then
  begin
    aComponent.GetInterface(IDynamicSQLComponent,obj);
    Result := obj <> nil;
    if not Result and RaiseException then
      raise Exception.CreateFmt(sNoIDynamicSQLComponent,[aComponent.Name])
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


