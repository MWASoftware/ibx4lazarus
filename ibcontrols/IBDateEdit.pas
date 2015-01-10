unit IBDateEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn,
  DB, DBCtrls, LCLType,  LResources;

type

  { TIBDateEdit }

  TIBDateEdit = class(TDateEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FChanging: boolean;
    function GetDataField: string;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: boolean);
    procedure ActiveChanged(Sender: TObject);
    procedure HandleDataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TextChanged; override;
public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Field: TField read GetField;
    property DataSet: TDataSet read GetDataSet;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

implementation

resourcestring
  sNotDateTimeField = 'Field type must be Date or Timestamp';

{ TIBDateEdit }

function TIBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName
end;

function TIBDateEdit.GetDataSet: TDataSet;
begin
  Result := FDataLink.DataSet
end;

function TIBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TIBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field
end;

function TIBDateEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly
end;

procedure TIBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value
end;

procedure TIBDateEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,Value);
end;

procedure TIBDateEdit.SetReadOnly(const Value: boolean);
begin
  FDataLink.ReadOnly := Value
end;

procedure TIBDateEdit.ActiveChanged(Sender: TObject);
var FieldName: string;
begin
  if assigned(DataSet) then
  begin
    if DataSet.Active and (Field <> nil) and not ((Field is TDateTimeField)
      or  (FIeld is TDateField)) then
    begin
      FieldName := DataField;
      DataField := '';
      raise Exception.CreateFmt(sNotDateTimeField,[FieldName])
    end;
    if not DataSet.Active then
      EditText := ''
  end
end;

procedure TIBDateEdit.HandleDataChange(Sender: TObject);
begin
  if FChanging then Exit;
  FChanging := true;
  try
    if (Field = nil) or Field.IsNull then
      Date := NullDate
    else
      Date := Field.AsDateTime
  finally
    FChanging := false
  end;
end;

procedure TIBDateEdit.UpdateData(Sender: TObject);
begin
  if Trim(EditText) = '' then
    Field.Clear
  else
    Field.AsDateTime := Date ;
end;

procedure TIBDateEdit.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
end;

procedure TIBDateEdit.TextChanged;
begin
  inherited TextChanged;
  if FDataLink.Active and not FChanging and (Date <> Field.AsDateTime) then
  begin
    FChanging := true;
    try
        FDataLink.Edit;
        FDataLink.Modified;
    finally
      FChanging := false
    end;
    FDataLink.UpdateRecord
  end;
end;

constructor TIBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := @HandleDataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChanged;
end;

destructor TIBDateEdit.Destroy;
begin
  if assigned(FDataLink) then FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TIBDateEdit.Clear;
begin
  FDataLink.Edit;
  inherited Clear
end;

end.
