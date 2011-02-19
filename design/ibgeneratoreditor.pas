unit IBGeneratorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, IBDatabase, IBCustomDataSet, IBSystemTables;

type

  { TGeneratorEditor }

  TGeneratorEditor = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    GeneratorNames: TComboBox;
    FieldNames: TComboBox;
    IncrementBy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OnNewRecord: TRadioButton;
    OnPost: TRadioButton;
    UpDown1: TUpDown;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FGenerator: TIBGenerator;
    FTableName: string;
    FIBSystemTables: TIBSystemTables;
    { private declarations }
    procedure LoadGenerators;
    procedure LoadFieldNames;
    function GetPrimaryKey: string;
    procedure SetGenerator(const AValue: TIBGenerator);
    procedure SetDatabase(ADatabase: TIBDatabase; ATransaction: TIBTransaction);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Generator: TIBGenerator read FGenerator write SetGenerator;
  end; 

function EditGenerator(AGenerator: TIBGenerator): boolean;

implementation

uses IBQuery;

function EditGenerator(AGenerator: TIBGenerator): boolean;
var Database: TIBDatabase;
begin
  Result := false;
  if (AGenerator.Owner is TIBQuery and ((AGenerator.Owner as TIBQuery).SQL.Text = '')) or
   (AGenerator.Owner is TIBDataSet and ((AGenerator.Owner as TIBDataSet).SelectSQL.Text = '')) then
  begin
    ShowMessage('No Select SQL Found!');
    Exit
  end;
  if assigned(Database) then
  begin
    Database := AGenerator.Owner.Database;
    if not assigned(AGenerator.Owner.Transaction)then
    begin
      ShowMessage('No Transaction assigned to DataSet!');
      Exit
    end;
    Database.Connected := true;

    with TGeneratorEditor.Create(Application) do
    try
      Generator := AGenerator;
      Result := ShowModal = mrOK
    finally
      Free
    end;
  end;
end;

{ TGeneratorEditor }

procedure TGeneratorEditor.FormShow(Sender: TObject);
begin
  LoadGenerators;
  LoadFieldNames;
  if Generator.GeneratorName <> '' then
    GeneratorNames.ItemIndex := GeneratorNames.Items.IndexOf(Generator.GeneratorName);
  if Generator.FieldName <> '' then
    FieldNames.ItemIndex := FieldNames.Items.IndexOf(Generator.FieldName)
  else
    FieldNames.ItemIndex := FieldNames.Items.IndexOf(GetPrimaryKey);

  if Generator.ApplyOnEvent = gaeOnNewRecord then
    OnNewRecord.Checked := true
  else
    OnPost.Checked := true;
  IncrementBy.Text := IntToStr(Generator.Increment);
end;

procedure TGeneratorEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    Generator.GeneratorName := GeneratorNames.Text;
    Generator.FieldName := FieldNames.Text;
    if OnNewRecord.Checked then
      Generator.ApplyOnEvent := gaeOnNewRecord
    else
      Generator.ApplyOnEvent := gaeOnPostRecord;
    Generator.Increment := StrToInt(IncrementBy.Text)

  end;
end;

procedure TGeneratorEditor.LoadGenerators;
begin
  FIBSystemTables.GetGenerators(GeneratorNames.Items);
  if GeneratorNames.Items.Count > 0 then
    GeneratorNames.ItemIndex := 0
end;

procedure TGeneratorEditor.LoadFieldNames;
begin
  if FGenerator.Owner is TIBDataSet then
    FIBSystemTables.GetTableAndColumns((FGenerator.Owner as TIBDataSet).SelectSQL.Text,FTableName,FieldNames.Items)
  else
  if FGenerator.Owner is TIBQuery then
    FIBSystemTables.GetTableAndColumns((FGenerator.Owner as TIBQuery).SQL.Text,FTableName,FieldNames.Items)
  else
    raise Exception.CreateFmt('Don''t know how to edit a %s',[FGenerator.Owner.ClassName])
end;

function TGeneratorEditor.GetPrimaryKey: string;
var Keys: TStringList;
begin
  Result := '';
  Keys := TStringList.Create;
  try
    FIBSystemTables.GetPrimaryKeys(FTableName,Keys);
    if Keys.Count > 0 then
      Result := Keys[0];
  finally
    Keys.Free
  end;
end;

procedure TGeneratorEditor.SetGenerator(const AValue: TIBGenerator);
begin
  FGenerator := AValue;
  SetDatabase(Generator.Owner.Database,Generator.Owner.Transaction);
end;

procedure TGeneratorEditor.SetDatabase(ADatabase: TIBDatabase; ATransaction: TIBTransaction);
begin
  if not assigned(ADatabase) then
    raise Exception.Create('A Database must be assigned');
  if not assigned(ATransaction) then
    raise Exception.Create('A Transaction must be assigned');
  FIBSystemTables.SelectDatabase( ADatabase,ATransaction)
end;

constructor TGeneratorEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIBSystemTables := TIBSystemTables.Create
end;

destructor TGeneratorEditor.Destroy;
begin
  if assigned(FIBSystemTables) then FIBSystemTables.Free;
  inherited Destroy;
end;

initialization
  {$I ibgeneratoreditor.lrs}

end.

