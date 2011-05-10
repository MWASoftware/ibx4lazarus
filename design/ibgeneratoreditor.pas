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

unit IBGeneratorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, IBDatabase, IBCustomDataSet, IBSystemTables;

type

  { TGeneratorEditor }

  TGeneratorEditor = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    GeneratorNames: TComboBox;
    FieldNames: TComboBox;
    IBTransaction1: TIBTransaction;
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

{$R *.lfm}

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
  Database := AGenerator.Owner.Database;

  if assigned(Database) then
    try
      Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

    with TGeneratorEditor.Create(Application) do
    try
      Generator := AGenerator;
      Result := ShowModal = mrOK
    finally
      Free
    end;
end;

{ TGeneratorEditor }

procedure TGeneratorEditor.FormShow(Sender: TObject);
begin
  LoadGenerators;
  LoadFieldNames;
  if Generator.Generator <> '' then
    GeneratorNames.ItemIndex := GeneratorNames.Items.IndexOf(Generator.Generator);
  if Generator.Field <> '' then
    FieldNames.ItemIndex := FieldNames.Items.IndexOf(UpperCase(Generator.Field))
  else
    FieldNames.ItemIndex := FieldNames.Items.IndexOf(GetPrimaryKey);

  if FieldNames.ItemIndex = -1 then
    FieldNames.Text := Generator.Field;

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
    Generator.Generator := GeneratorNames.Text;
    Generator.Field := FieldNames.Text;
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
  IBTransaction1.DefaultDatabase := Generator.Owner.Database;
  SetDatabase(Generator.Owner.Database,IBTransaction1);
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

end.

