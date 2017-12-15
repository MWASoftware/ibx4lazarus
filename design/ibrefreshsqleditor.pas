unit ibrefreshsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IBSQLEditFrame, IBDatabase, IBCustomDataSet,
  IBLookupComboEditBox, IBDynamicGrid;

type

  { TIBRefreshSQLEditorForm }

  TIBRefreshSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FieldNamesGrid: TIBDynamicGrid;
    GenerateBtn: TButton;
    GenerateParams: TCheckBox;
    IBSQLEditFrame1: TIBSQLEditFrame;
    IncludeSysTables: TCheckBox;
    PrimaryKeysGrid: TIBDynamicGrid;
    SelectSelectAll: TCheckBox;
    SelectTableNames: TIBLookupComboEditBox;
    TestBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    QuoteFields: TCheckBox;
    procedure GenerateBtnClick(Sender: TObject);
    procedure IncludeSysTablesChange(Sender: TObject);
    procedure SelectSelectAllChange(Sender: TObject);
    procedure SelectTableNamesDblClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrimaryKeyListDblClick(Sender: TObject);
  private
    { private declarations }
      procedure HandleUserTablesOpened(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    { public declarations }
  end; 

var
  IBRefreshSQLEditorForm: TIBRefreshSQLEditorForm;

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;

implementation

{$R *.lfm}

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;
begin
  Result := false;
  if assigned(DataSet) and assigned(DataSet.Database) then
    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBRefreshSQLEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
      IBSQLEditFrame1.Database := DataSet.Database;
      GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    IBSQLEditFrame1.SQLText.Lines.Assign(SelectSQL);
    Result := ShowModal = mrOK;
    if Result then
    begin
     SelectSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
     if assigned(DataSet) then
          DataSet.GenerateParamNames := GenerateParams.Checked
    end;
  finally
    Free
  end;
end;

{ TIBRefreshSQLEditorForm }

procedure TIBRefreshSQLEditorForm.FormShow(Sender: TObject);
begin
  GenerateBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  TestBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  if TestBtn.Enabled then;
    IBSQLEditFrame1.UserTables.Active := true;
end;

procedure TIBRefreshSQLEditorForm.PrimaryKeyListDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedPrimaryKey;
end;

procedure TIBRefreshSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedFieldName;
end;

procedure TIBRefreshSQLEditorForm.GenerateBtnClick(Sender: TObject);
begin
  IBSQLEditFrame1.GenerateRefreshSQL(QuoteFields.Checked);
end;

procedure TIBRefreshSQLEditorForm.IncludeSysTablesChange(Sender: TObject);
begin
  IBSQLEditFrame1.IncludeSystemTables := IncludeSysTables.Checked;
end;

procedure TIBRefreshSQLEditorForm.SelectSelectAllChange(Sender: TObject);
begin
  IBSQLEditFrame1.SelectAllFields(SelectSelectAll.Checked);
end;

procedure TIBRefreshSQLEditorForm.SelectTableNamesDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertTableName;
end;

procedure TIBRefreshSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  IBSQLEditFrame1.TestSQL(GenerateParams.Checked)
end;

procedure TIBRefreshSQLEditorForm.HandleUserTablesOpened(Sender: TObject);
begin
  SelectSelectAll.Checked := true;
end;

procedure TIBRefreshSQLEditorForm.Loaded;
begin
  inherited Loaded;
  if IBSQLEditFrame1 <> nil then
  begin
    IBSQLEditFrame1.OnUserTablesOpened := @HandleUserTablesOpened;
    if SelectTableNames <> nil then
      SelectTableNames.ListSource :=  IBSQLEditFrame1.UserTableSource;
    if FieldNamesGrid <> nil then
      FieldNamesGrid.DataSource := IBSQLEditFrame1.FieldsSource;
    if PrimaryKeysGrid <> nil then
      PrimaryKeysGrid.DataSource := IBSQLEditFrame1.PrimaryKeySource;
  end;
end;

end.
