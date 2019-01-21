unit DBAMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, DBCtrls, Menus, ActnList, db, SynEdit, SynHighlighterSQL,
  MainFormUnit, IBLookupComboEditBox, IBDynamicGrid, IBTreeView, IBDatabaseInfo,
  IBExtract, IBCustomDataSet;

type

  { TDBAMainForm }

  TDBAMainForm = class(TMainForm)
    IBTreeView1: TIBTreeView;
    ServersAndDatabases: TIBDataSet;
    ServersAndDatabasesHASCHILD: TIntegerField;
    ServersAndDatabasesID: TIntegerField;
    ServersAndDatabasesITEMNAME: TIBStringField;
    ServersAndDatabasesITEMTYPE: TIntegerField;
    ServersAndDatabasesPARENT: TIntegerField;
    Splitter6: TSplitter;
    TreeSource: TDataSource;
    TreeViewImages: TImageList;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBTreeView1Addition(Sender: TObject; Node: TTreeNode);
    procedure IBTreeView1EditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure IBTreeView1SelectionChanged(Sender: TObject);
    procedure ServersAndDatabasesAfterDelete(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterInsert(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterOpen(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterPost(DataSet: TDataSet);
    procedure ServersAndDatabasesValidatePost(Sender: TObject;
      var CancelPost: boolean);
  private
    FLocated: boolean;
    FNewItemType: integer;  {-1 => Server root, 0 => Server, 1 => database}
  public

  end;

var
  DBAMainForm: TDBAMainForm;

implementation

{$R *.lfm}

uses DataModule, Variants;

{ TDBAMainForm }

procedure TDBAMainForm.FormShow(Sender: TObject);
begin
  LocalDatabase.Connected := true;
  ServersAndDatabases.Active := true;
  inherited;
end;

procedure TDBAMainForm.IBTreeView1Addition(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := ServersAndDatabases.FieldByName('ItemType').AsInteger;
end;

procedure TDBAMainForm.IBTreeView1EditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
begin
  if (Node <> nil) and not VarIsNull(TIBTreeNode(Node).KeyValue) then
  begin
    UpdateServerData(TIBTreeNode(Node).KeyValue,Node.Text);
    UpdateDatabaseData(TIBTreeNode(Node).KeyValue,Node.Text);
  end;
end;

procedure TDBAMainForm.IBTreeView1SelectionChanged(Sender: TObject);
begin
  if RegisteredObjectsTree.Selected <> nil then
  case RegisteredObjectsTree.Selected.ImageIndex of
  -1:
    PageControl1.Visible := false;
  0:
    with TIBTreeView(RegisteredObjectsTree) do
    if not VarIsNull(SelectedKeyValue) then
    begin
      ServerDataList.ServerData[SelectedKeyValue].Select;
      PageControl1.Visible := true;
    end;

  1:
    with TIBTreeView(RegisteredObjectsTree) do
    if not VarIsNull(SelectedKeyValue) then
    begin
      if DatabaseDataList.DatabaseData[SelectedKeyValue].Select then
      PageControl1.Visible := true;
    end;
  end;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterDelete(DataSet: TDataSet);
begin
  ServersAndDatabases.CommitRetaining;
  if Dataset.FieldByName('ItemType').AsInteger = 0 then
    ServerDataList.Remove(Dataset.FieldByName('ID').AsInteger)
  else
    DatabaseDataList.Remove(Dataset.FieldByName('ID').AsInteger);
end;

procedure TDBAMainForm.ServersAndDatabasesAfterInsert(DataSet: TDataSet);
begin
  ServersAndDatabases.FieldByName('ItemType').AsInteger := FNewItemType;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterOpen(DataSet: TDataSet);
var CurServerDB: string;
begin
  if (RegisteredObjectsTree.Selected <> nil) and (RegisteredObjectsTree.Selected.ImageIndex = -1) then
    RegisteredObjectsTree.Selected.Expand(false);
  CurServerDB := LocalData.UserConfig[rgCurServerDB];
  if not FLocated and (CurServerDB <> '') then
     RegisteredObjectsTree.FindNode(StrIntListToVar(CurServerDB),true);
  FLocated := true;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterPost(DataSet: TDataSet);
begin
  ServersAndDatabases.Transaction.CommitRetaining;
  if Dataset.FieldByName('ItemType').AsInteger = 0 then
    ServerDataList.Update(Dataset.FieldByName('ID').AsInteger)
  else
    DatabaseDataList.Update(Dataset.FieldByName('ID').AsInteger);
end;

procedure TDBAMainForm.ServersAndDatabasesValidatePost(Sender: TObject;
  var CancelPost: boolean);
begin
  CancelPost := ServersAndDatabases.FieldByName('ItemName').AsString = '';
  if CancelPost then
    MessageDlg('A Server must be given a name',mtError,[mbOK],0)
end;

procedure TDBAMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  LocalDatabase.Connected := false;
  inherited;
end;

end.

