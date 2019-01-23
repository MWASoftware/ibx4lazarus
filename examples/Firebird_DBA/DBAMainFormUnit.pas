unit DBAMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, DBCtrls, Menus, ActnList, db, SynEdit, SynHighlighterSQL,
  MainFormUnit, IBLookupComboEditBox, IBDynamicGrid, IBTreeView, IBDatabaseInfo,
  IBExtract, IBCustomDataSet;

type
  TDBANodeItemType = (ntRoot,ntServer,ntDatabase);

  { TDBAMainForm }

  TDBAMainForm = class(TMainForm)
    CreateDatabase: TAction;
    AddDatabase: TAction;
    AddServer: TAction;
    ActionList2: TActionList;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    RegisteredObjectsTree: TIBTreeView;
    MenuItem22: TMenuItem;
    PopupMenu1: TPopupMenu;
    ServersAndDatabases: TIBDataSet;
    ServersAndDatabasesHASCHILD: TIntegerField;
    ServersAndDatabasesID: TIntegerField;
    ServersAndDatabasesITEMNAME: TIBStringField;
    ServersAndDatabasesITEMTYPE: TIntegerField;
    ServersAndDatabasesPARENT: TIntegerField;
    Splitter6: TSplitter;
    TreeSource: TDataSource;
    TreeViewImages: TImageList;
    procedure AddDatabaseExecute(Sender: TObject);
    procedure AddDatabaseUpdate(Sender: TObject);
    procedure AddServerExecute(Sender: TObject);
    procedure AddServerUpdate(Sender: TObject);
    procedure CreateDatabaseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RegisteredObjectsTreeAddition(Sender: TObject; Node: TTreeNode);
    procedure RegisteredObjectsTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure RegisteredObjectsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure RegisteredObjectsTreeSelectionChanged(Sender: TObject);
    procedure ServersAndDatabasesAfterDelete(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterInsert(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterOpen(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterPost(DataSet: TDataSet);
    procedure ServersAndDatabasesValidatePost(Sender: TObject;
      var CancelPost: boolean);
  private
    FLocated: boolean;
    FNewItemType: TDBANodeItemType;
  public

  end;

var
  DBAMainForm: TDBAMainForm;

implementation

{$R *.lfm}

uses DataModule, Variants, RegisterServerDlgUnit, CreateNewDBDlgUnit, RegisterExistingDBDlgUnit,
  ServerDataUnit, DatabaseDataUnit, LocalDataModule;

type

  { TDBATreeNode }

  TDBATreeNode = class(TIBTreeNode)
  private
    FItemType: TDBANodeItemType;
  public
    property ItemType: TDBANodeItemType read FItemType write FItemType;
  end;

{ TDBAMainForm }

procedure TDBAMainForm.FormShow(Sender: TObject);
begin
  LocalData.LocalDatabase.Connected := true;
  ServersAndDatabases.Active := true;
  inherited;
end;

procedure TDBAMainForm.RegisteredObjectsTreeAddition(Sender: TObject; Node: TTreeNode);
begin
  TDBATreeNode(Node).ItemType := TDBANodeItemType(ServersAndDatabases.FieldByName('ItemType').AsInteger);
  if TObject(Node.Data) is TRegisterServerDlg then
    Node.Data := ServerDataList.Update(TIBTreeNode(Node).KeyValue,TRegisterServerDlg(Node.Data))
  else
  if TObject(Node.Data) is TRegisterExistingDBDlg then
    Node.Data := DatabaseDataList.Update(TIBTreeNode(Node).KeyValue,TRegisterExistingDBDlg(Node.Data));
end;

procedure TDBAMainForm.RegisteredObjectsTreeCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TDBATreeNode;
end;

procedure TDBAMainForm.RegisteredObjectsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
begin
  if (Node <> nil) and not VarIsNull(TIBTreeNode(Node).KeyValue) then
    case TDBATreeNode(Node).ItemType of
    ntServer:
      ServerDataList.Update(TIBTreeNode(Node).KeyValue,nil);
    ntDatabase:
      DatabaseDataList.Update(TIBTreeNode(Node).KeyValue,nil);
    end;
end;

procedure TDBAMainForm.RegisteredObjectsTreeSelectionChanged(Sender: TObject);

 procedure ShowDBTabs(Visible: boolean);
 var i : integer;
 begin
   for i := 0 to PageControl1.PageCount -1  do
    if PageControl1.Pages[i].Tag = 0 then
       PageControl1.Pages[i].TabVisible := Visible;
 end;

begin
  if RegisteredObjectsTree.Selected <> nil then
  begin
    case TDBATreeNode(RegisteredObjectsTree.Selected).ItemType of
    ntRoot:
      PageControl1.Visible := false;

    ntServer:
      with TIBTreeView(RegisteredObjectsTree) do
      if not VarIsNull(SelectedKeyValue) then
      begin
        ServerDataList.ServerData[SelectedKeyValue].Select;
        PageControl1.Visible := true;
        ShowDBTabs(false);
      end;

    ntDatabase:
      with TIBTreeView(RegisteredObjectsTree) do
      if not VarIsNull(SelectedKeyValue) then
      begin
        DatabaseDataList.DatabaseData[SelectedKeyValue].Select;
        PageControl1.Visible := true;
        ShowDBTabs(true);
      end;
    end;
  end;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterDelete(DataSet: TDataSet);
begin
  ServersAndDatabases.Transaction.CommitRetaining;
  if Dataset.FieldByName('ItemType').AsInteger = 0 then
    ServerDataList.Remove(Dataset.FieldByName('ID').AsInteger)
  else
    DatabaseDataList.Remove(Dataset.FieldByName('ID').AsInteger);
end;

procedure TDBAMainForm.ServersAndDatabasesAfterInsert(DataSet: TDataSet);
begin
  ServersAndDatabases.FieldByName('ItemType').AsInteger := ord(FNewItemType);
end;

procedure TDBAMainForm.ServersAndDatabasesAfterOpen(DataSet: TDataSet);
var CurServerDB: string;
begin
  if (RegisteredObjectsTree.Selected <> nil) and (TDBATreeNode(RegisteredObjectsTree.Selected).ItemType = ntRoot) then
    RegisteredObjectsTree.Selected.Expand(false);
  CurServerDB := LocalData.UserConfig[rgCurServerDB];
  if not FLocated and (CurServerDB <> '') then
     RegisteredObjectsTree.FindNode(StrIntListToVar(CurServerDB),true);
  FLocated := true;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterPost(DataSet: TDataSet);
begin
  ServersAndDatabases.Transaction.CommitRetaining;
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
  LocalData.LocalDatabase.Connected := false;
  inherited;
end;

procedure TDBAMainForm.AddServerUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := (RegisteredObjectsTree.Selected <> nil) and
     (TDBATreeNode(RegisteredObjectsTree.Selected).ItemType = ntRoot);
end;

procedure TDBAMainForm.CreateDatabaseExecute(Sender: TObject);
begin
  if CreateNewDBDlg.ShowModal(TServerData(RegisteredObjectsTree.Selected.Data)) = mrOK then
  begin
    FNewItemType := ntDatabase;
    RegisteredObjectsTree.Selected.Expand(true);
    RegisteredObjectsTree.Selected := RegisteredObjectsTree.Items.AddChildObject(RegisteredObjectsTree.Selected,
                                                                                 CreateNewDBDlg.DatabaseName.Text,
                                                                                 CreateNewDBDlg);
  end;
end;

procedure TDBAMainForm.AddServerExecute(Sender: TObject);
begin
  if RegisterServerDlg.ShowModal = mrOK then
  begin
    FNewItemType := ntServer;
    RegisteredObjectsTree.Selected.Expand(true);
    RegisteredObjectsTree.Selected := RegisteredObjectsTree.Items.AddChildObject(RegisteredObjectsTree.Selected,
                                                                                 RegisterServerDlg.ServerName.Text,
                                                                                 RegisterServerDlg);
  end;
end;

procedure TDBAMainForm.AddDatabaseUpdate(Sender: TObject);
begin
  (Sender as TAction).Visible := (RegisteredObjectsTree.Selected <> nil) and
     (TDBATreeNode(RegisteredObjectsTree.Selected).ItemType = ntServer);
end;

procedure TDBAMainForm.AddDatabaseExecute(Sender: TObject);
begin
  if RegisterExistingDBDlg.ShowModal(TServerData(RegisteredObjectsTree.Selected.Data)) = mrOK then
  begin
    FNewItemType := ntDatabase;
    RegisteredObjectsTree.Selected.Expand(true);
    RegisteredObjectsTree.Selected := RegisteredObjectsTree.Items.AddChildObject(RegisteredObjectsTree.Selected,
                                                                                 RegisterExistingDBDlg.DatabaseName.Text,
                                                                                 RegisterExistingDBDlg);
  end;
end;

end.

