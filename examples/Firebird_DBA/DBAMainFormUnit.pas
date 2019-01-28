unit DBAMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, DBCtrls, Menus, ActnList, db, SynEdit, SynHighlighterSQL,
  MainFormUnit, IBLookupComboEditBox, IBDynamicGrid, IBTreeView, IBDatabaseInfo,
  IBExtract, IBCustomDataSet, IB;

type
  TDBANodeItemType = (ntRoot,ntServer,ntDatabase);

  { TDBAMainForm }

  TDBAMainForm = class(TMainForm)
    DeleteNode: TAction;
    CreateDatabase: TAction;
    AddDatabase: TAction;
    AddServer: TAction;
    ActionList2: TActionList;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
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
    procedure DeleteNodeExecute(Sender: TObject);
    procedure DeleteNodeUpdate(Sender: TObject);
    procedure Edit8EditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure RegisteredObjectsTreeAddition(Sender: TObject; Node: TTreeNode);
    procedure RegisteredObjectsTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure RegisteredObjectsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure RegisteredObjectsTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure RegisteredObjectsTreeSelectionChanged(Sender: TObject);
    procedure ServersAndDatabasesAfterDelete(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterInsert(DataSet: TDataSet);
    procedure ServersAndDatabasesAfterPost(DataSet: TDataSet);
    procedure ServersAndDatabasesBeforeDelete(DataSet: TDataSet);
    procedure ServersAndDatabasesValidatePost(Sender: TObject;
      var CancelPost: boolean);
    procedure ServerTabShow(Sender: TObject);
    procedure UserManagerTabShow(Sender: TObject);
  private
    FLocated: boolean;
    FNewItemType: TDBANodeItemType;
    FExpandNode: TTreeNode;
    procedure DoSelect(Data: PtrInt);
  protected
    procedure ConfigureForServerVersion; override;
  end;

var
  DBAMainForm: TDBAMainForm;

implementation

{$R *.lfm}

uses DataModule, Variants, RegisterServerDlgUnit, CreateNewDBDlgUnit, RegisterExistingDBDlgUnit,
  ServerDataUnit, DatabaseDataUnit, LocalDataModule, DBADataModule;

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
var CurServerDB: string;
begin
  PageControl1.Visible := false;
  LocalData.LocalDatabase.Connected := true;
  ServersAndDatabases.Active := true;
  inherited;
  PageControl1.ActivePage := ServerTab;
  FLocated := true;
  CurServerDB := LocalData.UserConfig[rgCurServerDB];
  if CurServerDB <> '' then
     RegisteredObjectsTree.Selected := RegisteredObjectsTree.FindNode(StrIntListToVar(CurServerDB),true);
end;

procedure TDBAMainForm.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem25.Visible :=  (RegisteredObjectsTree.Selected <> nil) and
       (TDBATreeNode(RegisteredObjectsTree.Selected).ItemType = ntServer);
  MenuItem27.Visible := MenuItem25.Visible;
end;

procedure TDBAMainForm.RegisteredObjectsTreeAddition(Sender: TObject; Node: TTreeNode);
begin
  if ServersAndDatabases.State = dsInsert then
    ServersAndDatabases.Post;
  TDBATreeNode(Node).ItemType := TDBANodeItemType(ServersAndDatabases.FieldByName('ItemType').AsInteger);
  if (TObject(Node.Data) is TRegisterServerDlg) or (TDBATreeNode(Node).ItemType = ntServer) then
    Node.Data := ServerDataList.Update(TIBTreeNode(Node).KeyValue,TRegisterServerDlg(Node.Data))
  else
  if (TObject(Node.Data) is TRegisterExistingDBDlg) or (TDBATreeNode(Node).ItemType = ntDatabase) then
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

procedure TDBAMainForm.RegisteredObjectsTreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  FExpandNode := Node;
end;

procedure TDBAMainForm.RegisteredObjectsTreeSelectionChanged(Sender: TObject);

  procedure NoSelection;
  begin
    PageControl1.Visible := false;
    StatusBar1.SimpleText := '';
  end;

begin
  if FLocated and (RegisteredObjectsTree.Selected <> nil) and (FExpandNode <> RegisteredObjectsTree.Selected) then
  begin
    case TDBATreeNode(RegisteredObjectsTree.Selected).ItemType of
    ntRoot:
      NoSelection;

    ntServer:
      with TIBTreeView(RegisteredObjectsTree) do
      if not VarIsNull(SelectedKeyValue) then
      try
        if ServerDataList.ServerData[SelectedKeyValue].Select then
        begin
          ConfigureForServerVersion;
          PageControl1.ActivePage := ServerTab;
          PageControl1.Visible := true;
          StatusBar1.SimpleText := Format('Server: %s - Logged in as user %s',[
                                          DBADatabaseData.ServerData.SERVERNAME,
                                          DBADatabaseData.ServiceUserName]);
          if DBADatabaseData.EmbeddedMode then
            StatusBar1.SimpleText := StatusBar1.SimpleText + ' in embedded mode';
          Selected.Expand(false);
          PageControl1.ActivePage.OnShow(nil);
        end
        else
          NoSelection;
      except
        On E: EIBClientError do
          NoSelection;
        else
        begin
          NoSelection;
          raise;
        end;
      end;

    ntDatabase:
      with TIBTreeView(RegisteredObjectsTree) do
      if not VarIsNull(SelectedKeyValue) then
      try
        if DatabaseDataList.DatabaseData[SelectedKeyValue].Select then
        begin
          ConfigureForServerVersion;
          PageControl1.Visible := true;
          PageControl1.ActivePage := Properties;
        end
        else
        begin
          NoSelection;
//          Application.QueueAsyncCall(@DoSelect,PtrInt(Selected.Parent));
        end;
      except
        NoSelection;
        raise;
      end;
    end;
  end;
  FExpandNode := nil;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterDelete(DataSet: TDataSet);
begin
  ServersAndDatabases.Transaction.CommitRetaining;
end;

procedure TDBAMainForm.ServersAndDatabasesAfterInsert(DataSet: TDataSet);
begin
  ServersAndDatabases.FieldByName('ItemType').AsInteger := ord(FNewItemType);
end;

procedure TDBAMainForm.ServersAndDatabasesAfterPost(DataSet: TDataSet);
begin
  ServersAndDatabases.Transaction.CommitRetaining;
end;

procedure TDBAMainForm.ServersAndDatabasesBeforeDelete(DataSet: TDataSet);
begin
  if Dataset.FieldByName('ItemType').AsInteger = 0 then
    ServerDataList.Remove(Dataset.FieldByName('ID').AsInteger)
  else
    DatabaseDataList.Remove(Dataset.FieldByName('ID').AsInteger);
end;

procedure TDBAMainForm.ServersAndDatabasesValidatePost(Sender: TObject;
  var CancelPost: boolean);
begin
  CancelPost := ServersAndDatabases.FieldByName('ItemName').AsString = '';
  if CancelPost then
    MessageDlg('A Server must be given a name',mtError,[mbOK],0)
end;

procedure TDBAMainForm.ServerTabShow(Sender: TObject);
begin
  if not Visible or (DBADatabaseData.ServerData = nil) then Exit;
  inherited;
end;

procedure TDBAMainForm.UserManagerTabShow(Sender: TObject);
begin
  if Visible and (DBADatabaseData.DatabaseData = nil) then
      UserListSource.DataSet.Active := not DBADatabaseData.EmbeddedMode
  else
    inherited;
end;

procedure TDBAMainForm.DoSelect(Data: PtrInt);
begin
  RegisteredObjectsTree.Selected := TTreeNode(Data);
end;

procedure TDBAMainForm.ConfigureForServerVersion;
var i: integer;
begin
  if DBADatabaseData.DatabaseData = nil then
  begin
    UserListSource.DataSet := DBDataModule.LegacyUserList;
    for i in [0,1,2,3,4,7,8,9,10] do
     PageControl1.Pages[i].TabVisible := false;
    PageControl1.Pages[6].TabVisible := not DBADatabaseData.EmbeddedMode;
    for i in [4,6,7,8] do
      UserManagerGrid.Columns[i].Visible := false;
    for i in [9,10] do
      UserManagerGrid.Columns[i].Visible := true;
    TagsHeader.Visible := false;
    TagsGrid.Visible := false;
    RolesHeaderPanel.Visible := false;
    RolesGrid.Visible := false;
  end
  else
  begin
    for i in [0,1,2,3,4,6,7,8,9,10] do
     PageControl1.Pages[i].TabVisible := true;
    RolesHeaderPanel.Visible := true;
    RolesGrid.Visible := true;
    inherited ConfigureForServerVersion;
  end;
end;

procedure TDBAMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
   LocalData.UserConfig[rgCurServerDB] :=
      VarToStrIntList(RegisteredObjectsTree.GetNodePath(RegisteredObjectsTree.Selected));
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

procedure TDBAMainForm.DeleteNodeExecute(Sender: TObject);
var Node: TTreeNode;
begin
  if MessageDlg(Format('Do you really want to delete "%s"?',[RegisteredObjectsTree.Selected.Text]),
    mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
    Node := RegisteredObjectsTree.Selected.Parent;
    RegisteredObjectsTree.Selected.Delete;
    RegisteredObjectsTree.Selected := Node;
  end;
end;

procedure TDBAMainForm.DeleteNodeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (RegisteredObjectsTree.Selected <> nil) and
       (TDBATreeNode(RegisteredObjectsTree.Selected).ItemType <> ntRoot);
end;

procedure TDBAMainForm.Edit8EditingDone(Sender: TObject);
begin
  DBADatabaseData.ServerData.DomainName := (Sender as TEdit).Text;
  DBADatabaseData.ServerData.Select(true);
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

