unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, SynGutterBase,
  SynGutterMarks, SynGutterLineNumber, SynGutterChanges, SynGutter,
  SynGutterCodeFolding, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList, StdCtrls, DbCtrls, ExtCtrls, db, IBLookupComboEditBox,
  IBDynamicGrid, IBDatabaseInfo, IBServices, IBExtract, IBQuery;

type

  { TMainForm }

  TMainForm = class(TForm)
    DisconnectAttachment: TAction;
    MenuItem17: TMenuItem;
    AttmtPopup: TPopupMenu;
    MenuItem18: TMenuItem;
    ToggleAutoRefresh: TAction;
    AttachSource: TDataSource;
    DBCheckBox1: TDBCheckBox;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit7: TDBEdit;
    DeleteTag: TAction;
    AddTag: TAction;
    AttmtGrid: TIBDynamicGrid;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    Panel7: TPanel;
    AttDetailsPanel: TPanel;
    UserPopup: TPopupMenu;
    UserTagPopup: TPopupMenu;
    SaveChanges: TAction;
    DeleteUser: TAction;
    ChgPassword: TAction;
    AddUser: TAction;
    AddFileBtn: TButton;
    AddShadowBtn: TButton;
    AllocatedPages: TEdit;
    AutoAdmin: TCheckBox;
    Button1: TButton;
    DatabaseAliasName: TDBEdit;
    DatabaseOnline: TCheckBox;
    DBCharacterSet: TIBLookupComboEditBox;
    DBCharSetRO: TDBEdit;
    DBEdit1: TDBEdit;
    DBEdit4: TDBEdit;
    DBIsReadOnly: TCheckBox;
    DBOwner: TDBEdit;
    DBText1: TDBText;
    DropDatabase: TAction;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit8: TEdit;
    FB3UserManager: TTabSheet;
    FilesTab: TTabSheet;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    AttmtTimer: TTimer;
    UserManagerGrid: TIBDynamicGrid;
    IBDynamicGrid4: TIBDynamicGrid;
    TagsGrid: TIBDynamicGrid;
    IsShadowChk: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LingerDelay: TEdit;
    MenuItem10: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem8: TMenuItem;
    NoReserve: TCheckBox;
    PageControl1: TPageControl;
    PagesAvail: TEdit;
    PagesUsed: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    TagsHeader: TPanel;
    PrimaryDBFile: TEdit;
    Properties: TTabSheet;
    RemoveShadowBtn: TButton;
    RoleSource: TDataSource;
    Save: TAction;
    IBExtract1: TIBExtract;
    SaveDialog: TSaveDialog;
    SchemaTab: TTabSheet;
    SecDatabase: TDBEdit;
    ServerLog: TMemo;
    ServerPropMemo: TMemo;
    ServerTab: TTabSheet;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    SQlSaveDialog: TSaveDialog;
    RemoveShadow: TAction;
    AddShadowSet: TAction;
    AddSecondary: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDatabase: TAction;
    CharSetSource: TDataSource;
    DatabaseSource: TDataSource;
    AttmtSource: TDataSource;
    DBCharSetSource: TDataSource;
    IBDatabaseInfo: TIBDatabaseInfo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Restore: TAction;
    Backup: TAction;
    MenuImages: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Quit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    SecFiles: TDataSource;
    ShadowSource: TDataSource;
    StatisticsTab: TTabSheet;
    StatsMemo: TMemo;
    StatsOptions: TComboBox;
    StatusBar1: TStatusBar;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    AttachmentsTab: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UserListSource: TDataSource;
    UserTagsSource: TDataSource;
    procedure AddSecondaryExecute(Sender: TObject);
    procedure AddShadowSetExecute(Sender: TObject);
    procedure AddTagExecute(Sender: TObject);
    procedure AddTagUpdate(Sender: TObject);
    procedure AddUserExecute(Sender: TObject);
    procedure AddUserUpdate(Sender: TObject);
    procedure AttachmentsTabHide(Sender: TObject);
    procedure AttachmentsTabShow(Sender: TObject);
    procedure AttmtTimerTimer(Sender: TObject);
    procedure AutoAdminChange(Sender: TObject);
    procedure BackupExecute(Sender: TObject);
    procedure ChgPasswordExecute(Sender: TObject);
    procedure ChgPasswordUpdate(Sender: TObject);
    procedure DatabaseOnlineChange(Sender: TObject);
    procedure DBCharacterSetEditingDone(Sender: TObject);
    procedure DBIsReadOnlyChange(Sender: TObject);
    procedure DeleteTagExecute(Sender: TObject);
    procedure DeleteTagUpdate(Sender: TObject);
    procedure DeleteUserExecute(Sender: TObject);
    procedure DisconnectAttachmentExecute(Sender: TObject);
    procedure DisconnectAttachmentUpdate(Sender: TObject);
    procedure DropDatabaseExecute(Sender: TObject);
    procedure DropDatabaseUpdate(Sender: TObject);
    procedure FB3UserManagerShow(Sender: TObject);
    procedure FilesTabShow(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IsShadowChkChange(Sender: TObject);
    procedure LingerDelayEditingDone(Sender: TObject);
    procedure NoReserveChange(Sender: TObject);
    procedure OpenDatabaseExecute(Sender: TObject);
    procedure PropertiesShow(Sender: TObject);
    procedure RemoveShadowExecute(Sender: TObject);
    procedure RemoveShadowUpdate(Sender: TObject);
    procedure RestoreExecute(Sender: TObject);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure SaveUpdate(Sender: TObject);
    procedure SchemaTabShow(Sender: TObject);
    procedure ServerTabShow(Sender: TObject);
    procedure StatisticsTabShow(Sender: TObject);
    procedure SweepIntervalEditingDone(Sender: TObject);
    procedure SyncWritesChange(Sender: TObject);
    procedure ToggleAutoRefreshExecute(Sender: TObject);
    procedure ToggleAutoRefreshUpdate(Sender: TObject);
  private
    FLoading: boolean;
    procedure HandleDBConnect(Sender: TObject);
    procedure HandleLoadData(Sender: TObject);
    procedure LoadData;
    procedure DoExtract(Data: PtrInt);
    procedure ConfigureUserManager;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses DataModule, ShutdownRegDlgUnit, AddSecondaryFileDlgUnit, NewUserDlgUnit,
  ChgPasswordDlgUnit;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := Properties;
  DatabaseData.AfterDBConnect := @HandleDBConnect;
  DatabaseData.AfterDataReload := @HandleLoadData;
  DatabaseData.Connect;
end;

procedure TMainForm.IsShadowChkChange(Sender: TObject);
begin
  if FLoading then Exit;
  if not DatabaseData.IsShadowDatabase then
  begin
    MessageDlg('A Normal Database cannot be changed into a Shadow Database',mtError,[mbOK],0);
    FLoading := true;
    try
      IsShadowChk.Checked := false;
    finally
      FLoading := false;
    end;
  end
  else
    DatabaseData.ActivateShadow;
end;

procedure TMainForm.LingerDelayEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  DatabaseData.LingerDelay := LingerDelay.Text;
end;

procedure TMainForm.NoReserveChange(Sender: TObject);
begin
  if FLoading then Exit;
  DatabaseData.NoReserve := NoReserve.Checked;
end;

procedure TMainForm.DBCharacterSetEditingDone(Sender: TObject);
begin
  with DBCharSetSource.Dataset do
  if State = dsEdit then
    Post;
end;

procedure TMainForm.AutoAdminChange(Sender: TObject);
begin
  if FLoading then Exit;
  try
    DatabaseData.AutoAdmin := AutoAdmin.Checked;
  except on E:Exception do
   begin
    MessageDlg(E.message,mtError,[mbOK],0);
     FLoading := true;
     try
       AutoAdmin.Checked := not AutoAdmin.Checked;
     finally
       FLoading := false;
     end;
   end;
  end;
end;

procedure TMainForm.AddSecondaryExecute(Sender: TObject);
var FileName: string;
    StartAt: integer;
    FileLength: integer;
    Pages: boolean;
begin
  StartAt := 0;
  if DatabaseData.IsDatabaseOnline then
  begin
    MessageDlg('The database must be shutdown before adding secondary files',
                 mtError,[mbOK],0);
    exit;
  end;

  if AddSecondaryFileDlg.ShowModal(FileName,StartAt,FileLength,Pages) = mrOK then
  begin
    if not Pages then
    begin
      StartAt := StartAt*1024*1024 div IBDatabaseInfo.PageSize;
      if FileLength <> -1 then
        FileLength := FileLength*1024*1024 div IBDatabaseInfo.PageSize;
    end;
    DatabaseData.AddSecondaryFile(FileName,StartAt,FileLength);
  end;
end;

procedure TMainForm.AddShadowSetExecute(Sender: TObject);
begin
  DatabaseData.AddShadowSet;
end;

procedure TMainForm.AddTagExecute(Sender: TObject);
begin
  UserTagsSource.DataSet.Append;
end;

procedure TMainForm.AddTagUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserTagsSource.State = dsBrowse);
end;

procedure TMainForm.AddUserExecute(Sender: TObject);
var NewUserName: string;
    NewPassword: string;
begin
  NewUserName := '';
  if NewUserDlg.ShowModal(NewUserName,NewPassword) = mrOK then
  with UserListSource.DataSet do
  begin
    Append;
    FieldByName('UserName').AsString := NewUserName;
    FieldByName('USERPASSWORD').AsString := NewPassword;
  end;
end;

procedure TMainForm.AddUserUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserListSource.State = dsBrowse);
end;

procedure TMainForm.AttachmentsTabHide(Sender: TObject);
begin
  AttachSource.DataSet.Active := false;
  AttmtTimer.Enabled := false;
end;

procedure TMainForm.AttachmentsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  AttachSource.DataSet.Active := true;
  AttmtGrid.ShowEditorPanel; {assume located at current connection}
end;

procedure TMainForm.AttmtTimerTimer(Sender: TObject);
begin
  DatabaseData.CurrentTransaction.Commit; {force a refresh}
end;

procedure TMainForm.BackupExecute(Sender: TObject);
begin
  DatabaseData.BackupDatabase;
end;

procedure TMainForm.ChgPasswordExecute(Sender: TObject);
var NewPassword: string;
begin
  NewPassword := '';
  if ChgPasswordDlg.ShowModal(NewPassword) = mrOK then
  with UserListSource.DataSet do
  begin
    Edit;
    FieldByName('USERPASSWORD').AsString := NewPassword;
  end;
end;

procedure TMainForm.ChgPasswordUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserListSource.DataSet.Active and (UserListSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.DatabaseOnlineChange(Sender: TObject);
var ShutDownMode: TShutDownMode;
    Delay: integer;
begin
  if FLoading then Exit;
  ShutDownMode := DenyTransaction;
  Delay := 60;
  if DatabaseOnline.Checked then
    DatabaseData.BringDatabaseOnline
  else
  if ShutdownReqDlg.ShowModal(DatabaseAliasName.Text,ShutDownMode,Delay) = mrOK then
    DatabaseData.Shutdown(ShutdownMode,Delay);
end;

procedure TMainForm.DBIsReadOnlyChange(Sender: TObject);
begin
  if FLoading then Exit;
  try
    DatabaseData.DBReadOnly := DBIsReadOnly.Checked;
  except on E:Exception do
     MessageDlg(E.message,mtError,[mbOK],0);
  end;
end;

procedure TMainForm.DeleteTagExecute(Sender: TObject);
begin
  UserTagsSource.DataSet.Delete;
end;

procedure TMainForm.DeleteTagUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserTagsSource.DataSet.Active and (UserTagsSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.DeleteUserExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete user ' + Trim(UserListSource.DataSet.FieldByName('UserName').AsString),
     mtConfirmation,[mbYes,mbNo],0) = mrYes then
     UserListSource.DataSet.Delete;
end;

procedure TMainForm.DisconnectAttachmentExecute(Sender: TObject);
begin
  if MessageDlg('Disconnect Attachment ID ' + AttachSource.DataSet.FieldByName('MON$ATTACHMENT_ID').AsString,
       mtConfirmation,[mbYes,mbNo],0) = mrYes then
    AttachSource.DataSet.Delete;
end;

procedure TMainForm.DisconnectAttachmentUpdate(Sender: TObject);
begin
  with AttachSource.DataSet do
  (Sender as TAction).Enabled := Active and (RecordCount > 0)
    and (FieldByName('MON$ATTACHMENT_ID').AsInteger <>
          FieldByName('CURRENT_CONNECTION').AsInteger);
end;

procedure TMainForm.DropDatabaseExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you really want to delete the database "%s". You will lose all your data!',
        [IBDatabaseInfo.Database.DatabaseName]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
    DatabaseData.DropDatabase;
    DatabaseData.Connect;
    if not IBDatabaseInfo.Database.Connected then Close;
  end;
end;

procedure TMainForm.DropDatabaseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBDatabaseInfo.Database.Connected;
end;

procedure TMainForm.FB3UserManagerShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  UserListSource.DataSet.Active := true;
end;

procedure TMainForm.FilesTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  PrimaryDBFile.Text := IBDatabaseInfo.DBFileName;
  SecFiles.DataSet.Active := true;
  ShadowSource.DataSet.Active := true;
end;

procedure TMainForm.OpenDatabaseExecute(Sender: TObject);
begin
  DatabaseData.Connect;
  if not IBDatabaseInfo.Database.Connected then Close;
end;

procedure TMainForm.PropertiesShow(Sender: TObject);
begin
  if Visible and IBDatabaseInfo.Database.Connected then
    LoadData;
end;

procedure TMainForm.RemoveShadowExecute(Sender: TObject);
var ShadowSet: integer;
begin
  ShadowSet := ShadowSource.DataSet.FieldByName('RDB$Shadow_Number').AsInteger;
  DatabaseData.RemoveShadowSet(ShadowSet);
end;

procedure TMainForm.RemoveShadowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=  ShadowSource.DataSet.Active and (ShadowSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.RestoreExecute(Sender: TObject);
begin
  DatabaseData.RestoreDatabase;
end;

procedure TMainForm.SaveChangesExecute(Sender: TObject);
begin
  if RoleSource.DataSet.State in [dsEdit,dsInsert] then
    RoleSource.DataSet.Post;
  if UserListSource.DataSet.State in [dsEdit,dsInsert] then
    UserListSource.DataSet.Post;
end;

procedure TMainForm.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserListSource.DataSet.State in [dsInsert,dsEdit]) or
                                 (RoleSource.DataSet.State in [dsInsert,dsEdit]) or
                                 (UserTagsSource.DataSet.State in [dsInsert,dsEdit]) ;
end;

procedure TMainForm.SaveExecute(Sender: TObject);
begin
  if PageControl1.ActivePage = SchemaTab then
  begin
    if SQLSaveDialog.Execute then
       SynEdit1.Lines.SaveToFile(SQLSaveDialog.FileName);
  end
  else
  if PageControl1.ActivePage = StatisticsTab then
  begin
    if SaveDialog.Execute then
      StatsMemo.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.SaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (PageControl1.ActivePage = SchemaTab) or
                                 (PageControl1.ActivePage = StatisticsTab);
end;

procedure TMainForm.SchemaTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  SynEdit1.Lines.Clear;
  Application.QueueAsyncCall(@DoExtract,0);
end;

procedure TMainForm.ServerTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  DatabaseData.LoadServerProperties(ServerPropMemo.Lines);
  DatabaseData.LoadServerLog(ServerLog.Lines);
end;

procedure TMainForm.StatisticsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  StatsMemo.Lines.Clear;
  DatabaseData.LoadDatabaseStatistics(StatsOptions.ItemIndex,StatsMemo.Lines);
end;

procedure TMainForm.SweepIntervalEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  DatabaseData.SweepInterval := StrtoInt(SweepInterval.Text);
end;

procedure TMainForm.SyncWritesChange(Sender: TObject);
begin
  if FLoading then Exit;
  DatabaseData.ForcedWrites := SyncWrites.Checked;
end;

procedure TMainForm.ToggleAutoRefreshExecute(Sender: TObject);
begin
  AttmtTimer.Enabled := not AttmtTimer.Enabled;
end;

procedure TMainForm.ToggleAutoRefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := AttachSource.DataSet.Active;
  (Sender as TAction).Checked := AttmtTimer.Enabled;
end;

procedure TMainForm.HandleDBConnect(Sender: TObject);
begin
  ConfigureUserManager;
  PageControl1.ActivePage := Properties;
  StatusBar1.SimpleText := Format('Database: %s - Logged in as user %s',
         [DatabaseData.IBDatabase1.DatabaseName,DatabaseData.IBDatabase1.Params.Values['user_name']]);
end;

procedure TMainForm.HandleLoadData(Sender: TObject);
begin
  if assigned(PageControl1.ActivePage.OnShow) then
    PageControl1.ActivePage.OnShow(nil);
end;

procedure TMainForm.LoadData;
begin
  if FLoading then Exit;
  FLoading := true;
  try
    Edit1.Text := IBDatabaseInfo.DBSiteName;
    Edit2.Text :=  Format('%d.%d',[IBDatabaseInfo.ODSMajorVersion,IBDatabaseInfo.ODSMinorVersion]);
    Edit5.Text :=  IBDatabaseInfo.Version;
    Edit6.Text :=  IntToStr(IBDatabaseInfo.DBSQLDialect);
    Edit8.Text := DatabaseData.IBDatabase1.DatabaseName;
    Edit10.Text := IntToStr(IBDatabaseInfo.CurrentMemory);
    Edit11.Text := IntToStr(IBDatabaseInfo.MaxMemory);
    Edit12.Text := IntToStr(IBDatabaseInfo.NumBuffers);
    AllocatedPages.Text := IntToStr(IBDatabaseInfo.Allocation);
    DBIsReadOnly.Checked := DatabaseData.DBReadOnly;
    SyncWrites.Checked := DatabaseData.ForcedWrites;
    SweepInterval.Text := IntToStr(IBDatabaseInfo.SweepInterval);
    NoReserve.Checked := DatabaseData.NoReserve;
    LingerDelay.Text := DatabaseData.LingerDelay;
    DatabaseOnline.Checked := DatabaseData.IsDatabaseOnline;
    IsShadowChk.Checked := DatabaseData.IsShadowDatabase;
    if IBDatabaseInfo.ODSMajorVersion >= 12 then
    begin
      PagesUsed.Text := IntToStr(IBDatabaseInfo.PagesUsed);
      PagesAvail.Text := IntToStr(IBDatabaseInfo.PagesFree);
      AutoAdmin.Checked := DatabaseData.AutoAdmin;
    end
    else
    begin
      PagesUsed.Text := '';
      PagesAvail.Text := '';
    end;
  finally
    FLoading := false;
  end;
end;

procedure TMainForm.DoExtract(Data: PtrInt);
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    IBExtract1.ExtractObject(eoDatabase);
    SynEdit1.Lines.Assign(IBExtract1.Items);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ConfigureUserManager;
var i: integer;
begin
  if IBDatabaseInfo.ODSMajorVersion >= 12 then
  begin
    for i in [9,10] do
      UserManagerGrid.Columns[i].Visible := false;
      for i in [4,6,7,8] do
        UserManagerGrid.Columns[i].Visible := true ;
    UserListSource.DataSet := DatabaseData.UserList;
    TagsHeader.Visible := true;
    TagsGrid.Visible := true;
  end
  else
  begin
    for i in [4,6,7,8] do
      UserManagerGrid.Columns[i].Visible := false;
      for i in [9,10] do
        UserManagerGrid.Columns[i].Visible := true;
      UserListSource.DataSet := DatabaseData.LegacyUserList;
      TagsHeader.Visible := false;
      TagsGrid.Visible := false;
  end;
end;

end.

