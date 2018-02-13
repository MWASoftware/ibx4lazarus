unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ActnList, StdCtrls, DbCtrls, db, IBLookupComboEditBox,
  IBDynamicGrid, IBDatabaseInfo, IBServices;

type

  { TMainForm }

  TMainForm = class(TForm)
    RemoveShadow: TAction;
    AddShadowSet: TAction;
    AddSecondary: TAction;
    AddFileBtn: TButton;
    AddShadowBtn: TButton;
    DBEdit4: TDBEdit;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDatabase: TAction;
    AllocatedPages: TEdit;
    AutoAdmin: TCheckBox;
    CharSetSource: TDataSource;
    DatabaseOnline: TCheckBox;
    DatabaseSource: TDataSource;
    AttmtSource: TDataSource;
    DBCharacterSet: TIBLookupComboEditBox;
    DBCharSetRO: TDBEdit;
    DBCharSetSource: TDataSource;
    DBEdit1: TDBEdit;
    DatabaseAliasName: TDBEdit;
    DBEdit3: TDBEdit;
    DBIsReadOnly: TCheckBox;
    DBOwner: TDBEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit8: TEdit;
    IBDatabaseInfo: TIBDatabaseInfo;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LingerDelay: TEdit;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    NoReserve: TCheckBox;
    PagesAvail: TEdit;
    PagesUsed: TEdit;
    PrimaryDBFile: TEdit;
    RemoveShadowBtn: TButton;
    Restore: TAction;
    Backup: TAction;
    MenuImages: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Quit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    PageControl1: TPageControl;
    SecDatabase: TDBEdit;
    SecFiles: TDataSource;
    ShadowSource: TDataSource;
    StatusBar1: TStatusBar;
    Properties: TTabSheet;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    FilesTab: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AddSecondaryExecute(Sender: TObject);
    procedure AddShadowSetExecute(Sender: TObject);
    procedure AutoAdminChange(Sender: TObject);
    procedure BackupExecute(Sender: TObject);
    procedure DatabaseOnlineChange(Sender: TObject);
    procedure DBCharacterSetEditingDone(Sender: TObject);
    procedure DBIsReadOnlyChange(Sender: TObject);
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
    procedure SweepIntervalEditingDone(Sender: TObject);
    procedure SyncWritesChange(Sender: TObject);
  private
    FLoading: boolean;
    procedure HandleDBConnect(Sender: TObject);
    procedure HandleLoadData(Sender: TObject);
    procedure LoadData;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses DataModule, ShutdownRegDlgUnit, AddSecondaryFileDlgUnit;

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

procedure TMainForm.BackupExecute(Sender: TObject);
begin
  DatabaseData.BackupDatabase;
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

procedure TMainForm.FilesTabShow(Sender: TObject);
begin
  if not Visible then Exit;
  PrimaryDBFile.Text := IBDatabaseInfo.DBFileName;
  SecFiles.DataSet.Active := true;
  ShadowSource.DataSet.Active := true;
end;

procedure TMainForm.OpenDatabaseExecute(Sender: TObject);
begin
  DatabaseData.Connect;
end;

procedure TMainForm.PropertiesShow(Sender: TObject);
begin
  if Visible then
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

procedure TMainForm.HandleDBConnect(Sender: TObject);
begin
  PageControl1.ActivePage := Properties;
end;

procedure TMainForm.HandleLoadData(Sender: TObject);
begin
  if assigned(PageControl1.ActivePage.OnShow) then
    PageControl1.ActivePage.OnShow(nil);
end;

procedure TMainForm.LoadData;
begin
  if FLoading or not IBDatabaseInfo.Database.Connected then Exit;
  FLoading := true;
  try
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

end.

