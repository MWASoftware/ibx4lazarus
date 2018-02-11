unit DatabasePropertiesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DbCtrls, Buttons, ActnList, ExtCtrls, ComCtrls,
  db, IBDatabaseInfo, IBQuery, IBSQL, IBDatabase, IBServices, IBUpdate,
  IBCustomDataSet, IBExtract, IBLookupComboEditBox, IBDynamicGrid, IB;

type

  { TDatabaseProperties }

  TDatabaseProperties = class(TForm)
    ActionList1: TActionList;
    AddFileBtn: TButton;
    AddSecondary: TAction;
    AddShadowBtn: TButton;
    AddShadowSet: TAction;
    AllocatedPages: TEdit;
    AutoAdmin: TCheckBox;
    Button1: TButton;
    CharSetLookup: TIBQuery;
    CharSetSource: TDataSource;
    DatabaseOnline: TCheckBox;
    DatabaseQuery: TIBQuery;
    DatabaseSource: TDataSource;
    DBCharacterSet: TIBLookupComboEditBox;
    DBCharSet: TIBQuery;
    DBCharSetRO: TDBEdit;
    DBCharSetSource: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBIsReadOnly: TCheckBox;
    DBOwner: TDBEdit;
    DBSecFiles: TIBQuery;
    IBExtract1: TIBExtract;
    PrimaryDBFile: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    IBConfigService1: TIBConfigService;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    IBStatisticalService1: TIBStatisticalService;
    CurrentTransaction: TIBTransaction;
    ExecDDL: TIBSQL;
    IBUpdate1: TIBUpdate;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LingerDelay: TEdit;
    NoReserve: TCheckBox;
    PageControl1: TPageControl;
    PagesAvail: TEdit;
    PageSize: TEdit;
    PagesUsed: TEdit;
    RemoveShadow: TAction;
    RemoveShadowBtn: TButton;
    SecDatabase: TDBEdit;
    SecFiles: TDataSource;
    SecGlobalAuth: TIBQuery;
    ShadowFiles: TIBQuery;
    ShadowFilesFileMode: TStringField;
    ShadowFilesRDBFILE_FLAGS: TSmallintField;
    ShadowFilesRDBFILE_LENGTH: TIntegerField;
    ShadowFilesRDBFILE_NAME: TIBStringField;
    ShadowFilesRDBFILE_SEQUENCE: TSmallintField;
    ShadowFilesRDBFILE_START: TIntegerField;
    ShadowFilesRDBSHADOW_NUMBER: TSmallintField;
    ShadowSource: TDataSource;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    GeneralTab: TTabSheet;
    FilesTab: TTabSheet;
    Schema: TTabSheet;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure AddSecondaryExecute(Sender: TObject);
    procedure AddShadowSetExecute(Sender: TObject);
    procedure AutoAdminChange(Sender: TObject);
    procedure DatabaseOnlineChange(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeClose(DataSet: TDataSet);
    procedure DBCharSetAfterClose(DataSet: TDataSet);
    procedure DBCharSetBeforeOpen(DataSet: TDataSet);
    procedure DBIsReadOnlyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LingerDelayEditingDone(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure CurrentTransactionAfterTransactionEnd(Sender: TObject);
    procedure IBUpdate1ApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure IsShadowChkChange(Sender: TObject);
    procedure NoReserveChange(Sender: TObject);
    procedure DBCharacterSetEditingDone(Sender: TObject);
    procedure RemoveShadowExecute(Sender: TObject);
    procedure RemoveShadowUpdate(Sender: TObject);
    procedure SchemaShow(Sender: TObject);
    procedure SecGlobalAuthAfterOpen(DataSet: TDataSet);
    procedure SweepIntervalEditingDone(Sender: TObject);
    procedure SyncWritesChange(Sender: TObject);
    procedure FilesTabShow(Sender: TObject);
  private
    FDatabaseOnline: boolean;
    FShadowDatabase: boolean;
    FLoading: boolean;
    FDisconnecting: boolean;
    FDBPassword: string;
    procedure GetDBFlags;
    procedure LoadData;
    procedure Connect;
    procedure DoExtract(Data: PtrInt);
  public
    function ShowModal(ActiveService: TIBCustomService; DBName: string; aPassword: string): TModalResult;
    function IsDatabaseOnline: boolean;
  end;

var
  DatabaseProperties: TDatabaseProperties;

implementation

{$R *.lfm}

uses IBUtils, DBLoginDlgUnit, FBMessages, IBErrorCodes,
  BringOnlineDlgUnit, ShutdownRegDlgUnit, ShutdownDatabaseDlgUnit,
  AddShadowSetDlgUnit, AddSecondaryFileDlgUnit;

const
  sAddSecondarySQL  = 'Alter Database Add File ''%s'' Starting at %d';
  sAddSecondarySQL2 = 'Alter Database Add File ''%s'' Starting at %d Length %d';
  sRemoveShadow     = 'Drop Shadow %d';
  sRemoveShadow12   = 'Drop Shadow %d DELETE FILE';
  sPreserveShadow   = 'Drop Shadow %d PRESERVE FILE';

resourcestring
  sPreserveShadowFiles = 'Preserve Shadow Set Files after drop?';


{ TDatabaseProperties }

procedure TDatabaseProperties.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBDatabase1.LoginPrompt := false;
  CurrentTransaction.Active := true;
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    SecDatabase.Enabled := false;
    SecDatabase.DataField := '';
    DBCharacterSet.Visible := false;
    DBCharSetRO.Visible := true;
    AutoAdmin.Enabled := false;
    DBOwner.Enabled := false;
    DBOwner.DataField := '';
    LingerDelay.Enabled := false;
    PagesUsed.Enabled := false;
    PagesAvail.Enabled := false;
  end
  else
    DBCharSetRO.Visible := false;

  {Virtual tables did not exist prior to Firebird 2.1 - so don't bother with old version}
  with IBDatabaseInfo do
    if (ODSMajorVersion > 11) or ((ODSMajorVersion = 11) and (ODSMinorVersion >= 1)) then
      DataBaseQuery.Active := true;
  if PageControl1.ActivePage = FilesTab then
    FilesTab.FilesTabShow(nil);
end;

procedure TDatabaseProperties.IBDatabase1AfterDisconnect(Sender: TObject);
begin
  FDisconnecting := false;
end;

procedure TDatabaseProperties.IBDatabase1BeforeDisconnect(Sender: TObject);
begin
  FDisconnecting := true;
end;

{Login dialog when using an Alt. security database}

procedure TDatabaseProperties.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
begin
  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword) = mrOK then
  begin
    FDBPassword := aPassword; {remember for reconnect}
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;

    {Copy to config service as we also need to login to the alt. sec. database}
    IBConfigService1.Params.Values['user_name'] := aUserName;
    IBConfigService1.Params.Values['password'] := aPassword;
    IBConfigService1.Params.Values['expected_db'] := IBConfigService1.DatabaseName;
    IBConfigService1.LoginPrompt := false;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

{Reopen datasets if database not closing when a transaction ends}

procedure TDatabaseProperties.CurrentTransactionAfterTransactionEnd(Sender: TObject
  );
begin
  if not FDisconnecting then
  begin
    CurrentTransaction.Active := true;
    DatabaseQuery.Active := true;
  end;
end;

procedure TDatabaseProperties.IBUpdate1ApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  if UpdateKind = ukModify then
  begin
    ExecDDL.SQL.Text := 'ALTER DATABASE SET DEFAULT CHARACTER SET ' +
                Params.ByName('RDB$CHARACTER_SET_NAME').AsString;
    ExecDDL.ExecQuery;
  end;
end;

procedure TDatabaseProperties.IsShadowChkChange(Sender: TObject);
begin
  if FLoading then Exit;
  if not FShadowDatabase then
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
  begin
    IBConfigService1.ActivateShadow;
    while IBConfigService1.IsServiceRunning do;
    MessageDlg('Shadow Database activated. You should now rename the file or change the database alias name to point to the shadow',
      mtInformation,[mbOK],0);
  end;
end;

procedure TDatabaseProperties.NoReserveChange(Sender: TObject);
begin
  if FLoading then Exit;
  IBConfigService1.SetReserveSpace(NoReserve.Checked);
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseProperties.DBCharacterSetEditingDone(Sender: TObject);
begin
  if DBCharSet.State = dsEdit then
    DBCharSet.Post;
end;

procedure TDatabaseProperties.RemoveShadowExecute(Sender: TObject);
var ShadowSet: integer;
begin
  ShadowSet := ShadowSource.DataSet.FieldByName('RDB$Shadow_Number').AsInteger;
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    if MessageDlg(Format(sRemoveShadow,[ShadowSet]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
       ExecDDL.SQL.Text := Format(sRemoveShadow,[ShadowSet]);
  end
  else
    case MessageDlg(Format(sPreserveShadowFiles,[ShadowSet]),mtConfirmation,[mbYes,mbNo,mbCancel],0) of
    mrNo:
      ExecDDL.SQL.Text  :=Format(sRemoveShadow12,[ShadowSet]);
    mrYes:
      ExecDDL.SQL.Text := Format(sPreserveShadow,[ShadowSet]);
    mrCancel:
      Exit;
    end;
  ExecDDL.ExecQuery;
  CurrentTransaction.Commit;
end;

procedure TDatabaseProperties.RemoveShadowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=  ShadowFiles.Active and (ShadowFiles.RecordCount > 0);
end;

procedure TDatabaseProperties.SchemaShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoExtract,0);
end;

procedure TDatabaseProperties.SecGlobalAuthAfterOpen(DataSet: TDataSet);
begin
  AutoAdmin.Checked := DataSet.FieldByName('Mappings').AsInteger > 0;
end;

procedure TDatabaseProperties.SweepIntervalEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  IBConfigService1.SetSweepInterval(StrToInt(SweepInterval.Text));
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseProperties.SyncWritesChange(Sender: TObject);
begin
  if FLoading then Exit;
  IBConfigService1.SetAsyncMode(not SyncWrites.Checked);
  while IBConfigService1.IsServiceRunning do;
end;

procedure TDatabaseProperties.FilesTabShow(Sender: TObject);
begin
  DBSecFiles.Active := true;
  ShadowFiles.Active := true;
end;

{If we want to know if this is a shadow database or if it is shutdown then
 we have to look a the attributes in the database header.}

procedure TDatabaseProperties.GetDBFlags;
var Line: string;
begin
  if not IBStatisticalService1.Active then
    IBStatisticalService1.Assign(IBConfigService1);
  FDatabaseOnline := true;
  FShadowDatabase := false;
  with IBStatisticalService1 do
  begin
    Active := True;
    try
      ServiceStart;
      while not Eof do
      begin
         Line := GetNextLine;
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
           FDatabaseOnline := false;
         if (Pos('Attributes',Line) <> 0) and (Pos('shadow',Line) <> 0) then
           FShadowDatabase := true;

      end
    finally
      Active := False;
    end
  end;
end;

procedure TDatabaseProperties.LoadData;
begin
  if FLoading then Exit;
  IBConfigService1.Active := true;
  FLoading := true;
  try
    Edit2.Text :=  Format('%d.%d',[IBDatabaseInfo.ODSMajorVersion,IBDatabaseInfo.ODSMinorVersion]);
    Edit5.Text :=  IBDatabaseInfo.Version;
    Edit6.Text :=  IntToStr(IBDatabaseInfo.DBSQLDialect);
    Edit7.Text := IBConfigService1.ServerName;
    Edit8.Text := IBDatabase1.DatabaseName;
    Edit10.Text := IntToStr(IBDatabaseInfo.CurrentMemory);
    Edit11.Text := IntToStr(IBDatabaseInfo.MaxMemory);
    Edit12.Text := IntToStr(IBDatabaseInfo.NumBuffers);
    AllocatedPages.Text := IntToStr(IBDatabaseInfo.Allocation);
    PageSize.Text := IntToStr(IBDatabaseInfo.PageSize);
    DBIsReadOnly.Checked := IBDatabaseInfo.ReadOnly <> 0;
    SyncWrites.Checked := IBDatabaseInfo.ForcedWrites = 1;
    SweepInterval.Text := IntToStr(IBDatabaseInfo.SweepInterval);
    NoReserve.Checked := IBDatabaseInfo.NoReserve = 0;
    PrimaryDBFile.Text := IBDatabaseInfo.DBFileName;
    GetDBFlags;
    DatabaseOnline.Checked := FDatabaseOnline;
    IsShadowChk.Checked := FShadowDatabase;
    if IBDatabaseInfo.ODSMajorVersion >= 12 then
    begin
      PagesUsed.Text := IntToStr(IBDatabaseInfo.PagesUsed);
      PagesAvail.Text := IntToStr(IBDatabaseInfo.PagesFree);
      SecGlobalAuth.Active := true; {sets AutoAdmin}
      SecGlobalAuth.Active := false;
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

{When we connect to the database we may need to loop around if we need to
 connect via an alt. sec. database and the user doesn't enter the password
 right first time.}

procedure TDatabaseProperties.Connect;
begin
  {Was the password remembered from an earlier login i.e. we are reconnecting
   after performing some task.}
  if FDBPassword <> '' then
    IBDatabase1.Params.Values['password'] := FDBPassword;  {needed for reconnect}
  try
    IBDatabase1.Connected := true;
  except on E:EIBInterBaseError do
    if E.IBErrorCode = isc_login then
    begin
      IBConfigService1.Active := false;
      IBStatisticalService1.Active := false;
      IBDatabase1.LoginPrompt := true;
      repeat
        try
          IBDatabase1.Connected := true;
        except
         on E:EIBClientError do
           raise;

        On E:Exception do
          begin
            MessageDlg(E.Message,mtError,[mbOK],0);
            FDBPassword := '';
          end;
        end;
      until IBDatabase1.Connected;
    end
    else
      raise;
  end;
  IBDatabase1.LoginPrompt := false;
  LoadData;
end;

procedure TDatabaseProperties.DoExtract(Data: PtrInt);
begin
  IBExtract1.ExtractObject(eoDatabase);
  SynEdit1.Lines.Assign(IBExtract1.Items);
end;

procedure TDatabaseProperties.DBCharSetAfterClose(DataSet: TDataSet);
begin
  CharSetLookup.Active := false;
end;

procedure TDatabaseProperties.DBCharSetBeforeOpen(DataSet: TDataSet);
begin
  CharSetLookup.Active := true;
end;

{If we change the database to read only or back to read/write then
 we must disconnect and the reconnect}

procedure TDatabaseProperties.DBIsReadOnlyChange(Sender: TObject);
begin
  if FLoading then Exit;
  IBDatabase1.Connected := false;
  try
    try
      IBConfigService1.SetReadOnly(DBIsReadOnly.Checked);
      while IBConfigService1.IsServiceRunning do;
    except on E:Exception do
     begin
       MessageDlg(E.message,mtError,[mbOK],0);
       FLoading := true;
       try
         DBIsReadOnly.Checked := not DBIsReadOnly.Checked;
       finally
         FLoading := false;
       end;
     end;
    end
  finally
    Connect;
  end;
end;

procedure TDatabaseProperties.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := GeneralTab;
end;

procedure TDatabaseProperties.LingerDelayEditingDone(Sender: TObject);
begin
  if (StrToInt(LingerDelay.Text) =  DatabaseQuery.FieldByName('RDB$LINGER').AsInteger) then Exit;

  if (LingerDelay.Text = '') or (StrToInt(LingerDelay.Text) = 0) then
  begin
    if MessageDlg('Turn off Linger Permanently?',mtConfirmation,[mbYes,mbNo],0) = mrNo then
    begin
      IBConfigService1.SetNoLinger;
      DatabaseQueryAfterOpen(DatabaseQuery);   {refresh linger edit box}
      Exit;
    end;
    ExecDDL.SQL.Text := 'ALTER DATABASE DROP LINGER'
  end
  else
    ExecDDL.SQL.Text := 'ALTER DATABASE SET LINGER TO ' + LingerDelay.Text;
  with ExecDDL do
  begin
    Transaction.Active := true;
    ExecQuery;
    Transaction.Commit;
  end;
end;

{Linger only because available in Firebird 3, so care needed not to raise
 an exception with earlier versions}

procedure TDatabaseProperties.DatabaseQueryAfterOpen(DataSet: TDataSet);
var Linger: TField;
begin
  DBCharSet.Active :=true;
  Linger := DataSet.FindField('RDB$LINGER');
  if Linger <> nil then
  begin
    if Linger.IsNull then
      LingerDelay.Text := '0'
    else
      LingerDelay.Text := Linger.AsString;
  end
  else
    LingerDelay.Text := 'n/a';
end;

procedure TDatabaseProperties.DatabaseQueryBeforeClose(DataSet: TDataSet);
begin
  DBCharSet.Active := false;
end;

procedure TDatabaseProperties.DatabaseOnlineChange(Sender: TObject);
var ShutDownMode: TShutdownMode;
    Delay: integer;
begin
   if FLoading then Exit;
   if DatabaseOnline.Checked then
   begin
     {Bring the database back online}
     BringOnlineDlg.ShowModal(IBConfigService1);
     CurrentTransaction.Commit; {refresh}
   end
   else
   begin
     {Shutdown the database}
     ShutDownMode := Forced;
     if ShutdownReqDlg.ShowModal(IBConfigService1.DatabaseName,ShutDownMode,Delay) = mrOK then
     begin
       IBDatabase1.Connected := false;
       try
         ShutdownDatabaseDlg.Shutdown(IBConfigService1,ShutDownMode,Delay);
       finally
         Connect;
       end;
     end
     else
       LoadData;
   end;
end;

procedure TDatabaseProperties.AutoAdminChange(Sender: TObject);
begin
  if FLoading then Exit;
  try
    IBConfigService1.SetAutoAdmin(AutoAdmin.Checked);
    while IBConfigService1.IsServiceRunning do;
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

procedure TDatabaseProperties.AddShadowSetExecute(Sender: TObject);
var CurrentLocation: TBookmark;
    ShadowSet: integer;
begin
  if ShadowSource.DataSet.RecordCount = 0 then
    ShadowSet := 1
  else
  with ShadowSource.DataSet do
  begin
    CurrentLocation := Bookmark;
    DisableControls;
    try
      Last;
      ShadowSet := FieldByName('RDB$Shadow_Number').AsInteger + 1;
    finally
      Bookmark := CurrentLocation;
      EnableControls
    end
  end;
  AddShadowSetDlg.ShowModal(ShadowSet);
  CurrentTransaction.Commit;
end;

procedure TDatabaseProperties.AddSecondaryExecute(Sender: TObject);
var FileName: string;
    StartAt: integer;
    FileLength: integer;
    Pages: boolean;
    SQLText: string;
begin
  StartAt := 0;
  if IsDatabaseOnline then
    raise Exception.Create('The database must be shutdown before adding secondary files');

  if AddSecondaryFileDlg.ShowModal(FileName,StartAt,FileLength,Pages) = mrOK then
  begin
    if not Pages then
    begin
      StartAt := StartAt*1024*1024 div IBDatabaseInfo.PageSize;
      if FileLength <> -1 then
        FileLength := FileLength*1024*1024 div IBDatabaseInfo.PageSize;
    end;
    if FileLength <> -1 then
      SQLText := Format(sAddSecondarySQL2,[FileName,StartAt,FileLength])
    else
      SQLText := Format(sAddSecondarySQL,[FileName,StartAt]);
    ExecDDL.SQL.Text := SQLText;
    ExecDDL.ExecQuery;
    CurrentTransaction.Commit;
  end;
end;

function TDatabaseProperties.ShowModal(ActiveService: TIBCustomService;
  DBName: string; aPassword: string): TModalResult;
var index: integer;
begin
  FDBPassword := '';
  IBConfigService1.Assign(ActiveService);
  IBConfigService1.DatabaseName := DBName;
  IBStatisticalService1.DatabaseName := DBName;

  with IBConfigService1 do
  begin

    {Setup the database params from what we know about the server}

    IBDatabase1.Params.Clear;
    IBDatabase1.DatabaseName := MakeConnectString(ServerName,DBName,Protocol,PortNo);
    index := Params.IndexOfName('user_name');
    if index <> -1 then
      IBDatabase1.Params.Values['user_name'] := Params.Values['user_name'];
    if aPassword <> '' then
    begin
      IBDatabase1.Params.Values['password'] := aPassword;
      IBDatabase1.LoginPrompt := false;
    end
    else
    begin
      IBDatabase1.LoginPrompt := true;
      IBConfigService1.Active := false;
    end;
  end;

  {Now connect to the database}
  try
    Connect;
  except on E:Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      Exit;
    end;
  end;
  Result := inherited ShowModal;
  IBDatabase1.Connected := false;
end;

function TDatabaseProperties.IsDatabaseOnline: boolean;
begin
  GetDBFlags;
  Result := FDatabaseOnline;
end;

end.

