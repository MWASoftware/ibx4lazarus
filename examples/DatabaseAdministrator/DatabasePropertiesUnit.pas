unit DatabasePropertiesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Buttons,  ActnList, ExtCtrls, db, IBDatabaseInfo, IBQuery,
  IBSQL, IBDatabase, IBServices, IBUpdate, IBLookupComboEditBox, IB;

type

  { TDatabaseProperties }

  TDatabaseProperties = class(TForm)
    AllocatedPages: TEdit;
    Bevel1: TBevel;
    Button1: TButton;
    AutoAdmin: TCheckBox;
    DatabaseOnline: TCheckBox;
    DatabaseQuery: TIBQuery;
    DatabaseSource: TDataSource;
    CharSetSource: TDataSource;
    DBCharSetSource: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBOwner: TDBEdit;
    DBIsReadOnly: TCheckBox;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    LingerDelay: TEdit;
    Edit2: TEdit;
    PagesUsed: TEdit;
    PagesAvail: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    IBConfigService1: TIBConfigService;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    CharSetLookup: TIBQuery;
    DBCharSet: TIBQuery;
    DBCharacterSet: TIBLookupComboEditBox;
    SecGlobalAuth: TIBQuery;
    IBStatisticalService1: TIBStatisticalService;
    IBTransaction1: TIBTransaction;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NoReserve: TCheckBox;
    PageSize: TEdit;
    SecDatabase: TDBEdit;
    ExecDDL: TIBSQL;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    procedure AutoAdminChange(Sender: TObject);
    procedure DatabaseOnlineChange(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeClose(DataSet: TDataSet);
    procedure DatabaseQueryBeforeOpen(DataSet: TDataSet);
    procedure DBCharSetAfterClose(DataSet: TDataSet);
    procedure DBCharSetBeforeOpen(DataSet: TDataSet);
    procedure DBIsReadOnlyChange(Sender: TObject);
    procedure LingerDelayEditingDone(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure IBTransaction1AfterTransactionEnd(Sender: TObject);
    procedure IBUpdate1ApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure IsShadowChkChange(Sender: TObject);
    procedure NoReserveChange(Sender: TObject);
    procedure DBCharacterSetEditingDone(Sender: TObject);
    procedure SecGlobalAuthAfterOpen(DataSet: TDataSet);
    procedure SweepIntervalEditingDone(Sender: TObject);
    procedure SyncWritesChange(Sender: TObject);
  private
    FDatabaseOnline: boolean;
    FShadowDatabase: boolean;
    FLoading: boolean;
    FDisconnecting: boolean;
    FDBPassword: string;
    procedure GetDBFlags;
    procedure LoadData;
    procedure Connect;
  public
    function ShowModal(ActiveService: TIBCustomService; DBName: string; aPassword: string): TModalResult;
    function IsDatabaseOnline: boolean;
  end;

var
  DatabaseProperties: TDatabaseProperties;

implementation

{$R *.lfm}

uses IBUtils, DBLoginDlgUnit, FBMessages, IBErrorCodes,
  BringOnlineDlgUnit, ShutdownRegDlgUnit, ShutdownDatabaseDlgUnit;

{ TDatabaseProperties }

procedure TDatabaseProperties.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBDatabase1.LoginPrompt := false;
  IBTransaction1.Active := true;
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    SecDatabase.Enabled := false;
    SecDatabase.DataField := '';
    DBCharacterSet.ReadOnly := true;
    AutoAdmin.Enabled := false;
    DBOwner.Enabled := false;
    DBOwner.DataField := '';
    LingerDelay.Enabled := false;
    PagesUsed.Enabled := false;
    PagesAvail.Enabled := false;
  end;

  {Virtual tables did not exist prior to Firebird 2.1 - so don't bother with old version}
  with IBDatabaseInfo do
    if (ODSMajorVersion > 11) or ((ODSMajorVersion = 11) and (ODSMinorVersion >= 1)) then
      DataBaseQuery.Active := true;
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

procedure TDatabaseProperties.IBTransaction1AfterTransactionEnd(Sender: TObject
  );
begin
  if not FDisconnecting then
  begin
    IBTransaction1.Active := true;
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
  IBConfigService1.ActivateShadow;
  while IBConfigService1.IsServiceRunning do;
  MessageDlg('Shadow Database activated. You should now rename the file or change the database alias name to point to the shadow',
    mtInformation,[mbOK],0);
  IsShadowChk.Enabled := false;
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
    GetDBFlags;
    DatabaseOnline.Checked := FDatabaseOnline;
    IsShadowChk.Checked := FShadowDatabase;
    IsShadowChk.Enabled := FShadowDatabase;
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

procedure TDatabaseProperties.DatabaseQueryBeforeOpen(DataSet: TDataSet);
begin
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
     IBTransaction1.Commit; {refresh}
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

