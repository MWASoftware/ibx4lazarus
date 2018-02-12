unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Menus, IBServices, IB;

type

  TRunServiceProc = procedure of object;

  { TMainForm }

  TMainForm = class(TForm)
    IBConfigService1: TIBConfigService;
    MenuItem6: TMenuItem;
    Shutdown: TAction;
    BringOnline: TAction;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    Sweep: TAction;
    LimboTransactions: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    Validate: TAction;
    Statistics: TAction;
    ActionList1: TActionList;
    CLoseBtn: TButton;
    BackupBtn: TButton;
    RestoreBtn: TButton;
    ServerLOgBtn: TButton;
    DatabaseBtn: TButton;
    UsersBtn: TButton;
    IBLogService1: TIBLogService;
    IBOnlineValidationService1: TIBOnlineValidationService;
    IBServerProperties1: TIBServerProperties;
    IBStatisticalService1: TIBStatisticalService;
    IBValidationService1: TIBValidationService;
    Memo1: TMemo;
    procedure BringOnlineExecute(Sender: TObject);
    procedure BringOnlineUpdate(Sender: TObject);
    procedure CLoseBtnClick(Sender: TObject);
    procedure BackupBtnClick(Sender: TObject);
    procedure RestoreBtnClick(Sender: TObject);
    procedure ServerLOgBtnClick(Sender: TObject);
    procedure DatabaseBtnClick(Sender: TObject);
    procedure ShutdownExecute(Sender: TObject);
    procedure SweepExecute(Sender: TObject);
    procedure UsersBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBServerProperties1Login(Service: TIBCustomService;
      LoginParams: TStrings);
    procedure AltSecDBLogin(Service: TIBCustomService;
      LoginParams: TStrings);
    procedure LimboTransactionsExecute(Sender: TObject);
    procedure StatisticsExecute(Sender: TObject);
    procedure ValidateExecute(Sender: TObject);
  private
    { private declarations }
    FValidationService: TIBControlAndQueryService;
    FDBName: string;
    FServerUserName: string;
    FServerPassword: string;
    FShutDownMode: TShutdownMode;
    FDelay: integer;
    procedure SetDBName(AValue: string);
    procedure UseServerLogin;
    function RunService(aService: TIBCustomService; RunProc: TRunServiceProc
      ): boolean;
    procedure RunShowStatistics;
    procedure RunValidation;
    procedure RunLimboTransactions;
    procedure RunSweep;
    procedure RunBringOnline;
    procedure RunShutdown;
    property DBName: string read FDBName write SetDBName;
  public
    { public declarations }
    function IsDatabaseOnline: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses IBErrorCodes, FBMessages, ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit, AltDBSvcLoginDlgUnit,
  ShutdownDatabaseDlgUnit, ShutdownRegDlgUnit;

resourcestring
  sDBSweep     = 'Database sweep started';
  sSweepOK     = 'Sweep successfully completed';


{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
var i: integer;
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  RestoreDlg.IBRestoreService1.DatabaseName.Clear;
  RestoreDlg.IBRestoreService1.DatabaseName.Add(GetTempDir + 'mytest.fdb');
  FDBName := IBStatisticalService1.DatabaseName;
  with IBServerProperties1 do
  begin
    while not Active do
    begin
      try
        Active := true;
      except
       on E:EIBClientError do
        begin
          Close;
          Exit
        end;
       On E:Exception do
         MessageDlg(E.Message,mtError,[mbOK],0);
      end;
    end; {Loop until logged in or user cancels}

    {Display the server properties}
    FetchVersionInfo;
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    Memo1.Lines.Add(Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
    FetchDatabaseInfo;
    Memo1.Lines.Add('No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    Memo1.Lines.Add('No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      Memo1.Lines.Add('DB Name = ' + DatabaseInfo.DbName[i]);
    FetchConfigParams;
    Memo1.Lines.Add('Base Location = ' + ConfigParams.BaseLocation);
    Memo1.Lines.Add('Lock File Location = ' + ConfigParams.LockFileLocation);
    Memo1.Lines.Add('Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
  end;
  IBServerProperties1.OnLogin := @AltSecDBLogin;
  {Leave IBServerProperties1 as active and use this as the common service interface}
end;

{This is the initial logon to the default security database on the server}

procedure TMainForm.IBServerProperties1Login(Service: TIBCustomService;
  LoginParams: TStrings);
var aServiceName: string;
    aUserName: string;
    aPassword: string;
begin
  aServiceName := Service.ServerName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if SvcLoginDlg.ShowModal(aServiceName, aUserName, aPassword) = mrOK then
  begin
    Service.ServerName := aServiceName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FServerUserName := aUserName;
    FServerPassword := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

{This is the login dialog for a alt. security database}

procedure TMainForm.AltSecDBLogin(Service: TIBCustomService;
  LoginParams: TStrings);
var aServiceName: string;
    aUserName: string;
    aPassword: string;
begin
  aServiceName := Service.ServerName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if AltDBSvcLoginDlg.ShowModal(aServiceName, aUserName, aPassword) = mrOK then
  begin
    Service.ServerName := aServiceName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TMainForm.LimboTransactionsExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  with LimboTransactionsForm do
  begin
    if SelectDBDlg.ShowModal(aDBName) = mrOK then
    begin
      DBName := aDBName;
      RunService(LimboTransactionValidation,@RunLimboTransactions);
    end;
  end;
end;

procedure TMainForm.StatisticsExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if SelectDBDlg.ShowModal(aDBName) = mrOK then
  begin
    DBName := aDBName;
    IBStatisticalService1.Options := [DataPages];
    RunService(IBStatisticalService1,@RunShowStatistics);
  end;
end;

procedure TMainForm.ValidateExecute(Sender: TObject);
var UseOnlineValidation: boolean;
    aDBName: string;
begin
  UseOnlineValidation := false;
  aDBName := DBName;
  if SelectValidationDlg.ShowModal(IBServerProperties1.ServerName,aDBName,UseOnlineValidation) = mrOK then
  begin
    DBName := aDBName;
    if UseOnlineValidation then
      FValidationService := IBOnlineValidationService1
    else
    begin
      FValidationService :=  IBValidationService1;
      IBValidationService1.Options := [ValidateFull];
    end;
    RunService(FValidationService,@RunValidation);
  end;
end;

procedure TMainForm.SetDBName(AValue: string);
begin
  if FDBName = AValue then Exit;
  UseServerLogin;
  FDBName := AValue;
end;

procedure TMainForm.UseServerLogin;
var index: integer;
begin
  index := IBServerProperties1.Params.IndexOfName('expected_db');
  if index <> -1 then
  begin
    {Log back in at Server Level}
    IBServerProperties1.Active := false;
    IBServerProperties1.LoginPrompt := false;
    IBServerProperties1.Params.Values['user_name'] := FServerUserName;
    IBServerProperties1.Params.Values['password'] := FServerPassword;
    IBServerProperties1.Params.Delete(index);
    IBServerProperties1.Active := true;
  end;
end;

{Common code for launching a service that might need to use and alt. security database}

function TMainForm.RunService(aService: TIBCustomService; RunProc: TRunServiceProc
  ): boolean;

  procedure AltDBLogin;
  var index: integer;
  begin
    with IBServerProperties1 do
    begin
      Active := false;
      LoginPrompt := true;
      Params.Add('expected_db='+DBName);
      index := Params.IndexOfName('password');
      if index <> -1 then
        Params.Delete(index);

      {Now make sure we are logged in}

      while not Active do
      begin
        try
          Active := true;
        except
         on E:EIBClientError do
            raise;
         On E:Exception do
         begin
           MessageDlg(E.Message,mtError,[mbOK],0);
           Active := false;
         end;
        end;
      end; {Loop until logged in or user cancels}

    end;
  end;

begin
  Result := false;
  if aService is TIBValidationService then
    TIBValidationService(aService).DatabaseName := DBName
  else
  if aService is TIBOnlineValidationService then
      TIBOnlineValidationService(aService).DatabaseName := DBName
  else
  if aService is TIBStatisticalService then
    TIBStatisticalService(aService).DatabaseName := DBName
  else
  if aService is TIBConfigService then
    TIBConfigService(aService).DatabaseName := DBName;
  try
    repeat
      with aService do
      begin
        Active := false;
        Assign(IBServerProperties1);
      end;
      try
        RunProc;
        Result := true;
      except
        on E:EIBClientError do {Typically Login cancelled}
          begin
            MessageDlg(E.Message,mtError,[mbOK],0);
            Exit;
          end;
         on E: EIBInterBaseError do
           if E.IBErrorCode = isc_sec_context then {Need expected_db}
             AltDBLogin
           else
             raise;
        end;
      aService.Active := false;
    until Result;
  except on E:Exception do
    MessageDlg(E.Message,mtError,[mbOK],0);
  end;
end;

procedure TMainForm.RunShowStatistics;
begin
  with IBStatisticalService1 do
  begin
    ServiceStart;
    Memo1.Lines.Add('Database Statistics for ' + IBStatisticalService1.DatabaseName);
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TMainForm.RunValidation;
begin
  with FValidationService do
  begin
    ServiceStart;
    Memo1.Lines.Add('Running...');
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
    Memo1.Lines.Add('Validation Completed');
    MessageDlg('Validation Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TMainForm.RunLimboTransactions;
begin
  with LimboTransactionsForm do
  begin
    {test access credentials}
    LimboTransactionValidation.ServiceStart;
    LimboTransactionValidation.FetchLimboTransactionInfo;
    ShowModal;
  end;
end;

procedure TMainForm.RunSweep;
var ReportCount: integer;
begin
  ReportCount := 0;
  with IBValidationService1 do
  begin
    Memo1.Lines.Add(Format(sDBSweep,[DatabaseName]));
    try
      ServiceStart;
      While not Eof do
      begin
        Inc(ReportCount);
        Memo1.Lines.Add(GetNextLine);
        Application.ProcessMessages;
      end
    finally
      while IsServiceRunning do;
    end
  end;
  Memo1.Lines.Add(sSweepOK);
end;

function TMainForm.IsDatabaseOnline: boolean;
var Line: string;
begin
  {Scan header page to see if database is online - assumes that service is already set up}
  Result := true;
  with IBStatisticalService1 do
  begin
    Assign(IBServerProperties1);
    Options := [HeaderPages];
    Active := True;
    try
      ServiceStart;
      while not Eof do
      begin
         Line := GetNextLine;
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
           Result := false;

      end;
      while IsServiceRunning do;
    finally
      Active := False;
    end
  end;
end;

procedure TMainForm.RunBringOnline;
begin
  if IsDatabaseOnline then
    MessageDlg('Database is already online!',mtInformation,[mbOK],0)
  else
  begin
    IBConfigService1.Assign(IBServerProperties1);
    IBConfigService1.DatabaseName := DBName;
    IBConfigService1.BringDatabaseOnline;
    while IBConfigService1.IsServiceRunning do;
    if IsDatabaseOnline then
      MessageDlg('Database is back online',mtInformation,[mbOK],0)
    else
      MessageDlg('Database is still shutdown!',mtError,[mbOK],0);
  end;
end;

procedure TMainForm.RunShutdown;
begin
  if not IsDatabaseOnline then
    MessageDlg('Database is already shutdown!',mtInformation,[mbOK],0)
  else
  begin
    ShutdownDatabaseDlg.IBConfigService.DatabaseName := DBName;
    ShutdownDatabaseDlg.Shutdown(FShutDownMode,FDelay);
  end;
end;

procedure TMainForm.CLoseBtnClick(Sender: TObject);
begin
  Close
end;

procedure TMainForm.BringOnlineExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if SelectDBDlg.ShowModal(aDBName) = mrOK then
  begin
    DBName := aDBName;
    RunService(IBStatisticalService1,@RunBringOnline);
  end;
end;

procedure TMainForm.BringOnlineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not ShutdownDatabaseDlg.Aborting;
end;

procedure TMainForm.BackupBtnClick(Sender: TObject);
begin
  BackupDlg.IBBackupService1.ServerName := IBServerProperties1.ServerName;
  BackupDlg.IBBackupService1.DatabaseName := DBName;
  if BackupDlg.ShowModal = mrOK then
  begin
    DBName := BackupDlg.IBBackupService1.DatabaseName;
    Runservice(BackupDlg.IBBackupService1,@BackupDlg.RunBackup);
  end;
end;

procedure TMainForm.RestoreBtnClick(Sender: TObject);
begin
  RestoreDlg.IBRestoreService1.ServerName := IBServerProperties1.ServerName;
  RestoreDlg.IBRestoreService1.DatabaseName[0] := DBName;
  if RestoreDlg.ShowModal = mrOK then
  begin
    DBName := RestoreDlg.IBRestoreService1.DatabaseName[0];
    UseServerLogin; {Avoid server hanging if we use an alt. sec. database wrongly}
    RunService(RestoreDlg.IBRestoreService1,@RestoreDlg.RunRestore);
  end;
end;

procedure TMainForm.ServerLOgBtnClick(Sender: TObject);
begin
  Memo1.Lines.Add('Server Log');
  {No chance that we will need an alt. security database - so just assign it the
   server connection}
  IBLogService1.Assign(IBServerProperties1);
  with IBLogService1 do
  begin
    ServiceStart;
    while not Eof do
    begin
      Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TMainForm.DatabaseBtnClick(Sender: TObject);
begin
  PopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMainForm.ShutdownExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  FShutDownMode := DenyTransaction;
  if ShutdownReqDlg.ShowModal(aDBName,FShutDownMode,FDelay) = mrOK then
  begin
    DBName := aDBName;
    RunService(ShutdownDatabaseDlg.IBConfigService,@RunShutdown);
  end;
end;

procedure TMainForm.SweepExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if SelectDBDlg.ShowModal(aDBName) = mrOK then
  begin
    DBName := aDBName;
    IBValidationService1.Options := [SweepDB];
    RunService(IBValidationService1,@RunSweep);
  end;
end;

procedure TMainForm.UsersBtnClick(Sender: TObject);
begin
  UseServerLogin;
  with ListUsersForm do
  begin
    {No chance that we will need an alt. security database - so just assign it the
     server connection}
    IBSecurityService1.Assign(IBServerProperties1);
    ShowModal;
  end;
end;

end.

