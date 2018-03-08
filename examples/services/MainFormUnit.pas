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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Menus, IBXServices, IB;

const
  sDefaultDatabaseName = 'employee';

type

  TRunServiceProc = procedure of object;

  { TMainForm }

  TMainForm = class(TForm)
    IBConfigService1: TIBXConfigService;
    IBXServicesConnection1: TIBXServicesConnection;
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
    IBLogService1: TIBXLogService;
    IBOnlineValidationService1: TIBXOnlineValidationService;
    IBServerProperties1: TIBXServerProperties;
    IBStatisticalService1: TIBXStatisticalService;
    IBValidationService1: TIBXValidationService;
    Memo1: TMemo;
    procedure BringOnlineExecute(Sender: TObject);
    procedure BringOnlineUpdate(Sender: TObject);
    procedure CLoseBtnClick(Sender: TObject);
    procedure BackupBtnClick(Sender: TObject);
    procedure IBXServicesConnection1Login(Service: TIBXServicesConnection;
      var aServerName: string; LoginParams: TStrings);
    procedure IBXServicesConnection1SecurityContextException(
      Service: TIBXServicesConnection; var aAction: TSecContextAction);
    procedure RestoreBtnClick(Sender: TObject);
    procedure ServerLOgBtnClick(Sender: TObject);
    procedure DatabaseBtnClick(Sender: TObject);
    procedure ShutdownExecute(Sender: TObject);
    procedure SweepExecute(Sender: TObject);
    procedure UsersBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LimboTransactionsExecute(Sender: TObject);
    procedure StatisticsExecute(Sender: TObject);
    procedure ValidateExecute(Sender: TObject);
  private
    { private declarations }
    FDBName: string;
    FServerUserName: string;
    FServerPassword: string;
    FShutDownMode: TDBShutdownMode;
    FDelay: integer;
    procedure SetDBName(AValue: string);
    property DBName: string read FDBName write SetDBName;
  public
    { public declarations }
    function IsDatabaseOnline: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses FBMessages, ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit,
  ShutdownDatabaseDlgUnit, ShutdownRegDlgUnit;

resourcestring
  sDBSweep     = 'Database sweep started';
  sSweepOK     = 'Sweep successfully completed';
  sSecContext  = 'This database appears to use an alternative security database. '+
                 'You must now log into the alternative security database using login '+
                 'credentials for the alternative security database.';


{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
var i: integer;
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  Application.ExceptionDialog := aedOkMessageBox;
  FDBName := sDefaultDatabaseName;

  {Open the Services API connection }
  with IBXServicesConnection1 do
  begin
    while not Connected do
    begin
      try
        Connected := true;
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
  end;

  {Now display the server properties}
  with IBServerProperties1, ServicesConnection do
  begin
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    Memo1.Lines.Add(Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
    Memo1.Lines.Add('No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    Memo1.Lines.Add('No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      Memo1.Lines.Add('DB Name = ' + DatabaseInfo.DbName[i]);
    Memo1.Lines.Add('Base Location = ' + ConfigParams.BaseLocation);
    Memo1.Lines.Add('Lock File Location = ' + ConfigParams.LockFileLocation);
    Memo1.Lines.Add('Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
    Memo1.Lines.Add('Message File Location = ' + ConfigParams.MessageFileLocation);
    for i := Low(ConfigParams.ConfigFileParams) to High(ConfigParams.ConfigFileParams) do
      Memo1.Lines.Add(ConfigParams.ConfigFileParams[i]);
    for i := Low(ConfigParams.ConfigFileData.ConfigFileKey) to High(ConfigParams.ConfigFileData.ConfigFileKey) do
      Memo1.Lines.Add(Format('%d=%s',[ConfigParams.ConfigFileData.ConfigFileKey[i],ConfigParams.ConfigFileData.ConfigFileValue[i]]));
  end;
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
      IBXLimboTransactionResolutionService1.DatabaseName := DBName;
      ShowModal;
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
    IBStatisticalService1.DatabaseName := DBName;
    Memo1.Lines.Add('Database Statistics for ' + IBStatisticalService1.DatabaseName);
    IBStatisticalService1.Execute(Memo1.Lines);
  end;
end;

procedure TMainForm.ValidateExecute(Sender: TObject);
var UseOnlineValidation: boolean;
    aDBName: string;
begin
  UseOnlineValidation := false;
  aDBName := DBName;
  if SelectValidationDlg.ShowModal(IBXServicesConnection1.ServerName,aDBName,UseOnlineValidation) = mrOK then
  begin
    DBName := aDBName;
    Memo1.Lines.Add('Running...');
    if UseOnlineValidation then
    begin
      IBOnlineValidationService1.DatabaseName := DBName;
      IBOnlineValidationService1.Execute(Memo1.Lines);
    end
    else
    begin
      IBValidationService1.Options := [ValidateFull];
      IBValidationService1.DatabaseName := DBName;
      IBValidationService1.Execute(Memo1.Lines);
    end;
    Memo1.Lines.Add('Validation Completed');
    MessageDlg('Validation Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TMainForm.SetDBName(AValue: string);
begin
  if FDBName = AValue then Exit;
  FDBName := AValue;
end;

function TMainForm.IsDatabaseOnline: boolean;
var Lines: TStringList;
    i: integer;
    line: string;
begin
  {Scan header page to see if database is online - assumes that service is already set up}
  Result := true;
  with IBStatisticalService1 do
  begin
    Options := [HeaderPages];
    DatabaseName := DBName;
    Lines := TStringList.Create;
    try
      Execute(Lines);
      for i := 0 to Lines.Count - 1 do
      begin
        line := Lines[i];
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
         begin
           Result := false;
           break;
         end;

      end;
    finally
      Lines.Free;
    end
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
    if IsDatabaseOnline then
      MessageDlg('Database is already online!',mtInformation,[mbOK],0)
    else
    begin
      IBConfigService1.DatabaseName := DBName;
      IBConfigService1.BringDatabaseOnline;
      if IsDatabaseOnline then
        MessageDlg('Database is back online',mtInformation,[mbOK],0)
      else
        MessageDlg('Database is still shutdown!',mtError,[mbOK],0);
    end;
  end;
end;

procedure TMainForm.BringOnlineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not ShutdownDatabaseDlg.Aborting;
end;

procedure TMainForm.BackupBtnClick(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if BackupDlg.ShowModal(aDBName,Memo1.Lines) = mrOK then
    DBName := aDBName;
end;

{Logon to the current security database on the server}

procedure TMainForm.IBXServicesConnection1Login(
  Service: TIBXServicesConnection; var aServerName: string; LoginParams: TStrings);
var aServiceName: string;
    aUserName: string;
    aPassword: string;
begin
  aServiceName := aServerName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if SvcLoginDlg.ShowModal(aServiceName, aUserName, aPassword) = mrOK then
  begin
    Service.ServerName := aServiceName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FServerUserName := aUserName;
    FServerPassword := aPassword;
    aServerName := aServiceName;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TMainForm.IBXServicesConnection1SecurityContextException(
  Service: TIBXServicesConnection; var aAction: TSecContextAction);
begin
  if MessageDlg(sSecContext,mtInformation,[mbYes,mbNo],0) = mrYes then
    aAction := scReconnect;
end;

procedure TMainForm.RestoreBtnClick(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if RestoreDlg.ShowModal(aDBName,Memo1.Lines) = mrOK then
    DBName := aDBName;
end;

procedure TMainForm.ServerLOgBtnClick(Sender: TObject);
begin
  Memo1.Lines.Add('Server Log');
  IBLogService1.Execute(Memo1.Lines);
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
    if not IsDatabaseOnline then
      MessageDlg('Database is already shutdown!',mtInformation,[mbOK],0)
    else
      ShutdownDatabaseDlg.Shutdown(DBName,FShutDownMode,FDelay);
  end;
end;

procedure TMainForm.SweepExecute(Sender: TObject);
var aDBName: string;
begin
  aDBName := DBName;
  if SelectDBDlg.ShowModal(aDBName) = mrOK then
  with IBValidationService1 do
  begin
    DBName := aDBName;
    DatabaseName := DBName;
    Options := [SweepDB];
    Memo1.Lines.Add(Format(sDBSweep,[DatabaseName]));
    Execute(Memo1.Lines);
    Memo1.Lines.Add(sSweepOK);
  end;
end;

procedure TMainForm.UsersBtnClick(Sender: TObject);
begin
  ListUsersForm.ShowModal;
end;

end.

