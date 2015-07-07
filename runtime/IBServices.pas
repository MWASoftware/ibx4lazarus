{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

{
  InterBase Express provides component interfaces to
  functions introduced in InterBase 6.0.  The Services
  components (TIB*Service, TIBServerProperties)
  function only if you have installed InterBase 6.0 or
  later software, including Firebird
}

unit IBServices;

{$Mode Delphi}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, IBHeader, IB, IBExternals, CustApp;

const
  DefaultBufferSize = 32000;

  SPBPrefix = 'isc_spb_';
  SPBConstantNames: array[1..isc_spb_last_spb_constant] of String = (
    'user_name',
    'sys_user_name',
    'sys_user_name_enc',
    'password',
    'password_enc',
    'command_line',
    'db_name',
    'verbose',
    'options',
    'connect_timeout',
    'dummy_packet_interval',
    'sql_role_name'
  );

  SPBConstantValues: array[1..isc_spb_last_spb_constant] of Integer = (
    isc_spb_user_name_mapped_to_server,
    isc_spb_sys_user_name_mapped_to_server,
    isc_spb_sys_user_name_enc_mapped_to_server,
    isc_spb_password_mapped_to_server,
    isc_spb_password_enc_mapped_to_server,
    isc_spb_command_line_mapped_to_server,
    isc_spb_dbname_mapped_to_server,
    isc_spb_verbose_mapped_to_server,
    isc_spb_options_mapped_to_server,
    isc_spb_connect_timeout_mapped_to_server,
    isc_spb_dummy_packet_interval_mapped_to_server,
    isc_spb_sql_role_name_mapped_to_server
  );

type
  TProtocol = (TCP, SPX, NamedPipe, Local);
  TOutputBufferOption = (ByLine, ByChunk);

  TIBCustomService = class;

  TLoginEvent = procedure(Database: TIBCustomService;
    LoginParams: TStrings) of object;

  { TIBCustomService }

  TIBCustomService = class(TComponent)
  private
    FIBLoaded: Boolean;
    FParamsChanged : Boolean;
    FSPB, FQuerySPB : PChar;
    FSPBLength, FQuerySPBLength : Short;
    FTraceFlags: TTraceFlags;
    FOnLogin: TLoginEvent;
    FLoginPrompt: Boolean;
    FBufferSize: Integer;
    FOutputBuffer: PChar;
    FQueryParams: String;
    FServerName: string;
    FHandle: TISC_SVC_HANDLE;
    FStreamedActive  : Boolean;
    FOnAttach: TNotifyEvent;
    FOutputBufferOption: TOutputBufferOption;
    FProtocol: TProtocol;
    FParams: TStrings;
    function GetActive: Boolean;
    function GetServiceParamBySPB(const Idx: Integer): String;
    procedure SetActive(const Value: Boolean);
    procedure SetBufferSize(const Value: Integer);
    procedure SetParams(const Value: TStrings);
    procedure SetServerName(const Value: string);
    procedure SetProtocol(const Value: TProtocol);
    procedure SetServiceParamBySPB(const Idx: Integer;
      const Value: String);
    function IndexOfSPBConst(st: String): Integer;
    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    procedure CheckServerName;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    function ParseString(var RunLen: Integer): string;
    function ParseInteger(var RunLen: Integer): Integer;
    procedure GenerateSPB(sl: TStrings; var SPB: String; var SPBLength: Short);

  protected
    procedure Loaded; override;
    function Login: Boolean;
    procedure CheckActive;
    procedure CheckInactive;
    procedure HandleException(Sender: TObject);
    property OutputBuffer : PChar read FOutputBuffer;
    property OutputBufferOption : TOutputBufferOption read FOutputBufferOption write FOutputBufferOption;
    property BufferSize : Integer read FBufferSize write SetBufferSize default DefaultBufferSize;
    procedure InternalServiceQuery;
    property ServiceQueryParams: String read FQueryParams write FQueryParams;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    property Handle: TISC_SVC_HANDLE read FHandle;
    property ServiceParamBySPB[const Idx: Integer]: String read GetServiceParamBySPB
                                                      write SetServiceParamBySPB;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property ServerName: string read FServerName write SetServerName;
    property Protocol: TProtocol read FProtocol write SetProtocol default Local;
    property Params: TStrings read FParams write SetParams;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default True;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property OnAttach: TNotifyEvent read FOnAttach write FOnAttach;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

  TDatabaseInfo = class
  public
    NoOfAttachments: Integer;
    NoOfDatabases: Integer;
    DbName: array of string;
    constructor Create;
    destructor Destroy; override;
  end;

  TLicenseInfo = class
  public
    Key: array of string;
    Id: array of string;
    Desc: array of string;
    LicensedUsers: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TLicenseMaskInfo = class
  public
    LicenseMask: Integer;
    CapabilityMask: Integer;
  end;

  TConfigFileData = class
  public
    ConfigFileValue: array of integer;
    ConfigFileKey: array of integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TConfigParams = class
  public
    ConfigFileData: TConfigFileData;
    ConfigFileParams: array of string;
    BaseLocation: string;
    LockFileLocation: string;
    MessageFileLocation: string;
    SecurityDatabaseLocation: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TVersionInfo = class
    ServerVersion: String;
    ServerImplementation: string;
    ServiceVersion: Integer;
  end;

  TPropertyOption = (Database, License, LicenseMask, ConfigParameters, Version);
  TPropertyOptions = set of TPropertyOption;

  TIBServerProperties = class(TIBCustomService)
  private
    FOptions: TPropertyOptions;
    FDatabaseInfo: TDatabaseInfo;
    FLicenseInfo: TLicenseInfo;
    FLicenseMaskInfo: TLicenseMaskInfo;
    FVersionInfo: TVersionInfo;
    FConfigParams: TConfigParams;
    procedure ParseConfigFileData(var RunLen: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Fetch;
    procedure FetchDatabaseInfo;
    procedure FetchLicenseInfo;
    procedure FetchLicenseMaskInfo;
    procedure FetchConfigParams;
    procedure FetchVersionInfo;
    property DatabaseInfo: TDatabaseInfo read FDatabaseInfo;
    property LicenseInfo: TLicenseInfo read FLicenseInfo;
    property LicenseMaskInfo: TLicenseMaskInfo read FLicenseMaskInfo;
    property VersionInfo: TVersionInfo read FVersionInfo;
    property ConfigParams: TConfigParams read FConfigParams;
  published
    property Options : TPropertyOptions read FOptions write FOptions;
  end;

  TIBControlService = class (TIBCustomService)
  private
    FStartParams: String;
    FStartSPB: PChar;
    FStartSPBLength: Integer;
    function GetIsServiceRunning: Boolean;
  protected
    property ServiceStartParams: String read FStartParams write FStartParams;
    procedure SetServiceStartOptions; virtual;
    procedure ServiceStartAddParam (Value: string; param: Integer); overload;
    procedure ServiceStartAddParam (Value: Integer; param: Integer); overload;
    procedure InternalServiceStart;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ServiceStart; virtual;
    property IsServiceRunning : Boolean read GetIsServiceRunning;
  end;

  TIBControlAndQueryService = class (TIBControlService)
  private
    FEof: Boolean;
    FAction: Integer;
    procedure SetAction(Value: Integer);
  protected
    property Action: Integer read FAction write SetAction;
  public
    constructor create (AOwner: TComponent); override;
    function GetNextLine : String;
    function GetNextChunk : String;
    property Eof: boolean read FEof;
  published
    property BufferSize;
  end;

  TShutdownMode = (Forced, DenyTransaction, DenyAttachment);

  TIBConfigService = class(TIBControlService)
  private
    FDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
  protected

  public
    procedure ServiceStart; override;
    procedure ShutdownDatabase (Options: TShutdownMode; Wait: Integer);
    procedure SetSweepInterval (Value: Integer);
    procedure SetDBSqlDialect (Value: Integer);
    procedure SetPageBuffers (Value: Integer);
    procedure ActivateShadow;
    procedure BringDatabaseOnline;
    procedure SetReserveSpace (Value: Boolean);
    procedure SetAsyncMode (Value: Boolean);
    procedure SetReadOnly (Value: Boolean);
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
  end;

  TIBLogService = class(TIBControlAndQueryService)
  private

  protected
    procedure SetServiceStartOptions; override;
  public
  published
  end;

  TStatOption = (DataPages, DbLog, HeaderPages, IndexPages, SystemRelations);
  TStatOptions = set of TStatOption;

  TIBStatisticalService = class(TIBControlAndQueryService)
  private
    FDatabaseName: string;
    FOptions: TStatOptions;
    procedure SetDatabaseName(const Value: string);
  protected
    procedure SetServiceStartOptions; override;
  public
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Options :  TStatOptions read FOptions write FOptions;
  end;


  TIBBackupRestoreService = class(TIBControlAndQueryService)
  private
    FVerbose: Boolean;
  protected
  public
  published
    property Verbose : Boolean read FVerbose write FVerbose default False;
  end;

  TBackupOption = (IgnoreChecksums, IgnoreLimbo, MetadataOnly, NoGarbageCollection,
    OldMetadataDesc, NonTransportable, ConvertExtTables);
  TBackupOptions = set of TBackupOption;

  TIBBackupService = class (TIBBackupRestoreService)
  private
    FDatabaseName: string;
    FOptions: TBackupOptions;
    FBackupFile: TStrings;
    FBlockingFactor: Integer;
    procedure SetBackupFile(const Value: TStrings);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { a name=value pair of filename and length }
    property BackupFile: TStrings read FBackupFile write SetBackupFile;
    property BlockingFactor: Integer read FBlockingFactor write FBlockingFactor;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Options : TBackupOptions read FOptions write FOptions;
  end;

  TRestoreOption = (DeactivateIndexes, NoShadow, NoValidityCheck, OneRelationAtATime,
    Replace, CreateNewDB, UseAllSpace);

  TRestoreOptions = set of TRestoreOption;
  TIBRestoreService = class (TIBBackupRestoreService)
  private
    FDatabaseName: TStrings;
    FBackupFile: TStrings;
    FOptions: TRestoreOptions;
    FPageSize: Integer;
    FPageBuffers: Integer;
    procedure SetBackupFile(const Value: TStrings);
    procedure SetDatabaseName(const Value: TStrings);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { a name=value pair of filename and length }
    property DatabaseName: TStrings read FDatabaseName write SetDatabaseName;
    property BackupFile: TStrings read FBackupFile write SetBackupFile;
    property PageSize: Integer read FPageSize write FPageSize;
    property PageBuffers: Integer read FPageBuffers write FPageBuffers;
    property Options : TRestoreOptions read FOptions write FOptions default [CreateNewDB];
  end;

  TValidateOption = (LimboTransactions, CheckDB, IgnoreChecksum, KillShadows, MendDB,
    SweepDB, ValidateDB, ValidateFull);
  TValidateOptions = set of TValidateOption;

  TTransactionGlobalAction = (CommitGlobal, RollbackGlobal, RecoverTwoPhaseGlobal,
                             NoGlobalAction);
  TTransactionState = (LimboState, CommitState, RollbackState, UnknownState);
  TTransactionAdvise = (CommitAdvise, RollbackAdvise, UnknownAdvise);
  TTransactionAction = (CommitAction, RollbackAction);

  TLimboTransactionInfo = class
  public
    MultiDatabase: Boolean;
    ID: Integer;
    HostSite: String;
    RemoteSite: String;
    RemoteDatabasePath: String;
    State: TTransactionState;
    Advise: TTransactionAdvise;
    Action: TTransactionAction;
  end;

  TIBValidationService = class(TIBControlAndQueryService)
  private
    FDatabaseName: string;
    FOptions: TValidateOptions;
    FLimboTransactionInfo: array of TLimboTransactionInfo;
    FGlobalAction: TTransactionGlobalAction;
    procedure SetDatabaseName(const Value: string);
    function GetLimboTransactionInfo(index: integer): TLimboTransactionInfo;
    function GetLimboTransactionInfoCount: integer;

  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FetchLimboTransactionInfo;
    procedure FixLimboTransactionErrors;
    property LimboTransactionInfo[Index: integer]: TLimboTransactionInfo read GetLimboTransactionInfo;
    property LimboTransactionInfoCount: Integer read GetLimboTransactionInfoCount;

  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Options: TValidateOptions read FOptions write FOptions;
    property GlobalAction: TTransactionGlobalAction read FGlobalAction
                                         write FGlobalAction;
  end;

  TUserInfo = class
  public
    UserName: string;
    FirstName: string;
    MiddleName: string;
    LastName: string;
    GroupID: Integer;
    UserID: Integer;
  end;

  TSecurityAction = (ActionAddUser, ActionDeleteUser, ActionModifyUser, ActionDisplayUser);
  TSecurityModifyParam = (ModifyFirstName, ModifyMiddleName, ModifyLastName, ModifyUserId,
                         ModifyGroupId, ModifyPassword);
  TSecurityModifyParams = set of TSecurityModifyParam;

  TIBSecurityService = class(TIBControlAndQueryService)
  private
    FUserID: Integer;
    FGroupID: Integer;
    FFirstName: string;
    FUserName: string;
    FPassword: string;
    FSQLRole: string;
    FLastName: string;
    FMiddleName: string;
    FUserInfo: array of TUserInfo;
    FSecurityAction: TSecurityAction;
    FModifyParams: TSecurityModifyParams;
    procedure ClearParams;
    procedure SetSecurityAction (Value: TSecurityAction);
    procedure SetFirstName (Value: String);
    procedure SetMiddleName (Value: String);
    procedure SetLastName (Value: String);
    procedure SetPassword (Value: String);
    procedure SetUserId (Value: Integer);
    procedure SetGroupId (Value: Integer);

    procedure FetchUserInfo;
    function GetUserInfo(Index: Integer): TUserInfo;
    function GetUserInfoCount: Integer;

  protected
    procedure Loaded; override;
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisplayUsers;
    procedure DisplayUser(UserName: string);
    procedure AddUser;
    procedure DeleteUser;
    procedure ModifyUser;
    property  UserInfo[Index: Integer]: TUserInfo read GetUserInfo;
    property  UserInfoCount: Integer read GetUserInfoCount;

  published
    property SecurityAction: TSecurityAction read FSecurityAction
                                             write SetSecurityAction;
    property SQlRole : string read FSQLRole write FSQLrole;
    property UserName : string read FUserName write FUserName;
    property FirstName : string read FFirstName write SetFirstName;
    property MiddleName : string read FMiddleName write SetMiddleName;
    property LastName : string read FLastName write SetLastName;
    property UserID : Integer read FUserID write SetUserID;
    property GroupID : Integer read FGroupID write SetGroupID;
    property Password : string read FPassword write setPassword;
  end;


implementation

uses
  IBIntf , IBSQLMonitor, Math;

{ TIBCustomService }

procedure TIBCustomService.Attach;
var
  SPB: String;
  ConnectString: String;
begin
  CheckInactive;
  CheckServerName;

  if FLoginPrompt and not Login then
    IBError(ibxeOperationCancelled, [nil]);

  { Generate a new SPB if necessary }
  if FParamsChanged then
  begin
    FParamsChanged := False;
    GenerateSPB(FParams, SPB, FSPBLength);
    IBAlloc(FSPB, 0, FsPBLength);
    Move(SPB[1], FSPB[0], FSPBLength);
  end;
  case FProtocol of
    TCP: ConnectString := FServerName + ':service_mgr'; {do not localize}
    SPX: ConnectString := FServerName + '@service_mgr'; {do not localize}
    NamedPipe: ConnectString := '\\' + FServerName + '\service_mgr'; {do not localize}
    Local: ConnectString := 'service_mgr'; {do not localize}
  end;
  if call(isc_service_attach(StatusVector, Length(ConnectString),
                         PChar(ConnectString), @FHandle,
                         FSPBLength, FSPB), False) > 0 then
  begin
    FHandle := nil;
    IBDataBaseError;
  end;

  if Assigned(FOnAttach) then
    FOnAttach(Self);

  MonitorHook.ServiceAttach(Self);
end;

procedure TIBCustomService.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive and (not Active) then
      Attach;
  except
    if csDesigning in ComponentState then
       HandleException(self)
    else
      raise;
  end;
end;

function TIBCustomService.Login: Boolean;
var
  IndexOfUser, IndexOfPassword: Integer;
  Username, Password: String;
  LoginParams: TStrings;
begin
  if Assigned(FOnLogin) then begin
    result := True;
    LoginParams := TStringList.Create;
    try
      LoginParams.Assign(Params);
      FOnLogin(Self, LoginParams);
      Params.Assign (LoginParams);
    finally
      LoginParams.Free;
    end;
  end
  else
  if assigned(IBGUIInterface)  then
  begin
    IndexOfUser := IndexOfSPBConst(SPBConstantNames[isc_spb_user_name]);
    if IndexOfUser <> -1 then
      Username := Copy(Params[IndexOfUser],
                                         Pos('=', Params[IndexOfUser]) + 1, {mbcs ok}
                                         Length(Params[IndexOfUser]));
    IndexOfPassword := IndexOfSPBConst(SPBConstantNames[isc_spb_password]);
    if IndexOfPassword <> -1 then
      Password := Copy(Params[IndexOfPassword],
                                         Pos('=', Params[IndexOfPassword]) + 1, {mbcs ok}
                                         Length(Params[IndexOfPassword]));
    result := IBGUIInterface.ServerLoginDialog(serverName, Username, Password);
    if result then
    begin
      IndexOfPassword := IndexOfSPBConst(SPBConstantNames[isc_spb_password]);
      if IndexOfUser = -1 then
        Params.Add(SPBConstantNames[isc_spb_user_name] + '=' + Username)
      else
        Params[IndexOfUser] := SPBConstantNames[isc_spb_user_name] +
                                 '=' + Username;
      if IndexOfPassword = -1 then
        Params.Add(SPBConstantNames[isc_spb_password] + '=' + Password)
      else
        Params[IndexOfPassword] := SPBConstantNames[isc_spb_password] +
                                     '=' + Password;
    end
  end
  else
    IBError(ibxeNoLoginDialog,[]);
end;

procedure TIBCustomService.CheckActive;
begin
  if FStreamedActive and (not Active) then
    Loaded;
  if FHandle = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TIBCustomService.CheckInactive;
begin
  if FHandle <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TIBCustomService.HandleException(Sender: TObject);
var aParent: TComponent;
begin
  aParent := Owner;
  while aParent <> nil do
  begin
    if aParent is TCustomApplication then
    begin
      TCustomApplication(aParent).HandleException(Sender);
      Exit;
    end;
    aParent := aParent.Owner;
  end;
  SysUtils.ShowException(ExceptObject,ExceptAddr);
end;

constructor TIBCustomService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  FserverName := '';
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FSPB := nil;
  FQuerySPB := nil;
  FBufferSize := DefaultBufferSize;
  FHandle := nil;
  FLoginPrompt := True;
  FTraceFlags := [];
  FOutputbuffer := nil;
  FProtocol := Local;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOwner).ConsoleApplication then
    LoginPrompt := false;
end;

destructor TIBCustomService.Destroy;
begin
  if FIBLoaded then
  begin
    if FHandle <> nil then
      Detach;
    FreeMem(FSPB);
    FSPB := nil;
    FParams.Free;
  end;
  ReallocMem(FOutputBuffer, 0);
  inherited Destroy;
end;

procedure TIBCustomService.Detach;
begin
  CheckActive;
  if (Call(isc_service_detach(StatusVector, @FHandle), False) > 0) then
  begin
    FHandle := nil;
    IBDataBaseError;
  end
  else
    FHandle := nil;
  MonitorHook.ServiceDetach(Self);
end;

function TIBCustomService.GetActive: Boolean;
begin
  result := FHandle <> nil;
end;

function TIBCustomService.GetServiceParamBySPB(const Idx: Integer): String;
var
  ConstIdx, EqualsIdx: Integer;
begin
  if (Idx > 0) and (Idx <= isc_spb_last_spb_constant) then
  begin
    ConstIdx := IndexOfSPBConst(SPBConstantNames[Idx]);
    if ConstIdx = -1 then
      result := ''
    else
    begin
      result := Params[ConstIdx];
      EqualsIdx := Pos('=', result); {mbcs ok}
      if EqualsIdx = 0 then
        result := ''
      else
        result := Copy(result, EqualsIdx + 1, Length(result));
    end;
  end
  else
    result := '';
end;

procedure TIBCustomService.InternalServiceQuery;
begin
  FQuerySPBLength := Length(FQueryParams);
  if FQuerySPBLength = 0 then
    IBError(ibxeQueryParamsError, [nil]);
  IBAlloc(FQuerySPB, 0, FQuerySPBLength);
  Move(FQueryParams[1], FQuerySPB[0], FQuerySPBLength);
  if (FOutputBuffer = nil) then
    IBAlloc(FOutputBuffer, 0, FBufferSize);
  try
    if call(isc_service_query(StatusVector, @FHandle, nil, 0, nil,
                           FQuerySPBLength, FQuerySPB,
                           FBufferSize, FOutputBuffer), False) > 0 then
    begin
      FHandle := nil;
      IBDataBaseError;
    end;
  finally
    FreeMem(FQuerySPB);
    FQuerySPB := nil;
    FQuerySPBLength := 0;
    FQueryParams := '';
  end;
  MonitorHook.ServiceQuery(Self);
end;

procedure TIBCustomService.SetActive(const Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value <> Active then   
      if Value then
        Attach
      else
        Detach;
end;

procedure TIBCustomService.SetBufferSize(const Value: Integer);
begin
  if (FOutputBuffer <> nil) and (Value <> FBufferSize) then
    IBAlloc(FOutputBuffer, 0, FBufferSize);
end;

procedure TIBCustomService.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TIBCustomService.SetServerName(const Value: string);
begin
  if FServerName <> Value then
  begin
    CheckInactive;
    FServerName := Value;
  end;
end;

procedure TIBCustomService.SetProtocol(const Value: TProtocol);
begin
  if FProtocol <> Value then
  begin
    CheckInactive;
    FProtocol := Value;
    if (Value = Local) then
      FServerName := '';
  end;
end;

procedure TIBCustomService.SetServiceParamBySPB(const Idx: Integer;
  const Value: String);
var
  ConstIdx: Integer;
begin
  ConstIdx := IndexOfSPBConst(SPBConstantNames[Idx]);
  if (Value = '') then
  begin
    if ConstIdx <> -1 then
      Params.Delete(ConstIdx);
  end
  else
  begin
    if (ConstIdx = -1) then
      Params.Add(SPBConstantNames[Idx] + '=' + Value)
    else
      Params[ConstIdx] := SPBConstantNames[Idx] + '=' + Value;
  end;
end;

function TIBCustomService.IndexOfSPBConst(st: String): Integer;
var
  i, pos_of_str: Integer;
begin
  result := -1;
  for i := 0 to Params.Count - 1 do
  begin
    pos_of_str := Pos(st, Params[i]); {mbcs ok}
    if (pos_of_str = 1) or (pos_of_str = Length(SPBPrefix) + 1) then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TIBCustomService.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TIBCustomService.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TIBCustomService.CheckServerName;
begin
  if (FServerName = '') and (FProtocol <> Local) then
    IBError(ibxeServerNameMissing, [nil]);
end;

function TIBCustomService.Call(ErrCode: ISC_STATUS;
  RaiseError: Boolean): ISC_STATUS;
begin
  result := ErrCode;
  if RaiseError and (ErrCode > 0) then
    IBDataBaseError;
end;

function TIBCustomService.ParseString(var RunLen: Integer): string;
var
  Len: UShort;
  tmp: Char;
begin
  Len := isc_vax_integer(OutputBuffer + RunLen, 2);
  RunLen := RunLen + 2;
  if (Len <> 0) then
  begin
    tmp := OutputBuffer[RunLen + Len];
    OutputBuffer[RunLen + Len] := #0;
    result := String(PChar(@OutputBuffer[RunLen]));
    OutputBuffer[RunLen + Len] := tmp;
    RunLen := RunLen + Len;
  end
  else
    result := '';
end;

function TIBCustomService.ParseInteger(var RunLen: Integer): Integer;
begin
  result := isc_vax_integer(OutputBuffer + RunLen, 4);
  RunLen := RunLen + 4;
end;

{
 * GenerateSPB -
 *  Given a string containing a textual representation
 *  of the Service parameters, generate a service
 *  parameter buffer, and return it and its length
 *  in SPB and SPBLength, respectively.
}
procedure TIBCustomService.GenerateSPB(sl: TStrings; var SPB: String;
  var SPBLength: Short);
var
  i, j, SPBVal, SPBServerVal: UShort;
  param_name, param_value: String;
begin
  { The SPB is initially empty, with the exception that
   the SPB version must be the first byte of the string.
  }
  SPBLength := 2;
  SPB := Char(isc_spb_version);
  SPB := SPB + Char(isc_spb_current_version);
  { Iterate through the textual service parameters, constructing
   a SPB on-the-fly }
  if sl.Count > 0 then
  for i := 0 to sl.Count - 1 do
  begin
   { Get the parameter's name and value from the list,
     and make sure that the name is all lowercase with
     no leading 'isc_spb_' prefix }
    if (Trim(sl.Names[i]) = '') then continue;
    param_name := LowerCase(sl.Names[i]); {mbcs ok}
    param_value := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(SPBPrefix, param_name) = 1) then {mbcs ok}
      Delete(param_name, 1, Length(SPBPrefix));
    { We want to translate the parameter name to some integer
      value. We do this by scanning through a list of known
      service parameter names (SPBConstantNames, defined above). }
    SPBVal := 0;
    SPBServerVal := 0;
    { Find the parameter }
    for j := 1 to isc_spb_last_spb_constant do
      if (param_name = SPBConstantNames[j]) then
      begin
        SPBVal := j;
        SPBServerVal := SPBConstantValues[j];
        break;
      end;
    case SPBVal of
      isc_spb_user_name, isc_spb_password:
      begin
        SPB := SPB +
               Char(SPBServerVal) +
               Char(Length(param_value)) +
               param_value;
        Inc(SPBLength, 2 + Length(param_value));
      end;
      else
      begin
        if (SPBVal > 0) and
           (SPBVal <= isc_dpb_last_dpb_constant) then
          IBError(ibxeSPBConstantNotSupported,
                   [SPBConstantNames[SPBVal]])
        else
          IBError(ibxeSPBConstantUnknown, [SPBVal]);
      end;
    end;
  end;
end;

{ TIBServerProperties }
constructor TIBServerProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseInfo := TDatabaseInfo.Create;
  FLicenseInfo := TLicenseInfo.Create;
  FLicenseMaskInfo := TLicenseMaskInfo.Create;
  FVersionInfo := TVersionInfo.Create;
  FConfigParams := TConfigParams.Create;
end;

destructor TIBServerProperties.Destroy;
begin
  FDatabaseInfo.Free;
  FLicenseInfo.Free;
  FLicenseMaskInfo.Free;
  FVersionInfo.Free;
  FConfigParams.Free;
  inherited Destroy;
end;

procedure TIBServerProperties.ParseConfigFileData(var RunLen: Integer);
begin
  Inc(RunLen);
  with FConfigParams.ConfigFileData do
  begin
    SetLength (ConfigFileValue, Length(ConfigFileValue)+1);
    SetLength (ConfigFileKey, Length(ConfigFileKey)+1);

    ConfigFileKey[High(ConfigFileKey)] := Integer(OutputBuffer[RunLen-1]);
    ConfigFileValue[High(ConfigFileValue)] := ParseInteger(RunLen);
  end;
end;

procedure TIBServerProperties.Fetch;
begin
  if (Database in Options) then
    FetchDatabaseInfo;
  if (License in Options) then
    FetchLicenseInfo;
  if (LicenseMask in Options) then
    FetchLicenseMaskInfo;
  if (ConfigParameters in Options) then
    FetchConfigParams;
  if (Version in Options) then
    FetchVersionInfo;
end;

procedure TIBServerProperties.FetchConfigParams;
var
  RunLen: Integer;

begin
  ServiceQueryParams := Char(isc_info_svc_get_config) +
                        Char(isc_info_svc_get_env) +
                        Char(isc_info_svc_get_env_lock) +
                        Char(isc_info_svc_get_env_msg) +
                        Char(isc_info_svc_user_dbpath);

  InternalServiceQuery;
  RunLen := 0;
  While (not (Integer(OutputBuffer[RunLen]) = isc_info_end)) do
  begin
    case Integer(OutputBuffer[RunLen]) of
      isc_info_svc_get_config:
      begin
        FConfigParams.ConfigFileData.ConfigFileKey := nil;
        FConfigParams.ConfigFileData.ConfigFileValue := nil;
        Inc (RunLen);
        while (not (Integer(OutputBuffer[RunLen]) = isc_info_flag_end)) do
          ParseConfigFileData (RunLen);
        if (Integer(OutputBuffer[RunLen]) = isc_info_flag_end) then
          Inc (RunLen);
      end;

      isc_info_svc_get_env:
      begin
        Inc (RunLen);
        FConfigParams.BaseLocation := ParseString(RunLen);
      end;

      isc_info_svc_get_env_lock:
      begin
        Inc (RunLen);
        FConfigParams.LockFileLocation := ParseString(RunLen);
      end;

      isc_info_svc_get_env_msg:
      begin
        Inc (RunLen);
        FConfigParams.MessageFileLocation := ParseString(RunLen);
      end;

      isc_info_svc_user_dbpath:
      begin
        Inc (RunLen);
        FConfigParams.SecurityDatabaseLocation := ParseString(RunLen);
      end;
      else
        IBError(ibxeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TIBServerProperties.FetchDatabaseInfo;
var
  i, RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_svr_db_info);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_svr_db_info)) then
      IBError(ibxeOutputParsingError, [nil]);
  RunLen := 1;
  if (OutputBuffer[RunLen] <> Char(isc_spb_num_att)) then
      IBError(ibxeOutputParsingError, [nil]);
  Inc(RunLen);
  FDatabaseInfo.NoOfAttachments := ParseInteger(RunLen);
  if (OutputBuffer[RunLen] <> Char(isc_spb_num_db)) then
      IBError(ibxeOutputParsingError, [nil]);
  Inc(RunLen);
  FDatabaseInfo.NoOfDatabases := ParseInteger(RunLen);
  FDatabaseInfo.DbName := nil;
  SetLength(FDatabaseInfo.DbName, FDatabaseInfo.NoOfDatabases);
  i := 0;
  while (OutputBuffer[RunLen] <> Char(isc_info_flag_end)) do
  begin
    if (OutputBuffer[RunLen] <> Char(SPBConstantValues[isc_spb_dbname])) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FDatabaseInfo.DbName[i] := ParseString(RunLen);
    Inc (i);
  end;
end;

procedure TIBServerProperties.FetchLicenseInfo;
var
  i, RunLen: Integer;
  done: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_license) +
                        Char(isc_info_svc_get_licensed_users);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;
  i := 0;
  FLicenseInfo.key := nil;
  FLicenseInfo.id := nil;
  FLicenseInfo.desc := nil;

  While done < 2 do begin
    Inc(Done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_get_license:
      begin
        while (OutputBuffer[RunLen] <> Char(isc_info_flag_end)) do
        begin
          if (i >= Length(FLicenseInfo.key)) then
          begin
            SetLength(FLicenseInfo.key, i + 10);
            SetLength(FLicenseInfo.id, i + 10);
            SetLength(FLicenseInfo.desc, i + 10);
          end;
          if (OutputBuffer[RunLen] <> Char(isc_spb_lic_id)) then
              IBError(ibxeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.id[i] := ParseString(RunLen);
          if (OutputBuffer[RunLen] <> Char(isc_spb_lic_key)) then
              IBError(ibxeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.key[i] := ParseString(RunLen);
          if (OutputBuffer[RunLen] <> Char(7)) then
              IBError(ibxeOutputParsingError, [nil]);
          Inc(RunLen);
          FLicenseInfo.desc[i] := ParseString(RunLen);
          Inc(i);
        end;
        Inc(RunLen);
        if (Length(FLicenseInfo.key) > i) then
        begin
          SetLength(FLicenseInfo.key, i);
          SetLength(FLicenseInfo.id, i);
          SetLength(FLicenseInfo.desc, i);
        end;
      end;
      isc_info_svc_get_licensed_users:
        FLicenseInfo.LicensedUsers := ParseInteger(RunLen);
      else
        IBError(ibxeOutputParsingError, [nil]);
    end;
  end;
end;

procedure TIBServerProperties.FetchLicenseMaskInfo();
var
  done,RunLen:integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_license_mask) +
                        Char(isc_info_svc_capabilities);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;
  While done <= 1 do
  begin
    Inc(done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_get_license_mask:
        FLicenseMaskInfo.LicenseMask := ParseInteger(RunLen);
      isc_info_svc_capabilities:
        FLicenseMaskInfo.CapabilityMask := ParseInteger(RunLen);
      else
        IBError(ibxeOutputParsingError, [nil]);
    end;
  end;
end;


procedure TIBServerProperties.FetchVersionInfo;
var
  RunLen: Integer;
  done: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_version) +
                        Char(isc_info_svc_server_version) +
                        Char(isc_info_svc_implementation);
  InternalServiceQuery;
  RunLen := 0;
  done := 0;

  While done <= 2 do
  begin
    Inc(done);
    Inc(RunLen);
    case Integer(OutputBuffer[RunLen-1]) of
      isc_info_svc_version:
        FVersionInfo.ServiceVersion := ParseInteger(RunLen);
      isc_info_svc_server_version:
        FVersionInfo.ServerVersion := ParseString(RunLen);
      isc_info_svc_implementation:
        FVersionInfo.ServerImplementation := ParseString(RunLen);
      else
        IBError(ibxeOutputParsingError, [nil]);
    end;
  end;
end;

{ TIBControlService }
procedure TIBControlService.SetServiceStartOptions;
begin

end;

function TIBControlService.GetIsServiceRunning: Boolean;
var
  RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_running);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_running)) then
    IBError(ibxeOutputParsingError, [nil]);
  RunLen := 1;
  if (ParseInteger(RunLen) = 1) then
    result := True
  else
    result := False;
end;

procedure TIBControlService.ServiceStartAddParam (Value: string; param: Integer);
var
  Len: UShort;
begin
  Len := Length(Value);
  if Len > 0 then
  begin
    FStartParams  := FStartParams +
                     Char(Param) +
                     PChar(@Len)[0] +
                     PChar(@Len)[1] +
                     Value;
  end;
end;

procedure TIBControlService.ServiceStartAddParam (Value: Integer; param: Integer);
begin
  FStartParams  := FStartParams +
                   Char(Param) +
                   PChar(@Value)[0] +
                   PChar(@Value)[1] +
                   PChar(@Value)[2] +
                   PChar(@Value)[3];
end;

constructor TIBControlService.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FStartParams := '';
  FStartSPB := nil;
  FStartSPBLength := 0;
end;

procedure TIBControlService.InternalServiceStart;
begin
  FStartSPBLength := Length(FStartParams);
  if FStartSPBLength = 0 then
    IBError(ibxeStartParamsError, [nil]);
  IBAlloc(FStartSPB, 0, FStartSPBLength);
  Move(FStartParams[1], FStartSPB[0], FstartSPBLength);
  try
    if call(isc_service_start(StatusVector, @FHandle, nil,
                           FStartSPBLength, FStartSPB), False) > 0 then
    begin
      FHandle := nil;
      IBDataBaseError;
    end;
  finally
    FreeMem(FStartSPB);
    FStartSPB := nil;
    FStartSPBLength := 0;
    FStartParams := '';
  end;
  MonitorHook.ServiceStart(Self);
end;

procedure TIBControlService.ServiceStart;
begin
  CheckActive;
  SetServiceStartOptions;
  InternalServiceStart;
end;

{ TIBConfigService }

procedure TIBConfigService.ServiceStart;
begin
  IBError(ibxeUseSpecificProcedures, [nil]);
end;

procedure TIBConfigService.ActivateShadow;
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (isc_spb_prp_activate, SPBConstantValues[isc_spb_options]);
  InternalServiceStart;
end;

procedure TIBConfigService.BringDatabaseOnline;
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (isc_spb_prp_db_online, SPBConstantValues[isc_spb_options]);
  InternalServiceStart;
end;

procedure TIBConfigService.SetAsyncMode(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                        Char(isc_spb_prp_write_mode);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_wm_async)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_wm_sync);
  InternalServiceStart;
end;

procedure TIBConfigService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TIBConfigService.SetPageBuffers(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_page_buffers);
  InternalServiceStart;
end;

procedure TIBConfigService.SetReadOnly(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                         Char(isc_spb_prp_access_mode);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_am_readonly)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_am_readwrite);
  InternalServiceStart;
end;

procedure TIBConfigService.SetReserveSpace(Value: Boolean);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartParams := ServiceStartParams +
                        Char(isc_spb_prp_reserve_space);
  if Value then
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_res)
  else
    ServiceStartParams  := ServiceStartParams +
                           Char(isc_spb_prp_res_use_full);
  InternalServiceStart;
end;

procedure TIBConfigService.SetSweepInterval(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_sweep_interval);
  InternalServiceStart;
end;

procedure TIBConfigService.SetDBSqlDialect(Value: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (Value, isc_spb_prp_set_sql_dialect);
  InternalServiceStart;
end;

procedure TIBConfigService.ShutdownDatabase(Options: TShutdownMode;
  Wait: Integer);
begin
  ServiceStartParams  := Char(isc_action_svc_properties);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if (Options = Forced) then
    ServiceStartAddParam (Wait, isc_spb_prp_shutdown_db)
  else if (Options = DenyTransaction) then
    ServiceStartAddParam (Wait, isc_spb_prp_deny_new_transactions)
  else
    ServiceStartAddParam (Wait, isc_spb_prp_deny_new_attachments);
  InternalServiceStart;
end;

{ TIBStatisticalService }

procedure TIBStatisticalService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TIBStatisticalService.SetServiceStartOptions;
var
  param: Integer;
begin
  if FDatabaseName = '' then
    IBError(ibxeStartParamsError, [nil]);
  param := 0;
  if (DataPages in Options) then
    param := param or isc_spb_sts_data_pages;
  if (DbLog in Options) then
    param := param or isc_spb_sts_db_log;
  if (HeaderPages in Options) then
    param := param or isc_spb_sts_hdr_pages;
  if (IndexPages in Options) then
    param := param or isc_spb_sts_idx_pages;
  if (SystemRelations in Options) then
    param := param or isc_spb_sts_sys_relations;
  Action := isc_action_svc_db_stats;
  ServiceStartParams  := Char(isc_action_svc_db_stats);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
end;

{ TIBBackupService }
procedure TIBBackupService.SetServiceStartOptions;
var
  param, i: Integer;
  value: String;
begin
  if FDatabaseName = '' then
    IBError(ibxeStartParamsError, [nil]);
  param := 0;
  if (IgnoreChecksums in Options) then
    param := param or isc_spb_bkp_ignore_checksums;
  if (IgnoreLimbo in Options) then
    param := param or isc_spb_bkp_ignore_limbo;
  if (MetadataOnly in Options) then
    param := param or isc_spb_bkp_metadata_only;
  if (NoGarbageCollection in Options) then
    param := param or isc_spb_bkp_no_garbage_collect;
  if (OldMetadataDesc in Options) then
    param := param or isc_spb_bkp_old_descriptions;
  if (NonTransportable in Options) then
    param := param or isc_spb_bkp_non_transportable;
  if (ConvertExtTables in Options) then
    param := param or isc_spb_bkp_convert;
  Action := isc_action_svc_backup;
  ServiceStartParams  := Char(isc_action_svc_backup);
  ServiceStartAddParam(FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  ServiceStartAddParam(param, SPBConstantValues[isc_spb_options]);
  if Verbose then
    ServiceStartParams := ServiceStartParams + Char(SPBConstantValues[isc_spb_verbose]);
  if FBlockingFactor > 0 then
    ServiceStartAddParam(FBlockingFactor, isc_spb_bkp_factor);
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then
      continue;
    if (Pos('=', FBackupFile[i]) <> 0) then
    begin {mbcs ok}
      ServiceStartAddParam(FBackupFile.Names[i], isc_spb_bkp_file);
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_bkp_length);
    end
    else
      ServiceStartAddParam(FBackupFile[i], isc_spb_bkp_file);
  end;
end;

constructor TIBBackupService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFile := TStringList.Create;
end;

destructor TIBBackupService.Destroy;
begin
  FBackupFile.Free;
  inherited Destroy;
end;

procedure TIBBackupService.SetBackupFile(const Value: TStrings);
begin
  FBackupFile.Assign(Value);
end;

{ TIBRestoreService }

procedure TIBRestoreService.SetServiceStartOptions;
var
  param, i: Integer;
  value: String;
begin
  param := 0;
  if (DeactivateIndexes in Options) then
    param := param or isc_spb_res_deactivate_idx;
  if (NoShadow in Options) then
    param := param or isc_spb_res_no_shadow;
  if (NoValidityCheck in Options) then
    param := param or isc_spb_res_no_validity;
  if (OneRelationAtATime in Options) then
    param := param or isc_spb_res_one_at_a_time;
  if (Replace in Options) then
    param := param or isc_spb_res_replace;
  if (CreateNewDB in Options) then
    param := param or isc_spb_res_create;
  if (UseAllSpace in Options) then
    param := param or isc_spb_res_use_all_space;
  Action := isc_action_svc_restore;
  ServiceStartParams  := Char(isc_action_svc_restore);
  ServiceStartAddParam(param, SPBConstantValues[isc_spb_options]);
  if Verbose then ServiceStartParams := ServiceStartParams + Char(SPBConstantValues[isc_spb_verbose]);
  if FPageSize > 0 then
    ServiceStartAddParam(FPageSize, isc_spb_res_page_size);
  if FPageBuffers > 0 then
    ServiceStartAddParam(FPageBuffers, isc_spb_res_buffers);
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then continue;
    if (Pos('=', FBackupFile[i]) <> 0) then  {mbcs ok}
    begin 
      ServiceStartAddParam(FBackupFile.Names[i], isc_spb_bkp_file);
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_bkp_length);
    end
    else
      ServiceStartAddParam(FBackupFile[i], isc_spb_bkp_file);
  end;
  for i := 0 to FDatabaseName.Count - 1 do
  begin
    if (Trim(FDatabaseName[i]) = '') then continue;
    if (Pos('=', FDatabaseName[i]) <> 0) then {mbcs ok}
    begin 
      ServiceStartAddParam(FDatabaseName.Names[i], SPBConstantValues[isc_spb_dbname]);
      value := Copy(FDatabaseName[i], Pos('=', FDatabaseName[i]) + 1, Length(FDatabaseName[i])); {mbcs ok}
      param := StrToInt(value);
      ServiceStartAddParam(param, isc_spb_res_length);
    end
    else
      ServiceStartAddParam(FDatabaseName[i], SPBConstantValues[isc_spb_dbname]);
  end;
end;

constructor TIBRestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseName := TStringList.Create;
  FBackupFile := TStringList.Create;
  Include (FOptions, CreateNewDB);
end;

destructor TIBRestoreService.Destroy;
begin
  FDatabaseName.Free;
  FBackupFile.Free;
  inherited Destroy;
end;

procedure TIBRestoreService.SetBackupFile(const Value: TStrings);
begin
  FBackupFile.Assign(Value);
end;

procedure TIBRestoreService.SetDatabaseName(const Value: TStrings);
begin
  FDatabaseName.Assign(Value);
end;

{ TIBValidationService }
constructor TIBValidationService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TIBValidationService.Destroy;
var
  i : Integer;
begin
  for i := 0 to High(FLimboTransactionInfo) do
    FLimboTransactionInfo[i].Free;
  FLimboTransactionInfo := nil;
  inherited Destroy;
end;

procedure TIBValidationService.FetchLimboTransactionInfo;
var
  i, RunLen: Integer;
  Value: Char;
begin
  ServiceQueryParams := Char(isc_info_svc_limbo_trans);
  InternalServiceQuery;
  RunLen := 0;
  if (OutputBuffer[RunLen] <> Char(isc_info_svc_limbo_trans)) then
    IBError(ibxeOutputParsingError, [nil]);
  Inc(RunLen, 3);
  for i := 0 to High(FLimboTransactionInfo) do
    FLimboTransactionInfo[i].Free;
  FLimboTransactionInfo := nil;
  i := 0;
  while (OutputBuffer[RunLen] <> Char(isc_info_end)) do
  begin
    if (i >= Length(FLimboTransactionInfo)) then
      SetLength(FLimboTransactionInfo, i + 10);
    if FLimboTransactionInfo[i] = nil then
      FLimboTransactionInfo[i] := TLimboTransactionInfo.Create;
    with FLimboTransactionInfo[i] do
    begin
      if (OutputBuffer[RunLen] = Char(isc_spb_single_tra_id)) then
      begin
        Inc(RunLen);
        MultiDatabase := False;
        ID := ParseInteger(RunLen);
      end
      else
      begin
        Inc(RunLen);
        MultiDatabase := True;
        ID := ParseInteger(RunLen);
        HostSite := ParseString(RunLen);
        if (OutputBuffer[RunLen] <> Char(isc_spb_tra_state)) then
          IBError(ibxeOutputParsingError, [nil]);
        Inc(RunLen);
        Value := OutputBuffer[RunLen];
        Inc(RunLen);
        if (Value = Char(isc_spb_tra_state_limbo)) then
          State := LimboState
        else
          if (Value = Char(isc_spb_tra_state_commit)) then
            State := CommitState
          else
            if (Value = Char(isc_spb_tra_state_rollback)) then
              State := RollbackState
            else
              State := UnknownState;
        RemoteSite := ParseString(RunLen);
        RemoteDatabasePath := ParseString(RunLen);
        Value := OutputBuffer[RunLen];
        Inc(RunLen);
        if (Value = Char(isc_spb_tra_advise_commit)) then
        begin
          Advise := CommitAdvise;
          Action:= CommitAction;
        end
        else
          if (Value = Char(isc_spb_tra_advise_rollback)) then
          begin
            Advise := RollbackAdvise;
            Action := RollbackAction;
          end
          else
          begin
            { if no advice commit as default }
            Advise := UnknownAdvise;
            Action:= CommitAction;
          end;
      end;
      Inc (i);
    end;
  end;
  if (i > 0) then
    SetLength(FLimboTransactionInfo, i+1);
end;

procedure TIBValidationService.FixLimboTransactionErrors;
var
  i: Integer;
begin
  ServiceStartParams  := Char(isc_action_svc_repair);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if (FGlobalAction = NoGlobalAction) then
  begin
    i := 0;
    while (FLimboTransactionInfo[i].ID <> 0) do
    begin
      if (FLimboTransactionInfo[i].Action = CommitAction) then
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_commit_trans)
      else
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_rollback_trans);                              
      Inc(i);
    end;
  end
  else
  begin
    i := 0;
    if (FGlobalAction = CommitGlobal) then
      while (FLimboTransactionInfo[i].ID <> 0) do
      begin
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_commit_trans);
        Inc(i);
      end
    else
      while (FLimboTransactionInfo[i].ID <> 0) do
      begin
        ServiceStartAddParam (FLimboTransactionInfo[i].ID, isc_spb_rpr_rollback_trans);
        Inc(i);
      end;
  end;
  InternalServiceStart;
end;

function TIBValidationService.GetLimboTransactionInfo(index: integer): TLimboTransactionInfo;
begin
  if index <= High(FLimboTransactionInfo) then
    result := FLimboTransactionInfo[index]
  else
    result := nil;
end;

function TIBValidationService.GetLimboTransactionInfoCount: integer;
begin
  Result := High(FLimboTransactionInfo);
end;

procedure TIBValidationService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TIBValidationService.SetServiceStartOptions;
var
  param: Integer;
begin
  Action := isc_action_svc_repair;
  if FDatabaseName = '' then
    IBError(ibxeStartParamsError, [nil]);
  param := 0;
  if (SweepDB in Options) then
    param := param or isc_spb_rpr_sweep_db;
  if (ValidateDB in Options) then
    param := param or isc_spb_rpr_validate_db;
  ServiceStartParams  := Char(isc_action_svc_repair);
  ServiceStartAddParam (FDatabaseName, SPBConstantValues[isc_spb_dbname]);
  if param > 0 then
    ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
  param := 0;
  if (LimboTransactions in Options) then
    param := param or isc_spb_rpr_list_limbo_trans;
  if (CheckDB in Options) then
    param := param or isc_spb_rpr_check_db;
  if (IgnoreChecksum in Options) then
    param := param or isc_spb_rpr_ignore_checksum;
  if (KillShadows in Options) then
    param := param or isc_spb_rpr_kill_shadows;
  if (MendDB in Options) then
    param := param or isc_spb_rpr_mend_db;
  if (ValidateFull in Options) then
  begin
     param := param or isc_spb_rpr_full;
     if not (MendDB in Options) then
       param := param or isc_spb_rpr_validate_db;
  end;
  if param > 0 then
    ServiceStartAddParam (param, SPBConstantValues[isc_spb_options]);
end;

{ TIBSecurityService }
constructor TIBSecurityService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModifyParams := [];
end;

destructor TIBSecurityService.Destroy;
var
  i : Integer;
begin
  for i := 0 to High(FUserInfo) do
    FUserInfo[i].Free;
  FUserInfo := nil;
  inherited Destroy;
end;

procedure TIBSecurityService.FetchUserInfo;
var
  i, RunLen: Integer;
begin
  ServiceQueryParams := Char(isc_info_svc_get_users);
  InternalServiceQuery;
  RunLen := 0;
  if (OutputBuffer[RunLen] <> Char(isc_info_svc_get_users)) then
    IBError(ibxeOutputParsingError, [nil]);
  Inc(RunLen);
  for i := 0 to High(FUserInfo) do
    FUserInfo[i].Free;
  FUserInfo := nil;
  i := 0;
  { Don't have any use for the combined length
   so increment past by 2 }
  Inc(RunLen, 2);
  while (OutputBuffer[RunLen] <> Char(isc_info_end)) do
  begin
    if (i >= Length(FUSerInfo)) then
      SetLength(FUserInfo, i + 10);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_username)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    if FUserInfo[i] = nil then
      FUserInfo[i] := TUserInfo.Create;
    FUserInfo[i].UserName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_firstname)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].FirstName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_middlename)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].MiddleName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_lastname)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].LastName := ParseString(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_userId)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].UserId := ParseInteger(RunLen);
    if (OutputBuffer[RunLen] <> Char(isc_spb_sec_groupid)) then
      IBError(ibxeOutputParsingError, [nil]);
    Inc(RunLen);
    FUserInfo[i].GroupID := ParseInteger(RunLen);
    Inc (i);
  end;
  if (i > 0) then
    SetLength(FUserInfo, i+1);
end;

function TIBSecurityService.GetUserInfo(Index: Integer): TUserInfo;
begin
  if Index <= High(FUSerInfo) then
    result := FUserInfo[Index]
  else
    result := nil;
end;

function TIBSecurityService.GetUserInfoCount: Integer;
begin
  Result := Max(High(FUSerInfo),0);
end;

procedure TIBSecurityService.AddUser;
begin
  SecurityAction := ActionAddUser;
  ServiceStart;
end;

procedure TIBSecurityService.DeleteUser;
begin
  SecurityAction := ActionDeleteUser;
  ServiceStart;
end;

procedure TIBSecurityService.DisplayUsers;
begin
  SecurityAction := ActionDisplayUser;
  ServiceStartParams  := Char(isc_action_svc_display_user);
  InternalServiceStart;
  FetchUserInfo;
end;

procedure TIBSecurityService.DisplayUser(UserName: String);
begin
  SecurityAction := ActionDisplayUser;
  ServiceStartParams  := Char(isc_action_svc_display_user);
  ServiceStartAddParam (UserName, isc_spb_sec_username);
  InternalServiceStart;
  FetchUserInfo;
end;

procedure TIBSecurityService.ModifyUser;
begin
  SecurityAction := ActionModifyUser;
  ServiceStart;
end;

procedure TIBSecurityService.SetSecurityAction (Value: TSecurityAction);
begin
  FSecurityAction := Value;
  if Value = ActionDeleteUser then
    ClearParams;
end;

procedure TIBSecurityService.ClearParams;
begin
  FModifyParams := [];
  FFirstName := '';
  FMiddleName := '';
  FLastName := '';
  FGroupID := 0;
  FUserID := 0;
  FPassword := '';
end;

procedure TIBSecurityService.SetFirstName (Value: String);
begin
  FFirstName := Value;
  Include (FModifyParams, ModifyFirstName);
end;

procedure TIBSecurityService.SetMiddleName (Value: String);
begin
  FMiddleName := Value;
  Include (FModifyParams, ModifyMiddleName);
end;

procedure TIBSecurityService.SetLastName (Value: String);
begin
  FLastName := Value;
  Include (FModifyParams, ModifyLastName);
end;

procedure TIBSecurityService.SetPassword (Value: String);
begin
  FPassword := Value;
  Include (FModifyParams, ModifyPassword);
end;

procedure TIBSecurityService.SetUserId (Value: Integer);
begin
  FUserId := Value;
  Include (FModifyParams, ModifyUserId);
end;

procedure TIBSecurityService.SetGroupId (Value: Integer);
begin
  FGroupId := Value;
  Include (FModifyParams, ModifyGroupId);
end;

procedure TIBSecurityService.Loaded; 
begin
  inherited Loaded;
  ClearParams;
end;

procedure TIBSecurityService.SetServiceStartOptions;
var
  Len: UShort;

begin
  case FSecurityAction of
    ActionAddUser:
    begin
      Action := isc_action_svc_add_user;
      if ( Pos(' ', FUserName) > 0 ) then
        IBError(ibxeStartParamsError, [nil]);
      Len := Length(FUserName);
      if (Len = 0) then
        IBError(ibxeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_add_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
      ServiceStartAddParam (FUserID, isc_spb_sec_userid);
      ServiceStartAddParam (FGroupID, isc_spb_sec_groupid);
      ServiceStartAddParam (FPassword, isc_spb_sec_password);
      ServiceStartAddParam (FFirstName, isc_spb_sec_firstname);
      ServiceStartAddParam (FMiddleName, isc_spb_sec_middlename);
      ServiceStartAddParam (FLastName, isc_spb_sec_lastname);
    end;
    ActionDeleteUser:
    begin
      Action := isc_action_svc_delete_user;
      Len := Length(FUserName);
      if (Len = 0) then
        IBError(ibxeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_delete_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
    end;
    ActionModifyUser:
    begin
      Action := isc_action_svc_modify_user;
      Len := Length(FUserName);
      if (Len = 0) then
        IBError(ibxeStartParamsError, [nil]);
      ServiceStartParams  := Char(isc_action_svc_modify_user);
      ServiceStartAddParam (FSQLRole, SPBConstantValues[isc_spb_sql_role_name]);
      ServiceStartAddParam (FUserName, isc_spb_sec_username);
      if (ModifyUserId in FModifyParams) then
        ServiceStartAddParam (FUserID, isc_spb_sec_userid);
      if (ModifyGroupId in FModifyParams) then
        ServiceStartAddParam (FGroupID, isc_spb_sec_groupid);
      if (ModifyPassword in FModifyParams) then
        ServiceStartAddParam (FPassword, isc_spb_sec_password);
      if (ModifyFirstName in FModifyParams) then
        ServiceStartAddParam (FFirstName, isc_spb_sec_firstname);
      if (ModifyMiddleName in FModifyParams) then
        ServiceStartAddParam (FMiddleName, isc_spb_sec_middlename);
      if (ModifyLastName in FModifyParams) then
        ServiceStartAddParam (FLastName, isc_spb_sec_lastname);
    end;
  end;
  ClearParams;
end;

{ TIBUnStructuredService }
constructor TIBControlAndQueryService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEof := False;
  FAction := 0;
end;

procedure TIBControlAndQueryService.SetAction(Value: Integer);
begin
  FEof := False;
  FAction := Value;
end;


function TIBControlAndQueryService.GetNextChunk: String;
var
  Length: Integer;
begin
  if (FEof = True) then
  begin
    result := '';
    exit;
  end;
  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);
  ServiceQueryParams := Char(isc_info_svc_to_eof);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_to_eof)) then
    IBError(ibxeOutputParsingError, [nil]);
  Length := isc_vax_integer(OutputBuffer + 1, 2);
  if (OutputBuffer[3 + Length] = Char(isc_info_truncated)) then
    FEof := False
  else
    if (OutputBuffer[3 + Length] = Char(isc_info_end)) then
      FEof := True
    else
      IBError(ibxeOutputParsingError, [nil]);
  OutputBuffer[3 + Length] := #0;
  result := String(PChar(@OutputBuffer[3]));
end;

function TIBControlAndQueryService.GetNextLine: String;
var
  Length: Integer;
begin
  if (FEof = True) then
  begin
    result := '';
    exit;
  end;
  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);
  ServiceQueryParams := Char(isc_info_svc_line);
  InternalServiceQuery;
  if (OutputBuffer[0] <> Char(isc_info_svc_line)) then
    IBError(ibxeOutputParsingError, [nil]);
  Length := isc_vax_integer(OutputBuffer + 1, 2);
  if (OutputBuffer[3 + Length] <> Char(isc_info_end)) then
    IBError(ibxeOutputParsingError, [nil]);
  if (length <> 0) then
    FEof := False
  else
  begin
    result := '';
    FEof := True;
    exit;
  end;
  OutputBuffer[3 + Length] := #0;
  result := String(PChar(@OutputBuffer[3]));
end;

{ TIBLogService }

procedure TIBLogService.SetServiceStartOptions;
begin
  Action := isc_action_svc_get_ib_log;
  ServiceStartParams  := Char(isc_action_svc_get_ib_log);
end;

{ TDatabaseInfo }

constructor TDatabaseInfo.Create;
begin
  DbName := nil;
end;

destructor TDatabaseInfo.Destroy;
begin
  DbName := nil;
  inherited Destroy;
end;

{ TLicenseInfo }

constructor TLicenseInfo.Create;
begin
  Key := nil;
  Id := nil;
  Desc := nil;
end;

destructor TLicenseInfo.Destroy;
begin
  Key := nil;
  Id := nil;
  Desc := nil;
  inherited Destroy;
end;

{ TConfigFileData }

constructor TConfigFileData.Create;
begin
  ConfigFileValue := nil;
  ConfigFileKey := nil;
end;

destructor TConfigFileData.Destroy;
begin
  ConfigFileValue := nil;
  ConfigFileKey := nil;
  inherited Destroy;
end;

{ TConfigParams }

constructor TConfigParams.Create;
begin
  ConfigFileData := TConfigFileData.Create;
  ConfigFileParams := nil;
end;

destructor TConfigParams.Destroy;
begin
  ConfigFileData.Free;
  ConfigFileParams := nil;
  inherited Destroy;
end;

end.
