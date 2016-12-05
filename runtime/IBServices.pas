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
{$codepage UTF8}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, IBHeader, IB, IBExternals, CustApp, IBTypes, IBSQL;

const
  DefaultBufferSize = 32000;

  SPBPrefix = 'isc_spb_';
  isc_spb_last_spb_constant = 12;
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
    isc_spb_user_name,
    isc_spb_sys_user_name,
    isc_spb_sys_user_name_enc,
    isc_spb_password,
    isc_spb_password_enc,
    isc_spb_command_line,
    isc_spb_dbname,
    isc_spb_verbose,
    isc_spb_options,
    isc_spb_connect_timeout,
    isc_spb_dummy_packet_interval,
    isc_spb_sql_role_name
  );

type
  TOutputBufferOption = (ByLine, ByChunk);

  TIBCustomService = class;

  TLoginEvent = procedure(Database: TIBCustomService;
    LoginParams: TStrings) of object;

  { TIBCustomService }

  TIBCustomService = class(TComponent)
  private
    FParamsChanged : Boolean;
    FQueryParams: String;
    FSPB : ISPB;
    FSRB: ISRB;
    FSQPB: ISQPB;
    FTraceFlags: TTraceFlags;
    FOnLogin: TLoginEvent;
    FLoginPrompt: Boolean;
    FServerName: string;
    FService: IServiceManager;
    FStreamedActive  : Boolean;
    FOnAttach: TNotifyEvent;
    FOutputBufferOption: TOutputBufferOption;
    FProtocol: TProtocol;
    FParams: TStrings;
    FServiceQueryResults: IServiceQueryResults;
    function GetActive: Boolean;
    function GetServiceParamBySPB(const Idx: Integer): String;
    function GetSQPB: ISQPB;
    function GetSRB: ISRB;
    procedure SetActive(const Value: Boolean);
    procedure SetParams(const Value: TStrings);
    procedure SetServerName(const Value: string);
    procedure SetProtocol(const Value: TProtocol);
    procedure SetService(AValue: IServiceManager);
    procedure SetServiceParamBySPB(const Idx: Integer;
      const Value: String);
    function IndexOfSPBConst(action: byte): Integer;
    function GetSPBConstName(action: byte): string;
    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    procedure CheckServerName;
    function GenerateSPB(sl: TStrings): ISPB;

  protected
    procedure Loaded; override;
    function Login(var aServerName: string): Boolean;
    procedure CheckActive;
    procedure CheckInactive;
    procedure HandleException(Sender: TObject);
    procedure InternalServiceQuery;
    property SRB: ISRB read GetSRB;
    property SQPB: ISQPB read GetSQPB;
    property ServiceQueryResults: IServiceQueryResults read FServiceQueryResults;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    property ServiceIntf: IServiceManager read FService write SetService;
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

  { TIBControlService }

  TIBControlService = class (TIBCustomService)
  private
    function GetIsServiceRunning: Boolean;
  protected
    procedure CheckServiceNotRunning;
    procedure InternalServiceStart;
    procedure SetServiceStartOptions; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ServiceStart; virtual;
    property IsServiceRunning : Boolean read GetIsServiceRunning;
  end;

  { TIBControlAndQueryService }

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
    function WriteNextChunk(stream: TStream): integer;
    property Eof: boolean read FEof;
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

  TStatOption = (DataPages, {DbLog,} HeaderPages, IndexPages, SystemRelations);
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

  TBackupLocation = (flServerSide,flClientSide);

  { TIBBackupRestoreService }

  TIBBackupRestoreService = class(TIBControlAndQueryService)
  private
    FBackupFileLocation: TBackupLocation;
    FVerbose: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Verbose : Boolean read FVerbose write FVerbose default False;
    property BackupFileLocation: TBackupLocation read FBackupFileLocation
                                                      write FBackupFileLocation default flServerSide;
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

  { TIBRestoreService }

  TIBRestoreService = class (TIBBackupRestoreService)
  private
    FDatabaseName: TStrings;
    FBackupFile: TStrings;
    FOptions: TRestoreOptions;
    FPageSize: Integer;
    FPageBuffers: Integer;
    FSendBytes: integer;
    procedure SetBackupFile(const Value: TStrings);
    procedure SetDatabaseName(const Value: TStrings);
  protected
    procedure SetServiceStartOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SendNextChunk(stream: TStream; var line: String): integer;
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
  IBSQLMonitor, Math, FBMessages;

{ TIBBackupRestoreService }

constructor TIBBackupRestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFileLocation := flServerSide;
end;

{ TIBCustomService }

procedure TIBCustomService.Attach;
var aServerName: string;
begin
  CheckInactive;
  CheckServerName;

  aServerName := FServerName;

  if FLoginPrompt and not Login(aServerName) then
    IBError(ibxeOperationCancelled, [nil]);

  { Generate a new SPB if necessary }
  if FParamsChanged then
  begin
    FParamsChanged := False;
    FSPB := GenerateSPB(FParams);
  end;

  FService := FirebirdAPI.GetServiceManager(aServerName,FProtocol,FSPB);

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

function TIBCustomService.Login(var aServerName: string): Boolean;
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
      aServerName := ServerName;
    finally
      LoginParams.Free;
    end;
  end
  else
  if assigned(IBGUIInterface)  then
  begin
    IndexOfUser := IndexOfSPBConst(isc_spb_user_name);
    if IndexOfUser <> -1 then
      Username := Copy(Params[IndexOfUser],
                                         Pos('=', Params[IndexOfUser]) + 1, {mbcs ok}
                                         Length(Params[IndexOfUser]));
    IndexOfPassword := IndexOfSPBConst(isc_spb_password);
    if IndexOfPassword <> -1 then
      Password := Copy(Params[IndexOfPassword],
                                         Pos('=', Params[IndexOfPassword]) + 1, {mbcs ok}
                                         Length(Params[IndexOfPassword]));
    result := IBGUIInterface.ServerLoginDialog(aServerName, Username, Password);
    if result then
    begin
      IndexOfPassword := IndexOfSPBConst(isc_spb_password);
      if IndexOfUser = -1 then
        Params.Add(GetSPBConstName(isc_spb_user_name) + '=' + Username)
      else
        Params[IndexOfUser] := GetSPBConstName(isc_spb_user_name) +
                                 '=' + Username;
      if IndexOfPassword = -1 then
        Params.Add(GetSPBConstName(isc_spb_password) + '=' + Password)
      else
        Params[IndexOfPassword] := GetSPBConstName(isc_spb_password) +
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
  if FService = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TIBCustomService.CheckInactive;
begin
  if FService <> nil then
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
  FserverName := '';
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FLoginPrompt := True;
  FTraceFlags := [];
  FService := nil;
  FSRB := nil;
  FSPB := nil;
  FServiceQueryResults := nil;
  FProtocol := Local;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOwner).ConsoleApplication then
    LoginPrompt := false;
end;

destructor TIBCustomService.Destroy;
begin
  if FService <> nil then
      Detach;
  FSRB := nil;
  FSPB := nil;
  FParams.Free;
  FServiceQueryResults := nil;
  inherited Destroy;
end;

procedure TIBCustomService.Detach;
begin
  CheckActive;
  FService.Detach;
  FService := nil;
  MonitorHook.ServiceDetach(Self);
end;

function TIBCustomService.GetActive: Boolean;
begin
  result := FService <> nil;
end;

function TIBCustomService.GetServiceParamBySPB(const Idx: Integer): String;
var
  ConstIdx, EqualsIdx: Integer;
begin
  if (Idx > 0) and (Idx <= isc_spb_last_spb_constant) then
  begin
    ConstIdx := IndexOfSPBConst(Idx);
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

function TIBCustomService.GetSQPB: ISQPB;
begin
  CheckActive;
  if FSQPB = nil then
    FSQPB := FService.AllocateSQPB;
  Result := FSQPB;
end;

function TIBCustomService.GetSRB: ISRB;
begin
  CheckActive;
  if FSRB = nil then
    FSRB := FService.AllocateSRB;
  Result := FSRB;
end;

procedure TIBCustomService.InternalServiceQuery;
begin
  FServiceQueryResults := FService.Query(FSQPB,FSRB);
  FSQPB := nil;
  FSRB := nil;
  MonitorHook.ServiceQuery(Self);
end;

procedure TIBCustomService.SetActive(const Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value <> Active then
    begin
      if Value then
        Attach
      else
        Detach;
    end
   else if Value then
   begin
     FService.Detach;
     FService.Attach;
   end;
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

procedure TIBCustomService.SetService(AValue: IServiceManager);
begin
  if FService = AValue then Exit;
  FService := AValue;
  if AValue <> nil then
    FServerName := FService.getServerName;
end;

procedure TIBCustomService.SetServiceParamBySPB(const Idx: Integer;
  const Value: String);
var
  ConstIdx: Integer;
begin
  ConstIdx := IndexOfSPBConst(Idx);
  if (Value = '') then
  begin
    if ConstIdx <> -1 then
      Params.Delete(ConstIdx);
  end
  else
  begin
    if (ConstIdx = -1) then
      Params.Add(GetSPBConstName(Idx) + '=' + Value)
    else
      Params[ConstIdx] := GetSPBConstName(Idx) + '=' + Value;
  end;
end;

function TIBCustomService.IndexOfSPBConst(action: byte): Integer;
var
  i,  pos_of_str: Integer;
  st: string;
begin
  result := -1;
  st := GetSPBConstName(action);
  if st <> '' then
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

function TIBCustomService.GetSPBConstName(action: byte): string;
var i: integer;
begin
  Result := '';
  for i := Low(SPBConstantValues) to High(SPBConstantValues) do
    if SPBConstantValues[i] = action then
    begin
      Result := SPBConstantNames[i];
      break;
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

{
 * GenerateSPB -
 *  Given a string containing a textual representation
 *  of the Service parameters, generate a service
 *  parameter buffer, and return it .
}
function TIBCustomService.GenerateSPB(sl: TStrings): ISPB;
var
  i, j, SPBVal, SPBServerVal: UShort;
  param_name, param_value: String;
begin
  { The SPB is initially empty, with the exception that
   the SPB version must be the first byte of the string.
  }
  Result := FirebirdAPI.AllocateSPB;

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
    case SPBServerVal of
      isc_spb_user_name, isc_spb_password:
        Result.Add(SPBServerVal).AsString := param_value;
      else
      begin
        if GetSPBConstName(SPBServerVal) <> '' then
          IBError(ibxeSPBConstantNotSupported,
                   [GetSPBConstName(SPBServerVal)])
        else
          IBError(ibxeSPBConstantUnknown, [SPBServerVal]);
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
  i, j: Integer;

begin
  SRB.Add(isc_info_svc_get_config);
  SRB.Add(isc_info_svc_get_env);
  SRB.Add(isc_info_svc_get_env_lock);
  SRB.Add(isc_info_svc_get_env_msg);
  SRB.Add(isc_info_svc_user_dbpath);

  InternalServiceQuery;

  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_get_config:
      begin
        SetLength (FConfigParams.ConfigFileData.ConfigFileValue, Count);
        SetLength (FConfigParams.ConfigFileData.ConfigFileKey, Count);

        for j := 0 to Count - 1 do
        begin
          FConfigParams.ConfigFileData.ConfigFileKey[j] := getItemType;
          FConfigParams.ConfigFileData.ConfigFileValue[j] := AsInteger;
        end;
      end;

      isc_info_svc_get_env:
        FConfigParams.BaseLocation := AsString;

      isc_info_svc_get_env_lock:
        FConfigParams.LockFileLocation := AsString;

      isc_info_svc_get_env_msg:
        FConfigParams.MessageFileLocation := AsString;

      isc_info_svc_user_dbpath:
        FConfigParams.SecurityDatabaseLocation := AsString;

      else
        IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;

procedure TIBServerProperties.FetchDatabaseInfo;
var
  i,j: Integer;
begin
  SRB.Add(isc_info_svc_svr_db_info);
  InternalServiceQuery;

  SetLength(FDatabaseInfo.DbName,0);
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_svr_db_info:
        for j := 0 to FServiceQueryResults[i].Count - 1 do
        with FServiceQueryResults[i][j] do
        case getItemType of
        isc_spb_num_att:
          FDatabaseInfo.NoOfAttachments := AsInteger;

        isc_spb_num_db:
          FDatabaseInfo.NoOfDatabases := AsInteger;

        isc_spb_dbname:
          begin
            SetLength(FDatabaseInfo.DbName,length(FDatabaseInfo.DbName)+1);
            FDatabaseInfo.DbName[length(FDatabaseInfo.DbName)-1] := AsString;
          end;
        else
          IBError(ibxeOutputParsingError, [getItemType]);
        end;
      else
        IBError(ibxeOutputParsingError, [getItemType]);
    end;
 end;
end;

procedure TIBServerProperties.FetchLicenseInfo;
var
  i,j : Integer;
begin
  SRB.Add(isc_info_svc_get_license);
  SRB.Add(isc_info_svc_get_licensed_users);
  InternalServiceQuery;

  SetLength(FLicenseInfo.key, 0);
  SetLength(FLicenseInfo.id, 0);
  SetLength(FLicenseInfo.desc, 0);

  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_get_license:
        begin
          SetLength(FLicenseInfo.key, Count);
          SetLength(FLicenseInfo.id, Count);
          SetLength(FLicenseInfo.desc, Count);

          for j := 0 to Count -1 do
          with Items[j] do
          case getItemType of
             isc_spb_lic_id:
                FLicenseInfo.id[j] := AsString;

             isc_spb_lic_key:
                FLicenseInfo.key[j] := AsString;

             isc_spb_lic_desc:
               FLicenseInfo.desc[j] := AsString;
          else
            IBError(ibxeOutputParsingError, [getItemType]);
          end;
        end;
      else
        IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;

procedure TIBServerProperties.FetchLicenseMaskInfo();
var
  i : Integer;
begin
  SRB.Add(isc_info_svc_get_license_mask);
  SRB.Add(isc_info_svc_capabilities);
  InternalServiceQuery;

  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_get_license_mask:
        FLicenseMaskInfo.LicenseMask := AsInteger;
      isc_info_svc_capabilities:
        FLicenseMaskInfo.CapabilityMask := AsInteger;
      else
        IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;


procedure TIBServerProperties.FetchVersionInfo;
var
  i : Integer;
begin
  SRB.Add(isc_info_svc_version);
  SRB.Add(isc_info_svc_server_version);
  SRB.Add(isc_info_svc_implementation);
  InternalServiceQuery;

  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_version:
        FVersionInfo.ServiceVersion := AsInteger;
      isc_info_svc_server_version:
        FVersionInfo.ServerVersion := AsString;
      isc_info_svc_implementation:
        FVersionInfo.ServerImplementation := AsString;
      else
        IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;

{ TIBControlService }

procedure TIBControlService.SetServiceStartOptions;
begin

end;

function TIBControlService.GetIsServiceRunning: Boolean;
begin
  SRB.Add(isc_info_svc_running);
  InternalServiceQuery;

  Result := (FServiceQueryResults.Count > 0) and
             (FServiceQueryResults[0].getItemType = isc_info_svc_running) and
              (FServiceQueryResults[0].AsInteger = 1);
end;

procedure TIBControlService.CheckServiceNotRunning;
begin
  if IsServiceRunning then
    IBError(ibxeServiceRunning,[nil]);
end;

constructor TIBControlService.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FSRB := nil;
  FSPB := nil;
end;

procedure TIBControlService.InternalServiceStart;
begin
  if SRB = nil then
    IBError(ibxeStartParamsError, [nil]);

  try
    FService.Start(SRB);
  finally
    FSRB := nil;
  end;
  MonitorHook.ServiceStart(Self);
end;

procedure TIBControlService.ServiceStart;
begin
  CheckActive;
  CheckServiceNotRunning;
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
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  SRB.Add(isc_spb_options).AsInteger := isc_spb_prp_activate;
  InternalServiceStart;
end;

procedure TIBConfigService.BringDatabaseOnline;
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  SRB.Add(isc_spb_options).AsInteger := isc_spb_prp_db_online;
  InternalServiceStart;
end;

procedure TIBConfigService.SetAsyncMode(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  with SRB.Add(isc_spb_prp_write_mode) do
  if Value then
    AsInteger := isc_spb_prp_wm_async
  else
    AsInteger := isc_spb_prp_wm_sync;
  InternalServiceStart;
end;

procedure TIBConfigService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TIBConfigService.SetPageBuffers(Value: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  SRB.Add(isc_spb_prp_page_buffers).AsInteger := Value;
  InternalServiceStart;
end;

procedure TIBConfigService.SetReadOnly(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  with SRB.Add(isc_spb_prp_access_mode) do
  if Value then
    AsInteger := isc_spb_prp_am_readonly
  else
    AsInteger := isc_spb_prp_am_readwrite;
  InternalServiceStart;
end;

procedure TIBConfigService.SetReserveSpace(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  with SRB.Add(isc_spb_prp_reserve_space) do
  if Value then
    AsInteger := isc_spb_prp_res
  else
    AsInteger := isc_spb_prp_res_use_full;
  InternalServiceStart;
end;

procedure TIBConfigService.SetSweepInterval(Value: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  SRB.Add(isc_spb_prp_sweep_interval).AsInteger := Value;
  InternalServiceStart;
end;

procedure TIBConfigService.SetDBSqlDialect(Value: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  SRB.Add(isc_spb_prp_set_sql_dialect).AsInteger := Value;
  InternalServiceStart;
end;

procedure TIBConfigService.ShutdownDatabase(Options: TShutdownMode;
  Wait: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  SRB.Add(isc_spb_dbname).AsString :=  FDatabaseName;
  if (Options = Forced) then
  SRB.Add(isc_spb_prp_shutdown_db).AsInteger := Wait
  else if (Options = DenyTransaction) then
    SRB.Add(isc_spb_prp_deny_new_transactions).AsInteger := Wait
  else
    SRB.Add(isc_spb_prp_deny_new_attachments).AsInteger := Wait;
  InternalServiceStart;
end;


{ TIBStatisticalService }

procedure TIBStatisticalService.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TIBStatisticalService.SetServiceStartOptions;
var param: integer;
begin
  if FDatabaseName = '' then
    IBError(ibxeStartParamsError, [nil]);

  param := 0;
  if (DataPages in Options) then
    param := param or isc_spb_sts_data_pages;
{  if (DbLog in Options) then
    param := param or isc_spb_sts_db_log; -- removed from Firebird 2}
  if (HeaderPages in Options) then
    param := param or isc_spb_sts_hdr_pages;
  if (IndexPages in Options) then
    param := param or isc_spb_sts_idx_pages;
  if (SystemRelations in Options) then
    param := param or isc_spb_sts_sys_relations;
  Action := isc_action_svc_db_stats;
  SRB.Add(isc_action_svc_db_stats);
  SRB.Add(isc_spb_dbname).AsString := FDatabaseName;
  SRB.Add(isc_spb_options).AsInteger := param;
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
  SRB.Add(isc_action_svc_backup);
  SRB.Add(isc_spb_dbname).AsString := FDatabaseName;
  SRB.Add(isc_spb_options).AsInteger := param;
  if Verbose  and (BackupFileLocation = flServerSide) then
    SRB.Add(isc_spb_verbose);
  if FBlockingFactor > 0 then
    SRB.Add(isc_spb_bkp_factor).AsInteger := FBlockingFactor;
  if BackupFileLocation = flServerSide then
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then
      continue;
    if (Pos('=', FBackupFile[i]) <> 0) then
    begin {mbcs ok}
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile.Names[i];
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      SRB.Add(isc_spb_bkp_length).AsInteger := StrToInt(value);;
    end
    else
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile[i];
  end
  else
  SRB.Add(isc_spb_bkp_file).AsString := 'stdout';
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
  SRB.Add(isc_action_svc_restore);
  SRB.Add(isc_spb_options).AsInteger := param;
  if Verbose then
    SRB.Add(isc_spb_verbose);
  if FPageSize > 0 then
    SRB.Add(isc_spb_res_page_size).AsInteger := FPageSize;
  if FPageBuffers > 0 then
    SRB.Add(isc_spb_res_buffers).AsInteger := FPageBuffers;
  if BackupFileLocation = flServerSide then
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then continue;
    if (Pos('=', FBackupFile[i]) <> 0) then  {mbcs ok}
    begin 
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile.Names[i];
      value := Copy(FBackupFile[i], Pos('=', FBackupFile[i]) + 1, Length(FBackupFile.Names[i])); {mbcs ok}
      SRB.Add(isc_spb_bkp_length).AsInteger := StrToInt(value);;
    end
    else
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile[i];
  end
  else
    SRB.Add(isc_spb_bkp_file).AsString := 'stdin';

  for i := 0 to FDatabaseName.Count - 1 do
  begin
    if (Trim(FDatabaseName[i]) = '') then continue;
    if (Pos('=', FDatabaseName[i]) <> 0) then {mbcs ok}
    begin
      SRB.Add(isc_spb_dbname).AsString := FDatabaseName.Names[i];
      value := Copy(FDatabaseName[i], Pos('=', FDatabaseName[i]) + 1, Length(FDatabaseName[i])); {mbcs ok}
      SRB.Add(isc_spb_res_length).AsInteger :=  StrToInt(value);
    end
    else
      SRB.Add(isc_spb_dbname).AsString := FDatabaseName[i];
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

function TIBRestoreService.SendNextChunk(stream: TStream; var line: String
  ): integer;
var
  i: Integer;
begin
  Result := 0;
  line := '';
  if (FEof = True) then
    exit;

  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);

  SRB.Add(isc_info_svc_line);
  SRB.Add(isc_info_svc_stdin);

  SQPB.Add(isc_info_svc_timeout).AsInteger := 1;
  if FSendBytes > 0 then
    Result := SQPB.Add(isc_info_svc_line).CopyFrom(stream,FSendBytes);
  InternalServiceQuery;

  FSendBytes := 0;
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_line:
         line := AsString;

      isc_info_svc_stdin:
        FSendBytes := AsInteger;

      isc_info_svc_timeout,
      isc_info_data_not_ready:
        {ignore};
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
  FEOF := (FSendBytes = 0) and (line = '');
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

  procedure NextLimboTransaction(index: integer);
  begin
    SetLength(FLimboTransactionInfo, index+1);
    FLimboTransactionInfo[index] := TLimboTransactionInfo.Create;
    { if no advice commit as default }
    FLimboTransactionInfo[index].Advise := UnknownAdvise;
    FLimboTransactionInfo[index].Action:= CommitAction;
  end;

var
  i,j, k: Integer;
  Value: Char;
begin
  for i := 0 to High(FLimboTransactionInfo) do
    FLimboTransactionInfo[i].Free;
  SetLength(FLimboTransactionInfo,0);

  SRB.Add(isc_info_svc_limbo_trans);
  InternalServiceQuery;

  k := -1;
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  case getItemType of
  isc_info_svc_limbo_trans:
    begin
      if FServiceQueryResults[i].Count = 0 then continue;
      NextLimboTransaction(0);
      for j := 0 to FServiceQueryResults[i].Count - 1 do
      begin
        with FServiceQueryResults[i][j] do
        begin
          case getItemType of
            isc_spb_single_tra_id:
            begin
              Inc(k);
              if k > 0 then
                NextLimboTransaction(k);
              FLimboTransactionInfo[k].MultiDatabase := False;
              FLimboTransactionInfo[k].ID := AsInteger;
            end;

            isc_spb_multi_tra_id:
            begin
              Inc(k);
              if k > 0 then
                NextLimboTransaction(k);
              FLimboTransactionInfo[k].MultiDatabase := True;
              FLimboTransactionInfo[k].ID := AsInteger;
            end;

            isc_spb_tra_host_site:
              FLimboTransactionInfo[k].HostSite := AsString;

            isc_spb_tra_state:
              case AsByte of
                isc_spb_tra_state_limbo:
                  FLimboTransactionInfo[k].State := LimboState;

                isc_spb_tra_state_commit:
                  FLimboTransactionInfo[k].State := CommitState;

                isc_spb_tra_state_rollback:
                  FLimboTransactionInfo[k].State := RollbackState;

                else
                  FLimboTransactionInfo[k].State := UnknownState;
              end;

            isc_spb_tra_remote_site:
              FLimboTransactionInfo[k].RemoteSite := AsString;

            isc_spb_tra_db_path:
              FLimboTransactionInfo[k].RemoteDatabasePath := AsString;

            isc_spb_tra_advise:
            with FLimboTransactionInfo[k] do
            begin
              case (AsByte) of
              isc_spb_tra_advise_commit:
              begin
                Advise := CommitAdvise;
                Action:= CommitAction;
              end;

              isc_spb_tra_advise_rollback:
              begin
                Advise := RollbackAdvise;
                Action := RollbackAction;
              end;

              else
                Advise := UnknownAdvise;
              end;
            end;

            else
              IBError(ibxeOutputParsingError, [getItemType]);
          end;
        end;
      end;
    end;
  else
    IBError(ibxeOutputParsingError, [getItemType]);
  end;
end;

procedure TIBValidationService.FixLimboTransactionErrors;
var
  i: Integer;
begin
  SRB.Add(isc_action_svc_repair);
  SRB.Add(isc_spb_dbname).AsString := FDatabaseName;
  case FGlobalAction of
  NoGlobalAction:
    begin
      for i := 0 to LimboTransactionInfoCount - 1 do
      begin
        if (FLimboTransactionInfo[i].Action = CommitAction) then
          SRB.Add(isc_spb_rpr_commit_trans).AsInteger :=  FLimboTransactionInfo[i].ID
        else
          SRB.Add(isc_spb_rpr_rollback_trans).AsInteger :=  FLimboTransactionInfo[i].ID;
      end;
    end;

  CommitGlobal:
    begin
      for i := 0 to LimboTransactionInfoCount - 1 do
        SRB.Add(isc_spb_rpr_commit_trans).AsInteger :=  FLimboTransactionInfo[i].ID;
    end;

    RollbackGlobal:
      begin
        for i := 0 to LimboTransactionInfoCount - 1 do
          SRB.Add(isc_spb_rpr_rollback_trans).AsInteger :=  FLimboTransactionInfo[i].ID;
      end;

    RecoverTwoPhaseGlobal:
    begin
      for i := 0 to LimboTransactionInfoCount - 1 do
        SRB.Add(isc_spb_rpr_recover_two_phase).AsInteger :=  FLimboTransactionInfo[i].ID;
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
  Result := Length(FLimboTransactionInfo);
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
  SRB.Add(isc_action_svc_repair);
  SRB.Add(isc_spb_dbname).AsString := FDatabaseName;
  param := 0;
  if (SweepDB in Options) then
    param := param or isc_spb_rpr_sweep_db;
  if (ValidateDB in Options) then
    param := param or isc_spb_rpr_validate_db;

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
   SRB.Add(isc_spb_options).AsInteger := param;
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
  i, j, k: Integer;
begin
  SRB.Add(isc_info_svc_get_users);
  InternalServiceQuery;

  for i := 0 to High(FUserInfo) do
    FUserInfo[i].Free;
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
    isc_info_svc_get_users:
      begin
        SetLength(FUserInfo,1);
        k := 0;
        FUserInfo[0] := TUserInfo.Create;
        FUserInfo[0].UserName := '';
        for j := 0 to FServiceQueryResults[i].Count - 1 do
        begin
          with FServiceQueryResults[i][j] do
          case getItemType of
          isc_spb_sec_username:
            begin
              if FUserInfo[k].UserName <> '' then
              begin
                Inc(k);
                SetLength(FUserInfo,k+1);
                if FUserInfo[k] = nil then
                  FUserInfo[k] := TUserInfo.Create;
              end;
              FUserInfo[k].UserName := AsString;
            end;

          isc_spb_sec_firstname:
            FUserInfo[k].FirstName := AsString;

          isc_spb_sec_middlename:
            FUserInfo[k].MiddleName := AsString;

          isc_spb_sec_lastname:
            FUserInfo[k].LastName := AsString;

          isc_spb_sec_userId:
            FUserInfo[k].UserId := AsInteger;

          isc_spb_sec_groupid:
            FUserInfo[k].GroupID := AsInteger;

          else
            IBError(ibxeOutputParsingError, [getItemType]);
          end;
        end;
      end;
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
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
  Result := Length(FUserInfo);
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
  SRB.Add(isc_action_svc_display_user);
  InternalServiceStart;
  FetchUserInfo;
end;

procedure TIBSecurityService.DisplayUser(UserName: String);
begin
  SecurityAction := ActionDisplayUser;
  SRB.Add(isc_action_svc_display_user);
  SRB.Add(isc_spb_sec_username).AsString := UserName;
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
      SRB.Add(isc_action_svc_add_user);
      SRB.Add(isc_spb_sec_username).AsString := FUserName;
      SRB.Add(isc_spb_sql_role_name).AsString := FSQLRole;
      SRB.Add(isc_spb_sec_userid).AsInteger := FUserID;
      SRB.Add(isc_spb_sec_groupid).AsInteger := FGroupID;
      SRB.Add(isc_spb_sec_password).AsString := FPassword;
      SRB.Add(isc_spb_sec_firstname).AsString := FFirstName;
      SRB.Add(isc_spb_sec_middlename).AsString := FMiddleName;
      SRB.Add(isc_spb_sec_lastname).AsString := FLastName;
    end;
    ActionDeleteUser:
    begin
      Action := isc_action_svc_delete_user;
      Len := Length(FUserName);
      if (Len = 0) then
        IBError(ibxeStartParamsError, [nil]);
      SRB.Add(isc_action_svc_delete_user);
      SRB.Add(isc_spb_sec_username).AsString := FUserName;
      SRB.Add(isc_spb_sql_role_name).AsString := FSQLRole;
    end;
    ActionModifyUser:
    begin
      Action := isc_action_svc_modify_user;
      Len := Length(FUserName);
      if (Len = 0) then
        IBError(ibxeStartParamsError, [nil]);
      SRB.Add(isc_action_svc_modify_user);
      SRB.Add(isc_spb_sec_username).AsString := FUserName;
      SRB.Add(isc_spb_sql_role_name).AsString := FSQLRole;
      if (ModifyUserId in FModifyParams) then
        SRB.Add(isc_spb_sec_userid).AsInteger := FUserID;
      if (ModifyGroupId in FModifyParams) then
        SRB.Add(isc_spb_sec_groupid).AsInteger := FGroupID;
      if (ModifyPassword in FModifyParams) then
        SRB.Add(isc_spb_sec_password).AsString := FPassword;
      if (ModifyFirstName in FModifyParams) then
        SRB.Add(isc_spb_sec_firstname).AsString := FFirstName;
      if (ModifyMiddleName in FModifyParams) then
        SRB.Add(isc_spb_sec_middlename).AsString := FMiddleName;
      if (ModifyLastName in FModifyParams) then
        SRB.Add(isc_spb_sec_lastname).AsString := FLastName;
    end;
  end;
  ClearParams;
end;

{ TIBUnStructuredService }
constructor TIBControlAndQueryService.create(AOwner: TComponent);
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
  i: Integer;
begin
  if (FEof = True) then
  begin
    result := '';
    exit;
  end;
  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);

  SRB.Add(isc_info_svc_to_eof);
  InternalServiceQuery;

  FEof := True;
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_to_eof:
        Result := AsString;

      isc_info_truncated:
        FEof := False;
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;

function TIBControlAndQueryService.WriteNextChunk(stream: TStream): integer;
var
  i: Integer;
begin
  result := 0;
  if (FEof = True) then
    exit;
  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);

  SQPB.Add(isc_info_svc_timeout).AsInteger := 1;
  SRB.Add(isc_info_svc_to_eof);
  InternalServiceQuery;

  FEof := True;
  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_to_eof:
      begin
        Result := CopyTo(stream,0);
        FEof := Result = 0;
      end;

      isc_info_truncated:
        FEof := False;

      isc_info_svc_timeout:
        {ignore};
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
end;

function TIBControlAndQueryService.GetNextLine: String;
var
  i: Integer;
begin
  Result := '';
  if (FEof = True) then
    exit;

  if (FAction = 0) then
    IBError(ibxeQueryParamsError, [nil]);

  SRB.Add(isc_info_svc_line);
  InternalServiceQuery;

  for i := 0 to FServiceQueryResults.Count - 1 do
  with FServiceQueryResults[i] do
  begin
    case getItemType of
      isc_info_svc_line:
         Result := AsString;
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
  FEOF := Result = '';
end;

{ TIBLogService }

procedure TIBLogService.SetServiceStartOptions;
begin
  Action := isc_action_svc_get_ib_log;
  SRB.Add(isc_action_svc_get_ib_log);
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
