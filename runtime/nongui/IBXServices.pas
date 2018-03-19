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
{    Associates Ltd 2011 - 2018                                                }
{                                                                        }
{************************************************************************}

unit IBXServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, IB, IBDatabase, IBTypes, IBSQLMonitor, IBExternals, memds;

type
  TIBXCustomService = class;
  TIBXControlService = class;
  TIBXServicesConnection = class;

  IIBXServicesClient = interface
    procedure OnAfterConnect(Sender: TIBXServicesConnection; aDatabaseName: string);
    procedure OnBeforeDisconnect(Sender: TIBXServicesConnection);
  end;

  TSecContextAction = (scRaiseError, scReconnect);

  TIBXServicesLoginEvent = procedure(Service: TIBXServicesConnection; var aServerName: string; LoginParams: TStrings) of object;
  TIBXServicesSecContextEvent = procedure(Service: TIBXServicesConnection; var aAction: TSecContextAction) of object;

  { TIBXServicesConnection }

  TIBXServicesConnection = class(TIBXMonitoredConnection)
  private
    FDatabase: TIBDatabase;
    FConnectString: string;
    FOnSecurityContextException: TIBXServicesSecContextEvent;
    FParams: TStrings;
    FIBXServices: array of IIBXServicesClient;
    FOnLogin: TIBXServicesLoginEvent;
    FService: IServiceManager;
    FPortNo: string;
    FServerName: string;
    FProtocol: TProtocol;
    FServerVersionNo: array [1..4] of integer;
    FExpectedDB: string;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckServerName;
    function GenerateSPB(sl: TStrings): ISPB;
    function GetServerVersionNo(index: integer): integer;
    function GetSPBConstName(action: byte): string;
    procedure HandleException(Sender: TObject);
    procedure HandleSecContextException(Sender: TIBXControlService; var action: TSecContextAction);
    function Login(var aServerName: string; LoginParams: TStrings): Boolean;
    procedure ParamsChanging(Sender: TObject);
    procedure SetConnectString(AValue: string);
    procedure SetParams(AValue: TStrings);
    procedure SetPortNo(AValue: string);
    procedure SetProtocol(AValue: TProtocol);
    procedure SetServerName(AValue: string);
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function GetDataset(Index : longint) : TDataset; override;
    function GetDataSetCount : Longint; override;
    procedure ReadState(Reader: TReader); override;
    procedure RegisterIntf(intf: IIBXServicesClient);
    procedure UnRegisterIntf(intf: IIBXServicesClient);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConnectUsing(aDatabase: TIBDatabase);
    {Copies database parameters as give in the DBParams to the Services connection
      omitting any parameters not appropriate for Services API. Typically, the
      DBParams are TIBDatabase.Params}
    procedure SetDBParams(DBParams: TStrings);
    property ServerVersionNo[index: integer]: integer read GetServerVersionNo;
    property ServiceIntf: IServiceManager read FService;
  published
    property Connected;
    property ConnectString: string read FConnectString write SetConnectString;
    property LoginPrompt default True;
    property Protocol: TProtocol read FProtocol write SetProtocol default Local;
    property PortNo: string read FPortNo write SetPortNo;
    property Params: TStrings read FParams write SetParams;
    property ServerName: string read FServerName write SetServerName;
    property TraceFlags;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnLogin: TIBXServicesLoginEvent read FOnLogin write FOnLogin;
    property OnSecurityContextException: TIBXServicesSecContextEvent read FOnSecurityContextException
                                         write FOnSecurityContextException;
 end;

 { TIBXCustomService }

 TIBXCustomService = class(TIBXMonitoredService,IIBXServicesClient)
 private
   FSRB: ISRB;
   FSQPB: ISQPB;
   FServiceQueryResults: IServiceQueryResults;
   FServicesConnection: TIBXServicesConnection;
   procedure CheckActive;
   function GetSQPB: ISQPB;
   function GetSRB: ISRB;
   procedure SetServicesConnection(AValue: TIBXServicesConnection);
 protected
   procedure Clear; virtual;
   procedure OnAfterConnect(Sender: TIBXServicesConnection; aDatabaseName: string); virtual;
   procedure OnBeforeDisconnect(Sender: TIBXServicesConnection); virtual;
   procedure InternalServiceQuery(RaiseExceptionOnError: boolean=true);
   procedure DoServiceQuery; virtual;
   procedure Notification( AComponent: TComponent; Operation: TOperation); override;
   property SRB: ISRB read GetSRB;
   property SQPB: ISQPB read GetSQPB;
   property ServiceQueryResults: IServiceQueryResults read FServiceQueryResults;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Assign(Source: TPersistent); override;
 published
   property ServicesConnection: TIBXServicesConnection read FServicesConnection
     write SetServicesConnection;
   property TraceFlags;
end;

 { TDatabaseInfo }

 TDatabaseInfo = class
 public
   NoOfAttachments: Integer;
   NoOfDatabases: Integer;
   DbName: array of string;
   constructor Create;
   destructor Destroy; override;
 end;

 { TConfigFileData }

 TConfigFileData = class
 public
   ConfigFileValue: array of integer;
   ConfigFileKey: array of integer;
   constructor Create;
   destructor Destroy; override;
 end;

 { TConfigParams }

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

 { TIBXServerProperties }

 TIBXServerProperties = class(TIBXCustomService)
 private
   FDatabaseInfo: TDatabaseInfo;
   FVersionInfo: TVersionInfo;
   FConfigParams: TConfigParams;
   function GetConfigParams: TConfigParams;
   function GetDatabaseInfo: TDatabaseInfo;
   function GetVersionInfo: TVersionInfo;
 protected
   procedure Clear; override;
 public
   property DatabaseInfo: TDatabaseInfo read GetDatabaseInfo;
   property VersionInfo: TVersionInfo read GetVersionInfo;
   property ConfigParams: TConfigParams read GetConfigParams;
 end;

 { TIBXControlService }

 TIBXControlService = class(TIBXCustomService)
 private
   FDatabaseName: string;
   FAction: TSecContextAction;
   FLastStartSRB: ISRB;
   function GetIsServiceRunning: Boolean;
   procedure HandleSecContextErr;
   procedure CallSecContextException;
   procedure SetDatabaseName(AValue: string);
 protected
   procedure DatabaseNameChanged; virtual;
   procedure OnAfterConnect(Sender: TIBXServicesConnection; aDatabaseName: string); override;
   procedure AddDBNameToSRB;
   procedure CheckServiceNotRunning;
   procedure InternalServiceStart;
   procedure DoServiceQuery; override;
   procedure SetServiceStartOptions; virtual;
   procedure ServiceStart; virtual;
   property DatabaseName: string read FDatabaseName write SetDatabaseName;
 public
   procedure Assign(Source: TPersistent); override;
   property IsServiceRunning : Boolean read GetIsServiceRunning;
 end;

 TIBXOnGetNextLine = procedure(Sender: TObject; var Line: string) of object;

 { TIBXControlAndQueryService }

 TIBXControlAndQueryService = class (TIBXControlService)
 private
   FEof: Boolean;
   FSendBytes: integer;
   FOnGetNextLine: TIBXOnGetNextLine;
   FServiceStarted: boolean;
   FDataSets: TList;
 protected
   function GetNextLine : String;
   function GetNextChunk : String;
   procedure ServiceStart; override;
   function ReceiveNextChunk(stream: TStream): integer;
   function SendNextChunk(stream: TStream; var line: String): integer;
   procedure DoOnGetNextLine(Line: string);
   procedure OnBeforeDisconnect(Sender: TIBXServicesConnection); override;
   procedure Notification( AComponent: TComponent; Operation: TOperation); override;
   procedure RegisterDataSet(aDataSet: TDataSet);
   procedure UnRegisterDataSet(aDataSet: TDataSet);
 public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   procedure Execute(OutputLog: TStrings); virtual;
   property Eof: boolean read FEof;
 published
   property OnGetNextLine: TIBXOnGetNextLine read FOnGetNextLine write FOnGetNextLine;
 end;

 { TIBXLogService }

 TIBXLogService = class(TIBXControlAndQueryService)
 protected
   procedure SetServiceStartOptions; override;
 end;

 TDBShutdownMode = (Forced, DenyTransaction, DenyAttachment);

 { TIBXConfigService }

 TIBXConfigService = class(TIBXControlService)
 public
   procedure ShutdownDatabase (Options: TDBShutdownMode; Wait: Integer);
   procedure SetSweepInterval (Value: Integer);
   procedure SetDBSqlDialect (Value: Integer);
   procedure SetPageBuffers (Value: Integer);
   procedure ActivateShadow;
   procedure BringDatabaseOnline;
   procedure SetReserveSpace (Value: Boolean);
   procedure SetAsyncMode (Value: Boolean);
   procedure SetReadOnly (Value: Boolean);
   procedure SetNoLinger;
 published
   property DatabaseName;
 end;

 TStatOption = (DataPages, HeaderPages, IndexPages, SystemRelations);
 TStatOptions = set of TStatOption;

 { TIBXStatisticalService }

 TIBXStatisticalService = class(TIBXControlAndQueryService)
 private
   FOptions: TStatOptions;
 protected
   procedure SetServiceStartOptions; override;
 published
   property DatabaseName;
   property Options: TStatOptions read FOptions write FOptions;
 end;

 TBackupStatsOption = (bsTotalTime,bsTimeDelta,bsPageReads,bsPageWrites);
 TBackupStatsOptions = set of TBackupStatsOption;

 { TIBXBackupRestoreService }

 TIBXBackupRestoreService = class(TIBXControlAndQueryService)
 private
   FStatisticsRequested: TBackupStatsOptions;
   FVerbose: Boolean;
 protected
   procedure SetServiceStartOptions; override;
   property Verbose : Boolean read FVerbose write FVerbose default False;
   property StatisticsRequested: TBackupStatsOptions read FStatisticsRequested write FStatisticsRequested;
 published
 end;

 TBackupOption = (IgnoreChecksums, IgnoreLimbo, MetadataOnly, NoGarbageCollection,
   OldMetadataDesc, NonTransportable, ConvertExtTables, NoDBTriggers);
 TBackupOptions = set of TBackupOption;

 { TIBXBackupService }

 TIBXBackupService = class (TIBXBackupRestoreService)
 private
   FOptions: TBackupOptions;
   FBlockingFactor: Integer;
 protected
   procedure SetServiceStartOptions; override;
   procedure SetBackupTarget; virtual; abstract;
 published
   property BlockingFactor: Integer read FBlockingFactor write FBlockingFactor;
   property DatabaseName;
   property Options : TBackupOptions read FOptions write FOptions;
 end;

 { TIBXClientSideBackupService }

 TIBXClientSideBackupService = class(TIBXBackupService)
 protected
   procedure Execute(OutputLog: TStrings); override;
   procedure SetBackupTarget; override;
 public
   procedure BackupToStream(S: TStream; var BytesWritten: integer);
   procedure BackupToFile(aFileName: string; var BytesWritten: integer);
 end;

 { TIBXServerSideBackupService }

 TIBXServerSideBackupService = class(TIBXBackupService)
 private
   FBackupFiles: TStrings;
   procedure SetBackupFile(const Value: TStrings);
 protected
   procedure SetBackupTarget; override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   {Use inherited Execute method to perform backup}
 published
   { a name=value pair of filename and length }
   property BackupFiles: TStrings read FBackupFiles write SetBackupFile;
   property StatisticsRequested;
   property Verbose;
 end;

 TRestoreOption = (DeactivateIndexes, NoShadow, NoValidityCheck, OneRelationAtATime,
   Replace, CreateNewDB, UseAllSpace, RestoreMetaDataOnly);

 TRestoreOptions = set of TRestoreOption;

 { TIBXRestoreService }

 TIBXRestoreService = class (TIBXBackupRestoreService)
 private
   FDatabaseFiles: TStrings;
   FOptions: TRestoreOptions;
   FPageSize: Integer;
   FPageBuffers: Integer;
   procedure SetDatabaseFiles(const Value: TStrings);
 protected
   procedure DatabaseNameChanged; override;
   procedure SetServiceStartOptions; override;
   procedure SetArchiveSource; virtual; abstract;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
 published
   { a name=value pair of filename and length }
   property DatabaseFiles: TStrings read FDatabaseFiles write SetDatabaseFiles;
   property PageSize: Integer read FPageSize write FPageSize;
   property PageBuffers: Integer read FPageBuffers write FPageBuffers;
   property Options : TRestoreOptions read FOptions write FOptions default [CreateNewDB];
   property StatisticsRequested;
   property Verbose;
 end;

 { TIBXClientSideRestoreService }

 TIBXClientSideRestoreService = class(TIBXRestoreService)
 protected
   procedure Execute(OutputLog: TStrings); override;
   procedure SetArchiveSource; override;
 public
   procedure RestoreFromStream(S: TStream; OutputLog: TStrings);
   procedure RestoreFromFile(aFileName: string; OutputLog: TStrings);
   procedure RestoreFromFiles(FileList: TStrings; OutputLog: TStrings);
 end;

 { TIBXServerSideRestoreService }

 TIBXServerSideRestoreService = class(TIBXRestoreService)
 private
   FBackupFiles: TStrings;
   procedure SetBackupFiles(const Value: TStrings);
 protected
   procedure SetArchiveSource; override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   {use inherited Execute method to perform restore}
 published
   property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
 end;

  { TIBXOnlineValidationService }

  TIBXOnlineValidationService = class(TIBXControlAndQueryService)
  private
    FExcludeIndexes: string;
    FExcludeTables: string;
    FIncludeIndexes: string;
    FIncludeTables: string;
    FLockTimeout: integer;
  protected
    procedure SetServiceStartOptions; override;
    procedure ServiceStart; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property IncludeTables: string read FIncludeTables write FIncludeTables;
    property ExcludeTables: string read FExcludeTables write FExcludeTables;
    property IncludeIndexes: string read FIncludeIndexes write FIncludeIndexes;
    property ExcludeIndexes: string read FExcludeIndexes write FExcludeIndexes;
    property LockTimeout: integer read FLockTimeout write FLockTimeout default 10;
    property DatabaseName;
  end;

  TValidateOption = (CheckDB, IgnoreChecksum, KillShadows, MendDB,
    SweepDB, ValidateDB, ValidateFull);
  TValidateOptions = set of TValidateOption;

  { TIBXValidationService }

  TIBXValidationService = class(TIBXControlAndQueryService)
  private
    FOptions: TValidateOptions;
  protected
    procedure SetServiceStartOptions; override;
  public
    {use inherited Execute method to perform validation}
  published
    property DatabaseName;
    property Options: TValidateOptions read FOptions write FOptions;
  end;

  TUserInfo = class
  public
    UserName: string;
    FirstName: string;
    MiddleName: string;
    LastName: string;
    GroupID: Integer;
    UserID: Integer;
    AdminRole: boolean;
  end;

  TSecurityAction = (ActionAddUser, ActionDeleteUser, ActionModifyUser, ActionDisplayUser);
  TSecurityModifyParam = (ModifyFirstName, ModifyMiddleName, ModifyLastName, ModifyUserId,
                         ModifyGroupId, ModifyPassword, ModifyAdminRole);
  TSecurityModifyParams = set of TSecurityModifyParam;

  { TIBXSecurityService }

  TIBXSecurityService = class(TIBXControlAndQueryService)
  private
    FAdminRole: boolean;
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
    procedure SetAdminRole(AValue: boolean);
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
    procedure Execute(OutputLog: TStrings); override;
    procedure Loaded; override;
    procedure SetServiceStartOptions; override;
    property SecurityAction: TSecurityAction read FSecurityAction
                                             write SetSecurityAction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisplayUsers;
    procedure DisplayUser(aUserName: string);
    procedure AddUser;
    procedure DeleteUser;
    procedure ModifyUser;
    function HasAdminRole: boolean;
    procedure SetAutoAdmin(Value: Boolean);
    property UserInfo[Index: Integer]: TUserInfo read GetUserInfo;
    property UserInfoCount: Integer read GetUserInfoCount;

  published
    property SQlRole : string read FSQLRole write FSQLrole;
    property UserName : string read FUserName write FUserName;
    property FirstName : string read FFirstName write SetFirstName;
    property MiddleName : string read FMiddleName write SetMiddleName;
    property LastName : string read FLastName write SetLastName;
    property UserID : Integer read FUserID write SetUserID;
    property GroupID : Integer read FGroupID write SetGroupID;
    property Password : string read FPassword write setPassword;
    property AdminRole: boolean read FAdminRole write SetAdminRole;
  end;

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

 { TIBXLimboTransactionResolutionService }

  TIBXLimboTransactionResolutionService = class(TIBXControlAndQueryService)
  private
    FLimboTransactionInfo: array of TLimboTransactionInfo;
    FGlobalAction: TTransactionGlobalAction;
    function GetLimboTransactionInfo(index: integer): TLimboTransactionInfo;
    function GetLimboTransactionInfoCount: integer;
    function FetchLimboTransactionInfo: integer;

  protected
    procedure SetServiceStartOptions; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Execute(OutputLog: TStrings); override;
    property LimboTransactionInfo[Index: integer]: TLimboTransactionInfo read GetLimboTransactionInfo;
    property LimboTransactionInfoCount: Integer read GetLimboTransactionInfoCount;

  published
    property DatabaseName;
    property GlobalAction: TTransactionGlobalAction read FGlobalAction
                                         write FGlobalAction;

  end;

  TRequiredSources = class of TIBXControlAndQueryService;

  { TIBXServicesDataSet }

  TIBXServicesDataSet = class(TMemDataSet)
  private
    FSource: TIBXControlAndQueryService;
    procedure SetSource(AValue: TIBXControlAndQueryService);
  protected
    FRequiredSource: TRequiredSources;
    procedure DoBeforeClose; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  published
    property Source: TIBXControlAndQueryService read FSource write SetSource;
  end;

  { TIBXServicesUserList }

  TIBXServicesUserList = class(TIBXServicesDataSet)
  private
    FLoading: boolean;
  protected
    procedure DoBeforePost; override;
    procedure DoAfterInsert; override;
    procedure DoAfterPost; override;
    procedure DoAfterOpen; override;
    procedure InternalDelete; override;
  public
    constructor Create(AOwner:TComponent); override;
  end;

  { TIBXServicesLimboTransactionsList }

  TIBXServicesLimboTransactionsList = class(TIBXServicesDataSet)
  private
    FLoading: boolean;
  protected
    procedure DoBeforeInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterPost; override;
    procedure DoBeforePost; override;
  public
    constructor Create(AOwner:TComponent); override;
    procedure Delete; override;
    procedure FixErrors(GlobalAction: TTransactionGlobalAction; OutputLog: TStrings);
  end;

implementation

uses FBMessages, IBUtils, RegExpr, CustApp, IBErrorCodes;

const
  SPBPrefix = 'isc_spb_';
  isc_spb_last_spb_constant = 13;
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
    'sql_role_name',
    'expected_db'
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
    isc_spb_sql_role_name,
    isc_spb_expected_db
  );

  { TIBXClientSideRestoreService }

procedure TIBXClientSideRestoreService.Execute(OutputLog: TStrings);
begin
  // Do nothing
end;

procedure TIBXClientSideRestoreService.SetArchiveSource;
begin
  SRB.Add(isc_spb_bkp_file).AsString := 'stdin';
end;

procedure TIBXClientSideRestoreService.RestoreFromStream(S: TStream;
    OutputLog: TStrings);
var line: string;
begin
  ServiceStart;
  try
    while not Eof do
    begin
      SendNextChunk(S,line);
      if line <> '' then
      begin
        DoOnGetNextLine(line);
        if OutputLog <> nil then
          OutputLog.Add(line);
      end;
    end;
  finally
    while IsServiceRunning do; {flush}
  end;
end;

procedure TIBXClientSideRestoreService.RestoreFromFile(aFileName: string;
    OutputLog: TStrings);
var F: TFileStream;
begin
  F := TFileStream.Create(aFileName,fmOpenRead);
  try
   RestoreFromStream(F,OutputLog)
  finally
    F.Free;
  end;
end;

procedure TIBXClientSideRestoreService.RestoreFromFiles(FileList: TStrings;
    OutputLog: TStrings);
var i: integer;
    F: TFileStream;
    line: string;
begin
  ServiceStart;
  for i := 0 to FileList.Count - 1 do
  begin
    F := TFileStream.Create(FileList[i],fmOpenRead);
    try
      while Eof do
      begin
        SendNextChunk(F,line);
        if line <> '' then
        begin
          DoOnGetNextLine(line);
          if OutputLog <> nil then
            OutputLog.Add(line);
        end;
      end;
    finally
      F.Free;
      while IsServiceRunning do; {flush}
      FEof := false;
    end;
  end;
end;

  { TIBXClientSideBackupService }

procedure TIBXClientSideBackupService.Execute(OutputLog: TStrings);
begin
  //Do nothing
end;

procedure TIBXClientSideBackupService.SetBackupTarget;
begin
  SRB.Add(isc_spb_bkp_file).AsString := 'stdout';
end;

procedure TIBXClientSideBackupService.BackupToStream(S: TStream;
    var BytesWritten: integer);
var InitialSize: integer;
begin
  InitialSize := S.Size;
  ServiceStart;
  while not Eof do
    ReceiveNextChunk(S);
  BytesWritten := S.Size - InitialSize;
end;

procedure TIBXClientSideBackupService.BackupToFile(aFileName: string;
    var BytesWritten: integer);
var F: TFileStream;
begin
  F := TFileStream.Create(aFileName,fmCreate);
  try
    BackupToStream(F,BytesWritten);
  finally
    F.Free;
  end;
end;

  { TIBXServicesLimboTransactionsList }

procedure TIBXServicesLimboTransactionsList.DoBeforeInsert;
begin
  inherited DoBeforeInsert;
  if not FLoading then
    IBError(ibxeNoLimboTransactionInsert,[nil]);
end;

procedure TIBXServicesLimboTransactionsList.DoAfterOpen;

    function TypeToStr(MultiDatabase: boolean): string;
    begin
      if MultiDatabase then
        Result := 'Multi DB'
      else
        Result := 'Single DB';
    end;

    function StateToStr(State: TTransactionState): string;
    begin
      case State of
      LimboState:
        Result := 'Limbo';
      CommitState:
        Result := 'Commit';
      RollbackState:
        Result := 'Rollback';
      else
        Result := 'Unknown';
      end;
    end;

    function AdviseToStr(Advise: TTransactionAdvise): string;
    begin
      case Advise of
      CommitAdvise:
        Result := 'Commit';
      RollbackAdvise:
        Result := 'Rollback';
      else
        Result := 'Unknown';
      end;
    end;

    function ActionToStr(anAction: TTransactionAction): string;
    begin
      case anAction of
      CommitAction:
        Result := 'Commit';
      RollbackAction:
        Result := 'Rollback';
      end;
    end;

var i: integer;
begin
  if FLoading then Exit;
  FLoading := true;
  with FSource as TIBXLimboTransactionResolutionService do
  try
    FetchLimboTransactionInfo;
    for i := 0 to LimboTransactionInfoCount - 1 do
    with LimboTransactionInfo[i] do
    begin
      Append;
      FieldByName('TransactionID').AsInteger := ID;
      FieldByName('TransactionType').AsString := TypeToStr(MultiDatabase);
      FieldByName('HostSite').AsString := HostSite;
      FieldByName('RemoteSite').AsString := RemoteSite;
      FieldByName('DatabasePath').AsString := RemoteDatabasePath;
      FieldByName('State').AsString := StateToStr(State);
      FieldByName('RecommendedAction').AsString := AdviseToStr(Advise);
      FieldByName('RequestedAction').AsString := ActionToStr(Action);
      Post;
    end;
  finally
    FLoading := false;
  end;
  inherited DoAfterOpen;
end;

procedure TIBXServicesLimboTransactionsList.DoAfterPost;
begin
  if not FLoading then
    inherited DoAfterPost;
end;

procedure TIBXServicesLimboTransactionsList.DoBeforePost;
  var i: integer;
begin
  inherited DoBeforePost;
  if FLoading then Exit;
  with FSource as TIBXLimboTransactionResolutionService do
  for i := 0 to LimboTransactionInfoCount - 1 do
    with LimboTransactionInfo[i] do
    begin
      if ID = FieldByName('TransactionID').AsInteger then
      begin
       if FieldByName('RequestedAction').AsString = 'Commit' then
         Action := CommitAction
       else
         if FieldByName('RequestedAction').AsString = 'Rollback' then
           Action := RollbackAction;
       break;
      end;
    end;
end;


  constructor TIBXServicesLimboTransactionsList.Create(AOwner: TComponent);
  var i: integer;
  begin
    inherited Create(AOwner);
    FRequiredSource := TIBXLimboTransactionResolutionService;
    with FieldDefs do
    if Count = 0 then
    begin
      Add('TransactionID',ftInteger);
      Add('TransactionType',ftString,16);
      Add('HostSite',ftString,256);
      Add('RemoteSite',ftString,256);
      Add('DatabasePath',ftString,256);
      Add('State',ftString,32);
      Add('RecommendedAction',ftString,32);
      Add('RequestedAction',ftString,32);
      for i := 0 to Count - 2 do
        Items[i].Attributes := Items[i].Attributes + [faReadOnly];
    end;
  end;

  procedure TIBXServicesLimboTransactionsList.Delete;
  begin
    //Do nothing
  end;

    procedure TIBXServicesLimboTransactionsList.FixErrors(
    GlobalAction: TTransactionGlobalAction; OutputLog: TStrings);
  begin
    if State = dsEdit then Post;
    (FSource as TIBXLimboTransactionResolutionService).GlobalAction := GlobalAction;
    (FSource as TIBXLimboTransactionResolutionService).Execute(OutputLog);
    Active := false;
    Active := true;
  end;

  { TIBXServicesUserList }

  procedure TIBXServicesUserList.DoBeforePost;
    procedure SetParams;
    begin
      with FSource as TIBXSecurityService do
      begin
        UserID := FieldByName('UserID').AsInteger;
        GroupID := FieldByName('GroupID').AsInteger;
        UserName := FieldByName('SEC$USER_NAME').AsString;
        FirstName := FieldByName('SEC$FIRST_NAME').AsString;
        MiddleName := FieldByName('SEC$MIDDLE_NAME').AsString;
        LastName := FieldByName('SEC$LAST_NAME').AsString;
        if not FieldByName('SEC$PASSWORD').IsNull then
          Password := FieldByName('SEC$PASSWORD').AsString;
        AdminRole := FieldByName('SEC$ADMIN').AsBoolean;
      end;
    end;

  begin
    inherited DoBeforePost;
    if FLoading then Exit;
    case State of
    dsEdit:
      begin
        SetParams;
        (FSource as TIBXSecurityService).ModifyUser;
      end;
    dsInsert:
      begin
        SetParams;
        (FSource as TIBXSecurityService).AddUser;
      end;
    end;
  end;

  procedure TIBXServicesUserList.DoAfterInsert;
  begin
    FieldByName('UserID').AsInteger := 0;
    FieldByName('GroupID').AsInteger := 0;
    FieldByName('SEC$PASSWORD').Clear;
    FieldByName('SEC$ADMIN').AsBoolean := false;
    inherited DoAfterInsert;
  end;

  procedure TIBXServicesUserList.DoAfterPost;
  begin
    {Refresh}
    if not FLoading then
    begin
      inherited DoAfterPost;
      with FSource as TIBXSecurityService do
      begin
        DisplayUser(FieldByName('SEC$USER_NAME').AsString);
        if UserInfoCount > 0 then
        with UserInfo[0] do
        begin
          FieldByName('UserID').AsInteger := UserID;
          FieldByName('GroupID').AsInteger := GroupID;
          FieldByName('SEC$USER_NAME').AsString := UserName;
          FieldByName('SEC$FIRST_NAME').AsString := FirstName;
          FieldByName('SEC$MIDDLE_NAME').AsString := MiddleName;
          FieldByName('SEC$LAST_NAME').AsString := LastName;
          FieldByName('SEC$PASSWORD').Clear;
          FieldByName('SEC$ADMIN').AsBoolean := AdminRole;
        end;
      end;
    end;
  end;

  procedure TIBXServicesUserList.DoAfterOpen;
  var i: integer;
      Buf: TStringList;
  begin
    buf := TStringList.Create; {Used to sort user info}
    try
      with FSource as TIBXSecurityService do
      begin
        buf.Sorted := true;
        DisplayUsers;
        FLoading := true;
        try
          for i := 0 to UserInfoCount - 1 do
            buf.AddObject(UserInfo[i].UserName,UserInfo[i]);

          for i := 0 to buf.Count - 1 do
          with TUserInfo(buf.Objects[i]) do
          begin
            Append;
            FieldByName('UserID').AsInteger := UserID;
            FieldByName('GroupID').AsInteger := GroupID;
            FieldByName('SEC$USER_NAME').AsString := UserName;
            FieldByName('SEC$FIRST_NAME').AsString := FirstName;
            FieldByName('SEC$MIDDLE_NAME').AsString := MiddleName;
            FieldByName('SEC$LAST_NAME').AsString := LastName;
            FieldByName('SEC$PASSWORD').Clear;
            FieldByName('SEC$ADMIN').AsBoolean := AdminRole;
            Post;
          end;
        finally
          FLoading := false;
        end;
      end;
    finally
      Buf.Free;
    end;
    inherited DoAfterOpen;
  end;

  procedure TIBXServicesUserList.InternalDelete;
  begin
    with FSource as TIBXSecurityService do
    begin
      UserName := FieldByName('SEC$USER_NAME').AsString;
      DeleteUser;
    end;
    inherited InternalDelete;
  end;

  constructor TIBXServicesUserList.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FRequiredSource := TIBXSecurityService;
    with FieldDefs do
    if Count = 0 then
    begin
      Add('UserID',ftInteger);
      Add('GroupID',ftInteger);
      Add('SEC$USER_NAME',ftString,31);
      Add('SEC$FIRST_NAME',ftString,32);
      Add('SEC$MIDDLE_NAME',ftString,32);
      Add('SEC$LAST_NAME',ftString,32);
      Add('SEC$PASSWORD',ftString,32);
      Add('SEC$ADMIN',ftBoolean);
    end;
  end;

  { TIBXServicesDataSet }

  procedure TIBXServicesDataSet.SetSource(AValue: TIBXControlAndQueryService);
  begin
   if FSource = AValue then Exit;
   if (AValue <> nil) and not (AValue is FRequiredSource) then
     IBError(ibxeNotRequiredDataSetSource,[AValue.ClassName]);
   if FSource <> nil then
   begin
     FSource.UnRegisterDataSet(self);
     RemoveFreeNotification(FSource);
   end;
   FSource := AValue;
   if FSource <> nil then
   begin
      FSource.RegisterDataSet(self);
      FreeNotification(FSource);
   end;
  end;

procedure TIBXServicesDataSet.DoBeforeClose;
begin
 if csDestroying in ComponentState then Exit;
 if State in [dsEdit,dsInsert] then Post;
 Clear(false);
 inherited DoBeforeClose;
end;

procedure TIBXServicesDataSet.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSource) then
    FSource := nil;
end;

destructor TIBXServicesDataSet.Destroy;
begin
  Source := nil;
  inherited Destroy;
end;

  { TIBXLimboTransactionResolutionService }

  function TIBXLimboTransactionResolutionService.GetLimboTransactionInfo(
    index: integer): TLimboTransactionInfo;
  begin
    if index < GetLimboTransactionInfoCount then
      Result := FLimboTransactionInfo[index]
    else
      Result := nil;
  end;

  function TIBXLimboTransactionResolutionService.GetLimboTransactionInfoCount: integer;
  begin
    Result := Length(FLimboTransactionInfo);
    if Result = 0 then
      Result := FetchLimboTransactionInfo;
  end;

  procedure TIBXLimboTransactionResolutionService.SetServiceStartOptions;
  var i: integer;
  begin
    SRB.Add(isc_action_svc_repair);
    AddDBNameToSRB;
    if Length(FLimboTransactionInfo) = 0 then
      SRB.Add(isc_spb_options).AsInteger := isc_spb_rpr_list_limbo_trans
    else
    {Fixing existing transactions}
    begin
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
    end;
  end;

  destructor TIBXLimboTransactionResolutionService.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;

  procedure TIBXLimboTransactionResolutionService.Clear;
  var
    i : Integer;
  begin
    for i := 0 to High(FLimboTransactionInfo) do
      FLimboTransactionInfo[i].Free;
    SetLength(FLimboTransactionInfo,0);
  end;


  function TIBXLimboTransactionResolutionService.FetchLimboTransactionInfo: integer;

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
  begin
    Clear;
    Result := 0;
    ServiceStart;
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
    Result := Length(FLimboTransactionInfo);
  end;

procedure TIBXLimboTransactionResolutionService.Execute(OutputLog: TStrings);
begin
    if Length(FLimboTransactionInfo) > 0 then
    begin
      ServiceStart; {Fix is implicit in non-zero list of Limbo transactions}
      while not Eof do
        OutputLog.Add(GetNextLine);
      while IsServiceRunning do;
      Clear;
    end;
end;

  { TIBXSecurityService }

  constructor TIBXSecurityService.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FModifyParams := [];
  end;

  destructor TIBXSecurityService.Destroy;
  var
    i : Integer;
  begin
    for i := 0 to High(FUserInfo) do
      FUserInfo[i].Free;
    FUserInfo := nil;
    inherited Destroy;
  end;

  procedure TIBXSecurityService.Execute(OutputLog: TStrings);
begin
  //Do nothing
end;

  procedure TIBXSecurityService.FetchUserInfo;
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

            isc_spb_sec_admin:
              FUserInfo[k].AdminRole := AsInteger <> 0;

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

  function TIBXSecurityService.GetUserInfo(Index: Integer): TUserInfo;
  begin
    if Index <= High(FUSerInfo) then
      result := FUserInfo[Index]
    else
      result := nil;
  end;

  function TIBXSecurityService.GetUserInfoCount: Integer;
  begin
    Result := Length(FUserInfo);
  end;

  procedure TIBXSecurityService.AddUser;
  begin
    SecurityAction := ActionAddUser;
    ServiceStart;
    while IsServiceRunning do;
  end;

  procedure TIBXSecurityService.DeleteUser;
  begin
    SecurityAction := ActionDeleteUser;
    ServiceStart;
    while IsServiceRunning do;
  end;

  procedure TIBXSecurityService.DisplayUsers;
  begin
    SecurityAction := ActionDisplayUser;
    ClearParams;
    FUserName := '';
    ServiceStart;
    FetchUserInfo;
  end;

  procedure TIBXSecurityService.DisplayUser(aUserName: string);
  begin
    SecurityAction := ActionDisplayUser;
    ClearParams;
    FUserName := aUserName;
    ServiceStart;
    FetchUserInfo;
  end;

  procedure TIBXSecurityService.ModifyUser;
  begin
    SecurityAction := ActionModifyUser;
    ServiceStart;
    while IsServiceRunning do;
  end;

  function TIBXSecurityService.HasAdminRole: boolean;
  begin
    CheckActive;
    with ServicesConnection do
    Result :=  (ServerVersionNo[1] > 2) or
               ((ServerVersionNo[1] = 2) and (ServerVersionNo[2] = 5));
  end;

  procedure TIBXSecurityService.SetAutoAdmin(Value: Boolean);
  begin
    CheckActive;
    {only available for Firebird 2.5 and later}
    with ServicesConnection do
    if (ServerVersionNo[1] < 2) or
               ((ServerVersionNo[1] = 2) and (ServerVersionNo[2] < 5)) then Exit;
    if Value then
      SRB.Add(isc_action_svc_set_mapping)
    else
      SRB.Add(isc_action_svc_drop_mapping);
    InternalServiceStart;
    while IsServiceRunning do;
  end;

  procedure TIBXSecurityService.SetSecurityAction (Value: TSecurityAction);
  begin
    FSecurityAction := Value;
    if Value = ActionDeleteUser then
      ClearParams;
  end;

  procedure TIBXSecurityService.ClearParams;
  begin
    FModifyParams := [];
    FFirstName := '';
    FMiddleName := '';
    FLastName := '';
    FGroupID := 0;
    FUserID := 0;
    FPassword := '';
  end;

  procedure TIBXSecurityService.SetAdminRole(AValue: boolean);
  begin
    FAdminRole := AValue;
    Include (FModifyParams, ModifyAdminRole);
  end;

  procedure TIBXSecurityService.SetFirstName (Value: String);
  begin
    FFirstName := Value;
    Include (FModifyParams, ModifyFirstName);
  end;

  procedure TIBXSecurityService.SetMiddleName (Value: String);
  begin
    FMiddleName := Value;
    Include (FModifyParams, ModifyMiddleName);
  end;

  procedure TIBXSecurityService.SetLastName (Value: String);
  begin
    FLastName := Value;
    Include (FModifyParams, ModifyLastName);
  end;

  procedure TIBXSecurityService.SetPassword (Value: String);
  begin
    FPassword := Value;
    Include (FModifyParams, ModifyPassword);
  end;

  procedure TIBXSecurityService.SetUserId (Value: Integer);
  begin
    FUserId := Value;
    Include (FModifyParams, ModifyUserId);
  end;

  procedure TIBXSecurityService.SetGroupId (Value: Integer);
  begin
    FGroupId := Value;
    Include (FModifyParams, ModifyGroupId);
  end;

  procedure TIBXSecurityService.Loaded;
  begin
    inherited Loaded;
    ClearParams;
  end;

  procedure TIBXSecurityService.SetServiceStartOptions;
  var
    Len: UShort;

  begin
    case FSecurityAction of
    ActionDisplayUser:
      begin
        if HasAdminRole then
          SRB.Add(isc_action_svc_display_user_adm) {Firebird 2.5 and later only}
        else
          SRB.Add(isc_action_svc_display_user);
        if UserName <> '' then
          SRB.Add(isc_spb_sec_username).AsString := UserName;
      end;

      ActionAddUser:
      begin
        if ( Pos(' ', FUserName) > 0 ) then
          IBError(ibxeStartParamsError, [nil]);
        Len := Length(FUserName);
        if (Len = 0) then
          IBError(ibxeStartParamsError, [nil]);
        SRB.Add(isc_action_svc_add_user);
        SRB.Add(isc_spb_sec_username).AsString := FUserName;
        if FSQLRole <> '' then
          SRB.Add(isc_spb_sql_role_name).AsString := FSQLRole;
        SRB.Add(isc_spb_sec_userid).AsInteger := FUserID;
        SRB.Add(isc_spb_sec_groupid).AsInteger := FGroupID;
        SRB.Add(isc_spb_sec_password).AsString := FPassword;
        SRB.Add(isc_spb_sec_firstname).AsString := FFirstName;
        SRB.Add(isc_spb_sec_middlename).AsString := FMiddleName;
        SRB.Add(isc_spb_sec_lastname).AsString := FLastName;
        if HasAdminRole then
          SRB.Add(isc_spb_sec_admin).AsInteger := ord(FAdminRole);
      end;

      ActionDeleteUser:
      begin
        Len := Length(FUserName);
        if (Len = 0) then
          IBError(ibxeStartParamsError, [nil]);
        SRB.Add(isc_action_svc_delete_user);
        SRB.Add(isc_spb_sec_username).AsString := FUserName;
        if FSQLRole <> '' then
          SRB.Add(isc_spb_sql_role_name).AsString := FSQLRole;
      end;

      ActionModifyUser:
      begin
        Len := Length(FUserName);
        if (Len = 0) then
          IBError(ibxeStartParamsError, [nil]);
        SRB.Add(isc_action_svc_modify_user);
        SRB.Add(isc_spb_sec_username).AsString := FUserName;
        if FSQLRole <> '' then
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
        if (ModifyAdminRole in FModifyParams) and HasAdminRole then
        begin
          if FAdminRole then
            SRB.Add(isc_spb_sec_admin).AsInteger := 1
          else
            SRB.Add(isc_spb_sec_admin).AsInteger := 0;
        end;
      end;
    end;
    ClearParams;
  end;


{ TIBXValidationService }

procedure TIBXValidationService.SetServiceStartOptions;
var
  param: Integer;
begin
  SRB.Add(isc_action_svc_repair);
  AddDBNAmeToSRB;

  param := 0;
  if (SweepDB in Options) then
    param := param or isc_spb_rpr_sweep_db;
  if (ValidateDB in Options) then
    param := param or isc_spb_rpr_validate_db;

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

{ TIBXOnlineValidationService }

procedure TIBXOnlineValidationService.SetServiceStartOptions;
begin
  SRB.Add(isc_action_svc_validate);
  AddDBNameToSRB;
  if IncludeTables <> '' then
    SRB.Add(isc_spb_val_tab_incl).AsString := IncludeTables;
  if ExcludeTables <> '' then
    SRB.Add(isc_spb_val_tab_excl).AsString := ExcludeTables;
  if IncludeIndexes <> '' then
    SRB.Add(isc_spb_val_idx_incl).AsString := IncludeIndexes;
  if ExcludeIndexes <> '' then
    SRB.Add(isc_spb_val_idx_excl).AsString := ExcludeIndexes;
  if LockTimeout <> 0 then
    SRB.Add(isc_spb_val_lock_timeout).AsInteger := LockTimeout;
end;

constructor TIBXOnlineValidationService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLockTimeout := 10;
end;

procedure TIBXOnlineValidationService.ServiceStart;
begin
  CheckActive;
  {Firebird 2.5 and later}
  with ServicesConnection do
  if (ServerVersionNo[1] < 2) or
             ((ServerVersionNo[1] = 2) and (ServerVersionNo[2] < 5)) then
    IBError(ibxeServiceUnavailable,[]);
  inherited ServiceStart;
end;

{ TIBXServerSideRestoreService }

procedure TIBXServerSideRestoreService.SetBackupFiles(const Value: TStrings);
begin
  FBackupFiles.Assign(Value);
end;

procedure TIBXServerSideRestoreService.SetArchiveSource;
var i: integer;
begin
  for i := 0 to FBackupFiles.Count - 1 do
  begin
    if (Trim(FBackupFiles[i]) = '') then continue;
    if (Pos('=', FBackupFiles[i]) <> 0) then  {mbcs ok}
    begin
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFiles.Names[i];
      SRB.Add(isc_spb_bkp_length).AsInteger := StrToInt(FBackupFiles.ValueFromIndex[i]);
    end
    else
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFiles[i];
  end
end;

constructor TIBXServerSideRestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TStringList.Create;
end;

destructor TIBXServerSideRestoreService.Destroy;
begin
  if assigned(FBackupFiles) then FBackupFiles.Free;
  inherited Destroy;
end;

{ TIBXRestoreService }

procedure TIBXRestoreService.SetDatabaseFiles(const Value: TStrings);
begin
  FDatabaseFiles.Assign(Value);
end;

procedure TIBXRestoreService.DatabaseNameChanged;
begin
  inherited DatabaseNameChanged;
  DatabaseFiles.Clear;
  DatabaseFiles.Add(DatabaseName);
end;

procedure TIBXRestoreService.SetServiceStartOptions;
var
  param: Integer;
  i: integer;
begin
  SRB.Add(isc_action_svc_restore);
  inherited SetServiceStartOptions;

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
  if (RestoreMetaDataOnly in Options) then
    param := param or isc_spb_res_metadata_only;
  SRB.Add(isc_spb_options).AsInteger := param;

  if FPageSize > 0 then
    SRB.Add(isc_spb_res_page_size).AsInteger := FPageSize;
  if FPageBuffers > 0 then
    SRB.Add(isc_spb_res_buffers).AsInteger := FPageBuffers;

  SetArchiveSource;

  if FDatabaseFiles.Count > 0 then
    FDatabaseName := FDatabaseFiles[0]; {needed if an isc_sec_context error}
  for i := 0 to FDatabaseFiles.Count - 1 do
  begin
    if (Trim(FDatabaseFiles[i]) = '') then continue;
    if (Pos('=', FDatabaseFiles[i]) <> 0) then {mbcs ok}
    begin
      SRB.Add(isc_spb_dbname).AsString := FDatabaseFiles.Names[i];
      SRB.Add(isc_spb_res_length).AsInteger :=  StrToInt(FDatabaseFiles.ValueFromIndex[i]);
    end
    else
      SRB.Add(isc_spb_dbname).AsString := FDatabaseFiles[i];
  end;
end;

constructor TIBXRestoreService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseFiles := TStringList.Create;
  Include (FOptions, CreateNewDB);
end;

destructor TIBXRestoreService.Destroy;
begin
  if FDatabaseFiles <> nil then FDatabaseFiles.Free;
  inherited Destroy;
end;

{ TIBXServerSideBackupService }

procedure TIBXServerSideBackupService.SetBackupFile(const Value: TStrings);
begin
  FBackupFiles.Assign(Value);
end;

procedure TIBXServerSideBackupService.SetBackupTarget;
var i: integer;
begin
  for i := 0 to FBackupFiles.Count - 1 do
  begin
    if (Trim(FBackupFiles[i]) = '') then
      continue;
    if (Pos('=', FBackupFiles[i]) <> 0) then
    begin {mbcs ok}
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFiles.Names[i];
      SRB.Add(isc_spb_bkp_length).AsInteger := StrToInt(FBackupFiles.ValueFromIndex[i]);
    end
    else
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFiles[i];
  end;
end;

constructor TIBXServerSideBackupService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TStringList.Create;
end;

destructor TIBXServerSideBackupService.Destroy;
begin
  if assigned(FBackupFiles) then FBackupFiles.Free;
  inherited Destroy;
end;

{ TIBXBackupService }

procedure TIBXBackupService.SetServiceStartOptions;
var
  param: Integer;
begin
  SRB.Add(isc_action_svc_backup);
  AddDBNameToSRB;
  inherited SetServiceStartOptions;

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
  {Firebird 2.5 and later}
  with ServicesConnection do
  if (ServerVersionNo[1] > 2) or
             ((ServerVersionNo[1] = 2) and (ServerVersionNo[2] = 5)) then
  begin
    if (NoDBTriggers in Options) then
      param := param or isc_spb_bkp_no_triggers;
  end;
  SRB.Add(isc_spb_options).AsInteger := param;

  if FBlockingFactor > 0 then
    SRB.Add(isc_spb_bkp_factor).AsInteger := FBlockingFactor;
  SetBackupTarget;
end;

{ TIBXBackupRestoreService }

procedure TIBXBackupRestoreService.SetServiceStartOptions;
var options: string;
begin
  if Verbose then
    SRB.Add(isc_spb_verbose);

  with ServicesConnection do
  {Firebird 2.5.5 and later}
  if (ServerVersionNo[1] < 2) or
     ((ServerVersionNo[1] = 2) and ((ServerVersionNo[2] < 5) or
          ((ServerVersionNo[2] = 5) and (ServerVersionNo[3] < 5)))) then Exit;

  if StatisticsRequested <> [] then
  begin
    options := '';
    if bsTotalTime in StatisticsRequested then
      options += 'T';
    if bsTimeDelta in StatisticsRequested then
      options += 'D';
    if bsPageReads in StatisticsRequested then
      options += 'R';
    if bsPageWrites in StatisticsRequested then
      options += 'W';
    SRB.Add(isc_spb_bkp_stat).AsString := options;
  end;
end;


{ TIBXStatisticalService }

procedure TIBXStatisticalService.SetServiceStartOptions;
var param: integer;
begin
  SRB.Add(isc_action_svc_db_stats);
  AddDBNameToSRB;

  param := 0;
  if (DataPages in Options) then
    param := param or isc_spb_sts_data_pages;
  if (HeaderPages in Options) then
    param := param or isc_spb_sts_hdr_pages;
  if (IndexPages in Options) then
    param := param or isc_spb_sts_idx_pages;
  if (SystemRelations in Options) then
    param := param or isc_spb_sts_sys_relations;
  SRB.Add(isc_spb_options).AsInteger := param;
end;

{ TIBXConfigService }

procedure TIBXConfigService.ShutdownDatabase(Options: TDBShutdownMode;
  Wait: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  if (Options = Forced) then
  SRB.Add(isc_spb_prp_shutdown_db).AsInteger := Wait
  else if (Options = DenyTransaction) then
    SRB.Add(isc_spb_prp_deny_new_transactions).AsInteger := Wait
  else
    SRB.Add(isc_spb_prp_deny_new_attachments).AsInteger := Wait;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetSweepInterval(Value: Integer);
begin
  CheckActive;
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_prp_sweep_interval).AsInteger := Value;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetDBSqlDialect(Value: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_prp_set_sql_dialect).AsInteger := Value;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetPageBuffers(Value: Integer);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_prp_page_buffers).AsInteger := Value;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.ActivateShadow;
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_options).AsInteger := isc_spb_prp_activate;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.BringDatabaseOnline;
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_options).AsInteger := isc_spb_prp_db_online;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetReserveSpace(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  with SRB.Add(isc_spb_prp_reserve_space) do
  if Value then
    AsByte := isc_spb_prp_res
  else
    AsByte := isc_spb_prp_res_use_full;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetAsyncMode(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  with SRB.Add(isc_spb_prp_write_mode) do
  if Value then
    AsByte := isc_spb_prp_wm_async
  else
    AsByte := isc_spb_prp_wm_sync;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetReadOnly(Value: Boolean);
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  with SRB.Add(isc_spb_prp_access_mode) do
  if Value then
    AsByte := isc_spb_prp_am_readonly
  else
    AsByte := isc_spb_prp_am_readwrite;
  InternalServiceStart;
  while IsServiceRunning do;
end;

procedure TIBXConfigService.SetNoLinger;
begin
  SRB.Add(isc_action_svc_properties);
  AddDBNameToSRB;
  SRB.Add(isc_spb_options).AsInteger := isc_spb_prp_nolinger;
  InternalServiceStart;
  while IsServiceRunning do;
end;

{ TIBXLogService }

procedure TIBXLogService.SetServiceStartOptions;
begin
  SRB.Add(isc_action_svc_get_ib_log);
end;

{ TIBXControlAndQueryService }

function TIBXControlAndQueryService.GetNextLine: String;
var
  i: Integer;
begin
  Result := '';
  if (FEof = True) then
    Exit;
  if not FServiceStarted then
    IBError(ibxeServiceNotStarted,[nil]);

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
  FEof := Result = '';
  Result := Trim(Result);
  DoOnGetNextLine(Result);
  if FEof then
    FServiceStarted := false;
end;

function TIBXControlAndQueryService.GetNextChunk: String;
var
  i: Integer;
begin
  if (FEof = True) then
  begin
    Result := '';
    exit;
  end;
  if not FServiceStarted then
    IBError(ibxeServiceNotStarted,[nil]);

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
  if FEof then
    FServiceStarted := false;
end;

procedure TIBXControlAndQueryService.ServiceStart;
begin
  FEof := false;
  FSendBytes := 0;
  inherited ServiceStart;
  FServiceStarted := true;
end;

function TIBXControlAndQueryService.ReceiveNextChunk(stream: TStream): integer;
var
  i: Integer;
  TimeOut: boolean;
begin
  Result := 0;
  TimeOut := false;
  if (FEof = True) then
    Exit;
  if not FServiceStarted then
    IBError(ibxeServiceNotStarted,[nil]);

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
        FEof := (Result = 0) and not TimeOut;
      end;

      isc_info_truncated:
        FEof := False;

      isc_info_svc_timeout:
        begin
          FEof := False;
          TimeOut := true;
        end

    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
  if FEof then
    FServiceStarted := false;
end;

function TIBXControlAndQueryService.SendNextChunk(stream: TStream;
  var line: String): integer;
var
  i: Integer;
begin
  Result := 0;
  line := '';
  if (FEof = True) then
    Exit;

  if not FServiceStarted then
    IBError(ibxeServiceNotStarted,[nil]);

  SRB.Add(isc_info_svc_line);
  SRB.Add(isc_info_svc_stdin);

  SQPB.Add(isc_info_svc_timeout).AsInteger := 1;
  if FSendBytes > 0 then
    Result := SQPB.Add(isc_info_svc_line).CopyFrom(stream,FSendBytes);
  try
    InternalServiceQuery;
  except
    FSendBytes := 0;
    raise;
  end;

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
  if FEof then
    FServiceStarted := false;
end;

procedure TIBXControlAndQueryService.DoOnGetNextLine(Line: string);
begin
  if assigned(FOnGetNextLine) then
    OnGetNextLine(self,Line);
end;

procedure TIBXControlAndQueryService.OnBeforeDisconnect(
  Sender: TIBXServicesConnection);
var i: integer;
begin
  inherited OnBeforeDisconnect(Sender);
  for i := 0 to FDataSets.Count - 1 do
    TDataSet(FDataSets[i]).Active := false;
end;

procedure TIBXControlAndQueryService.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TDataSet) then
    FDataSets.Remove(AComponent);
end;

procedure TIBXControlAndQueryService.RegisterDataSet(aDataSet: TDataSet);
begin
  if FDataSets.IndexOf(aDataset) = -1 then
  begin
    FDataSets.Add(aDataSet);
    FreeNotification(ADataSet);
  end;
end;

procedure TIBXControlAndQueryService.UnRegisterDataSet(aDataSet: TDataSet);
begin
  FDataSets.Remove(aDataSet);
  RemoveFreeNotification(aDataset);
end;

constructor TIBXControlAndQueryService.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDataSets := TList.Create;
end;

destructor TIBXControlAndQueryService.Destroy;
begin
  inherited Destroy;
  if assigned(FDataSets) then FDataSets.Free;
end;

procedure TIBXControlAndQueryService.Execute(OutputLog: TStrings);
begin
  ServiceStart;
  try
    while not Eof do
      if OutputLog <> nil then
        OutputLog.Add(GetNextLine)
      else
        GetNextLine;
  finally
    while IsServiceRunning do; {flush}
  end;
end;

{ TIBXControlService }

function TIBXControlService.GetIsServiceRunning: Boolean;
begin
  Result := (ServicesConnection <> nil) and (ServicesConnection.Connected);
  if Result then
  begin
    SRB.Add(isc_info_svc_running);
    InternalServiceQuery(false);
    Result := (FServiceQueryResults <> nil) and (FServiceQueryResults.Count > 0) and
                 (FServiceQueryResults[0].getItemType = isc_info_svc_running) and
                  (FServiceQueryResults[0].AsInteger = 1);
  end;
end;

procedure TIBXControlService.HandleSecContextErr;
begin
  FAction := scRaiseError;
  if MainThreadID = TThread.CurrentThread.ThreadID then
    CallSecContextException
  else
    TThread.Synchronize(TThread.CurrentThread,@CallSecContextException);
end;

procedure TIBXControlService.CallSecContextException;
begin
  ServicesConnection.HandleSecContextException(self,FAction)
end;

procedure TIBXControlService.SetDatabaseName(AValue: string);
begin
  if FDatabaseName = AValue then Exit;
  CheckServiceNotRunning;
  FDatabaseName := AValue;
  DatabaseNameChanged;
end;

procedure TIBXControlService.DatabaseNameChanged;
begin
  //Do nothing
end;

procedure TIBXControlService.OnAfterConnect(Sender: TIBXServicesConnection;
  aDatabaseName: string);
begin
  inherited OnAfterConnect(Sender,aDatabaseName);
  if aDatabaseName <> '' then
    DatabaseName := aDatabaseName;
end;

procedure TIBXControlService.AddDBNameToSRB;
begin
  if FDatabaseName = '' then
    IBError(ibxeStartParamsError, [nil]);
  SRB.Add(isc_spb_dbname).AsString := FDatabaseName;
end;

procedure TIBXControlService.CheckServiceNotRunning;
begin
  if IsServiceRunning then
    IBError(ibxeServiceRunning,[nil]);
end;

procedure TIBXControlService.InternalServiceStart;
var done: boolean;
    theError: EIBInterBaseError;
begin
  if SRB = nil then
    IBError(ibxeStartParamsError, [nil]);

  FLastStartSRB := SRB;
  done := false;
  theError := nil;
  try
    repeat
      CheckActive;
      done := ServicesConnection.ServiceIntf.Start(SRB,false);
      if not done then
      begin
        theError := EIBInterBaseError.Create(FirebirdAPI.GetStatus);
        if theError.IBErrorCode = isc_sec_context then
        begin
          HandleSecContextErr;
          if FAction = scRaiseError then
            raise theError
          else
          begin
            theError.Free;
            FSRB := FLastStartSRB;
          end;
        end
        else
          raise theError;
      end;
   until done;
  finally
    FSRB := nil;
  end;
  if tfService in ServicesConnection.TraceFlags then
    MonitorHook.ServiceStart(Self);
end;

procedure TIBXControlService.DoServiceQuery;
var done: boolean;
    LastSRB: ISRB;
    LastSQPB: ISQPB;
    theError: EIBInterBaseError;
begin
  done := false;
  theError := nil;
  repeat
    LastSRB := SRB;
    LastSQPB := SQPB;
    inherited DoServiceQuery;
    done := FServiceQueryResults <> nil;
    if not done then
    begin
      if FirebirdAPI.GetStatus.GetIBErrorCode = isc_sec_context then
      begin
        theError := EIBInterBaseError.Create(FirebirdAPI.GetStatus); {save exception}
        HandleSecContextErr;
        if FAction = scReconnect then
        begin
          {Restart service}
          theError.Free;
          FSRB := FLastStartSRB;
          InternalServiceStart;
          FSRB := LastSRB;
          FSQPB := LastSQPB;
        end
        else
          raise theError;
      end
      else
        break; {Let the caller handle the error}
    end;
  until done;
end;

procedure TIBXControlService.SetServiceStartOptions;
begin
  //Do nothing
end;

procedure TIBXControlService.ServiceStart;
begin
  CheckActive;
  CheckServiceNotRunning;
  SetServiceStartOptions;
  InternalServiceStart;
end;

procedure TIBXControlService.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TIBXControlService then
    DatabaseName := TIBXControlService(Source).DatabaseName;
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

{ TIBXServerProperties }

function TIBXServerProperties.GetConfigParams: TConfigParams;
var i, j: Integer;
begin
  CheckActive;
  if FConfigParams = nil then
  begin
    SRB.Add(isc_info_svc_get_config);
    SRB.Add(isc_info_svc_get_env);
    SRB.Add(isc_info_svc_get_env_lock);
    SRB.Add(isc_info_svc_get_env_msg);
    SRB.Add(isc_info_svc_user_dbpath);

    InternalServiceQuery;

    FConfigParams := TConfigParams.Create;
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
            FConfigParams.ConfigFileData.ConfigFileKey[j] := Items[j].getItemType;
            FConfigParams.ConfigFileData.ConfigFileValue[j] := Items[j].AsInteger;
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
  Result := FConfigParams;
end;

function TIBXServerProperties.GetDatabaseInfo: TDatabaseInfo;
var i,j: Integer;
begin
  if FDatabaseInfo = nil then
  begin
    SRB.Add(isc_info_svc_svr_db_info);
    InternalServiceQuery;

    FDatabaseInfo := TDatabaseInfo.Create;
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
  Result := FDatabaseInfo;
end;

function TIBXServerProperties.GetVersionInfo: TVersionInfo;
var i : Integer;
begin
  if FVersionInfo = nil then
  begin
    SRB.Add(isc_info_svc_version);
    SRB.Add(isc_info_svc_server_version);
    SRB.Add(isc_info_svc_implementation);
    InternalServiceQuery;

    FVersionInfo := TVersionInfo.Create;
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
  Result := FVersionInfo;
end;

procedure TIBXServerProperties.Clear;
begin
  inherited;
  if assigned(FDatabaseInfo) then FreeAndNil(FDatabaseInfo);
  if assigned(FVersionInfo) then FreeAndNil(FVersionInfo);
  if assigned(FConfigParams) then FreeAndNil(FConfigParams);
end;

{ TIBXCustomService }

procedure TIBXCustomService.CheckActive;
begin
  if ServicesConnection = nil then
    IBError(ibxeServiceActive,[nil]);
  ServicesConnection.CheckActive;
end;

function TIBXCustomService.GetSQPB: ISQPB;
begin
  CheckActive;
  if FSQPB = nil then
    FSQPB := ServicesConnection.ServiceIntf.AllocateSQPB;
  Result := FSQPB;
end;

function TIBXCustomService.GetSRB: ISRB;
begin
  CheckActive;
  if FSRB = nil then
    FSRB := ServicesConnection.ServiceIntf.AllocateSRB;
  Result := FSRB;
end;

procedure TIBXCustomService.SetServicesConnection(AValue: TIBXServicesConnection
  );
begin
  if FServicesConnection = AValue then Exit;
  if FServicesConnection <> nil then
  begin
    FServicesConnection.UnRegisterIntf(self);
    RemoveFreeNotification(FServicesConnection);
  end;
  Clear;
  FServicesConnection := AValue;
  if FServicesConnection <> nil then
  begin
    FServicesConnection.RegisterIntf(self);
    FreeNotification(FServicesConnection);
  end;
end;

procedure TIBXCustomService.OnBeforeDisconnect(Sender: TIBXServicesConnection);
begin
  Clear;
end;

procedure TIBXCustomService.InternalServiceQuery(RaiseExceptionOnError: boolean
  );
begin
  CheckActive;
  try
    FServiceQueryResults := nil;
    DoServiceQuery;
    if (FServiceQueryResults = nil) and RaiseExceptionOnError then
      raise EIBInterBaseError.Create(FirebirdAPI.GetStatus);
  finally
    FSQPB := nil;
    FSRB := nil;
  end;
  if tfService in ServicesConnection.TraceFlags then
    MonitorHook.ServiceQuery(Self);
end;

procedure TIBXCustomService.DoServiceQuery;
begin
  FServiceQueryResults := ServicesConnection.ServiceIntf.Query(FSQPB,FSRB,false);
end;

procedure TIBXCustomService.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = ServicesConnection) then
    ServicesConnection := nil;
end;

constructor TIBXCustomService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSRB := nil;
  FServiceQueryResults := nil;
  FSQPB := nil;
end;

destructor TIBXCustomService.Destroy;
begin
  if ServicesConnection <> nil then
  begin
    OnBeforeDisconnect(ServicesConnection);
    ServicesConnection := nil;
  end;
  inherited Destroy;
end;

procedure TIBXCustomService.Assign(Source: TPersistent);
begin
  if Source is TIBXCustomService then
    ServicesConnection := TIBXCustomService(Source).ServicesConnection;
end;

procedure TIBXCustomService.Clear;
begin
  FSRB := nil;
  FServiceQueryResults := nil;
  FSQPB := nil;
end;

procedure TIBXCustomService.OnAfterConnect(Sender: TIBXServicesConnection;
  aDatabaseName: string);
begin
  //Do nothing
end;

{ TIBXServicesConnection }

procedure TIBXServicesConnection.SetParams(AValue: TStrings);
begin
  FParams.Assign(AValue);
end;

procedure TIBXServicesConnection.SetPortNo(AValue: string);
begin
  if FPortNo = AValue then Exit;
  Connected := false;
  FPortNo := AValue;
  FConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPortNo);
end;

procedure TIBXServicesConnection.CheckActive;
begin
  if StreamedConnected and (not Connected) then
    Loaded;
  if FService = nil then
    IBError(ibxeServiceActive, [nil]);
end;

procedure TIBXServicesConnection.CheckInactive;
begin
  if FService <> nil then
    IBError(ibxeServiceInActive, [nil]);
end;

procedure TIBXServicesConnection.CheckServerName;
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
function TIBXServicesConnection.GenerateSPB(sl: TStrings): ISPB;
var
  i, j, SPBServerVal: UShort;
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
    param_value := sl.ValueFromIndex[i];
    if (Pos(SPBPrefix, param_name) = 1) then {mbcs ok}
      Delete(param_name, 1, Length(SPBPrefix));
    { We want to translate the parameter name to some integer
      value. We do this by scanning through a list of known
      service parameter names (SPBConstantNames, defined above). }
    SPBServerVal := 0;
    { Find the parameter }
    for j := 1 to isc_spb_last_spb_constant do
      if (param_name = SPBConstantNames[j]) then
      begin
        SPBServerVal := SPBConstantValues[j];
        break;
      end;
    case SPBServerVal of
      isc_spb_user_name,
      isc_spb_password,
      isc_spb_sql_role_name,
      isc_spb_expected_db:
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

function TIBXServicesConnection.GetServerVersionNo(index: integer): integer;
begin
  CheckActive;
  if (index >= Low(FServerVersionNo)) and (index <= High(FServerVersionNo)) then
    Result := FServerVersionNo[index]
  else
    IBError(ibxeInfoBufferIndexError,[index]);
end;

function TIBXServicesConnection.GetSPBConstName(action: byte): string;
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

procedure TIBXServicesConnection.HandleException(Sender: TObject);
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

procedure TIBXServicesConnection.HandleSecContextException(
  Sender: TIBXControlService; var action: TSecContextAction);
var OldServiceIntf: IServiceManager;
begin
  action := scRaiseError;
  if assigned(FOnSecurityContextException) then
    OnSecurityContextException(self,action);

  if action = scReconnect then
  begin
    FExpectedDB := Sender.DatabaseName;
    try
      OldServiceIntf := FService;
      Connected := false;
      while not Connected do
      begin
        try
          Connected := true;
        except
         on E:EIBClientError do
          begin
            action := scRaiseError;
            FService := OldServiceIntf;
            break;
          end;
         else
           HandleException(self);
        end;
      end;
    finally
      FExpectedDB := '';
    end;
  end;
end;

function TIBXServicesConnection.Login(var aServerName: string;
  LoginParams: TStrings): Boolean;
var
  IndexOfUser, IndexOfPassword: Integer;
  Username, Password: String;
  ExtLoginParams: TStrings;
begin
  if Assigned(FOnLogin) then
  begin
    Result := True;
    ExtLoginParams := TStringList.Create;
    try
      ExtLoginParams.Assign(Params);
      FOnLogin(Self, aServerName, ExtLoginParams);
      LoginParams.Assign (ExtLoginParams);
    finally
      ExtLoginParams.Free;
    end;
  end
  else
  if assigned(IBGUIInterface)  then
  begin
    IndexOfUser := LoginParams.IndexOfName(GetSPBConstName(isc_spb_user_name));
    if IndexOfUser <> -1 then
      Username := LoginParams.ValueFromIndex[IndexOfUser]
    else
      UserName := '';
    IndexOfPassword :=LoginParams.IndexOfName(GetSPBConstName(isc_spb_password));
    if IndexOfPassword <> -1 then
      Password := LoginParams.ValueFromIndex[IndexOfPassword]
    else
      Password := '';

    result := IBGUIInterface.ServerLoginDialog(aServerName, Username, Password);
    if result then
    begin
      LoginParams.Values[GetSPBConstName(isc_spb_user_name)] := UserName;
      LoginParams.Values[GetSPBConstName(isc_spb_password)] := Password;
    end
  end
  else
    IBError(ibxeNoLoginDialog,[]);
end;

procedure TIBXServicesConnection.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TIBXServicesConnection.SetConnectString(AValue: string);
var aServiceName: AnsiString;
    aProtocol: TProtocolAll;
begin
  if FConnectString = AValue then Exit;
  Connected := false;
  if not ParseConnectString(AValue,FServerName,aServiceName,aProtocol,FPortNo)
    or (aServiceName <> 'service_mgr') or (aProtocol = unknownProtocol) then
    IBError(ibxeBadConnectString, [nil]);
  FConnectString := AValue;
  FProtocol := TProtocol(aProtocol);
end;

procedure TIBXServicesConnection.SetProtocol(AValue: TProtocol);
begin
  if FProtocol = AValue then Exit;
  Connected := false;
  FProtocol := AValue;
  FConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPortNo);
end;

procedure TIBXServicesConnection.SetServerName(AValue: string);
begin
  if FServerName = AValue then Exit;
  Connected := false;
  FServerName := AValue;
  FConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPortNo);
end;

procedure TIBXServicesConnection.DoConnect;

  procedure ParseServerVersionNo;
  var Req: ISRB;
      Results: IServiceQueryResults;
      RegexObj: TRegExpr;
      s: string;
  begin
    Req := FService.AllocateSRB;
    Req.Add(isc_info_svc_server_version);
    Results := FService.Query(nil,Req);
    if (Results.Count = 1) and (Results[0].getItemType = isc_info_svc_server_version) then
    RegexObj := TRegExpr.Create;
    try
      {extact database file spec}
      RegexObj.ModifierG := false; {turn off greedy matches}
      RegexObj.Expression := '[A-Z][A-Z]-V([0-9]+)\.([0-9]+)\.([0-9]+)\.([0-9]+) .*';
      s := Results[0].AsString;
      if RegexObj.Exec(s) then
      begin
        FServerVersionNo[1] := StrToInt(RegexObj.Match[1]);
        FServerVersionNo[2] := StrToInt(RegexObj.Match[2]);
        FServerVersionNo[3] := StrToInt(RegexObj.Match[3]);
        FServerVersionNo[4] := StrToInt(RegexObj.Match[4]);
      end;
    finally
      RegexObj.Free;
    end;
  end;

var aServerName: string;
    aProtocol: TProtocolAll;
    aPortNo: string;
    aDBName: string;
    TempSvcParams: TStrings;
    SPB: ISPB;
    PW: ISPBItem;
    i: integer;
begin
  CheckInactive;
  CheckServerName;

  aServerName := FServerName;
  aProtocol := FProtocol;
  aPortNo := PortNo;
  aDBName := '';

  if FDatabase <> nil then
  {Get Connect String from Database Connect String}
  begin
    if ParseConnectString(FDatabase.Attachment.GetConnectString,aServerName,aDBName,aProtocol,aPortNo) and
      (aProtocol = Local) and
      (FDatabase.Attachment.GetRemoteProtocol <> '') then
    begin
      {Use loopback if database does not use embedded server}
      aServerName := 'Localhost';
      aProtocol := TCP;
    end;
  end;

  TempSvcParams := TStringList.Create;
  try
    TempSvcParams.Assign(FParams);
    if LoginPrompt and not Login(aServerName,TempSvcParams) then
      IBError(ibxeOperationCancelled, [nil]);

    {Use of non-default security database}
    if FExpectedDB <> '' then {set when handling an isc_sec_context exception}
      TempSvcParams.Values['expected_db'] := FExpectedDB
    else
    if (FDatabase <> nil) and (FDatabase.Attachment.GetSecurityDatabase <> 'Default')
        and (aDBName <> '') then
        {Connect using database using non-default security database}
      TempSvcParams.Values['expected_db'] := aDBName;
    SPB := GenerateSPB(TempSvcParams);
  finally
    TempSvcParams.Free;
  end;

  FService := FirebirdAPI.GetServiceManager(aServerName,aPortNo,aProtocol,SPB);
  PW := FService.getSPB.Find(isc_spb_password);
  if PW <> nil then PW.AsString := 'xxxxxxxx'; {Hide password}

  ParseServerVersionNo;

  for i := low(FIBXServices) to high(FIBXServices) do
    FIBXServices[i].OnAfterConnect(self,aDBName);

  if tfService in TraceFlags then
    MonitorHook.ServiceAttach(Self);
end;

procedure TIBXServicesConnection.DoDisconnect;
var i: integer;
begin
  CheckActive;
  for i := 0 to Length(FIBXServices) - 1 do
    FIBXServices[i].OnBeforeDisconnect(self);
  FService := nil;
  if tfService in TraceFlags then
    MonitorHook.ServiceDetach(Self);
end;

function TIBXServicesConnection.GetConnected: Boolean;
begin
  Result := FService <> nil;
end;

function TIBXServicesConnection.GetDataset(Index: longint): TDataset;
begin
  Result := inherited GetDataset(Index);
end;

function TIBXServicesConnection.GetDataSetCount: Longint;
begin
  Result := inherited GetDataSetCount;
end;

procedure TIBXServicesConnection.ReadState(Reader: TReader);
begin
  FParams.Clear;
  inherited ReadState(Reader);
end;

procedure TIBXServicesConnection.RegisterIntf(intf: IIBXServicesClient);
begin
  Setlength(FIBXServices,Length(FIBXServices) + 1);
  FIBXServices[Length(FIBXServices)-1] := intf;
end;

procedure TIBXServicesConnection.UnRegisterIntf(intf: IIBXServicesClient);
var i, j: integer;
begin
  for i := length(FIBXServices) - 1 downto 0 do
    if FIBXServices[i] = intf then
    begin
      for j := i + 1 to length(FIBXServices) - 1 do
        FIBXServices[j-1] := FIBXServices[j];
      SetLength(FIBXServices,Length(FIBXServices)-1);
      break;
    end;
end;

constructor TIBXServicesConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerName := '';
  FParams := TStringList.Create;
  Setlength(FIBXServices,0);
  TStringList(FParams).OnChanging := @ParamsChanging;
  FService := nil;
  FProtocol := Local;
  LoginPrompt := true;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOwner).ConsoleApplication then
    LoginPrompt := false;
end;

destructor TIBXServicesConnection.Destroy;
begin
  inherited Destroy;
  Setlength(FIBXServices,0);
  if assigned(FParams) then FParams.Free;
end;

procedure TIBXServicesConnection.ConnectUsing(aDatabase: TIBDatabase);
begin
  if not aDatabase.Connected then
    IBError(ibxeDatabaseNotConnected,[nil]);
  Connected := false;
  FDatabase := aDatabase;
  try
    Connected := true;
  finally
    FDatabase := nil;
  end;
end;

procedure TIBXServicesConnection.SetDBParams(DBParams: TStrings);
var i: integer;
    j: integer;
    k: integer;
    ParamName: string;
begin
  Params.Clear;
  for i := 0 to DBParams.Count - 1 do
  begin
    ParamName := DBParams[i];
    k := Pos('=',ParamName);
    if k > 0 then system.Delete(ParamName,k,Length(ParamName)-k+1);
    for j := 1 to isc_spb_last_spb_constant do
      if CompareText(ParamName,SPBConstantNames[j]) = 0 then
      begin
        Params.Add(DBParams[i]);
        break;
      end;
  end;
end;

end.

