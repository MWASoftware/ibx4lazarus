unit IBXServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, IB, IBTypes, IBExternals;

type
  TIBXCustomService = class;
  TIBXServicesConnection = class;
  TIBXServicesLoginEvent = procedure(Service: TIBXServicesConnection; LoginParams: TStrings) of object;

  { TIBXServicesConnection }

  TIBXServicesConnection = class(TCustomConnection)
  private
    FConnectString: string;
    FParams: TStrings;
    FIBXServices: TList;
    FOnLogin: TIBXServicesLoginEvent;
    FService: IServiceManager;
    FPortNo: string;
    FServerName: string;
    FProtocol: TProtocol;
    FServerVersionNo: array [1..4] of integer;
    FTraceFlags: TTraceFlags;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckServerName;
    function GenerateSPB(sl: TStrings): ISPB;
    function GetServerVersionNo(index: integer): integer;
    function GetSPBConstName(action: byte): string;
    function Login(var aServerName: string; LoginParams: TStrings): Boolean;
    procedure ParamsChanging(Sender: TObject);
    procedure SetConnectString(AValue: string);
    procedure SetParams(AValue: TStrings);
    procedure SetPortNo(AValue: string);
    procedure SetProtocol(AValue: TProtocol);
    procedure SetServerName(AValue: string);
    procedure SetService(AValue: IServiceManager);
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function GetDataset(Index : longint) : TDataset; override;
    function GetDataSetCount : Longint; override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ServerVersionNo[index: integer]: integer read GetServerVersionNo;
    property ServiceIntf: IServiceManager read FService write SetService;
  published
    property Connected;
    property ConnectString: string read FConnectString write SetConnectString;
    property LoginPrompt default True;
    property Protocol: TProtocol read FProtocol write SetProtocol default Local;
    property PortNo: string read FPortNo write SetPortNo;
    property Params: TStrings read FParams write SetParams;
    property ServerName: string read FServerName write SetServerName;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnLogin: TIBXServicesLoginEvent read FOnLogin write FOnLogin;
 end;

 { TIBXCustomService }

 TIBXCustomService = class(TComponent)
 private
   FSRB: ISRB;
   FSQPB: ISQPB;
   FServiceQueryResults: IServiceQueryResults;
   FServicesConnection: TIBXServicesConnection;
   FTraceFlags: TTraceFlags;
   procedure CheckActive;
   function GetSQPB: ISQPB;
   function GetSRB: ISRB;
   procedure SetServicesConnection(AValue: TIBXServicesConnection);
 protected
   procedure HandleServiceDetached(Sender: TIBXServicesConnection); virtual;
   procedure InternalServiceQuery;
   property SRB: ISRB read GetSRB;
   property SQPB: ISQPB read GetSQPB;
   property ServiceQueryResults: IServiceQueryResults read FServiceQueryResults;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
 published
   property ServicesConnection: TIBXServicesConnection read FServicesConnection
     write SetServicesConnection;
   property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
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
   procedure HandleServiceDetached(Sender: TIBXServicesConnection); override;
 public
   property DatabaseInfo: TDatabaseInfo read GetDatabaseInfo;
   property VersionInfo: TVersionInfo read GetVersionInfo;
   property ConfigParams: TConfigParams read GetConfigParams;
 end;

 { TIBXControlService }

 TIBXControlService = class(TIBXCustomService)
 private
   FDatabaseName: string;
   function GetIsServiceRunning: Boolean;
 protected
   procedure AddDBNameToSRB;
   procedure CheckServiceNotRunning;
   procedure InternalServiceStart;
   procedure SetServiceStartOptions; virtual;
   procedure ServiceStart; virtual;
   property DatabaseName: string read FDatabaseName write FDatabaseName;
 public
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
 protected
   function GetNextLine : String;
   function GetNextChunk : String;
   procedure ServiceStart; override;
   function WriteNextChunk(stream: TStream): integer;
   function SendNextChunk(stream: TStream; var line: String): integer;
   procedure DoOnGetNextLine(Line: string);
 public
   property Eof: boolean read FEof;
 published
   property OnGetNextLine: TIBXOnGetNextLine read FOnGetNextLine write FOnGetNextLine;
 end;

 { TIBXLogService }

 TIBXLogService = class(TIBXControlAndQueryService)
 protected
   procedure SetServiceStartOptions; override;
 public
   procedure GetServerLog(Lines: TStrings);
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
   procedure SetAutoAdmin(Value: Boolean);
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
 public
   procedure GetStatisticsReport(Lines: TStrings);
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
 published
   property Verbose : Boolean read FVerbose write FVerbose default False;
   property StatisticsRequested: TBackupStatsOptions read FStatisticsRequested write FStatisticsRequested;
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
   procedure SetBackupTarget; virtual;
 public
   procedure BackupToStream(S: TStream; var BytesWritten: integer);
   procedure BackupToFile(aFileName: string; var BytesWritten: integer);

 published
   property BlockingFactor: Integer read FBlockingFactor write FBlockingFactor;
   property DatabaseName;
   property Options : TBackupOptions read FOptions write FOptions;
 end;

 { TIBXServerSideBackupService }

 TIBXServerSideBackupService = class(TIBXBackupService)
 private
   FBackupFile: TStrings;
   procedure SetBackupFile(const Value: TStrings);
 protected
   procedure SetBackupTarget; override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure PerformBackup(Lines: TStrings);
 published
   { a name=value pair of filename and length }
   property BackupFile: TStrings read FBackupFile write SetBackupFile;
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
   procedure SetServiceStartOptions; override;
   procedure SetArchiveSource; virtual;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure RestoreFromStream(S: TStream; Lines: TStrings);
   procedure RestoreFromFile(aFileName: string; Lines: TStrings);
   procedure RestoreFromFiles(FileList: TStrings; Lines: TStrings);
 published
   { a name=value pair of filename and length }
   property DatabaseFiles: TStrings read FDatabaseFiles write SetDatabaseFiles;
   property PageSize: Integer read FPageSize write FPageSize;
   property PageBuffers: Integer read FPageBuffers write FPageBuffers;
   property Options : TRestoreOptions read FOptions write FOptions default [CreateNewDB];
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
   procedure PerformRestore(Lines: TStrings);
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
    procedure PerformValidation(Lines: TStrings);
  published
    property DatabaseName;
    property Options: TValidateOptions read FOptions write FOptions;
  end;



implementation

uses FBMessages, IBUtils, IBSQLMonitor, RegExpr, CustApp;

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

procedure TIBXValidationService.PerformValidation(Lines: TStrings);
begin
  ServiceStart;
  while not Eof do
    Lines.Add(GetNextLine);
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

procedure TIBXServerSideRestoreService.PerformRestore(Lines: TStrings);
begin
  ServiceStart;
  while not Eof do
    Lines.Add(GetNextLine);
  while IsServiceRunning do; {flush}
end;

{ TIBXRestoreService }

procedure TIBXRestoreService.SetDatabaseFiles(const Value: TStrings);
begin
  FDatabaseFiles.Assign(Value);
end;

procedure TIBXRestoreService.SetServiceStartOptions;
var
  param: Integer;
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

end;

procedure TIBXRestoreService.SetArchiveSource;
begin
  SRB.Add(isc_spb_bkp_file).AsString := 'stdin';
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

procedure TIBXRestoreService.RestoreFromStream(S: TStream; Lines: TStrings);
var line: string;
begin
  ServiceStart;
  while not Eof do
  begin
    SendNextChunk(S,line);
    if line <> '' then
    begin
      DoOnGetNextLine(line);
      Lines.Add(line);
    end;
  end;
  while IsServiceRunning do; {flush}
end;

procedure TIBXRestoreService.RestoreFromFile(aFileName: string; Lines: TStrings);
var F: TFileStream;
begin
  F := TFileStream.Create(aFileName,fmOpenRead);
  try
   RestoreFromStream(F,Lines)
  finally
    F.Free;
  end;
end;

procedure TIBXRestoreService.RestoreFromFiles(FileList: TStrings;
  Lines: TStrings);
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
          Lines.Add(line);
        end;
      end;
    finally
      F.Free;
    end;
    FEof := false;
  end;
  while IsServiceRunning do; {flush}
end;

{ TIBXServerSideBackupService }

procedure TIBXServerSideBackupService.SetBackupFile(const Value: TStrings);
begin
  FBackupFile.Assign(Value);
end;

procedure TIBXServerSideBackupService.SetBackupTarget;
var i: integer;
begin
  for i := 0 to FBackupFile.Count - 1 do
  begin
    if (Trim(FBackupFile[i]) = '') then
      continue;
    if (Pos('=', FBackupFile[i]) <> 0) then
    begin {mbcs ok}
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile.Names[i];
      SRB.Add(isc_spb_bkp_length).AsInteger := StrToInt(FBackupFile.ValueFromIndex[i]);
    end
    else
      SRB.Add(isc_spb_bkp_file).AsString := FBackupFile[i];
  end;
end;

constructor TIBXServerSideBackupService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFile := TStringList.Create;
end;

destructor TIBXServerSideBackupService.Destroy;
begin
  if assigned(FBackupFile) then FBackupFile.Free;
  inherited Destroy;
end;

procedure TIBXServerSideBackupService.PerformBackup(Lines: TStrings);
begin
  ServiceStart;
  while not Eof do
    Lines.Add(GetNextLine);
end;

{ TIBXBackupService }

procedure TIBXBackupService.SetServiceStartOptions;
var
  param, i: Integer;
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

procedure TIBXBackupService.SetBackupTarget;
begin
  SRB.Add(isc_spb_bkp_file).AsString := 'stdout';
end;

procedure TIBXBackupService.BackupToStream(S: TStream; var BytesWritten: integer
  );
var InitialSize: integer;
begin
  InitialSize := S.Size;
  ServiceStart;
  while not Eof do
    WriteNextChunk(S);
  BytesWritten := S.Size - InitialSize;
end;

procedure TIBXBackupService.BackupToFile(aFileName: string;
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

{ TIBXBackupRestoreService }

procedure TIBXBackupRestoreService.SetServiceStartOptions;
var options: string;
begin
  if Verbose then
    SRB.Add(isc_spb_verbose);

  with ServicesConnection do
  {Firebird 2.5 and later}
  if (ServerVersionNo[1] < 2) or
             ((ServerVersionNo[1] = 2) and (ServerVersionNo[2] < 5)) then Exit;

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

procedure TIBXStatisticalService.GetStatisticsReport(Lines: TStrings);
begin
  ServiceStart;
  while not Eof do
    Lines.Add(GetNextLine);
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

procedure TIBXConfigService.SetAutoAdmin(Value: Boolean);
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

procedure TIBXLogService.GetServerLog(Lines: TStrings);
begin
  Lines.Clear;
  ServiceStart;
  while not Eof do
    Lines.Add(GetNextLine);
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
         Result := Trim(AsString);
    else
      IBError(ibxeOutputParsingError, [getItemType]);
    end;
  end;
  FEof := Result = '';
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

function TIBXControlAndQueryService.WriteNextChunk(stream: TStream): integer;
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

{ TIBXControlService }

function TIBXControlService.GetIsServiceRunning: Boolean;
begin
  SRB.Add(isc_info_svc_running);
  InternalServiceQuery;

  Result := (FServiceQueryResults.Count > 0) and
             (FServiceQueryResults[0].getItemType = isc_info_svc_running) and
              (FServiceQueryResults[0].AsInteger = 1);
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
begin
  if SRB = nil then
    IBError(ibxeStartParamsError, [nil]);

  try
    CheckActive;
    ServicesConnection.ServiceIntf.Start(SRB);
  finally
    FSRB := nil;
  end;
  if tfService in ServicesConnection.TraceFlags then
    MonitorHook.ServiceStart(Self);
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
  Result := VersionInfo;
end;

procedure TIBXServerProperties.HandleServiceDetached(
  Sender: TIBXServicesConnection);
begin
  inherited HandleServiceDetached(Sender);
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
    FServicesConnection.FIBXServices.Remove(self);
  FServicesConnection := AValue;
  if FServicesConnection <> nil then
    FServicesConnection.FIBXServices.Add(self);
end;

procedure TIBXCustomService.HandleServiceDetached(Sender: TIBXServicesConnection
  );
begin
  FSRB := nil;
  FServiceQueryResults := nil;
  FSQPB := nil;
end;

procedure TIBXCustomService.InternalServiceQuery;
begin
  CheckActive;
  try
    FServiceQueryResults := ServicesConnection.ServiceIntf.Query(FSQPB,FSRB);
  finally
    FSQPB := nil;
    FSRB := nil;
  end;
  if tfService in ServicesConnection.TraceFlags then
    MonitorHook.ServiceQuery(Self);
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
  HandleServiceDetached(nil);
  inherited Destroy;
end;

{ TIBXServicesConnection }

procedure TIBXServicesConnection.SetParams(AValue: TStrings);
begin
  FParams.Assign(AValue);
end;

procedure TIBXServicesConnection.SetPortNo(AValue: string);
begin
  if FPortNo = AValue then Exit;
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
      FOnLogin(Self, ExtLoginParams);
      LoginParams.Assign (ExtLoginParams);
      aServerName := ServerName;
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
  if not ParseConnectString(AValue,FServerName,aServiceName,aProtocol,FPortNo)
    or (aServiceName <> 'service_mgr') or (aProtocol = unknownProtocol) then
    IBError(ibxeBadConnectString, [nil]);
  FConnectString := AValue;
  FProtocol := TProtocol(aProtocol);
end;

procedure TIBXServicesConnection.SetProtocol(AValue: TProtocol);
begin
  if FProtocol = AValue then Exit;
  FProtocol := AValue;
  FConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPortNo);
end;

procedure TIBXServicesConnection.SetServerName(AValue: string);
begin
  if FServerName = AValue then Exit;
  FServerName := AValue;
  FConnectString := MakeConnectString(FServerName,'service_mgr',FProtocol,FPortNo);
end;

procedure TIBXServicesConnection.SetService(AValue: IServiceManager);
begin
  if FService = AValue then Exit;
  FService := AValue;
  if AValue <> nil then
  begin
    FPortNo := FService.getPortNo;
    ServerName := FService.getServerName;
  end;
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
    TempSvcParams: TStrings;
    SPB: ISPB;
    PW: ISPBItem;
begin
  CheckInactive;
  CheckServerName;

  aServerName := FServerName;

  TempSvcParams := TStringList.Create;
  try
    TempSvcParams.Assign(FParams);
    if LoginPrompt and not Login(aServerName,TempSvcParams) then
      IBError(ibxeOperationCancelled, [nil]);
    SPB := GenerateSPB(TempSvcParams);
  finally
    TempSvcParams.Free;
  end;

  FService := FirebirdAPI.GetServiceManager(aServerName,PortNo,FProtocol,SPB);
  PW := FService.getSPB.Find(isc_spb_password);
  if PW <> nil then PW.AsString := 'xxxxxxxx'; {Hide password}

  ParseServerVersionNo;

  MonitorHook.ServiceAttach(Self);
end;

procedure TIBXServicesConnection.DoDisconnect;
var i: integer;
begin
  CheckActive;
  for i := 0 to FIBXServices.Count - 1 do
    TIBXCustomService(FIBXServices).HandleServiceDetached(self);
  FService := nil;
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

constructor TIBXServicesConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerName := '';
  FParams := TStringList.Create;
  FIBXServices := TList.Create;
  TStringList(FParams).OnChanging := @ParamsChanging;
  FTraceFlags := [];
  FService := nil;
  FProtocol := Local;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOwner).ConsoleApplication then
    LoginPrompt := false;
end;

destructor TIBXServicesConnection.Destroy;
begin
  inherited Destroy;
  if assigned(FIBXServices) then FIBXServices.Free;
  if assigned(FParams) then FParams.Free;
end;

end.

