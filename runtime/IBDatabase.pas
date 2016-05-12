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

unit IBDatabase;

{$Mode Delphi}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, FPTimer, IBHeader, IBExternals, DB,
  IB, CustApp;

const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach'
  );

  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo'
  );

type

  TIBDatabase = class;
  TIBTransaction = class;
  TIBBase = class;

  TIBDatabaseLoginEvent = procedure(Database: TIBDatabase;
    LoginParams: TStrings) of object;

  TIBFileName = type string;
  { TIBDatabase }
  TIBDataBase = class(TCustomConnection)
  private
    FAllowStreamedConnected: boolean;
    FHiddenPassword: string;
    FIBLoaded: Boolean;
    FOnLogin: TIBDatabaseLoginEvent;
    FSQLHourGlass: Boolean;
    FTraceFlags: TTraceFlags;
    FDBSQLDialect: Integer;
    FSQLDialect: Integer;
    FOnDialectDowngradeWarning: TNotifyEvent;
    FCanTimeout: Boolean;
    FSQLObjects: TList;
    FTransactions: TList;
    FDBName: TIBFileName;
    FDBParams: TStrings;
    FDBParamsChanged: Boolean;
    FDPB: PChar;
    FDPBLength: Short;
    FHandle: TISC_DB_HANDLE;
    FHandleIsShared: Boolean;
    FOnIdleTimer: TNotifyEvent;
    FDefaultTransaction: TIBTransaction;
    FInternalTransaction: TIBTransaction;
    FStreamedConnected: Boolean;
    FTimer: TFPTimer;
    FUserNames: TStringList;
    FDataSets: TList;
    FLoginCalled: boolean;
    FCharSetSizes: array of integer;
    FCharSetNames: array of RawByteString;
    FDefaultCharSetName: RawByteString;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    FCodePages: array of TSystemCodePage;
    FDefaultCodePage: TSystemCodePage;
    {$ENDIF}
    FUseDefaultSystemCodePage: boolean;
    procedure EnsureInactive;
    function GetDBSQLDialect: Integer;
    function GetSQLDialect: Integer;
    procedure SetSQLDialect(const Value: Integer);
    procedure ValidateClientSQLDialect;
    procedure DBParamsChange(Sender: TObject);
    procedure DBParamsChanging(Sender: TObject);
    function GetSQLObject(Index: Integer): TIBBase;
    function GetSQLObjectCount: Integer;
    function GetDBParamByDPB(const Idx: Integer): String;
    function GetIdleTimer: Integer;
    function GetTransaction(Index: Integer): TIBTransaction;
    function GetTransactionCount: Integer;
    function Login(var aDatabaseName: string): Boolean;
    procedure LoadCharSetInfo;
    procedure SetDatabaseName(const Value: TIBFileName);
    procedure SetDBParamByDPB(const Idx: Integer; Value: String);
    procedure SetDBParams(Value: TStrings);
    procedure SetDefaultTransaction(Value: TIBTransaction);
    procedure SetIdleTimer(Value: Integer);
    procedure TimeoutConnection(Sender: TObject);
    function GetIsReadOnly: Boolean;
    function AddSQLObject(ds: TIBBase): Integer;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;
    procedure InternalClose(Force: Boolean);

  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    procedure CheckStreamConnect;
    procedure HandleException(Sender: TObject);
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    function GetDataset(Index : longint) : TDataset; override;
    function GetDataSetCount : Longint; override;
    procedure ReadState(Reader: TReader); override;
    procedure SetConnected (Value : boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates(const DataSets: array of TDataSet);
    procedure CloseDataSets;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CreateDatabase;
    procedure DropDatabase;
    procedure ForceClose;
    procedure GetFieldNames(const TableName: string; List: TStrings);
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function IndexOfDBConst(st: String): Integer;
    function TestConnected: Boolean;
    procedure CheckDatabaseName;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    function AddTransaction(TR: TIBTransaction): Integer;
    function FindTransaction(TR: TIBTransaction): Integer;
    function FindDefaultTransaction(): TIBTransaction;
    procedure RemoveTransaction(Idx: Integer);
    procedure RemoveTransactions;
    procedure SetHandle(Value: TISC_DB_HANDLE);

    property Handle: TISC_DB_HANDLE read FHandle;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property DBParamByDPB[const Idx: Integer]: String read GetDBParamByDPB
                                                      write SetDBParamByDPB;
    property SQLObjectCount: Integer read GetSQLObjectCount;
    property SQLObjects[Index: Integer]: TIBBase read GetSQLObject;
    property HandleIsShared: Boolean read FHandleIsShared;
    property TransactionCount: Integer read GetTransactionCount;
    property Transactions[Index: Integer]: TIBTransaction read GetTransaction;
    property InternalTransaction: TIBTransaction read FInternalTransaction;
    property DefaultCharSetName: RawByteString read FDefaultCharSetName;

  published
    property Connected;
    property AllowStreamedConnected: boolean read FAllowStreamedConnected
             write FAllowStreamedConnected;
    property DatabaseName: TIBFileName read FDBName write SetDatabaseName;
    property Params: TStrings read FDBParams write SetDBParams;
    property LoginPrompt default True;
    property DefaultTransaction: TIBTransaction read FDefaultTransaction
                                                 write SetDefaultTransaction;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer;
    property SQLDialect : Integer read GetSQLDialect write SetSQLDialect default 3;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass default true;
    property DBSQLDialect : Integer read FDBSQLDialect;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property UseDefaultSystemCodePage: boolean read FUseDefaultSystemCodePage
                                               write FUseDefaultSystemCodePage;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnLogin: TIBDatabaseLoginEvent read FOnLogin write FOnLogin;
    property OnIdleTimer: TNotifyEvent read FOnIdleTimer write FOnIdleTimer;
    property OnDialectDowngradeWarning: TNotifyEvent read FOnDialectDowngradeWarning write FOnDialectDowngradeWarning;
  end;

  { TIBTransaction }

  TTransactionAction         = (TARollback, TACommit, TARollbackRetaining, TACommitRetaining);

  TIBTransaction = class(TComponent)
  private
    FAfterDelete: TNotifyEvent;
    FAfterEdit: TNotifyEvent;
    FAfterExecQuery: TNotifyEvent;
    FAfterInsert: TNotifyEvent;
    FAfterPost: TNotifyEvent;
    FAfterTransactionEnd: TNotifyEvent;
    FBeforeTransactionEnd: TNotifyEvent;
    FIBLoaded: Boolean;
    FCanTimeout         : Boolean;
    FDatabases          : TList;
    FOnStartTransaction: TNotifyEvent;
    FSQLObjects         : TList;
    FDefaultDatabase    : TIBDatabase;
    FHandle             : TISC_TR_HANDLE;
    FHandleIsShared     : Boolean;
    FOnIdleTimer          : TNotifyEvent;
    FStreamedActive     : Boolean;
    FTPB                : PChar;
    FTPBLength          : Short;
    FTimer              : TFPTimer;
    FDefaultAction      : TTransactionAction;
    FTRParams           : TStrings;
    FTRParamsChanged    : Boolean;
    FInEndTransaction   : boolean;
    FEndAction          : TTransactionAction;
    procedure DoBeforeTransactionEnd;
    procedure DoAfterTransactionEnd;
    procedure DoOnStartTransaction;
    procedure DoAfterExecQuery(Sender: TObject);
    procedure DoAfterEdit(Sender: TObject);
    procedure DoAfterDelete(Sender: TObject);
    procedure DoAfterInsert(Sender: TObject);
    procedure DoAfterPost(Sender: TObject);
    procedure EnsureNotInTransaction;
    procedure EndTransaction(Action: TTransactionAction; Force: Boolean);
    function GetDatabase(Index: Integer): TIBDatabase;
    function GetDatabaseCount: Integer;
    function GetSQLObject(Index: Integer): TIBBase;
    function GetSQLObjectCount: Integer;
    function GetInTransaction: Boolean;
    function GetIdleTimer: Integer;
    procedure BeforeDatabaseDisconnect(DB: TIBDatabase);
    procedure SetActive(Value: Boolean);
    procedure SetDefaultAction(Value: TTransactionAction);
    procedure SetDefaultDatabase(Value: TIBDatabase);
    procedure SetIdleTimer(Value: Integer);
    procedure SetTRParams(Value: TStrings);
    procedure TimeoutTransaction(Sender: TObject);
    procedure TRParamsChange(Sender: TObject);
    procedure TRParamsChanging(Sender: TObject);
    function AddSQLObject(ds: TIBBase): Integer;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;

  protected
    procedure Loaded; override;
    procedure SetHandle(Value: TISC_TR_HANDLE);
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure StartTransaction;
    procedure CheckInTransaction;
    procedure CheckNotInTransaction;

    function AddDatabase(db: TIBDatabase): Integer;
    function FindDatabase(db: TIBDatabase): Integer;
    function FindDefaultDatabase: TIBDatabase;
    function GetEndAction: TTransactionAction;
    procedure RemoveDatabase(Idx: Integer);
    procedure RemoveDatabases;
    procedure CheckDatabasesInList;

    property DatabaseCount: Integer read GetDatabaseCount;
    property Databases[Index: Integer]: TIBDatabase read GetDatabase;
    property SQLObjectCount: Integer read GetSQLObjectCount;
    property SQLObjects[Index: Integer]: TIBBase read GetSQLObject;
    property Handle: TISC_TR_HANDLE read FHandle;
    property HandleIsShared: Boolean read FHandleIsShared;
    property InTransaction: Boolean read GetInTransaction;
    property TPB: PChar read FTPB;
    property TPBLength: Short read FTPBLength;
  published
    property Active: Boolean read GetInTransaction write SetActive;
    property DefaultDatabase: TIBDatabase read FDefaultDatabase
                                           write SetDefaultDatabase;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer default 0;
    property DefaultAction: TTransactionAction read FDefaultAction write SetDefaultAction default taCommit;
    property Params: TStrings read FTRParams write SetTRParams;
    property OnIdleTimer: TNotifyEvent read FOnIdleTimer write FOnIdleTimer;
    property BeforeTransactionEnd: TNotifyEvent read FBeforeTransactionEnd
                                             write FBeforeTransactionEnd;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd
                                            write FAfterTransactionEnd;
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction
                                              write FOnStartTransaction;
    property AfterExecQuery: TNotifyEvent read FAfterExecQuery
                                              write FAfterExecQuery;
    property AfterEdit: TNotifyEvent read FAfterEdit write FAfterEdit;
    property AfterDelete: TNotifyEvent read FAfterDelete write FAfterDelete;
    property AfterInsert: TNotifyEvent read FAfterInsert write FAfterInsert;
    property AfterPost: TNotifyEvent read FAfterPost write FAfterPost;
  end;

  TTransactionEndEvent = procedure(Sender:TObject; Action: TTransactionAction) of object;
  TBeforeDatabaseConnectEvent = procedure (Sender: TObject; DBParams: TStrings;
                              var DBName: string) of object;

  { TIBBase }

  { Virtually all components in IB are "descendents" of TIBBase.
    It is to more easily manage the database and transaction
    connections. }
  TIBBase = class(TObject)
  protected
    FBeforeDatabaseConnect: TBeforeDatabaseConnectEvent;
    FDatabase: TIBDatabase;
    FIndexInDatabase: Integer;
    FTransaction: TIBTransaction;
    FIndexInTransaction: Integer;
    FOwner: TObject;
    FBeforeDatabaseDisconnect: TNotifyEvent;
    FAfterDatabaseDisconnect: TNotifyEvent;
    FAfterDatabaseConnect: TNotifyEvent;
    FOnDatabaseFree: TNotifyEvent;
    FBeforeTransactionEnd: TTransactionEndEvent;
    FAfterTransactionEnd: TNotifyEvent;
    FOnTransactionFree: TNotifyEvent;

    procedure DoBeforeDatabaseConnect(DBParams: TStrings;
                              var DBName: string); virtual;
    procedure DoAfterDatabaseConnect; virtual;
    procedure DoBeforeDatabaseDisconnect; virtual;
    procedure DoAfterDatabaseDisconnect; virtual;
    procedure DoDatabaseFree; virtual;
    procedure DoBeforeTransactionEnd(Action: TTransactionAction); virtual;
    procedure DoAfterTransactionEnd; virtual;
    procedure DoTransactionFree; virtual;
    function GetDBHandle: PISC_DB_HANDLE; virtual;
    function GetTRHandle: PISC_TR_HANDLE; virtual;
    procedure SetDatabase(Value: TIBDatabase); virtual;
    procedure SetTransaction(Value: TIBTransaction); virtual;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure CheckDatabase; virtual;
    procedure CheckTransaction; virtual;
    procedure DoAfterExecQuery(Sender: TObject); virtual;
    procedure DoAfterEdit(Sender: TObject); virtual;
    procedure DoAfterDelete(Sender: TObject); virtual;
    procedure DoAfterInsert(Sender: TObject); virtual;
    procedure DoAfterPost(Sender: TObject); virtual;
    function GetCharSetSize(CharSetID: integer): integer;
    function GetDefaultCharSetSize: integer;
    function GetCharSetName(CharSetID: integer): string;
    function GetDefaultCharSetName: RawByteString;
    {$IFDEF HAS_ANSISTRING_CODEPAGE}
    function GetCodePage(CharSetID: integer): TSystemCodePage;
    function GetDefaultCodePage: TSystemCodePage;
    {$ENDIF}
    procedure HandleException(Sender: TObject);
    procedure SetCursor;
    procedure RestoreCursor;
  public
    property BeforeDatabaseConnect: TBeforeDatabaseConnectEvent read FBeforeDatabaseConnect
                                                 write FBeforeDatabaseConnect;
    property AfterDatabaseConnect: TNotifyEvent read FAfterDatabaseConnect
                                                write FAfterDatabaseConnect;
    property BeforeDatabaseDisconnect: TNotifyEvent read FBeforeDatabaseDisconnect
                                                   write FBeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect: TNotifyEvent read FAfterDatabaseDisconnect
                                                  write FAfterDatabaseDisconnect;
    property OnDatabaseFree: TNotifyEvent read FOnDatabaseFree write FOnDatabaseFree;
    property BeforeTransactionEnd: TTransactionEndEvent read FBeforeTransactionEnd write FBeforeTransactionEnd;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd write FAfterTransactionEnd;
    property OnTransactionFree: TNotifyEvent read FOnTransactionFree write FOnTransactionFree;
    property Database: TIBDatabase read FDatabase
                                    write SetDatabase;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property Owner: TObject read FOwner;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
    property Transaction: TIBTransaction read FTransaction
                                          write SetTransaction;
  end;

procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);


implementation

uses IBIntf, IBSQLMonitor, IBCustomDataSet, IBDatabaseInfo, IBSQL, IBUtils,
     typInfo, IBCodePage;

{ TIBDatabase }

constructor TIBDataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  LoginPrompt := True;
  FSQLObjects := TList.Create;
  FTransactions := TList.Create;
  FDBName := '';
  FDBParams := TStringList.Create;
  FSQLHourGlass := true;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOWner).ConsoleApplication then
    LoginPrompt := false;
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  FDefaultCodePage := CP_NONE;
  {$ENDIF}
  FDBParamsChanged := True;
  TStringList(FDBParams).OnChange := DBParamsChange;
  TStringList(FDBParams).OnChanging := DBParamsChanging;
  FDPB := nil;
  FHandle := nil;
  FUserNames := nil;
  FInternalTransaction := TIBTransaction.Create(self);
  FInternalTransaction.DefaultDatabase := Self;
  FTimer := TFPTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutConnection;
  FDBSQLDialect := 1;
  FSQLDialect := 3;
  FTraceFlags := [];
  FDataSets := TList.Create;
  CheckStreamConnect;
end;

 destructor TIBDataBase.Destroy;
var
  i: Integer;
begin
  if FIBLoaded then
  begin
    IdleTimer := 0;
    if FHandle <> nil then
      ForceClose;
    for i := 0 to FSQLObjects.Count - 1 do
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoDatabaseFree;
    RemoveSQLObjects;
    RemoveTransactions;
    FInternalTransaction.Free;
    FreeMem(FDPB);
    FDPB := nil;
    FDBParams.Free;
    FSQLObjects.Free;
    FUserNames.Free;
    FTransactions.Free;
  end;
  FDataSets.Free;
  inherited Destroy;
end;

 function TIBDataBase.Call(ErrCode: ISC_STATUS; RaiseError: Boolean
   ): ISC_STATUS;
begin
  result := ErrCode;
  FCanTimeout := False;
  if RaiseError and (ErrCode > 0) then
    IBDataBaseError;
end;

 procedure TIBDataBase.CheckActive;
begin
  if StreamedConnected and (not Connected) then
    Loaded;
  if FHandle = nil then
    IBError(ibxeDatabaseClosed, [nil]);
end;

 procedure TIBDataBase.EnsureInactive;
begin
  if csDesigning in ComponentState then
  begin
    if FHandle <> nil then
      Close;
  end
end;

 procedure TIBDataBase.CheckInactive;
begin
  if FHandle <> nil then
    IBError(ibxeDatabaseOpen, [nil]);
end;

 procedure TIBDataBase.CheckDatabaseName;
begin
  if (Trim(FDBName) = '') then
    IBError(ibxeDatabaseNameMissing, [nil]);
end;

 function TIBDataBase.AddSQLObject(ds: TIBBase): Integer;
begin
  result := 0;
  if (ds.Owner is TIBCustomDataSet) then
    FDataSets.Add(ds.Owner);
  while (result < FSQLObjects.Count) and (FSQLObjects[result] <> nil) do
    Inc(result);
  if (result = FSQLObjects.Count) then
    FSQLObjects.Add(ds)
  else
    FSQLObjects[result] := ds;
end;

 function TIBDataBase.AddTransaction(TR: TIBTransaction): Integer;
begin
  result := FindTransaction(TR);
  if result <> -1 then
  begin
    result := -1;
    exit;
  end;
  result := 0;
  while (result < FTransactions.Count) and (FTransactions[result] <> nil) do
    Inc(result);
  if (result = FTransactions.Count) then
    FTransactions.Add(TR)
  else
    FTransactions[result] := TR;
end;

 procedure TIBDataBase.DoDisconnect;
begin
  if Connected then
    InternalClose(False);
  FDBSQLDialect := 1;
  SetLength(FCharSetSizes,0);
  SetLength(FCharSetNames,0);
  {$IFDEF HAS_ANSISTRING_CODEPAGE}
  SetLength(FCodePages,0);
  FDefaultCodePage := CP_NONE;
  {$ENDIF}
end;

 procedure TIBDataBase.CreateDatabase;
var
  tr_handle: TISC_TR_HANDLE;
begin
  CheckInactive;
  tr_handle := nil;
  Call(
    isc_dsql_execute_immediate(StatusVector, @FHandle, @tr_handle, 0,
                               PChar('CREATE DATABASE ''' + FDBName + ''' ' + {do not localize}
                               Params.Text), SQLDialect, nil),
    True);
end;

 procedure TIBDataBase.DropDatabase;
begin
  CheckActive;
  Call(isc_drop_database(StatusVector, @FHandle), True);
end;

 procedure TIBDataBase.DBParamsChange(Sender: TObject);
begin
  FDBParamsChanged := True;
end;

 procedure TIBDataBase.DBParamsChanging(Sender: TObject);
begin
  EnsureInactive;
  CheckInactive;
end;

 function TIBDataBase.FindTransaction(TR: TIBTransaction): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FTransactions.Count - 1 do
    if TR = Transactions[i] then
    begin
      result := i;
      break;
    end;
end;

 function TIBDataBase.FindDefaultTransaction: TIBTransaction;
var
  i: Integer;
begin
  result := FDefaultTransaction;
  if result = nil then
  begin
    for i := 0 to FTransactions.Count - 1 do
      if (Transactions[i] <> nil) and
        (TIBTransaction(Transactions[i]).DefaultDatabase = self) and
        (TIBTransaction(Transactions[i]) <> FInternalTransaction) then
       begin
         result := TIBTransaction(Transactions[i]);
         break;
       end;
  end;
end;

 procedure TIBDataBase.ForceClose;
begin
  if Connected then
    InternalClose(True);
end;

 function TIBDataBase.GetConnected: Boolean;
begin
  result := FHandle <> nil;
end;

 function TIBDataBase.GetSQLObject(Index: Integer): TIBBase;
begin
  result := FSQLObjects[Index];
end;

 function TIBDataBase.GetSQLObjectCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
    Inc(result);
end;

 function TIBDataBase.GetDBParamByDPB( const Idx: Integer): String;
var
  ConstIdx, EqualsIdx: Integer;
begin
  if (Idx > 0) and (Idx <= isc_dpb_last_dpb_constant) then
  begin
    ConstIdx := IndexOfDBConst(DPBConstantNames[Idx]);
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

 function TIBDataBase.GetIdleTimer: Integer;
begin
  result := FTimer.Interval;
end;

 function TIBDataBase.GetTransaction(Index: Integer): TIBTransaction;
begin
  result := FTransactions[Index];
end;

 function TIBDataBase.GetTransactionCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FTransactions.Count - 1 do
    if FTransactions[i] <> nil then
      Inc(result);
end;

 function TIBDataBase.IndexOfDBConst(st: String): Integer;
var
  i, pos_of_str: Integer;
begin
  result := -1;
  for i := 0 to Params.Count - 1 do
  begin
    pos_of_str := Pos(st, AnsiLowerCase(Params[i])); {mbcs ok}
    if (pos_of_str = 1) or (pos_of_str = Length(DPBPrefix) + 1) then
    begin
      result := i;
      break;
    end;
  end;
end;

 procedure TIBDataBase.InternalClose(Force: Boolean);
var
  i: Integer;
begin
  CheckActive;
  { Tell all connected transactions that we're disconnecting.
    This is so transactions can commit/rollback, accordingly
  }
  for i := 0 to FTransactions.Count - 1 do
  begin
    try
      if FTransactions[i] <> nil then
        Transactions[i].BeforeDatabaseDisconnect(Self);
    except
      if not Force then
        raise;
    end;
  end;
  for i := 0 to FSQLObjects.Count - 1 do
  begin
    try
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoBeforeDatabaseDisconnect;
    except
      if not Force then
        raise;
    end;
  end;

  if (not HandleIsShared) and
     (Call(isc_detach_database(StatusVector, @FHandle), False) > 0) and
     (not Force) then
    IBDataBaseError
  else
  begin
    FHandle := nil;
    FHandleIsShared := False;
  end;

  if not (csDesigning in ComponentState) then
    MonitorHook.DBDisconnect(Self);

  for i := 0 to FSQLObjects.Count - 1 do
    if FSQLObjects[i] <> nil then
      SQLObjects[i].DoAfterDatabaseDisconnect;
end;

procedure TIBDataBase.LoadCharSetInfo;
var Query: TIBSQL;
    i: integer;
begin
  if not FInternalTransaction.Active then
    FInternalTransaction.StartTransaction;
  Query := TIBSQL.Create(self);
  try
    Query.Database := Self;
    Query.Transaction := FInternalTransaction;
    Query.SQL.Text := 'Select RDB$CHARACTER_SET_ID, RDB$BYTES_PER_CHARACTER, RDB$CHARACTER_SET_NAME ' +
                      'From RDB$CHARACTER_SETS Order by 1 DESC'; {do not localize}
    Query.Prepare;
    Query.ExecQuery;
    if not Query.EOF then
    begin
      SetLength(FCharSetSizes,Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger + 1);
      SetLength(FCharSetNames,Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger + 1);
      {$IFDEF HAS_ANSISTRING_CODEPAGE}
      SetLength(FCodePages, Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger + 1);
      {$ENDIF}
      for i := 0 to Length(FCharSetSizes) - 1 do FCharSetSizes[i] := 1;
      repeat
        FCharSetSizes[Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger] :=
                 Query.FieldByName('RDB$BYTES_PER_CHARACTER').AsInteger;
        FCharSetNames[Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger] :=
                 Trim(Query.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
        {$IFDEF HAS_ANSISTRING_CODEPAGE}
        FCodePages[Query.FieldByName('RDB$CHARACTER_SET_ID').AsInteger] :=
          IBGetCodePage(Trim(Query.FieldByName('RDB$CHARACTER_SET_NAME').AsString));
        {$ENDIF}
        Query.Next;
      until Query.EOF;
    end;
  finally
    Query.free;
    FInternalTransaction.Commit;
  end;
end;

procedure TIBDataBase.CheckStreamConnect;
var
  i: integer;
begin
  try
    if not (csDesigning in ComponentState) and StreamedConnected and (not Connected) then
    begin
      for i := 0 to FTransactions.Count - 1 do
        if  FTransactions[i] <> nil then
        begin
          with TIBTransaction(FTransactions[i]) do
            if not Active then
              if FStreamedActive and not InTransaction then
              begin
                StartTransaction;
                FStreamedActive := False;
              end;
        end;
      if (FDefaultTransaction <> nil) and
         (FDefaultTransaction.FStreamedActive) and
         (not FDefaultTransaction.InTransaction) then
        FDefaultTransaction.StartTransaction;
      FStreamedConnected := False;
    end;
  except
    if csDesigning in ComponentState then
      HandleException(Self)
    else
      raise;
  end;
end;

procedure TIBDataBase.HandleException(Sender: TObject);
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

 procedure TIBDataBase.Notification(AComponent: TComponent;
   Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDefaultTransaction) then
  begin
    i := FindTransaction(FDefaultTransaction);
    if (i <> -1) then
      RemoveTransaction(i);
    FDefaultTransaction := nil;
  end;
end;

  function TIBDataBase.Login(var aDatabaseName: string): Boolean;
var
  IndexOfUser, IndexOfPassword: Integer;
  Username, Password, OldPassword: String;
  LoginParams: TStrings;

  procedure HidePassword;
  var
    I: Integer;
    IndexAt: Integer;
  begin
    IndexAt := 0;
    for I := 0 to Params.Count -1 do
      if Pos('password', LowerCase(Trim(Params.Names[i]))) = 1 then {mbcs ok}
      begin
        FHiddenPassword := Params.Values[Params.Names[i]];
        IndexAt := I;
        break;
      end;
    if IndexAt <> 0 then
      Params.Delete(IndexAt);
  end;

begin
  Result := false;
  if FLoginCalled then Exit;
  FLoginCalled := true;
  try
  if Assigned(FOnLogin) and not (csDesigning in ComponentState) then
  begin
    result := True;
    LoginParams := TStringList.Create;
    try
      LoginParams.Assign(Params);
      FOnLogin(Self, LoginParams);
      Params.Assign (LoginParams);
      aDatabaseName := FDBName;
      HidePassword;
    finally
      LoginParams.Free;
    end;
  end
  else
  if assigned(IBGUIInterface) then
  begin
    IndexOfUser := IndexOfDBConst(DPBConstantNames[isc_dpb_user_name]);
    if IndexOfUser <> -1 then
      Username := Copy(Params[IndexOfUser],
                                         Pos('=', Params[IndexOfUser]) + 1, {mbcs ok}
                                         Length(Params[IndexOfUser]));
    IndexOfPassword := IndexOfDBConst(DPBConstantNames[isc_dpb_password]);
    if IndexOfPassword <> -1 then
    begin
      Password := Copy(Params[IndexOfPassword],
                                         Pos('=', Params[IndexOfPassword]) + 1, {mbcs ok}
                                         Length(Params[IndexOfPassword]));
      OldPassword := password;
    end;
    result := IBGUIInterface.LoginDialogEx(aDatabaseName, Username, Password, False);
    if result then
    begin
      if IndexOfUser = -1 then
        Params.Add(DPBConstantNames[isc_dpb_user_name] + '=' + Username)
      else
        Params[IndexOfUser] := DPBConstantNames[isc_dpb_user_name] +
                                 '=' + Username;
      if (Password = OldPassword) then
        FHiddenPassword := ''
      else
      begin
        FHiddenPassword := Password;
        if OldPassword <> '' then
          HidePassword;
      end;
    end;
  end
  else
  if LoginPrompt then
     IBError(ibxeNoLoginDialog,[]);
  finally
    FLoginCalled := false
  end;
end;

 procedure TIBDataBase.DoConnect;
var
  DPB: String;
  TempDBParams: TStrings;
  I: integer;
  aDBName: string;

  {Call error analysis}
  sqlcode: Long;
  IBErrorCode: Long;
  status_vector: PISC_STATUS;
  {$ifdef WINDOWS}
  acp: uint;
  {$endif}
begin
  CheckInactive;
  CheckDatabaseName;
  if (not LoginPrompt) and (FHiddenPassword <> '') then
  begin
    FHiddenPassword := '';
    FDBParamsChanged := True;
  end;
  { Use builtin login prompt if requested }
  aDBName := FDBName;
  if (LoginPrompt or (csDesigning in ComponentState)) and not Login(aDBName) then
    IBError(ibxeOperationCancelled, [nil]);

  TempDBParams := TStringList.Create;
  try
   TempDBParams.Assign(FDBParams);
   if UseDefaultSystemCodePage then
   begin
     {$ifdef WINDOWS}
     acp := GetACP;
     if (acp >= 1250) and (acp <= 1258) then
       TempDBParams.Values['lc_ctype'] := Format('WIN%d',[acp])
     else
     {$endif}
     {$IFDEF HAS_ANSISTRING_CODEPAGE}
     TempDBParams.Values['lc_ctype'] := IBGetCharacterSetName(DefaultSystemCodePage);
     FDefaultCodePage := IBGetCodePage(AnsiUpperCase(TempDBParams.Values['lc_ctype']));
     {$ELSE}
       TempDBParams.Values['lc_ctype'] :='UTF8';
     {$ENDIF}
   end;
   {Opportunity to override defaults}
   for i := 0 to FSQLObjects.Count - 1 do
   begin
       if FSQLObjects[i] <> nil then
         SQLObjects[i].DoBeforeDatabaseConnect(TempDBParams,aDBName);
   end;
   FDefaultCharSetName := AnsiUpperCase(TempDBParams.Values['lc_ctype']);

   { Generate a new DPB if necessary }
   if (FDBParamsChanged or (TempDBParams.Text <> FDBParams.Text)) then
   begin
     FDBParamsChanged := False;
     if (not LoginPrompt and not (csDesigning in ComponentState)) or (FHiddenPassword = '') then
       GenerateDPB(TempDBParams, DPB, FDPBLength)
     else
     begin
        TempDBParams.Add('password=' + FHiddenPassword);
        GenerateDPB(TempDBParams, DPB, FDPBLength);
     end;
     IBAlloc(FDPB, 0, FDPBLength);
     Move(DPB[1], FDPB[0], FDPBLength);
   end;
  finally
   TempDBParams.Free;
  end;
  repeat
    if Call(isc_attach_database(StatusVector, Length(aDBName),
                         PChar(aDBName), @FHandle,
                         FDPBLength, FDPB), False) > 0 then
    begin
      {$IFDEF UNIX}
      if IsEmbeddedServer and (Pos(':',aDBName) = 0) then
      begin
        status_vector := StatusVector;
        IBErrorCode := StatusVectorArray[1];
        sqlcode := isc_sqlcode(StatusVector);

        if ((sqlcode = -901) and (IBErrorCode = 335544382)) {Access permissions on firebird temp}
           or
           ((sqlcode = -902) and (IBErrorCode = 335544373)) {Security DB Problem}
           then
           begin
             aDBName := 'localhost:' + aDBName;
             Continue;
           end;
      end;
      {$ENDIF}
      FHandle := nil;
      IBDataBaseError;
    end;
  until FHandle <> nil;
  if not (csDesigning in ComponentState) then
    FDBName := aDBName; {Synchronise at run time}
  FDBSQLDialect := GetDBSQLDialect;
  ValidateClientSQLDialect;
  for i := 0 to FSQLObjects.Count - 1 do
  begin
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoAfterDatabaseConnect;
  end;
  if not (csDesigning in ComponentState) then
    MonitorHook.DBConnect(Self);
  LoadCharSetInfo;
end;

 procedure TIBDataBase.RemoveSQLObject(Idx: Integer);
var
  ds: TIBBase;
begin
  if (Idx >= 0) and (FSQLObjects[Idx] <> nil) then
  begin
    ds := SQLObjects[Idx];
    FSQLObjects[Idx] := nil;
    ds.Database := nil;
    if (ds.owner is TDataSet) then
      FDataSets.Remove(TDataSet(ds.Owner));
  end;
end;

 procedure TIBDataBase.RemoveSQLObjects;
var
  i: Integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
  begin
    RemoveSQLObject(i);
    if (TIBBase(FSQLObjects[i]).owner is TDataSet) then
      FDataSets.Remove(TDataSet(TIBBase(FSQLObjects[i]).owner));
  end;
end;

 procedure TIBDataBase.RemoveTransaction(Idx: Integer);
var
  TR: TIBTransaction;
begin
  if ((Idx >= 0) and (FTransactions[Idx] <> nil)) then
  begin
    TR := Transactions[Idx];
    FTransactions[Idx] := nil;
    TR.RemoveDatabase(TR.FindDatabase(Self));
    if TR = FDefaultTransaction then
      FDefaultTransaction := nil;
  end;
end;

 procedure TIBDataBase.RemoveTransactions;
var
  i: Integer;
begin
  for i := 0 to FTransactions.Count - 1 do if FTransactions[i] <> nil then
    RemoveTransaction(i);
end;

 procedure TIBDataBase.SetDatabaseName( const Value: TIBFileName);
begin
  if FDBName <> Value then
  begin
    EnsureInactive;
    CheckInactive;
    FDBName := Value;
  end;
end;

 procedure TIBDataBase.SetDBParamByDPB( const Idx: Integer; Value: String);
var
  ConstIdx: Integer;
begin
  ConstIdx := IndexOfDBConst(DPBConstantNames[Idx]);
  if (Value = '') then
  begin
    if ConstIdx <> -1 then
      Params.Delete(ConstIdx);
  end
  else
  begin
    if (ConstIdx = -1) then
      Params.Add(DPBConstantNames[Idx] + '=' + Value)
    else
      Params[ConstIdx] := DPBConstantNames[Idx] + '=' + Value;
  end;
end;

 procedure TIBDataBase.SetDBParams(Value: TStrings);
begin
  FDBParams.Assign(Value);
end;

 procedure TIBDataBase.SetDefaultTransaction(Value: TIBTransaction);
var
  i: Integer;
begin
  if (FDefaultTransaction <> nil) and (FDefaultTransaction <> Value) then
  begin
    i := FindTransaction(FDefaultTransaction);
    if (i <> -1) and (FDefaultTransaction.DefaultDatabase <> self) then
      RemoveTransaction(i);
  end;
  if (Value <> nil) and (FDefaultTransaction <> Value) then
  begin
    Value.AddDatabase(Self);
    AddTransaction(Value);
  end;
  FDefaultTransaction := Value;
end;

 procedure TIBDataBase.SetHandle(Value: TISC_DB_HANDLE);
begin
  if HandleIsShared then
    Close
  else
    CheckInactive;
  FHandle := Value;
  FHandleIsShared := (Value <> nil);
end;

 procedure TIBDataBase.SetIdleTimer(Value: Integer);
begin
  if Value < 0 then
    IBError(ibxeTimeoutNegative, [nil])
  else
    if (Value = 0) then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := 0;
    end
    else
      if (Value > 0) then
      begin
        FTimer.Interval := Value;
        if not (csDesigning in ComponentState) then
          FTimer.Enabled := True;
      end;
end;

 function TIBDataBase.TestConnected: Boolean;
var
  DatabaseInfo: TIBDatabaseInfo;
begin
  result := Connected;
  if result then
  begin
    DatabaseInfo := TIBDatabaseInfo.Create(self);
    try
      DatabaseInfo.Database := self;
      { poke the server to see if connected }
      if DatabaseInfo.BaseLevel = 0 then ;
      DatabaseInfo.Free;
    except
      ForceClose;
      result := False;
      DatabaseInfo.Free;
    end;
  end;
end;

 procedure TIBDataBase.TimeoutConnection(Sender: TObject);
begin
  if Connected then
  begin
    if FCanTimeout then
    begin
      ForceClose;
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
    else
      FCanTimeout := True;
  end;
end;

 function TIBDataBase.GetIsReadOnly: Boolean;
var
  DatabaseInfo: TIBDatabaseInfo;
begin
  DatabaseInfo := TIBDatabaseInfo.Create(self);
  DatabaseInfo.Database := self;
  if (DatabaseInfo.ODSMajorVersion < 10) then
    result := false
  else
  begin
    if (DatabaseInfo.ReadOnly = 0) then
      result := false
    else
      result := true;
  end;
  DatabaseInfo.Free;
end;

 function TIBDataBase.GetSQLDialect: Integer;
begin
  Result := FSQLDialect;
end;


 procedure TIBDataBase.SetSQLDialect( const Value: Integer);
begin
  if (Value < 1) then IBError(ibxeSQLDialectInvalid, [nil]);
  if ((FHandle = nil) or (Value <= FDBSQLDialect))  then
    FSQLDialect := Value
  else
    IBError(ibxeSQLDialectInvalid, [nil]);
end;

 function TIBDataBase.GetDBSQLDialect: Integer;
var
  DatabaseInfo: TIBDatabaseInfo;
begin
  DatabaseInfo := TIBDatabaseInfo.Create(self);
  DatabaseInfo.Database := self;
  result := DatabaseInfo.DBSQLDialect;
  DatabaseInfo.Free;
end;

 procedure TIBDataBase.ValidateClientSQLDialect;
begin
  if (FDBSQLDialect < FSQLDialect) then
  begin
    FSQLDialect := FDBSQLDialect;
    if Assigned (FOnDialectDowngradeWarning) then
      FOnDialectDowngradeWarning(self);
  end;
end;

 procedure TIBDataBase.ApplyUpdates( const DataSets: array of TDataSet);
var
  I: Integer;
  DS: TIBCustomDataSet;
  TR: TIBTransaction;
begin
  TR := nil;
  for I := 0 to High(DataSets) do
  begin
    DS := TIBCustomDataSet(DataSets[I]);
    if DS.Database <> Self then
      IBError(ibxeUpdateWrongDB, [nil]);
    if TR = nil then
      TR := DS.Transaction;
    if (DS.Transaction <> TR) or (TR = nil) then
      IBError(ibxeUpdateWrongTR, [nil]);
  end;
  TR.CheckInTransaction;
  for I := 0 to High(DataSets) do
  begin
    DS := TIBCustomDataSet(DataSets[I]);
    DS.ApplyUpdates;
  end;
  TR.CommitRetaining;
end;

 procedure TIBDataBase.CloseDataSets;
var
  i: Integer;
begin
  for i := 0 to DataSetCount - 1 do
    if (DataSets[i] <> nil) then
      DataSets[i].close;
end;

 function TIBDataBase.GetDataset(Index: longint): TDataset;
begin
  if (Index >= 0) and (Index < FDataSets.Count) then
    Result := TDataSet(FDataSets[Index])
  else
    raise Exception.Create('Invalid Index to DataSets');
end;

 function TIBDataBase.GetDataSetCount: Longint;
begin
  Result := FDataSets.Count;
end;

procedure TIBDataBase.ReadState(Reader: TReader);
begin
  FDBParams.Clear;
  inherited ReadState(Reader);
end;

procedure TIBDataBase.SetConnected(Value: boolean);
begin
  if StreamedConnected and not AllowStreamedConnected then
  begin
    StreamedConnected := false;
    Value := false
  end;
  inherited SetConnected(Value);
end;

 procedure TIBDataBase.GetFieldNames( const TableName: string; List: TStrings);
var
  Query: TIBSQL;
begin
  if TableName = '' then
    IBError(ibxeNoTableName, [nil]);
  if not Connected then
    Open;
  if not FInternalTransaction.Active then
    FInternalTransaction.StartTransaction;
  Query := TIBSQL.Create(self);
  try
    Query.GoToFirstRecordOnExecute := False;
    Query.Database := Self;
    Query.Transaction := FInternalTransaction;
    Query.SQL.Text := 'Select R.RDB$FIELD_NAME ' + {do not localize}
      'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
      'where R.RDB$RELATION_NAME = ' + {do not localize}
      '''' +
      FormatIdentifierValue(SQLDialect, TableName) +
      ''' ' +
      'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '; {do not localize}
    Query.Prepare;
    Query.ExecQuery;
    with List do
    begin
      BeginUpdate;
      try
        Clear;
        while (not Query.EOF) and (Query.Next <> nil) do
          List.Add(TrimRight(Query.Current.ByName('RDB$FIELD_NAME').AsString)); {do not localize}
      finally
        EndUpdate;
      end;
    end;
  finally
    Query.free;
    FInternalTransaction.Commit;
  end;
end;

 procedure TIBDataBase.GetTableNames(List: TStrings; SystemTables: Boolean);
var
  Query : TIBSQL;
begin
  if not (csReading in ComponentState) then
  begin
    if not Connected then
      Open;
    if not FInternalTransaction.Active then
      FInternalTransaction.StartTransaction;
    Query := TIBSQL.Create(self);
    try
      Query.GoToFirstRecordOnExecute := False;
      Query.Database := Self;
      Query.Transaction := FInternalTransaction;
      if SystemTables then
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' + {do not localize}
                          ' where RDB$VIEW_BLR is NULL' {do not localize}
      else
        Query.SQL.Text := 'Select RDB$RELATION_NAME from RDB$RELATIONS' + {do not localize}
                          ' where RDB$VIEW_BLR is NULL and RDB$SYSTEM_FLAG = 0'; {do not localize}
      Query.Prepare;
      Query.ExecQuery;
      with List do
      begin
        BeginUpdate;
        try
          Clear;
          while (not Query.EOF) and (Query.Next <> nil) do
            List.Add(TrimRight(Query.Current[0].AsString));
        finally
          EndUpdate;
        end;
      end;
    finally
      Query.Free;
      FInternalTransaction.Commit;
    end;
  end;
end;

{ TIBTransaction }

constructor TIBTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  CheckIBLoaded;
  FDatabases := TList.Create;
  FSQLObjects := TList.Create;
  FHandle := nil;
  FTPB := nil;
  FTPBLength := 0;
  FTRParams := TStringList.Create;
  FTRParamsChanged := True;
  TStringList(FTRParams).OnChange := TRParamsChange;
  TStringList(FTRParams).OnChanging := TRParamsChanging;
  FTimer := TFPTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutTransaction;
  FDefaultAction := taCommit;
end;

destructor TIBTransaction.Destroy;
var
  i: Integer;
begin
  if FIBLoaded then
  begin
    if InTransaction then
      EndTransaction(FDefaultAction, True);
    for i := 0 to FSQLObjects.Count - 1 do
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoTransactionFree;
    RemoveSQLObjects;
    RemoveDatabases;
    FreeMem(FTPB);
    FTPB := nil;
    FTRParams.Free;
    FSQLObjects.Free;
    FDatabases.Free;
  end;
  inherited Destroy;
end;

function TIBTransaction.Call(ErrCode: ISC_STATUS;
  RaiseError: Boolean): ISC_STATUS;
var
  i: Integer;
begin
  result := ErrCode;
  for i := 0 to FDatabases.Count - 1 do if FDatabases[i] <> nil then
    Databases[i].FCanTimeout := False;
  FCanTimeout := False;
  if RaiseError and (result > 0) then
    IBDataBaseError;
end;

procedure TIBTransaction.CheckDatabasesInList;
begin
  if GetDatabaseCount = 0 then
    IBError(ibxeNoDatabasesInTransaction, [nil]);
end;

procedure TIBTransaction.CheckInTransaction;
begin
  if FStreamedActive and (not InTransaction) then
    Loaded;
  if (FHandle = nil) then
    IBError(ibxeNotInTransaction, [nil]);
end;

procedure TIBTransaction.DoBeforeTransactionEnd;
begin
  if Assigned(FBeforeTransactionEnd) then
    FBeforeTransactionEnd(self);
end;

procedure TIBTransaction.DoAfterTransactionEnd;
begin
  if Assigned(FAfterTransactionEnd) then
    FAfterTransactionEnd(self);
end;

procedure TIBTransaction.DoOnStartTransaction;
begin
  if assigned(FOnStartTransaction) then
    OnStartTransaction(self);
end;

procedure TIBTransaction.DoAfterExecQuery(Sender: TObject);
begin
  if assigned(FAfterExecQuery) then
    AfterExecQuery(Sender);
end;

procedure TIBTransaction.DoAfterEdit(Sender: TObject);
begin
  if assigned(FAfterEdit) then
    AfterEdit(Sender);
end;

procedure TIBTransaction.DoAfterDelete(Sender: TObject);
begin
  if assigned(FAfterDelete) then
    AfterDelete(Sender);
end;

procedure TIBTransaction.DoAfterInsert(Sender: TObject);
begin
  if assigned(FAfterInsert) then
    AfterInsert(Sender);
end;

procedure TIBTransaction.DoAfterPost(Sender: TObject);
begin
  if assigned(FAfterPost) then
    AfterPost(Sender);
end;

procedure TIBTransaction.EnsureNotInTransaction;
begin
  if csDesigning in ComponentState then
  begin
    if FHandle <> nil then
      Rollback;
  end;
end;

procedure TIBTransaction.CheckNotInTransaction;
begin
  if (FHandle <> nil) then
    IBError(ibxeInTransaction, [nil]);
end;

function TIBTransaction.AddDatabase(db: TIBDatabase): Integer;
var
  i: Integer;
  NilFound: Boolean;
begin
  i := FindDatabase(db);
  if i <> -1 then
  begin
    result := i;
    exit;
  end;
  NilFound := False;
  i := 0;
  while (not NilFound) and (i < FDatabases.Count) do
  begin
    NilFound := (FDatabases[i] = nil);
    if (not NilFound) then
      Inc(i);
  end;
  if (NilFound) then
  begin
    FDatabases[i] := db;
    result := i;
  end
  else
  begin
    result := FDatabases.Count;
    FDatabases.Add(db);
  end;
end;

function TIBTransaction.AddSQLObject(ds: TIBBase): Integer;
begin
  result := 0;
  while (result < FSQLObjects.Count) and (FSQLObjects[result] <> nil) do
    Inc(result);
  if (result = FSQLObjects.Count) then
    FSQLObjects.Add(ds)
  else
    FSQLObjects[result] := ds;
end;

procedure TIBTransaction.Commit;
begin
  EndTransaction(TACommit, False);
end;

procedure TIBTransaction.CommitRetaining;
begin
  EndTransaction(TACommitRetaining, False);
end;

procedure TIBTransaction.EndTransaction(Action: TTransactionAction;
  Force: Boolean);
var
  status: ISC_STATUS;
  i: Integer;
begin
  CheckInTransaction;
  if FInEndTransaction then Exit;
  FInEndTransaction := true;
  FEndAction := Action;
  try
  case Action of
    TARollback, TACommit:
    begin
      if (HandleIsShared) and
         (Action <> FDefaultAction) and
         (not Force) then
        IBError(ibxeCantEndSharedTransaction, [nil]);
      DoBeforeTransactionEnd;
      for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
        SQLObjects[i].DoBeforeTransactionEnd(Action);
      if InTransaction then
      begin
        if HandleIsShared then
        begin
          FHandle := nil;
          FHandleIsShared := False;
          status := 0;
        end
        else
          if (Action = TARollback) then
            status := Call(isc_rollback_transaction(StatusVector, @FHandle), False)
          else
            status := Call(isc_commit_transaction(StatusVector, @FHandle), False);
        if ((Force) and (status > 0)) then
          status := Call(isc_rollback_transaction(StatusVector, @FHandle), False);
        if Force then
          FHandle := nil
        else
          if (status > 0) then
            IBDataBaseError;
        for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
          SQLObjects[i].DoAfterTransactionEnd;
        DoAfterTransactionEnd;
      end;
    end;
    TACommitRetaining:
      Call(isc_commit_retaining(StatusVector, @FHandle), True);
    TARollbackRetaining:
      Call(isc_rollback_retaining(StatusVector, @FHandle), True);
  end;
  if not (csDesigning in ComponentState) then
  begin
    case Action of
      TACommit:
        MonitorHook.TRCommit(Self);
      TARollback:
        MonitorHook.TRRollback(Self);
      TACommitRetaining:
        MonitorHook.TRCommitRetaining(Self);
      TARollbackRetaining:
        MonitorHook.TRRollbackRetaining(Self);
    end;
  end;
  finally
    FInEndTransaction := false
  end;
end;

function TIBTransaction.GetDatabase(Index: Integer): TIBDatabase;
begin
  result := FDatabases[Index];
end;

function TIBTransaction.GetDatabaseCount: Integer;
var
  i, Cnt: Integer;
begin
  result := 0;
  Cnt := FDatabases.Count - 1;
  for i := 0 to Cnt do if FDatabases[i] <> nil then
    Inc(result);
end;

function TIBTransaction.GetSQLObject(Index: Integer): TIBBase;
begin
  result := FSQLObjects[Index];
end;

function TIBTransaction.GetSQLObjectCount: Integer;
var
  i, Cnt: Integer;
begin
  result := 0;
  Cnt := FSQLObjects.Count - 1;
  for i := 0 to Cnt do if FSQLObjects[i] <> nil then
    Inc(result);
end;

function TIBTransaction.GetInTransaction: Boolean;
begin
  result := (FHandle <> nil);
end;

function TIBTransaction.FindDatabase(db: TIBDatabase): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FDatabases.Count - 1 do
    if db = TIBDatabase(FDatabases[i]) then
    begin
      result := i;
      break;
    end;
end;

function TIBTransaction.FindDefaultDatabase: TIBDatabase;
var
  i: Integer;
begin
  result := FDefaultDatabase;
  if result = nil then
  begin
    for i := 0 to FDatabases.Count - 1 do
      if (TIBDatabase(FDatabases[i]) <> nil) and
        (TIBDatabase(FDatabases[i]).DefaultTransaction = self) then
      begin
        result := TIBDatabase(FDatabases[i]);
        break;
      end;
  end;
end;

function TIBTransaction.GetEndAction: TTransactionAction;
begin
  if FInEndTransaction then
     Result := FEndAction
  else
     IBError(ibxeIB60feature, [nil])
end;


function TIBTransaction.GetIdleTimer: Integer;
begin
  result := FTimer.Interval;
end;

procedure TIBTransaction.Loaded;
begin
  inherited Loaded;
end;

procedure TIBTransaction.BeforeDatabaseDisconnect(DB: TIBDatabase);
begin
  if InTransaction then
    EndTransaction(FDefaultAction, True);
end;

procedure TIBTransaction.RemoveDatabase(Idx: Integer);
var
  DB: TIBDatabase;
begin
  if ((Idx >= 0) and (FDatabases[Idx] <> nil)) then
  begin
    DB := Databases[Idx];
    FDatabases[Idx] := nil;
    DB.RemoveTransaction(DB.FindTransaction(Self));
    if DB = FDefaultDatabase then
      FDefaultDatabase := nil;
  end;
end;

procedure TIBTransaction.RemoveDatabases;
var
  i: Integer;
begin
  for i := 0 to FDatabases.Count - 1 do if FDatabases[i] <> nil then
    RemoveDatabase(i);
end;

procedure TIBTransaction.RemoveSQLObject(Idx: Integer);
var
  ds: TIBBase;
begin
  if ((Idx >= 0) and (FSQLObjects[Idx] <> nil)) then
  begin
    ds := SQLObjects[Idx];
    FSQLObjects[Idx] := nil;
    ds.Transaction := nil;
  end;
end;

procedure TIBTransaction.RemoveSQLObjects;
var
  i: Integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
    RemoveSQLObject(i);
end;

procedure TIBTransaction.Rollback;
begin
  EndTransaction(TARollback, False);
end;

procedure TIBTransaction.RollbackRetaining;
begin
  EndTransaction(TARollbackRetaining, False);
end;

procedure TIBTransaction.SetActive(Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value and not InTransaction then
      StartTransaction
    else
      if not Value and InTransaction then
        Rollback;
end;

procedure TIBTransaction.SetDefaultAction(Value: TTransactionAction);
begin
(*  if (Value = taRollbackRetaining) and (GetIBClientVersion < 6) then
    IBError(ibxeIB60feature, [nil]);*)
  FDefaultAction := Value;
end;

procedure TIBTransaction.SetDefaultDatabase(Value: TIBDatabase);
var
  i: integer;
begin
  if (FDefaultDatabase <> nil) and (FDefaultDatabase <> Value) then
  begin
    i := FDefaultDatabase.FindTransaction(self);
    if (i <> -1) then
      FDefaultDatabase.RemoveTransaction(i);
  end;
  if (Value <> nil) and (FDefaultDatabase <> Value) then
  begin
    Value.AddTransaction(Self);
    AddDatabase(Value);
    for i := 0 to FSQLObjects.Count - 1 do
      if (FSQLObjects[i] <> nil) and
         (TIBBase(FSQLObjects[i]).Database = nil) then
         SetObjectProp(TIBBase(FSQLObjects[i]).Owner, 'Database', Value);
  end;
  FDefaultDatabase := Value;
end;

procedure TIBTransaction.SetHandle(Value: TISC_TR_HANDLE);
begin
  if (HandleIsShared) then
    EndTransaction(DefaultAction, True)
  else
    CheckNotInTransaction;
  FHandle := Value;
  FHandleIsShared := (Value <> nil);
end;

procedure TIBTransaction.Notification( AComponent: TComponent;
                                        Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification( AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDefaultDatabase) then
  begin
    i := FindDatabase(FDefaultDatabase);
    if (i <> -1) then
      RemoveDatabase(i);
    FDefaultDatabase := nil;
  end;
end;

procedure TIBTransaction.SetIdleTimer(Value: Integer);
begin
  if Value < 0 then
    IBError(ibxeTimeoutNegative, [nil])
  else
    if (Value = 0) then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := 0;
    end
    else
      if (Value > 0) then
      begin
        FTimer.Interval := Value;
        if not (csDesigning in ComponentState) then
          FTimer.Enabled := True;
      end;
end;

procedure TIBTransaction.SetTRParams(Value: TStrings);
begin
  FTRParams.Assign(Value);
end;

procedure TIBTransaction.StartTransaction;
var
  pteb: PISC_TEB_ARRAY;
  TPB: String;
  i: Integer;
begin
  CheckNotInTransaction;
  CheckDatabasesInList;
  for i := 0 to FDatabases.Count - 1 do
   if  FDatabases[i] <> nil then
   begin
     with TIBDatabase(FDatabases[i]) do
     if not Connected then
       if FStreamedConnected then
       begin
         Open;
         FStreamedConnected := False;
       end
       else
         IBError(ibxeDatabaseClosed, [nil]);
   end;
  if FTRParamsChanged then
  begin
    FTRParamsChanged := False;
    GenerateTPB(FTRParams, TPB, FTPBLength);
    if FTPBLength > 0 then
    begin
      IBAlloc(FTPB, 0, FTPBLength);
      Move(TPB[1], FTPB[0], FTPBLength);
    end;
  end;

  pteb := nil;
  IBAlloc(pteb, 0, DatabaseCount * SizeOf(TISC_TEB));
  try
    for i := 0 to DatabaseCount - 1 do if Databases[i] <> nil then
    begin
      pteb^[i].db_handle := @(Databases[i].Handle);
      pteb^[i].tpb_length := FTPBLength;
      pteb^[i].tpb_address := FTPB;
    end;
    if Call(isc_start_multiple(StatusVector, @FHandle,
                               DatabaseCount, PISC_TEB(pteb)), False) > 0 then
    begin
      FHandle := nil;
      IBDataBaseError;
    end;
    if not (csDesigning in ComponentState) then
      MonitorHook.TRStart(Self);
  finally
    FreeMem(pteb);
  end;
  DoOnStartTransaction;
end;

procedure TIBTransaction.TimeoutTransaction(Sender: TObject);
begin
  if InTransaction then
  begin
    if FCanTimeout then
    begin
      EndTransaction(FDefaultAction, True);
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
    else
      FCanTimeout := True;
  end;
end;

procedure TIBTransaction.TRParamsChange(Sender: TObject);
begin
  FTRParamsChanged := True;
end;

procedure TIBTransaction.TRParamsChanging(Sender: TObject);
begin
  EnsureNotInTransaction;
  CheckNotInTransaction;
end;

{ TIBBase }
constructor TIBBase.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

destructor TIBBase.Destroy;
begin
  SetDatabase(nil);
  SetTransaction(nil);
  inherited Destroy;
end;

function TIBBase.GetCharSetSize(CharSetID: integer): integer;
begin
  if (CharSetID >= 0) and (CharSetID < Length(Database.FCharSetSizes)) then
    Result := Database.FCharSetSizes[CharSetID]
  else
    Result := 1; {Unknown character set}
end;

function TIBBase.GetDefaultCharSetSize: integer;
var DefaultCharSetName: string;
    i: integer;
begin
  DefaultCharSetName := GetDefaultCharSetName;
  Result := 4; {worse case}
  for i := 0 to Length(Database.FCharSetSizes) - 1 do
    if Database.FCharSetNames[i] = DefaultCharSetName then
    begin
      Result := Database.FCharSetSizes[i];
      break;
    end;
end;

function TIBBase.GetCharSetName(CharSetID: integer): string;
begin
  if (CharSetID >= 0) and (CharSetID < Length(Database.FCharSetNames)) then
    Result := Database.FCharSetNames[CharSetID]
  else
    Result := ''; {Unknown character set}
end;

function TIBBase.GetDefaultCharSetName: RawByteString;
begin
  Result := Database.FDefaultCharSetName;
end;

{$IFDEF HAS_ANSISTRING_CODEPAGE}
function TIBBase.GetCodePage(CharSetID: integer): TSystemCodePage;
begin
  if (CharSetID >= 0) and (CharSetID < Length(Database.FCodePages)) then
    Result := Database.FCodePages[CharSetID]
  else
    Result := CP_NONE; {Unknown character set}
end;

function TIBBase.GetDefaultCodePage: TSystemCodePage;
begin
  Result := Database.FDefaultCodePage;
end;

{$ENDIF}

procedure TIBBase.HandleException(Sender: TObject);
begin
  if assigned(Database) then
     Database.HandleException(Sender)
  else
     SysUtils.ShowException(ExceptObject,ExceptAddr);
end;

procedure TIBBase.SetCursor;
begin
  if Assigned(Database) and not Database.SQLHourGlass then
     Exit;
  if assigned(IBGUIInterface) then
     IBGUIInterface.SetCursor;
end;

procedure TIBBase.RestoreCursor;
begin
  if Assigned(Database) and not Database.SQLHourGlass then
     Exit;
  if assigned(IBGUIInterface) then
     IBGUIInterface.RestoreCursor;
end;

procedure TIBBase.CheckDatabase;
begin
  if (FDatabase = nil) then
    IBError(ibxeDatabaseNotAssigned, [nil]);
  FDatabase.CheckActive;
end;

procedure TIBBase.CheckTransaction;
begin
  if FTransaction = nil then
    IBError(ibxeTransactionNotAssigned, [nil]);
  FTransaction.CheckInTransaction;
end;

function TIBBase.GetDBHandle: PISC_DB_HANDLE;
begin
  CheckDatabase;
  result := @FDatabase.Handle;
end;

function TIBBase.GetTRHandle: PISC_TR_HANDLE;
begin
  CheckTransaction;
  result := @FTransaction.Handle;
end;

procedure TIBBase.DoBeforeDatabaseConnect(DBParams: TStrings; var DBName: string
  );
begin
  if assigned(FBeforeDatabaseConnect) then
    BeforeDatabaseConnect(self,DBParams,DBName);
end;

procedure TIBBase.DoAfterDatabaseConnect;
begin
  if assigned(FAfterDatabaseConnect) then
    AfterDatabaseConnect(self);
end;

procedure TIBBase.DoBeforeDatabaseDisconnect;
begin
  if Assigned(BeforeDatabaseDisconnect) then
    BeforeDatabaseDisconnect(Self);
end;

procedure TIBBase.DoAfterDatabaseDisconnect;
begin
  if Assigned(AfterDatabaseDisconnect) then
    AfterDatabaseDisconnect(Self);
end;

procedure TIBBase.DoDatabaseFree;
begin
  if Assigned(OnDatabaseFree) then
    OnDatabaseFree(Self);
  SetDatabase(nil);
  SetTransaction(nil);
end;

procedure TIBBase.DoBeforeTransactionEnd(Action: TTransactionAction);
begin
  if Assigned(BeforeTransactionEnd) then
    BeforeTransactionEnd(Self,Action);
end;

procedure TIBBase.DoAfterTransactionEnd;
begin
  if Assigned(AfterTransactionEnd) then
    AfterTransactionEnd(Self);
end;

procedure TIBBase.DoTransactionFree;
begin
  if Assigned(OnTransactionFree) then
    OnTransactionFree(Self);
  FTransaction := nil;
end;

procedure TIBBase.DoAfterExecQuery(Sender: TObject);
begin
  if FTransaction <> nil then
    FTransaction.DoAfterExecQuery(Sender);
end;

procedure TIBBase.DoAfterEdit(Sender: TObject);
begin
  if FTransaction <> nil then
    FTransaction.DoAfterEdit(Sender);
end;

procedure TIBBase.DoAfterDelete(Sender: TObject);
begin
  if FTransaction <> nil then
    FTransaction.DoAfterDelete(Sender);
end;

procedure TIBBase.DoAfterInsert(Sender: TObject);
begin
  if FTransaction <> nil then
    FTransaction.DoAfterInsert(Sender);
end;

procedure TIBBase.DoAfterPost(Sender: TObject);
begin
  if FTransaction <> nil then
    FTransaction.DoAfterPost(Sender);
end;

procedure TIBBase.SetDatabase(Value: TIBDatabase);
begin
  if (FDatabase <> nil) then
    FDatabase.RemoveSQLObject(FIndexInDatabase);
  FDatabase := Value;
  if (FDatabase <> nil) then
  begin
    FIndexInDatabase := FDatabase.AddSQLObject(Self);
    if (FTransaction = nil) then
      Transaction := FDatabase.FindDefaultTransaction;
  end;
end;

procedure TIBBase.SetTransaction(Value: TIBTransaction);
begin
  if (FTransaction <> nil) then
    FTransaction.RemoveSQLObject(FIndexInTransaction);
  FTransaction := Value;
  if (FTransaction <> nil) then
  begin
    FIndexInTransaction := FTransaction.AddSQLObject(Self);
    if (FDatabase = nil) then
      Database := FTransaction.FindDefaultDatabase;
  end;
end;

{ GenerateDPB -
  Given a string containing a textual representation
  of the database parameters, generate a database
  parameter buffer, and return it and its length
  in DPB and DPBLength, respectively. }

procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
var
  i, j, pval: Integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
begin
  { The DPB is initially empty, with the exception that
    the DPB version must be the first byte of the string. }
  DPBLength := 1;
  DPB := Char(isc_dpb_version1);

  {Iterate through the textual database parameters, constructing
   a DPB on-the-fly }
  for i := 0 to sl.Count - 1 do
  begin
    { Get the parameter's name and value from the list,
      and make sure that the name is all lowercase with
      no leading 'isc_dpb_' prefix
    }
    if (Trim(sl.Names[i]) = '') then
      continue;
    ParamName := LowerCase(sl.Names[i]); {mbcs ok}
    ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    if (Pos(DPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(DPBPrefix));
     { We want to translate the parameter name to some Integer
       value. We do this by scanning through a list of known
       database parameter names (DPBConstantNames, defined above) }
    DPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_dpb_last_dpb_constant do
      if (ParamName = DPBConstantNames[j]) then
      begin
        DPBVal := j;
        break;
      end;
     {  A database parameter either contains a string value (case 1)
       or an Integer value (case 2)
       or no value at all (case 3)
       or an error needs to be generated (case else)  }
    case DPBVal of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype,
      isc_dpb_sql_role_name, isc_dpb_sql_dialect:
      begin
        if DPBVal = isc_dpb_sql_dialect then
          ParamValue[1] := Char(Ord(ParamValue[1]) - 48);
        DPB := DPB +
               Char(DPBVal) +
               Char(Length(ParamValue)) +
               ParamValue;
        Inc(DPBLength, 2 + Length(ParamValue));
      end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(StrToInt(ParamValue));
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 +
               Char(isc_dpb_records);
        Inc(DPBLength, 3);
      end;
      isc_dpb_sweep_interval:
      begin
        pval := StrToInt(ParamValue);
        DPB := DPB +
               Char(DPBVal) +
               #4 +
               PChar(@pval)[0] +
               PChar(@pval)[1] +
               PChar(@pval)[2] +
               PChar(@pval)[3];
        Inc(DPBLength, 6);
      end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
      begin
        DPB := DPB +
               Char(DPBVal) +
               #1 + #0;
        Inc(DPBLength, 3);
      end;
      else
      begin
        if (DPBVal > 0) and
           (DPBVal <= isc_dpb_last_dpb_constant) then
          IBError(ibxeDPBConstantNotSupported, [DPBConstantNames[DPBVal]])
        else
          IBError(ibxeDPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

{ GenerateTPB -
  Given a string containing a textual representation
  of the transaction parameters, generate a transaction
  parameter buffer, and return it and its length in
  TPB and TPBLength, respectively. }
procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
var
  i, j, TPBVal, ParamLength: Integer;
  ParamName, ParamValue: string;
begin
  TPB := '';
  if (sl.Count = 0) then
    TPBLength := 0
  else
  begin
    TPBLength := sl.Count + 1;
    TPB := TPB + Char(isc_tpb_version3);
  end;
  for i := 0 to sl.Count - 1 do
  begin
    if (Trim(sl[i]) =  '') then
    begin
      Dec(TPBLength);
      Continue;
    end;
    if (Pos('=', sl[i]) = 0) then {mbcs ok}
      ParamName := LowerCase(sl[i]) {mbcs ok}
    else
    begin
      ParamName := LowerCase(sl.Names[i]); {mbcs ok}
      ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i])); {mbcs ok}
    end;
    if (Pos(TPBPrefix, ParamName) = 1) then {mbcs ok}
      Delete(ParamName, 1, Length(TPBPrefix));
    TPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_tpb_last_tpb_constant do
      if (ParamName = TPBConstantNames[j]) then
      begin
        TPBVal := j;
        break;
      end;
    { Now act on it }
    case TPBVal of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
      isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
      isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
      isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version:
        TPB := TPB + Char(TPBVal);
      isc_tpb_lock_read, isc_tpb_lock_write:
      begin
        TPB := TPB + Char(TPBVal);
        { Now set the string parameter }
        ParamLength := Length(ParamValue);
        Inc(TPBLength, ParamLength + 1);
        TPB := TPB + Char(ParamLength) + ParamValue;
      end;
      else
      begin
        if (TPBVal > 0) and
           (TPBVal <= isc_tpb_last_tpb_constant) then
          IBError(ibxeTPBConstantNotSupported, [TPBConstantNames[TPBVal]])
        else
          IBError(ibxeTPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

end.





