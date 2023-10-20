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
{    Associates Ltd 2011 - 2018                                               }
{                                                                        }
{************************************************************************}

unit IBDatabase;

{$Mode Delphi}

{$codepage UTF8}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, FPTimer, IBExternals, DB, IB, CustApp, IBInternals,
  syncobjs;

type
  TIBDatabase = class;
  TIBTransaction = class;
  TIBBase = class;

  TIBDatabaseLoginEvent = procedure(Database: TIBDatabase;
    LoginParams: TStrings) of object;


  TIBFileName = type string;
  { TIBDatabase }
  TIBDataBase = class(TIBXMonitoredConnection)
  private
    type TIBDatabaseCloseActions = (caNormal,caForce, caDropDatabase);
  private
    FCloseAction: TIBDatabaseCloseActions;
    FAttachment: IAttachment;
    FConfigOverrides: TStrings;
    FCreateDatabase: boolean;
    FCreateIfNotExists: boolean;
    FAllowStreamedConnected: boolean;
    FFirebirdLibraryPathName: TIBFileName;
    FHiddenPassword: string;
    FOnCreateDatabase: TNotifyEvent;
    FOnLogin: TIBDatabaseLoginEvent;
    FSQLHourGlass: Boolean;
    FSQLDialect: Integer;
    FOnDialectDowngradeWarning: TNotifyEvent;
    FSQLObjects: TList;
    FTransactions: TList;
    FDBName: TIBFileName;
    FDBParams: TStrings;
    FDBParamsChanged: Boolean;
    FOnIdleTimer: TNotifyEvent;
    FDefaultTransaction: TIBTransaction;
    FInternalTransaction: TIBTransaction;
    FTimer: TFPTimer;
    FDataSets: TList;
    FLoginCalled: boolean;
    FUseDefaultSystemCodePage: boolean;
    FUseHiddenPassword: boolean;
    FFirebirdAPI: IFirebirdAPI;
    procedure EnsureInactive;
    function GetAuthenticationMethod: string;
    function GetDBSQLDialect: Integer;
    function GetDefaultCharSetID: integer;
    function GetDefaultCharSetName: AnsiString;
    function GetDefaultCodePage: TSystemCodePage;
    function GetDPBConstantNames(index: byte): string;
    function GetFirebirdAPI: IFirebirdAPI;
    function GetRemoteProtocol: string;
    function GetSQLObjectsCount: Integer;
    function GetWireCompression: boolean;
    procedure SetAttachment(AValue: IAttachment);
    procedure SetConfigOverrides(AValue: TStrings);
    procedure SetFirebirdLibraryPathName(AValue: TIBFileName);
    procedure SetSQLDialect(const Value: Integer);
    procedure SetWireCompression(AValue: boolean);
    procedure ValidateClientSQLDialect;
    procedure DBParamsChange(Sender: TObject);
    procedure DBParamsChanging(Sender: TObject);
    function GenerateDPB(FirebirdAPI: IFirebirdAPI; sl: TStrings): IDPB;
    function GetSQLObject(Index: Integer): TIBBase;
    function GetSQLObjectCount: Integer;
    function GetIdleTimer: Integer;
    function GetTransaction(Index: Integer): TIBTransaction;
    function GetTransactionCount: Integer;
    function Login(var aDatabaseName: string): Boolean;
    procedure SetDatabaseName(const Value: TIBFileName);
    procedure SetDBParamByDPB(const Idx: byte; Value: String);
    procedure SetDBParams(Value: TStrings);
    procedure SetDefaultTransaction(Value: TIBTransaction);
    procedure SetIdleTimer(Value: Integer);
    procedure TimeoutConnection(Sender: TObject);
    function GetIsReadOnly: Boolean;
    function AddSQLObject(ds: TIBBase): Integer;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;
    procedure InternalClose;
    procedure InternalBeforeClose;
    procedure InternalAfterClose;
    procedure InternalBeforeConnect(aDBParams: TStrings; var aDBName: string;
      var aCreateIfNotExists: boolean);
    procedure InternalAfterConnect;
    procedure DoOnCreateDatabase;

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
    procedure CreateDatabase; overload;
    procedure CreateDatabase(createDatabaseSQL: string); overload;
    procedure DropDatabase;
    procedure ForceClose;
    procedure GetFieldNames(const TableName: string; List: TStrings);
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function IndexOfDBConst(st: String): Integer;
    function TestConnected: Boolean;
    procedure CheckDatabaseName;
    function AddTransaction(TR: TIBTransaction): Integer;
    function FindTransaction(TR: TIBTransaction): Integer;
    function FindDefaultTransaction(): TIBTransaction;
    procedure ReConnect;
    procedure RemoveTransaction(Idx: Integer);
    procedure RemoveTransactions;

    property Attachment: IAttachment read FAttachment write SetAttachment;
    property FirebirdAPI: IFirebirdAPI read GetFirebirdAPI;
    property DPBConstantNames[index: byte]: string read GetDPBConstantNames;
    property DBSQLDialect : Integer read GetDBSQLDialect;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property SQLObjectCount: Integer read GetSQLObjectCount; {ignores nil objects}
    property SQLObjectsCount: Integer read GetSQLObjectsCount;
    property SQLObjects[Index: Integer]: TIBBase read GetSQLObject;
    property TransactionCount: Integer read GetTransactionCount;
    property Transactions[Index: Integer]: TIBTransaction read GetTransaction;
    property InternalTransaction: TIBTransaction read FInternalTransaction;
    property DefaultCharSetName: AnsiString read GetDefaultCharSetName;
    property DefaultCharSetID: integer read GetDefaultCharSetID;
    property DefaultCodePage: TSystemCodePage read GetDefaultCodePage;
    property AuthenticationMethod: string read GetAuthenticationMethod;
    property RemoteProtocol: string read GetRemoteProtocol;

  published
    property Connected;
    property CreateIfNotExists: boolean read FCreateIfNotExists write FCreateIfNotExists;
    property AllowStreamedConnected: boolean read FAllowStreamedConnected
             write FAllowStreamedConnected;
    property DatabaseName: TIBFileName read FDBName write SetDatabaseName;
    property FirebirdLibraryPathName: TIBFileName read FFirebirdLibraryPathName
                                             write SetFirebirdLibraryPathName;
    property Params: TStrings read FDBParams write SetDBParams;
    property ConfigOverrides: TStrings read FConfigOverrides write SetConfigOverrides;
    property LoginPrompt default True;
    property DefaultTransaction: TIBTransaction read FDefaultTransaction
                                                 write SetDefaultTransaction;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer;
    property SQLDialect : Integer read FSQLDialect write SetSQLDialect default 3;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass default true;
    property TraceFlags;
    property UseDefaultSystemCodePage: boolean read FUseDefaultSystemCodePage
                                               write FUseDefaultSystemCodePage;
    property WireCompression: boolean read GetWireCompression write SetWireCompression
             stored false;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnCreateDatabase: TNotifyEvent read FOnCreateDatabase write FOnCreateDatabase;
    property OnLogin: TIBDatabaseLoginEvent read FOnLogin write FOnLogin;
    property OnIdleTimer: TNotifyEvent read FOnIdleTimer write FOnIdleTimer;
    property OnDialectDowngradeWarning: TNotifyEvent read FOnDialectDowngradeWarning
                                                     write FOnDialectDowngradeWarning;
  end;

  TDefaultEndAction = TARollback..TACommit;

  { TIBTransaction }

  TIBTransaction = class(TComponent)
  private
    class var FCriticalSection: TCriticalSection;
    class var FTransactionList: TList;
  private
    FTransactionIntf: ITransaction;
    FAfterDelete: TNotifyEvent;
    FAfterEdit: TNotifyEvent;
    FAfterExecQuery: TNotifyEvent;
    FAfterInsert: TNotifyEvent;
    FAfterPost: TNotifyEvent;
    FAfterTransactionEnd: TNotifyEvent;
    FBeforeTransactionEnd: TNotifyEvent;
    FDatabases          : TList;
    FOnStartTransaction: TNotifyEvent;
    FSQLObjects         : TList;
    FDefaultDatabase    : TIBDatabase;
    FOnIdleTimer          : TNotifyEvent;
    FStreamedActive     : Boolean;
    FTPB                : ITPB;
    FTimer              : TFPTimer;
    FDefaultAction      : TDefaultEndAction;
    FTRParams           : TStrings;
    FTRParamsChanged    : Boolean;
    FInEndTransaction   : boolean;
    FEndAction          : TTransactionAction;
    FTransactionName    : string;
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
    function GetIsReadOnly: boolean;
    function GetSQLObject(Index: Integer): TIBBase;
    function GetSQLObjectCount: Integer;
    function GetInTransaction: Boolean;
    function GetIdleTimer: Integer;
    procedure BeforeDatabaseDisconnect(DB: TIBDatabase);
    function GetTPBConstantNames(index: byte): string;
    function GetTransactionID: integer;
    procedure SetActive(Value: Boolean);
    procedure SetDefaultDatabase(Value: TIBDatabase);
    procedure SetIdleTimer(Value: Integer);
    procedure SetTransactionName(AValue: string);
    procedure SetTRParams(Value: TStrings);
    procedure TimeoutTransaction(Sender: TObject);
    procedure TRParamsChange(Sender: TObject);
    procedure TRParamsChanging(Sender: TObject);
    function AddSQLObject(ds: TIBBase): Integer;
    procedure RemoveSQLObject(Idx: Integer);
    procedure RemoveSQLObjects;
    function GenerateTPB(FirebirdAPI: IFirebirdAPI; sl: TStrings): ITPB;
  protected
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    class function FindTransactionNyName(aTransactionName: string): TIBTransaction;
    function GetEndAction: TTransactionAction;
    procedure RemoveDatabase(Idx: Integer);
    procedure RemoveDatabases;
    procedure CheckDatabasesInList;

    property DatabaseCount: Integer read GetDatabaseCount;
    property Databases[Index: Integer]: TIBDatabase read GetDatabase;
    property SQLObjectCount: Integer read GetSQLObjectCount;
    property SQLObjects[Index: Integer]: TIBBase read GetSQLObject;
    property InTransaction: Boolean read GetInTransaction;
    property TransactionIntf: ITransaction read FTransactionIntf;
    property TPB: ITPB read FTPB;
    property TPBConstantNames[index: byte]: string read GetTPBConstantNames;
    property TransactionID: integer read GetTransactionID;
    property IsReadOnly: boolean read GetIsReadOnly;
  published
    property TransactionName: string read FTransactionName write SetTransactionName;
    property Active: Boolean read GetInTransaction write SetActive;
    property DefaultDatabase: TIBDatabase read FDefaultDatabase
                                           write SetDefaultDatabase;
    property IdleTimer: Integer read GetIdleTimer write SetIdleTimer default 0;
    property DefaultAction: TDefaultEndAction read FDefaultAction write FDefaultAction default taCommit;
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
                              var DBName: string; var CreateIfNotExists: boolean) of object;

  { TIBBase }

  { Virtually all components in IB are "descendents" of TIBBase.
    It is to more easily manage the database and transaction
    connections. }
  TIBBase = class(TObject)
  private
    FOnCreateDatabase: TNotifyEvent;
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
                              var DBName: string; var CreateIfNotExists: boolean); virtual;
    procedure DoAfterDatabaseConnect; virtual;
    procedure DoBeforeDatabaseDisconnect; virtual;
    procedure DoAfterDatabaseDisconnect; virtual;
    procedure DoOnCreateDatabase; virtual;
    procedure DoDatabaseFree; virtual;
    procedure DoBeforeTransactionEnd(Action: TTransactionAction); virtual;
    procedure DoAfterTransactionEnd; virtual;
    procedure DoTransactionFree; virtual;
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
    property OnCreateDatabase: TNotifyEvent read FOnCreateDatabase
                                            write FOnCreateDatabase;
    property OnDatabaseFree: TNotifyEvent read FOnDatabaseFree write FOnDatabaseFree;
    property BeforeTransactionEnd: TTransactionEndEvent read FBeforeTransactionEnd write FBeforeTransactionEnd;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd write FAfterTransactionEnd;
    property OnTransactionFree: TNotifyEvent read FOnTransactionFree write FOnTransactionFree;
    property Database: TIBDatabase read FDatabase
                                    write SetDatabase;
    property Owner: TObject read FOwner;
    property Transaction: TIBTransaction read FTransaction
                                          write SetTransaction;
  end;


implementation

uses  IBSQLMonitor, IBCustomDataSet, IBDatabaseInfo, IBSQL, IBUtils,
     typInfo, IBMessages, IBErrorCodes {$IFDEF WINDOWS}, Windirs {$ENDIF};

{ TIBDatabase }

constructor TIBDataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoginPrompt := True;
  FSQLObjects := TList.Create;
  FTransactions := TList.Create;
  FConfigOverrides := TStringList.Create;
  FDBName := '';
  FDBParams := TStringList.Create;
  FSQLHourGlass := true;
  if (AOwner <> nil) and
     (AOwner is TCustomApplication) and
     TCustomApplication(AOWner).ConsoleApplication then
    LoginPrompt := false;
  FDBParamsChanged := True;
  TStringList(FDBParams).OnChange := DBParamsChange;
  TStringList(FDBParams).OnChanging := DBParamsChanging;
  FInternalTransaction := TIBTransaction.Create(self);
  FInternalTransaction.DefaultDatabase := Self;
  with FInternalTransaction.Params do
  begin
    Clear;
    Add('concurrency');
    Add('wait');
    Add('read');
  end;
  FTimer := TFPTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutConnection;
  FSQLDialect := 3;
  FDataSets := TList.Create;
  CheckStreamConnect;
  FCloseAction := caNormal;
end;

destructor TIBDataBase.Destroy;
var
  i: Integer;
begin
  IdleTimer := 0;
  if FAttachment <> nil then
    ForceClose;
  for i := 0 to FSQLObjects.Count - 1 do
    if FSQLObjects[i] <> nil then
      SQLObjects[i].DoDatabaseFree;
  RemoveSQLObjects;
  RemoveTransactions;
  FTimer.Free;
  FInternalTransaction.Free;
  FConfigOverrides.Free;
  FDBParams.Free;
  FSQLObjects.Free;
  FTransactions.Free;
  FDataSets.Free;
  FFirebirdAPI := nil;
  inherited Destroy;
end;

 procedure TIBDataBase.CheckActive;
begin
  if StreamedConnected and (not Connected) then
    Loaded;
  if FAttachment = nil then
    IBError(ibxeDatabaseClosed, [nil]);
end;

 procedure TIBDataBase.EnsureInactive;
begin
  if csDesigning in ComponentState then
  begin
    if FAttachment <> nil then
      Close;
  end
end;

function TIBDataBase.GetAuthenticationMethod: string;
begin
  CheckActive;
  Result := Attachment.GetAuthenticationMethod;
end;

 procedure TIBDataBase.CheckInactive;
begin
  if FAttachment <> nil then
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
    InternalClose;
end;

  procedure TIBDataBase.CreateDatabase;
begin
  CheckInactive;
  CheckDatabaseName;
  FCreateDatabase := true;
  Connected := true;
end;

procedure TIBDataBase.CreateDatabase(createDatabaseSQL: string);
begin
  CheckInactive;
  FAttachment := FirebirdAPI.CreateDatabase(createDatabaseSQL,SQLDialect);
  FDBName := Attachment.GetConnectString;
  DoOnCreateDatabase;
end;

 procedure TIBDataBase.DropDatabase;
begin
  if Connected then
  begin
    FCloseAction := caDropDatabase;
    try
      Connected := false;
    finally
      FCloseAction := caNormal;
    end;
  end;
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

  function TIBDataBase.FindDefaultTransaction(): TIBTransaction;
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

procedure TIBDataBase.ReConnect;
var OldLoginPrompt: boolean;
    OldPassword: string;
begin
  CheckActive;
  if FHiddenPassword <> '' then
  begin
    OldLoginPrompt := LoginPrompt;
    OldPassword := FHiddenPassword;
    LoginPrompt := false;
    FUseHiddenPassword := true;
    try
      Connected := false;
      FHiddenPassword := OldPassword;
      Connected := true;
    finally
      LoginPrompt := OldLoginPrompt;
      FUseHiddenPassword := false;
    end;
  end
  else
  begin
    Connected := false;
    Connected := true;
  end;
end;

 procedure TIBDataBase.ForceClose;
begin
  if Connected then
   begin
     FCloseAction := caForce;
     try
       Connected := false;
     finally
       FCloseAction := caNormal;
     end;
   end;
end;

 function TIBDataBase.GetConnected: Boolean;
begin
  result := (FAttachment <> nil) and FAttachment.IsConnected;
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

procedure TIBDataBase.InternalClose;
begin
  CheckActive;
  InternalBeforeClose;
  case FCloseAction of
  caNormal:
    FAttachment.Disconnect(false);
  caForce:
    FAttachment.Disconnect(true);
  caDropDatabase:
    FAttachment.DropDatabase;
  end;
  FAttachment := nil;
  FHiddenPassword := '';
  FCloseAction := caNormal;

  if not (csDesigning in ComponentState) then
    MonitorHook.DBDisconnect(Self);

  InternalAfterClose;
end;

procedure TIBDataBase.InternalBeforeClose;
var
  i: Integer;
begin
  { Tell all connected transactions that we're disconnecting.
    This is so transactions can commit/rollback, accordingly
  }
  for i := 0 to FTransactions.Count - 1 do
  begin
    try
      if FTransactions[i] <> nil then
        Transactions[i].BeforeDatabaseDisconnect(Self);
    except
      if FCloseAction <> caForce then
        raise;
    end;
  end;
  for i := 0 to FSQLObjects.Count - 1 do
  begin
    try
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoBeforeDatabaseDisconnect;
    except
      if FCloseAction <> caForce then
        raise;
    end;
  end;

end;

procedure TIBDataBase.InternalAfterClose;
var
  i: Integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do
    if FSQLObjects[i] <> nil then
      SQLObjects[i].DoAfterDatabaseDisconnect;
end;

procedure TIBDataBase.InternalBeforeConnect(aDBParams: TStrings; var aDBName: string;
                          var aCreateIfNotExists: boolean);
var i: integer;
begin
  {Opportunity to override defaults}
  for i := 0 to FSQLObjects.Count - 1 do
  begin
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoBeforeDatabaseConnect(aDBParams,aDBName, aCreateIfNotExists);
  end;
end;

procedure TIBDataBase.InternalAfterConnect;
var i: integer;
begin
  for i := 0 to FSQLObjects.Count - 1 do
  begin
      if FSQLObjects[i] <> nil then
        SQLObjects[i].DoAfterDatabaseConnect;
  end;
end;

 procedure TIBDataBase.DoOnCreateDatabase;
 var i: integer;
 begin
   for i := 0 to FSQLObjects.Count - 1 do
   begin
       if FSQLObjects[i] <> nil then
         SQLObjects[i].DoOnCreateDatabase;
   end;
   if assigned(FOnCreateDatabase) and (FAttachment <> nil) then
     OnCreateDatabase(self);
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
      StreamedConnected := False;
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
    IndexAt: Integer;
  begin
    IndexAt := Params.IndexOfName('password');
    if IndexAt <> -1 then
    begin
      FHiddenPassword := Params.ValueFromIndex[IndexAt];
      Params.Delete(IndexAt);
    end;
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
      aDatabaseName := aDatabaseName;
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
      Username := Params.ValueFromIndex[IndexOfUser];
    IndexOfPassword := IndexOfDBConst(DPBConstantNames[isc_dpb_password]);
    if IndexOfPassword <> -1 then
    begin
      Password := Params.ValueFromIndex[IndexOfPassword];
      OldPassword := password;
    end;

    result := IBGUIInterface.LoginDialogEx(aDatabaseName, Username, Password, False);
    if result then
    begin
      if Username <> '' then
      begin
        if IndexOfUser = -1 then
          Params.Values[DPBConstantNames[isc_dpb_user_name]] := Username
        else
          Params.ValueFromIndex[IndexOfUser] := Username;
      end
      else
      if IndexOfUser <> -1 then
        Params.Delete(IndexOfUser);
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

  function ExpandDBName(aDBName: string): string;
  const
    TmpPrefix = '$TEMP$';
    DataPrefix = '$DATADIR$';
  var
    LocalDirName: string;
  begin
    if Pos(TmpPrefix,aDBName) = 1 then
    begin
      system.Delete(aDBName,1,Length(TmpPrefix));
      Result := GetTempDir + aDBName
    end
    else
    if Pos(DataPrefix,aDBName) = 1 then
    begin
      system.Delete(aDBName,1,Length(DataPrefix));
      if Sysutils.VendorName <> '' then
        LocalDirName :=  Sysutils.VendorName
      else
        LocalDirName :=  'IBX';
      {$IFDEF UNIX}
      LocalDirName := GetUserDir + '.' + LocalDirName;
      {$ENDIF}
      {$IFDEF WINDOWS}
      LocalDirName := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA) + LocalDirName;
      {$ENDIF}
      CreateDir(LocalDirName);
      Result := LocalDirName + DirectorySeparator + aDBName;
    end
    else
      Result := aDBName;
  end;

var
  TempDBParams: TStrings;
  aDBName, oldDBName: string;
  Status: IStatus;
  CharSetID: integer;
  CharSetName: AnsiString;
  DPB: IDPB;
  PW: IDPBItem;
  aCreateIfNotExists: boolean;
begin
  DPB := nil;
  FCloseAction := caNormal;
  CheckInactive;
  CheckDatabaseName;
  if (not LoginPrompt) and (FHiddenPassword <> '') and not FUseHiddenPassword then
  begin
    FHiddenPassword := '';
    FDBParamsChanged := True;
  end;
  { Use builtin login prompt if requested }
  aDBName := ExpandDBName(FDBName);

  oldDBName := FDBName;
  if (LoginPrompt or (csDesigning in ComponentState)) and not Login(aDBName) then
    IBError(ibxeOperationCancelled, [nil]);
  if oldDBName <> FDBName then {user login dialog changed DatabaseName}
    aDBName := ExpandDBName(FDBName)
  else
    aDBName := ExpandDBName(aDBName); {in case built-in dialog changed aDBName}

  aCreateIfNotExists := CreateIfNotExists;
  TempDBParams := TStringList.Create;
  try
   TempDBParams.Assign(FDBParams);
   {$ifdef UNIX}
   {See below for WINDOWS UseDefaultSystemCodePage}
   if UseDefaultSystemCodePage then
     TempDBParams.Values['lc_ctype'] :='UTF8';
   {$endif}
   InternalBeforeConnect(TempDBParams,aDBName,aCreateIfNotExists);

   repeat
     { Generate a new DPB if necessary }
     if (DPB = nil) or FDBParamsChanged or (TempDBParams.Text <> FDBParams.Text) then
     begin
       FDBParamsChanged := False;
       if not FUseHiddenPassword and (not LoginPrompt and not (csDesigning in ComponentState)) or (FHiddenPassword = '') then
         DPB := GenerateDPB(FirebirdAPI,TempDBParams)
       else
       begin
          TempDBParams.Values['password'] := FHiddenPassword;
          DPB := GenerateDPB(FirebirdAPI,TempDBParams);
       end;
     end;

     if FCreateDatabase then
     begin
       FCreateDatabase := false;
       DPB.Add(isc_dpb_set_db_SQL_dialect).AsByte := SQLDialect; {create with this SQL Dialect}
       FAttachment := FirebirdAPI.CreateDatabase(aDBName,DPB, false);
       if FAttachment = nil then
         DPB := nil
       else
         DoOnCreateDatabase;
     end
     else
       FAttachment := FirebirdAPI.OpenDatabase(aDBName,DPB,false);

     if FAttachment = nil then
     begin
       Status := FirebirdAPI.GetStatus;
       {$IFDEF UNIX}
       if GetProtocol(aDBName) = Local then
       begin
           if ((Status.GetSQLCode = -901) and (Status.GetIBErrorCode = isc_random)) {Access permissions on firebird temp}
              or
              ((Status.GetSQLCode = -902) and (Status.GetIBErrorCode = isc_sys_request)) {Security DB Problem}
              or
              ((Status.GetSQLCode = -902) and (Status.GetIBErrorCode = isc_psw_attach)) {Security DB Problem}
              or
              ((Status.GetSQLCode = -904) and (Status.GetIBErrorCode = isc_lock_dir_access)) {Lock File Problem}
              then
              begin
                aDBName := 'localhost:' + aDBName;
                Continue;
             end
       end;
       {$ENDIF}
       if ((Status.GetSQLCode = -902) and (Status.GetIBErrorCode = isc_io_error)) {Database not found}
                        and aCreateIfNotExists and not (csDesigning in ComponentState) then
         FCreateDatabase := true
       else
         raise EIBInterBaseError.Create(Status,CP_ACP);
     end;

     if UseDefaultSystemCodePage and (FAttachment <> nil) then
     {Only now can we check the codepage in use by the Attachment.
      If not that required then re-open with required LCLType.}
     begin
       {$ifdef WINDOWS}
       if Attachment.CodePage2CharSetID(GetACP,CharSetID) then
       {$else}
       if Attachment.CodePage2CharSetID(DefaultSystemCodePage,CharSetID) then
       {$endif}
       begin
         CharSetName := Attachment.GetCharsetName(CharSetID);
         if CharSetName <> AnsiUpperCase(TempDBParams.Values['lc_ctype']) then
         begin
           TempDBParams.Values['lc_ctype'] := CharSetName;
           FDBParamsChanged := True;
           FAttachment := nil;
         end
       end
     end;

   until FAttachment <> nil;

  finally
   TempDBParams.Free;
  end;
  PW := Attachment.getDPB.Find(isc_dpb_password);
  if PW <> nil then PW.AsString := 'xxxxxxxx'; {Hide password}

  if not (csDesigning in ComponentState) then
    FDBName := aDBName; {Synchronise at run time}
  ValidateClientSQLDialect;
  InternalAfterConnect;
  if not (csDesigning in ComponentState) then
    MonitorHook.DBConnect(Self);
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

 procedure TIBDataBase.SetDBParamByDPB( const Idx: byte; Value: String);
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
    if not FAttachment.HasActivity then
    begin
      ForceClose;
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
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


 procedure TIBDataBase.SetSQLDialect( const Value: Integer);
begin
  if (Value < 1) then IBError(ibxeSQLDialectInvalid, [nil]);
  if (Attachment = nil) or (Value <= DBSQLDialect)  then
    FSQLDialect := Value
  else
    IBError(ibxeSQLDialectInvalid, [nil]);
end;

procedure TIBDataBase.SetWireCompression(AValue: boolean);
var Index: integer;
begin
  if AValue then
    FConfigOverrides.Values['WireCompression'] := 'true'
  else
  begin
    Index := FConfigOverrides.IndexOfName('WireCompression');
    if Index <> -1 then
      FConfigOverrides.Delete(Index);
  end;
end;

 function TIBDataBase.GetDBSQLDialect: Integer;
begin
  CheckActive;
  Result := Attachment.GetSQLDialect;
end;

function TIBDataBase.GetDefaultCharSetID: integer;
begin
  if (Attachment <> nil) and Attachment.HasDefaultCharSet then
    Result := Attachment.GetDefaultCharSetID
  else
    Result := 0;
end;

function TIBDataBase.GetDefaultCharSetName: AnsiString;
begin
  if Attachment <> nil then
    Result := Attachment.GetCharsetName(DefaultCharSetID)
  else
    Result := '';
end;

function TIBDataBase.GetDefaultCodePage: TSystemCodePage;
begin
  if Attachment <> nil then
    Attachment.CharSetID2CodePage(DefaultCharSetID,Result)
  else
    Result := CP_NONE;
end;

function TIBDataBase.GetDPBConstantNames(index: byte): string;
begin
  Result := FirebirdAPI.AllocateDPB.GetDPBParamTypeName(index);
  if Result = '' then
    IBError(ibxeDPBConstantUnknown,[index]);
end;

function TIBDataBase.GetFirebirdAPI: IFirebirdAPI;
var fblib: IFirebirdLibrary;
begin
  if FFirebirdAPI = nil then
  begin
    if (csDesigning in ComponentState) or (Trim(FFirebirdLibraryPathName) = '') then
      FFirebirdAPI := IB.FirebirdAPI
    else
    begin
      fblib := IB.LoadFBLibrary(FFirebirdLibraryPathName);
      if assigned(fblib) then
        FFirebirdAPI := fblib.GetFirebirdAPI;
    end;
  end;
  Result := FFirebirdAPI;
end;

function TIBDataBase.GetRemoteProtocol: string;
begin
  CheckActive;
  Result := Attachment.GetRemoteProtocol;
end;

function TIBDataBase.GetSQLObjectsCount: Integer;
begin
  Result := FSQLObjects.Count;
end;

function TIBDataBase.GetWireCompression: boolean;
begin
  Result := CompareText(FConfigOverrides.Values['WireCompression'],'true') = 0;
end;

procedure TIBDataBase.SetAttachment(AValue: IAttachment);
begin
  if FAttachment = AValue then Exit;
  if FAttachment <> nil then
  begin
    if Assigned(BeforeDisconnect) then
      BeforeDisconnect(self);
    InternalBeforeClose;
    FAttachment := nil;
    FFirebirdAPI := nil;
    InternalAfterClose;
    if Assigned(AfterDisconnect) then
      AfterDisconnect(self);
  end;
  if Assigned(BeforeConnect) then
    BeforeConnect(self);
  FAttachment := AValue;
  if FAttachment <> nil then
  begin
    ValidateClientSQLDialect;
    FDBName := FAttachment.GetConnectString;
    if FFirebirdLibraryPathName <> '' then
      FFirebirdLibraryPathName := FAttachment.getFirebirdAPI.GetFBLibrary.GetLibraryFilePath;
    InternalAfterConnect;
    if Assigned(AfterConnect) then
      AfterConnect(self);
  end;
end;

procedure TIBDataBase.SetConfigOverrides(AValue: TStrings);
begin
  if FConfigOverrides = AValue then Exit;
  FConfigOverrides.Assign(AValue);
end;

procedure TIBDataBase.SetFirebirdLibraryPathName(AValue: TIBFileName);
begin
  if FFirebirdLibraryPathName = AValue then Exit;
  FFirebirdLibraryPathName := AValue;
  ForceClose;
  FFirebirdAPI := nil;
end;

 procedure TIBDataBase.ValidateClientSQLDialect;
begin
  if (DBSQLDialect < FSQLDialect) then
  begin
    FSQLDialect := DBSQLDialect;
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
      'from RDB$RELATION_FIELDS R ' + {do not localize}
      'where R.RDB$RELATION_NAME = ' + {do not localize}
      '''' + ExtractIdentifier(DBSQLDialect, TableName) +
      ''' and Exists(Select * From RDB$FIELDS F Where R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME)' ; {do not localize}
    Query.Prepare;
    Query.ExecQuery;
    with List do
    begin
      BeginUpdate;
      try
        Clear;
        while (not Query.EOF) and Query.Next  do
          List.Add(TrimRight(Query.FieldByName('RDB$FIELD_NAME').AsString)); {do not localize}
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
          while (not Query.EOF) and Query.Next  do
            List.Add(TrimRight(Query.Fields[0].AsString));
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
var uuid: TGUID;
begin
  inherited Create(AOwner);
  FDatabases := TList.Create;
  FSQLObjects := TList.Create;
  FTPB := nil;
  FTRParams := TStringList.Create;
  FTRParamsChanged := True;
  TStringList(FTRParams).OnChange := TRParamsChange;
  TStringList(FTRParams).OnChanging := TRParamsChanging;
  FTimer := TFPTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 0;
  FTimer.OnTimer := TimeoutTransaction;
  FDefaultAction := taCommit;
  FTransactionList.Add(self);
  if (FTransactionName = '') and (CreateGUID(uuid) = 0) then
    FTransactionName := GUIDToString(uuid);
end;

destructor TIBTransaction.Destroy;
var
  i: Integer;
begin
  if InTransaction then
    EndTransaction(FDefaultAction, True);
  for i := 0 to FSQLObjects.Count - 1 do
    if FSQLObjects[i] <> nil then
      SQLObjects[i].DoTransactionFree;
  RemoveSQLObjects;
  RemoveDatabases;
  if assigned(FTransactionList) then
    FTransactionList.Remove(self);
  FTPB := nil;
  FTRParams.Free;
  FSQLObjects.Free;
  FDatabases.Free;
  inherited Destroy;
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
  if (TransactionIntf = nil) then
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
    if TransactionIntf <> nil then
      Rollback;
  end;
end;

procedure TIBTransaction.CheckNotInTransaction;
begin
  if (TransactionIntf <> nil) and  TransactionIntf.InTransaction then
    IBError(ibxeInTransaction, [nil]);
end;

function TIBTransaction.AddDatabase(db: TIBDatabase): Integer;
var
  i: Integer;
  NilFound: Boolean;
begin
  EnsureNotInTransaction;
  CheckNotInTransaction;
  FTransactionIntf := nil;

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


  procedure InternalDoBeforeTransactionEnd;
  var i: integer;
  begin
    try
      DoBeforeTransactionEnd;
    except on E: EIBInterBaseError do
      begin
        if not Force then
          raise;
      end;
    end;

    for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
    try
      SQLObjects[i].DoBeforeTransactionEnd(Action);
    except on E: EIBInterBaseError do
      begin
        if not Force then
            raise;
        end;
    end;
  end;

  procedure InternalDoAfterTransctionEnd;
  var i: integer;
  begin
    for i := 0 to FSQLObjects.Count - 1 do if FSQLObjects[i] <> nil then
      try
        SQLObjects[i].DoAfterTransactionEnd;
      except on E: EIBInterBaseError do
        begin
          if not Force then
            raise;
        end;
      end;
    try
      DoAfterTransactionEnd;
    except on E: EIBInterBaseError do
      begin
        if not Force then
          raise;
      end;
    end;
  end;

begin
  CheckInTransaction;
  if FInEndTransaction then Exit;
  FCriticalSection.Enter; {Ensure that only one thread can commit a transaction
                           at any one time}
  FEndAction := Action;
  FInEndTransaction := true;
  try
   case Action of
     TARollback:
       begin
         InternalDoBeforeTransactionEnd;
         FTransactionIntf.Rollback(Force);
         InternalDoAfterTransctionEnd;
         if not (csDesigning in ComponentState) then
           MonitorHook.TRRollback(Self);
       end;
     TACommit:
       begin
         InternalDoBeforeTransactionEnd;
         try
           FTransactionIntf.Commit;
         except on E: EIBInterBaseError do
           begin
             if Force then
               FTransactionIntf.Rollback(Force)
             else
               raise;
           end;
         end;
         InternalDoAfterTransctionEnd;
         if not (csDesigning in ComponentState) then
           MonitorHook.TRCommit(Self);
      end;
     TACommitRetaining:
       begin
         FTransactionIntf.CommitRetaining;
         if not (csDesigning in ComponentState) then
           MonitorHook.TRCommitRetaining(Self);
       end;

     TARollbackRetaining:
       begin
         FTransactionIntf.RollbackRetaining;
         if not (csDesigning in ComponentState) then
           MonitorHook.TRRollbackRetaining(Self);
       end;
     end;
  finally
    FInEndTransaction := false;
    FCriticalSection.Leave;
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

function TIBTransaction.GetIsReadOnly: boolean;
begin
  CheckInTransaction;
  Result := FTransactionIntf.GetIsReadOnly;
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
  result := (TransactionIntf <> nil) and TransactionIntf.InTransaction;
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

class function TIBTransaction.FindTransactionNyName(aTransactionName: string
  ): TIBTransaction;
var i: integer;
begin
  Result := nil;
  for i := 0 to FTransactionList.Count - 1 do
    if TIBTransaction(FTransactionList[i]).TransactionName = aTransactionName then
    begin
      Result := FTransactionList[i];
      break;
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
  FTransactionIntf := nil;
end;

function TIBTransaction.GetTPBConstantNames(index: byte): string;
begin
  CheckDatabasesInList;
  if FTPB = nil then
    FTPB := Databases[0].FirebirdAPI.AllocateTPB;
  Result := FTPB.GetDPBParamTypeName(index);
  if Result = '' then
    IBError(ibxeTPBConstantUnknown,[index]);
end;

function TIBTransaction.GetTransactionID: integer;
begin
  CheckInTransaction;
  Result := FTransactionIntf.GetTransactionID;
end;

procedure TIBTransaction.RemoveDatabase(Idx: Integer);
var
  DB: TIBDatabase;
begin
  if ((Idx >= 0) and (FDatabases[Idx] <> nil)) then
  begin
    EnsureNotInTransaction;
    CheckNotInTransaction;
    FTransactionIntf := nil;

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
  EnsureNotInTransaction;
  CheckNotInTransaction;
  FTransactionIntf := nil;

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

procedure TIBTransaction.SetTransactionName(AValue: string);
begin
  if FTransactionName = AValue then Exit;
  CheckNotInTransaction;
  FTransactionName := AValue;
end;

procedure TIBTransaction.SetTRParams(Value: TStrings);
begin
  FTRParams.Assign(Value);
end;

procedure TIBTransaction.StartTransaction;
var
  i: Integer;
  Attachments: array of IAttachment;
  ValidDatabaseCount: integer;
begin
  CheckNotInTransaction;
  CheckDatabasesInList;
  if TransactionIntf <> nil then
    TransactionIntf.Start(DefaultAction)
  else
  begin
    for i := 0 to FDatabases.Count - 1 do
     if  FDatabases[i] <> nil then
     begin
       with TIBDatabase(FDatabases[i]) do
       if not Connected then
         if StreamedConnected then
         begin
           Open;
           StreamedConnected := False;
         end
         else
           IBError(ibxeDatabaseClosed, [nil]);
     end;
    if FTRParamsChanged or (FTPB = nil) then
    begin
      FTRParamsChanged := False;
      FTPB :=  GenerateTPB(Databases[0].FirebirdAPI,FTRParams);
    end;

    ValidDatabaseCount := 0;
    for i := 0 to DatabaseCount - 1 do
      if Databases[i] <> nil then Inc(ValidDatabaseCount);

    if ValidDatabaseCount = 1 then
      FTransactionIntf := Databases[0].Attachment.StartTransaction(FTPB,
                                                DefaultAction,TransactionName)
    else
    begin
      SetLength(Attachments,ValidDatabaseCount);
      for i := 0 to DatabaseCount - 1 do
        if Databases[i] <> nil then
          Attachments[i] := Databases[i].Attachment;

      FTransactionIntf := Databases[0].FirebirdAPI.StartTransaction(Attachments,FTPB,
                                              DefaultAction,TransactionName);
    end;

  end;

  if not (csDesigning in ComponentState) then
     MonitorHook.TRStart(Self);

  DoOnStartTransaction;
end;

procedure TIBTransaction.TimeoutTransaction(Sender: TObject);
begin
  if InTransaction then
  begin
    if not TransactionIntf.HasActivity then
    begin
      EndTransaction(FDefaultAction, True);
      if Assigned(FOnIdleTimer) then
        FOnIdleTimer(Self);
    end
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
  FTransactionIntf := nil;
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

procedure TIBBase.DoBeforeDatabaseConnect(DBParams: TStrings;
  var DBName: string; var CreateIfNotExists: boolean);
begin
  if assigned(FBeforeDatabaseConnect) then
    BeforeDatabaseConnect(self,DBParams,DBName,CreateIfNotExists);
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

procedure TIBBase.DoOnCreateDatabase;
begin
  if assigned(FOnCreateDatabase) then
    OnCreateDatabase(self);
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

function TIBDataBase.GenerateDPB(FirebirdAPI: IFirebirdAPI; sl: TStrings): IDPB;
var
  i: Integer;
  ParamValue: string;
  DPBItem: IDPBItem;
begin
  Result := FirebirdAPI.AllocateDPB;

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

    DPBItem := Result.AddByTypeName(sl.Names[i]); {mbcs ok}
    ParamValue := sl.ValueFromIndex[i]; {mbcs ok}
     {  A database parameter either contains a string value (case 1)
       or an Integer value (case 2)
       or no value at all (case 3)
       or an error needs to be generated (case else)  }
    case DPBItem.getParamType of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_page_size,
      isc_dpb_sql_role_name:
        DPBItem.SetAsString(ParamValue);

      isc_dpb_sql_dialect:
      begin
        if (ParamValue = '') or (ParamValue[1] = '3') then
          DPBItem.SetAsString(#03)
        else
          DPBItem.SetAsString(#01)
      end;


      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
        DPBItem.SetAsByte(byte(ParamValue[1]));

      isc_dpb_sweep:
        DPBItem.SetAsByte(isc_dpb_records);

      isc_dpb_sweep_interval:
        DPBItem.SetAsInteger(StrToInt(ParamValue));

      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_map_attach, isc_dpb_quit_log:
        DPBItem.SetAsByte(0);
      else
          IBError(ibxeDPBConstantNotSupported, [DPBItem.getParamTypeName])
    end;
  end;
  if FConfigOverrides.Count > 0 then
    Result.Add(isc_dpb_config).SetAsString(FConfigOverrides.Text);
end;

{ GenerateTPB -
  Given a string containing a textual representation
  of the transaction parameters, generate a transaction
  parameter buffer, and return it and its length in
  TPB and TPBLength, respectively. }
function TIBTransaction.GenerateTPB(FirebirdAPI: IFirebirdAPI; sl: TStrings): ITPB;
var
  i: Integer;
  ParamName, ParamValue: string;
  TPBItem: ITPBItem;
begin
  Result := FirebirdAPI.AllocateTPB;
  for i := 0 to sl.Count - 1 do
  begin
    if (Trim(sl[i]) =  '') then
      Continue;

    ParamName := sl.Names[i];
    if ParamName = '' then ParamName := sl[i];
    TPBItem := Result.AddByTypeName(ParamName);
    ParamValue := sl.ValueFromIndex[i];

    case TPBItem.getParamType of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
      isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
      isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
      isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version:
        {nothing more to do};

      isc_tpb_lock_read, isc_tpb_lock_write:
        TPBItem.SetAsString(ParamValue);

      else
          IBError(ibxeTPBConstantNotSupported, [TPBItem.getParamTypeName])
    end;
  end;
end;


Initialization
  TIBTransaction.FCriticalSection := TCriticalSection.Create;
  TIBTransaction.FTransactionList := TList.Create;

Finalization
  if assigned(TIBTransaction.FCriticalSection) then TIBTransaction.FCriticalSection.Free;
  if assigned(TIBTransaction.FTransactionList) then TIBTransaction.FTransactionList.Free;

end.





