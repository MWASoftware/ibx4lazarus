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
{    Contributor: Tony Whyman, MWA Software (http://www.mwasoftare.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

{
  This unit has been significantly revised for the Lazarus port. Specially,
  there was a need to re-organise the code to isolate the Windows specific
  IPC and to introduce SV5 IPC as an alternative for Linux and other platforms.
}

unit IBSQLMonitor;

{$Mode Delphi}

interface

uses
  LMessages, Messages, LCLIntf, LCLType, LCLProc,  Forms, Controls, Dialogs,
  IB, IBUtils, IBSQL, IBCustomDataSet, IBDatabase, IBServices, IBXConst,SysUtils,
  Classes,
{$IFDEF WINDOWS }
  Windows
{$ELSE}
  unix
{$ENDIF}
;

{Note that the original inter-thread communication between the Reader Thread and
 the ISQL Monitor used the Windows PostMessage interface. This is currently not
 useable under the FPC RTL as AllocateHWnd is not functional. It has been replaced
 by the use of the Synchronize method.}

{$IFDEF WINDOWS}
{$DEFINE USE_WINDOWS_IPC}
{ $DEFINE USE_POSTMESSAGE}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE USE_SV5_IPC}
{$DEFINE HAS_SEMTIMEDOP}
{$ENDIF}

const
  WM_MIN_IBSQL_MONITOR = WM_USER;
  WM_MAX_IBSQL_MONITOR = WM_USER + 512;
  WM_IBSQL_SQL_EVENT = WM_MIN_IBSQL_MONITOR + 1;

type
  TIBCustomSQLMonitor = class;

  { TIBSQLMonitor }
  TSQLEvent = procedure(EventText: String; EventTime : TDateTime) of object;

  { TIBCustomSQLMonitor }

  TIBCustomSQLMonitor = class(TComponent)
  {$IFDEF USE_POSTMESSAGE}
  private
    FHWnd: HWND;
    procedure MonitorWndProc(var Message : TMessage);
  {$ENDIF}
  private
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure ReleaseObject;  {Called from Writer Thread}
    procedure ReceiveMessage(Msg: TObject);    {Called from Reader Thread}
    property OnSQL: TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property Enabled : Boolean read FEnabled write SetEnabled default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release;
 {$IFDEF USE_POSTMESSAGE}
    property Handle: THandle read FHWnd;
 {$ENDIF}
  end;

  TIBSQLMonitor = class(TIBCustomSQLMonitor)
  published
    property OnSQL;
    property TraceFlags;
    property Enabled;
  end;

  IIBSQLMonitorHook = interface
    ['{CF65434C-9B75-4298-BA7E-E6B85B3C769D}']
    procedure RegisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
    procedure SQLPrepare(qry: TIBSQL); 
    procedure SQLExecute(qry: TIBSQL);
    procedure SQLFetch(qry: TIBSQL); 
    procedure DBConnect(db: TIBDatabase);
    procedure DBDisconnect(db: TIBDatabase);
    procedure TRStart(tr: TIBTransaction); 
    procedure TRCommit(tr: TIBTransaction);
    procedure TRCommitRetaining(tr: TIBTransaction); 
    procedure TRRollback(tr: TIBTransaction);
    procedure TRRollbackRetaining(tr: TIBTransaction);
    procedure ServiceAttach(service: TIBCustomService); 
    procedure ServiceDetach(service: TIBCustomService);
    procedure ServiceQuery(service: TIBCustomService);
    procedure ServiceStart(service: TIBCustomService);
    procedure SendMisc(Msg : String);
    function GetTraceFlags : TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetTraceFlags(const Value : TTraceFlags);
    function GetEnabled : boolean;
    procedure SetEnabled(const Value : Boolean);
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;


function MonitorHook: IIBSQLMonitorHook;
procedure EnableMonitoring;
procedure DisableMonitoring;
function MonitoringEnabled: Boolean;

implementation

uses
   contnrs, syncobjs
   {$IFDEF USE_SV5_IPC}
   ,ipc, Errors, baseunix
   {$IF FPC_FULLVERSION <= 20402 } , initc {$ENDIF}
   {$ENDIF};

   {$STATIC ON}
const
  cMonitorHookSize = 1024;
  cMsgWaitTime = 1000;
  cWriteMessageAvailable = 'WriterMsgQueue';
{$IFDEF USE_SV5_IPC}
  IPCFileName: string = 'FB.SQL.MONITOR1_0';
  cNumberOfSemaphores = 10;
  cMutexSemaphore = 0;
  cMonitorCounter = 1;
  cReadReadyEventSemaphore = 2;
  cReadFinishedEventSemaphore = 4;
  cDataAvailableEventSemaphore = 6;
  cWriterBusyEventSemaphore = 8;
  cDefaultTimeout = 1; { 1 seconds }
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  MonitorHookNames: array[0..5] of String = (
    'FB.SQL.MONITOR.Mutex1_0',
    'FB.SQL.MONITOR.SharedMem1_0',
    'FB.SQL.MONITOR.WriteEvent1_0',
    'FB.SQL.MONITOR.WriteFinishedEvent1_0',
    'FB.SQL.MONITOR.ReadEvent1_0',
    'FB.SQL.MONITOR.ReadFinishedEvent1_0'
  );
  cDefaultTimeout = 1000; { 1 seconds }
{$ENDIF}

{$IFDEF USE_SV5_IPC}
{
  The call to semctl in ipc is broken in FPC release 2.4.2 and earlier. Hence
  need to replace with a working libc call. semtimedop is not present in 2.4.2 and earlier.
}

{$IF FPC_FULLVERSION <= 20402 }
Function real_semctl(semid:cint; semnum:cint; cmd:cint): cint; cdecl; varargs; external clib name 'semctl';

Function semctl(semid:cint; semnum:cint; cmd:cint; var arg: tsemun): cint;
  begin
    semctl := real_semctl(semid,semnum,cmd,pointer(arg));
  end;
{$IFDEF HAS_SEMTIMEDOP}
Function semtimedop(semid:cint; sops: psembuf; nsops: cuint; timeOut: ptimespec): cint; cdecl; external clib name 'semtimedop';
{$ENDIF}
Function semget(key:Tkey; nsems:cint; semflg:cint): cint; cdecl; external clib name 'semget';
Function semop(semid:cint; sops: psembuf; nsops: cuint): cint; cdecl; external clib name 'semop';

function GetLastErrno: cint;
begin
  Result := fpgetCerrno
end;
{$ELSE}
function GetLastErrno: cint;
begin
  Result := fpgetErrno
end;

{$ENDIF}
{$ENDIF}

type
  TGlobalInterface = class;

  { TIBSQLMonitorHook }
  TIBSQLMonitorHook = class(TInterfacedObject, IIBSQLMonitorHook)
  private
    FGlobalInterface: TGlobalInterface;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
    procedure SQLPrepare(qry: TIBSQL); virtual;
    procedure SQLExecute(qry: TIBSQL); virtual;
    procedure SQLFetch(qry: TIBSQL); virtual;
    procedure DBConnect(db: TIBDatabase); virtual;
    procedure DBDisconnect(db: TIBDatabase); virtual;
    procedure TRStart(tr: TIBTransaction); virtual;
    procedure TRCommit(tr: TIBTransaction); virtual;
    procedure TRCommitRetaining(tr: TIBTransaction); virtual;
    procedure TRRollback(tr: TIBTransaction); virtual;
    procedure TRRollbackRetaining(tr: TIBTransaction); virtual;
    procedure ServiceAttach(service: TIBCustomService); virtual;
    procedure ServiceDetach(service: TIBCustomService); virtual;
    procedure ServiceQuery(service: TIBCustomService); virtual;
    procedure ServiceStart(service: TIBCustomService); virtual;
    procedure SendMisc(Msg : String);
    function GetEnabled: Boolean;
    function GetTraceFlags: TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
    procedure ForceRelease;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
  end;

  { There are two possible objects.  One is a trace message object.
    This object holds the flag of the trace type plus the message.
    The second object is a Release object.  It holds the handle that
    the CM_RELEASE message is to be queued to. }

  { TTraceObject }

  TTraceObject = Class(TObject)
    FDataType : TTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
  public
    constructor Create(Msg : String; DataType : TTraceFlag); overload;
    constructor Create(obj : TTraceObject); overload;
    constructor Create(obj : TTraceObject; MsgOffset, MsgLen: integer); overload;
  end;

  { TReleaseObject }

  TReleaseObject = Class(TObject)
  {$IFDEF USE_POSTMESSAGE}
  FHandle: THandle;
  public
    constructor Create(Handle: THandle);
  {$ELSE}
    FMonitor : TIBCustomSQLMonitor;
  public
    constructor Create(Monitor : TIBCustomSQLMonitor);
  {$ENDIF}
  end;

  { TWriterThread }

  TWriterThread = class(TThread)
  private
    { Private declarations }
    FGlobalInterface: TGlobalInterface;
    FMsgs : TObjectList;
    FCriticalSection: TCriticalSection;
    FMsgAvailable: TEventObject;
    procedure RemoveFromList;
{$IFDEF USE_POSTMESSAGE}
  public
    procedure ReleaseMonitor(Handle: THandle);
{$ELSE}
    procedure PostRelease;
  public
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
{$ENDIF}
  protected
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure WriteToBuffer;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure WriteSQLData(Msg : String; DataType : TTraceFlag);
  end;

  { TReaderThread }

  TReaderThread = class(TThread)
  private
    { Private declarations }
    st : TTraceObject;
    FMonitors : TObjectList;
    FGlobalInterface: TGlobalInterface;
    FCriticalSection: TCriticalSection;
    procedure AlertMonitors;
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure ReadSQLData;
    procedure Execute; override;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure AddMonitor(Arg : TIBCustomSQLMonitor);
    procedure RemoveMonitor(Arg : TIBCustomSQLMonitor);
  end;

  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TIpcCommon }

  TIpcCommon = class
  private
    function GetSa: PSecurityAttributes;
  protected
    FInitialiser: boolean;  static;
    {$IFDEF USE_SV5_IPC}
    FSemaphoreSetID: cint;  static;
    FSharedMemoryID: cint;  static;
    function sem_op(SemNum, op: integer; flags: cshort = 0): cint;
    function sem_timedop(SemNum, op: integer; timeout_secs: integer; flags: cshort = 0): cint;
    function GetSemValue(SemNum: integer): cint;
    procedure SemInit(SemNum, AValue: cint);
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    FSa : TSecurityAttributes;
  private
    Sd : TSecurityDescriptor;
    {$ENDIF}
  public
    constructor Create;
    property Sa : PSecurityAttributes read GetSa;
  end;

  { TSharedMemory }

  {
    The shared memory segment is used for interprocess communication and
    holds both a message buffer and a number of shared variables. Shared
    memory is allocated to each shared variable using the Allocate function.
    An underlying assumption is that each process using the shared memory
    calls "Allocate" in the same order and for the same memory sizes.

    Windows:

    The Windows implementation uses Windows shared memory. This is identified
    by a global name known to every process. There is no security with
    the windows implementation and the shared memory can be read by
    any active process.

    Linux:

    The Linux implementation uses Linux shared memory. IPC_PRIVATE is used
    to allocate the memory and the resulting memory id is written to a
    well known file. By default this is in the current user's home directory,
    but this can be over-ridden to specify a globally unique filename.

    Access to the shared memory is restricted to the current user/group.
    Note that the Linux semaphore set is also created with the shared memory.
  }

  TSharedMemory = class(TIpcCommon)
  private
    FBuffer: PChar;
    FLastAllocationSize: integer;
    FUnused: integer;
    FBufptr: PChar;
    {$IFDEF USE_SV5_IPC}
    procedure DropSharedMemory;
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    FSharedBuffer: THandle;
    {$ENDIF}
    procedure GetSharedMemory(MemSize: integer);
  public
    constructor Create(MemSize: integer);
    destructor Destroy; override;
    function Allocate(Size: integer): PChar;
    property LastAllocationSize: integer read FLastAllocationSize;
  end;

  {TMutex}

  TMutex = class(TIpcCommon)
  private
    {$IFDEF USE_SV5_IPC}
    FMutexSemaphore: cint;
    FLockCount: integer;
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    FMutex: THandle;
    {$ENDIF}
  public
    {$IFDEF USE_SV5_IPC}
    constructor Create(SemNumber: cint);
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    constructor Create(MutexName: string);
    {$ENDIF}
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  { TSingleLockGate }

  {
    A single lock gate is either open or closed. When open, any thread can pass
    through it while, when closed, all threads are blocked as they try to pass
    through the gate. When the gate is opened, all blocked threads are resumed.

    There is an implementation assumption that only one writer thread at
    a time (i.e. the thread which locks or unlocks the gate) can have access to
    it at any one time. I.e. an external Mutex prevents race conditions.

    Windows:

    In the Windows implementation, a Windows Event is used to implement
    the "gate". No additional functionality is required as the behaviour
    of a Windows event meets the requirement.

    Linux:

    In the Linux implementation, the gate is implemented by a semaphore
    and a share memory integer used as a bi-state variable. When the gate
    is open, the bi-state variable is non-zero. It is set to zero when closed.
    Another shared memory integer is used to count the number of waiting
    threads, and a second semaphore is used to protect access to this.

    The event semaphore is initialised to zero. When a thread passes through the gate
    it checks the state. If open, the thread continues. If closed then it
    increments the count of waiting threads and then decrements the semaphore
    and hence enters an indefinite wait state.

    When the gate is locked, the state is set to zero. When unlocked, the state
    is set to one and the semaphore incremented by the number of waiting threads,
    which itself is then zeroed.

    Always initialised to the Unlocked state
  }

  TSingleLockGate = class(TIpcCommon)
  private
    FOwner: TGlobalInterface;
    {$IFDEF USE_SV5_IPC}
    FSemaphore: cint;
    FMutex: cint;
    FSignalledState: PInteger;
    FWaitingThreads: PInteger;
    function GetWaitingThreads: integer;
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    FEvent: THandle;
    {$ENDIF}
  public
    {$IFDEF USE_SV5_IPC}
    constructor Create(SemNum: cint; AOwner: TGlobalInterface);
    property WaitingThreads: integer read GetWaitingThreads;
  public
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    constructor Create(EventName: string; AOwner: TGlobalInterface);
    {$ENDIF}
    destructor Destroy; override;
    procedure PassthroughGate;
    procedure Unlock;
    procedure Lock;
  end;

  { TMultilockGate }

  { This type of Gate is used where several reader threads must pass
    through the gate before it can be opened for a writer thread.

    The reader threads register their interest by each locking the gate.
    The writer thread then waits on the locked gate until all the reader
    threads have separately unlocked the gate.

    There is an underlying assumption of a single writer. A Mutex must
    be used to control access to the gate from the writer side if this
    assumption is invalid.

    Linux:

    The Linux implementation uses a single semaphore to implement the gate,
    which is initialised to 1 (unlocked), and a count of the number of
    threads that have locked the gate (LockCount). A mutex semaphore
    protects access to the LockCount. When the gate is locked, the lockcount
    is incremented and, if the LockCount was originally zero, the semaphore is
    set to zero (Gate Closed).

    Unlocking the gate, is the reverse. The LockCount is decremented and, if it
    reaches zero, the semaphore is set to one (Gate Opened).

    When a writer passes through the gate, it checks the LockCount, if zero it
    proceeds to pass through the gate. Otherwise it decrements and waits on the
    semaphore. When the writer resumes, it increments the semaphore in order
    to return it to its unlocked state. The wait is a timed wait, as there is
    a risk that a reader thread may terminate while the gate is locked. If the
    LockCount is non-zero, it is decremented and the writer returns to wait on
    the gate.

    Windows:

    The Windows implementation uses an IPC Event and shared memory to hold
    an integer - the reader count.

    The readers lock the gate by resetting the event and incrementing the
    reader count. They unlock the gate by decrementing the reader count
    and calling set event when the reader count reaches zero.

    The writer waits on the event for the gate to open. This is a timed wait
    to avoid the writer being left in an indefinite wait state should a reader
    terminate abnormally.

    Always initialised to the Unlocked state
  }

  TMultilockGate = class(TIpcCommon)
  private
    FOnGateTimeout: TNotifyEvent;
    FOwner: TGlobalInterface;
    {$IFDEF USE_SV5_IPC}
    FSemaphore: cint;
    FMutex: cint;
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    FEvent: THandle;
    {$ENDIF}
    FLockCount: PInteger;
    function GetLockCount: integer;
  public
    {$IFDEF USE_SV5_IPC}
    constructor Create(SemNum: cint; AOwner: TGlobalInterface);
    {$ENDIF}
    {$IFDEF USE_WINDOWS_IPC}
    constructor Create(EventName: string; AOwner: TGlobalInterface);
    {$ENDIF}
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    procedure PassthroughGate;
    property LockCount: integer read GetLockCount;
    property OnGateTimeout: TNotifyEvent read FOnGateTimeout write FOnGateTimeout;
  end;

  { TGlobalInterface }

  TGlobalInterface = class(TIpcCommon)
  private
    FMaxBufferSize: integer;
    FSharedMemory: TSharedMemory;
    FWriteLock: TMutex;
    FBuffer: PChar;
    FTraceDataType,
    FBufferSize: PInteger;
    FTimeStamp: PDateTime;
    FReadReadyEvent: TMultiLockGate;
    FReadFinishedEvent: TMultiLockGate;
    FDataAvailableEvent: TSingleLockGate;
    FWriterBusyEvent: TSingleLockGate;
    {$IFDEF USE_WINDOWS_IPC}
    FMonitorCount: PInteger;
    procedure HandleGateTimeout(Sender: TObject);
    {$ENDIF}
    function GetMonitorCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncMonitorCount;
    procedure DecMonitorCount;
    procedure SendTrace(TraceObject: TTraceObject);
    procedure ReceiveTrace(TraceObject: TTraceObject);
    property DataAvailableEvent: TSingleLockGate read FDataAvailableEvent;
    property WriterBusyEvent: TSingleLockGate read FWriterBusyEvent;
    property ReadReadyEvent: TMultiLockGate read FReadReadyEvent;
    property ReadFinishedEvent: TMultiLockGate read FReadFinishedEvent;
    property WriteLock: TMutex read FWriteLock;
    property MonitorCount: integer read GetMonitorCount;
    property SharedMemory: TSharedMemory read FSharedMemory;
    property MaxBufferSize: integer read FMaxBufferSize;
  end;

{ TSharedMemory }

{$IFDEF USE_SV5_IPC}
procedure TSharedMemory.GetSharedMemory(MemSize: integer);
var F: cint;
begin
    {Get the Shared Memory and Semaphore IDs from the Global File if it exists
     or create them and the file otherwise }

    repeat
      F := fpOpen(IPCFileName, O_WrOnly or O_Creat or O_Excl);
      if F < 0 then
      begin
        if fpgetErrno = 17 {EEXIST} then
        begin
          { looks like it already exists}
          Sleep(100);
          F := fpOpen(IPCFileName,O_RdOnly);
          if (F < 0) and (fpgetErrno = 2 {ENOENT}) then
            {probably just got deleted }
          else
          if F < 0 then
            IBError(ibxeCannotCreateSharedResource,['Error accessing IPC File - ' +
                                                 StrError(fpgetErrno)]);
        end
        else
            IBError(ibxeCannotCreateSharedResource,['Error creating IPC File  - ' +
                                                 StrError(fpgetErrno)]);
      end
      else
        FInitialiser := true
    until F >= 0;

    if FInitialiser then
    begin
      FSharedMemoryID := shmget(IPC_PRIVATE,MemSize, IPC_CREAT or
                           S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP);
      if FSharedMemoryID < 0 then
          IBError(ibxeCannotCreateSharedResource,['Cannot create shared memory segment - ' +
                                                 StrError(fpgetErrno)]);

      FSemaphoreSetID := semget(IPC_PRIVATE, cNumberOfSemaphores,IPC_CREAT or
                           S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP);
      if FSemaphoreSetID < 0 then
          IBError(ibxeCannotCreateSharedResource,['Cannot create shared semaphore set - ' +
                                                 StrError(fpgetErrno)]);

      fpWrite(F,FSharedMemoryID,sizeof(FSharedMemoryID));
      fpWrite(F,FSemaphoreSetID,sizeof(FSemaphoreSetID));
    end
    else
    begin
      fpRead(F,FSharedMemoryID,sizeof(FSharedMemoryID));
      fpRead(F,FSemaphoreSetID,sizeof(FSemaphoreSetID));
      if GetSemValue(cMonitorCounter) = 0 then
      begin
        FInitialiser := true;
        //writeln('Opened file and is initialiser');
      end
    end;
    fpClose(F);
end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
procedure TSharedMemory.GetSharedMemory(MemSize: integer);
begin
  FSharedBuffer := CreateFileMapping(INVALID_HANDLE_VALUE, sa, PAGE_READWRITE,
                       0, MemSize, PChar(MonitorHookNames[1]));

  if GetLastError = ERROR_ALREADY_EXISTS then
    FSharedBuffer := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1]))
  else
    FInitialiser := true;
  if (FSharedBuffer = 0) then
      IBError(ibxeCannotCreateSharedResource, [GetLastError]);
end;
{$ENDIF}

{$IFDEF USE_SV5_IPC}
procedure TSharedMemory.DropSharedMemory;
var ds: TShmid_ds;
    arg: tsemun;
begin
  if shmctl(FSharedMemoryID,IPC_STAT,@ds) < 0 then
    IBError(ibxeSV5APIError,['Error getting shared memory info' + strError(fpgetErrno)]);
  if ds.shm_nattch = 0 then  { we are the last one out - so, turn off the lights }
  begin
    shmctl(FSharedMemoryID,IPC_RMID,nil);
    semctl(FSemaphoreSetID,0,IPC_RMID,arg);
    DeleteFile(IPCFileName);
  end;
end;
{$ENDIF}

constructor TSharedMemory.Create(MemSize: integer);
begin
  inherited Create;
  FInitialiser := false;
  GetSharedMemory(MemSize);
{$IFDEF USE_SV5_IPC}
  FBuffer := shmat(FSharedMemoryID,nil,0);
  if integer(FBuffer) = -1 then
    IBError(ibxeCannotCreateSharedResource,[StrError(Errno)]);
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if FBuffer = nil then
      IBError(ibxeCannotCreateSharedResource, [GetLastError]);
{$ENDIF}
  FBufPtr := FBuffer;
  FUnused := MemSize
end;

destructor TSharedMemory.Destroy;
begin
{$IFDEF USE_SV5_IPC}
  shmdt(FBuffer);
  DropSharedMemory;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  UnmapViewOfFile(FBuffer);
  CloseHandle(FSharedBuffer);
{$ENDIF}
  inherited Destroy;
end;

function TSharedMemory.Allocate(Size: integer): PChar;
begin
  if Size > FUnused then
      IBError(ibxeCannotCreateSharedResource, ['Not enough shared memory']);
  Result := FBufPtr;

  if Size = 0 then
  begin
    FLastAllocationSize := FUnused;
    FUnused := 0
  end
  else
  begin
    FLastAllocationSize := Size;
    Dec(FUnused,Size);
  end;
  Inc(FBufPtr,Size)
end;

{ TIpcCommon }

function TIpcCommon.GetSa: PSecurityAttributes;
begin
  {$IFDEF USE_WINDOWS_IPC}
  Result := @FSa
  {$ELSE}
  Result := nil
  {$ENDIF}
end;

{$IFDEF USE_SV5_IPC}
function TIpcCommon.sem_op(SemNum, op: integer; flags: cshort): cint;
var sembuf: TSEMbuf;
begin
    sembuf.sem_num := SemNum;
    sembuf.sem_op:= op;
    sembuf.sem_flg := flags or SEM_UNDO;
    Result := semop(FSemaphoreSetID,@sembuf,1);
end;

function TIpcCommon.sem_timedop(SemNum, op: integer; timeout_secs: integer;
  flags: cshort): cint;
var sembuf: TSEMbuf;
    timeout: TimeSpec;
begin
    sembuf.sem_num := SemNum;
    sembuf.sem_op:= op;
    sembuf.sem_flg := flags or SEM_UNDO;
    timeout.tv_sec := timeout_secs;
    timeout.tv_nsec := 0;
{$IFDEF HAS_SEMTIMEDOP}
    Result := semtimedop(FSemaphoreSetID,@sembuf,1,@timeout);
{$ELSE}
    Result := sem_op(FSemaphoreSetID,@sembuf,1);    {May hang on race condition}
{$ENDIF}
end;

function TIpcCommon.GetSemValue(SemNum: integer): cint;
var args :TSEMun;
begin
  Result := semctl(FSemaphoreSetID,SemNum,SEM_GETVAL,args);
  if Result < 0 then
     IBError(ibxeSV5APIError,['GetSemValue: '+strError(GetLastErrno)]);
end;

procedure TIpcCommon.SemInit(SemNum, AValue: cint);
var args :TSEMun;
begin
  //writeln('Initialising ',SemNum,' to ',AValue);
  args.val := AValue;
  if semctl(FSemaphoreSetID,SemNum,SEM_SETVAL,args)  < 0 then
     IBError(ibxeCannotCreateSharedResource,['Unable to initialise Semaphone ' +
                          IntToStr(SemNum) + '- ' + StrError(GetLastErrno)]);

end;

constructor TIpcCommon.Create;
begin
 {nothing}
end;

{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
constructor TIpcCommon.Create;
begin
  { Setup Security so anyone can connect to the MMF/Mutex/Event.  This is
    needed when IBX is used in a Service. }

  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  FSa.nLength := SizeOf(FSa);
  FSa.lpSecurityDescriptor := @Sd;
  FSa.bInheritHandle := true;
end;
{$ENDIF}


  { TMutex }

{$IFDEF USE_SV5_IPC}
constructor TMutex.Create(SemNumber: cint);
begin
  inherited Create;
  FMutexSemaphore := SemNumber;
  if FInitialiser then
    SemInit(FMutexSemaphore,1)
end;

{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
constructor TMutex.Create(MutexName: string);
begin
  inherited Create;
  if FInitialiser then
    FMutex := CreateMutex(sa, False, PChar(MutexName))
  else
    FMutex := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName));

  if FMutex = 0 then
    IBError(ibxeCannotCreateSharedResource, [GetLastError])
end;

{$ENDIF}

destructor TMutex.Destroy;
begin
{$IFDEF USE_WINDOWS_IPC}
  CloseHandle(FMutex);
{$ENDIF}
  inherited Destroy;
end;

{ Obtain ownership of the Mutex and prevent other threads from accessing protected resource }

procedure TMutex.Lock;
{$IFDEF USE_SV5_IPC}
begin
  //writeln('Lock: Entering Mutex ',FMutexSemaphore,' LockCount=',FLockCount,' State = ',GetSemValue(FMutexSemaphore));
  if FLockCount = 0 then
    sem_op(FMutexSemaphore,-1);
  Inc(FLockCount);
  //writeln('Lock: Mutex Exit');
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
begin
  WaitForSingleObject(FMutex, INFINITE);
{$ENDIF}
end;

{Give up ownership of the Mutex and allow other threads access }

procedure TMutex.Unlock;
begin
{$IFDEF USE_SV5_IPC}
  //writeln('UnLock: Entering Mutex, LockCount=',FLockCount);
  if FLockCount = 0 then Exit;
  Dec(FLockCount);
  if FLockCount = 0 then
    sem_op(FMutexSemaphore,1);
  //writeln('UnLock: Mutex Exit',' State = ',GetSemValue(FMutexSemaphore));
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  ReleaseMutex(FMutex);
{$ENDIF}
end;

{ TSingleLockGate }
{$IFDEF USE_SV5_IPC}

function TSingleLockGate.GetWaitingThreads: integer;
begin
  Result := FWaitingThreads^
end;

constructor TSingleLockGate.Create(SemNum: cint; AOwner: TGlobalInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FSignalledState := PInteger(FOwner.SharedMemory.Allocate(sizeof(FSignalledState)));
  FWaitingThreads := PInteger(FOwner.SharedMemory.Allocate(sizeof(FWaitingThreads)));
  FSemaphore := SemNum;
  FMutex := SemNum + 1;
  if FInitialiser then
  begin
    FSignalledState^ := 1;
    FWaitingThreads^ := 0;
    SemInit(FSemaphore,0);
    SemInit(FMutex,1);
  end;
end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
constructor TSingleLockGate.Create(EventName: string; AOwner: TGlobalInterface);
begin
  inherited Create;
  FOwner := AOwner;
  if FInitialiser then
    FEvent := CreateEvent(sa, true, true, PChar(EventName))
  else
    FEvent := OpenEvent(EVENT_ALL_ACCESS, true, PChar(EventName));

  if FEvent = 0 then
    IBError(ibxeCannotCreateSharedResource, [GetLastError])
end;

{$ENDIF}

destructor TSingleLockGate.Destroy;
begin
{$IFDEF USE_WINDOWS_IPC}
  CloseHandle(FEvent);
{$ENDIF}
  inherited Destroy;
end;


procedure TSingleLockGate.PassthroughGate;
begin
{$IFDEF USE_SV5_IPC}
  if FSignalledState^ = 0 then
  begin
    sem_op(FMutex,-1,0); //Acquire Mutex
    Inc(FWaitingThreads^);
    sem_op(FMutex,1,0); //Release Mutex
    //writeln(ClassName + ': Wait State Entered ',FSemaphore,' = ',GetSemValue(FSemaphore));
    sem_op(FSemaphore,-1,0); //Enter Wait
    //writeln(ClassName + ': Wait State Ends ',FSemaphore);
  end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  WaitForSingleObject(FEvent,INFINITE)
{$ENDIF}
end;

procedure TSingleLockGate.Unlock;
begin
{$IFDEF USE_SV5_IPC}
  if FSignalledState^ = 0 then
  begin
    FSignalledState^ := 1;
    sem_op(FMutex,-1,0); //Acquire Mutex
    //writeln(ClassName + ': Unlocking' ,FSemaphore);
    sem_op(FSemaphore,FWaitingThreads^,0);
    FWaitingThreads^ := 0;
    sem_op(FMutex,1,0); //Release Mutex
  end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  SetEvent(FEvent) //Event State set to "signaled"
{$ENDIF}
end;

procedure TSingleLockGate.Lock;
begin
{$IFDEF USE_SV5_IPC}
  if FSignalledState^ = 1 then
  begin
    //writeln(ClassName + ': Locking Gate ',FSemaphore);
    SemInit(FSemaphore,0);
    FSignalledState^ := 0;
  end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  ResetEvent(FEvent) //Event State set to "unsignaled"
{$ENDIF}
end;

{ TMultilockGate }

{$IFDEF USE_SV5_IPC}
constructor TMultilockGate.Create(SemNum: cint; AOwner: TGlobalInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FSemaphore := SemNum;
  FMutex := SemNum + 1;
  FLockCount := PInteger(FOwner.SharedMemory.Allocate(sizeof(FLockCount)));
  if FInitialiser then
  begin
    FLockCount^ := 0;
    SemInit(FSemaphore,1);
    SemInit(FMutex,1);
  end;
end;
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
constructor TMultilockGate.Create(EventName: string; AOwner: TGlobalInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FLockCount := PInteger(FOwner.SharedMemory.Allocate(sizeof(FLockCount)));
  if FInitialiser then
  begin
    FEvent := CreateEvent(sa, true, true, PChar(EventName));
    FLockCount^ := 0;
  end
  else
    FEvent := OpenEvent(EVENT_ALL_ACCESS, true, PChar(EventName));

  if FEvent = 0 then
    IBError(ibxeCannotCreateSharedResource, [GetLastError])
end;

{$ENDIF}

destructor TMultilockGate.Destroy;
begin
{$IFDEF USE_WINDOWS_IPC}
  CloseHandle(FEvent);
{$ENDIF}
  inherited Destroy;
end;

function TMultilockGate.GetLockCount: integer;
begin
  Result := FLockCount^
end;

procedure TMultilockGate.Lock;
begin
{$IFDEF USE_SV5_IPC}
    sem_op(FMutex,-1,0); //Acquire Mutex
    if FLockCount^ = 0 then
    begin
      //writeln(ClassName,': Locking ',FSemaphore);
      SemInit(FSemaphore,0);
    end;
    Inc(FLockCount^);
    sem_op(FMutex,1,0); //Release Mutex
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  InterlockedIncrement(FLockCount^);
  ResetEvent(FEvent);
  //writeln('Lock '+IntToStr(FLockCount^));
{$ENDIF}
end;

procedure TMultilockGate.Unlock;
begin
{$IFDEF USE_SV5_IPC}
    sem_op(FMutex,-1,0); //Acquire Mutex
    Dec(FLockCount^);
    if FLockCount^ <= 0 then
    begin
      //writeln(ClassName,': UnLocking ',FSemaphore);
      SemInit(FSemaphore,1);
      FLockCount^ := 0
    end;
    sem_op(FMutex,1,0); //Release Mutex
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  //writeln('Start UnLock '+IntToStr(FLockCount^));
  InterlockedDecrement(FLockCount^);
  if FLockCount^ <= 0 then
  begin
     SetEvent(FEvent);
     FLockCount^ := 0
  end;
  //writeln('UnLock '+IntToStr(FLockCount^));
{$ENDIF}
end;

procedure TMultilockGate.PassthroughGate;
{$IFDEF USE_SV5_IPC}
begin
  if FLockCount^ = 0 then
    Exit;
  //writeln(ClassName,': Waiting on ',FSemaphore);
  while sem_timedop(FSemaphore,-1,cDefaultTimeout) < 0 do
  {looks like we lost a reader}
  begin
    if FLockCount^ > 0 then
    begin
      UnLock;
      if assigned(FOnGateTimeout) then
        OnGateTimeout(self)
    end
  end;
  sem_op(FSemaphore,1);
  //writeln(ClassName,': Wait done on ',FSemaphore);
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
begin
  if FLockCount^ = 0 then
    Exit;
  while WaitForSingleObject(FEvent,cDefaultTimeout)= WAIT_TIMEOUT do
  { If we have timed out then we have lost a reader }
  begin
    if FLockCount^ > 0 then
    begin
      UnLock;
      if assigned(FOnGateTimeout) then
        OnGateTimeout(self)
    end
  end;
{$ENDIF}
end;


{ TGlobalInterface }

function TGlobalInterface.GetMonitorCount: integer;
begin
{$IFDEF USE_SV5_IPC}
  Result := GetSemValue(cMonitorCounter)
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  Result := FMonitorCount^
{$ENDIF}
end;

{$IFDEF USE_WINDOWS_IPC}
procedure TGlobalInterface.HandleGateTimeout(Sender: TObject);
begin
  //writeln(ClassName+': Gate TimeOut');
  DecMonitorCount
end;
{$ENDIF}

constructor TGlobalInterface.Create;
begin
  inherited Create;
  FSharedMemory := TSharedMemory.Create(cMonitorHookSize);

{$IFDEF USE_SV5_IPC}
  FWriteLock := TMutex.Create(cMutexSemaphore);

  FDataAvailableEvent := TSingleLockGate.Create(cDataAvailableEventSemaphore,self);
  FWriterBusyEvent := TSingleLockGate.Create(cWriterBusyEventSemaphore,self);
  FReadReadyEvent := TMultiLockGate.Create(cReadReadyEventSemaphore,self);
  FReadFinishedEvent := TMultiLockGate.Create(cReadFinishedEventSemaphore,self);

  if FInitialiser then
    SemInit(cMonitorCounter,0);
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  FWriteLock := TMutex.Create(PChar(MonitorHookNames[0]));
  FDataAvailableEvent := TSingleLockGate.Create(MonitorHookNames[2],self);
  FWriterBusyEvent := TSingleLockGate.Create(MonitorHookNames[3],self);
  FReadReadyEvent := TMultiLockGate.Create(MonitorHookNames[4],self);
  FReadReadyEvent.OnGateTimeout  := HandleGateTimeout;
  FReadFinishedEvent := TMultiLockGate.Create(MonitorHookNames[5],self);
  FReadFinishedEvent.OnGateTimeout  := HandleGateTimeout;

  FMonitorCount := PInteger(FSharedMemory.Allocate(sizeof(FMonitorCount)));

  if FInitialiser then
    FMonitorCount^ := 0;
{$ENDIF}
  FTraceDataType := PInteger(FSharedMemory.Allocate(sizeof(FTraceDataType)));
  FTimeStamp := PDateTime(FSharedMemory.Allocate(sizeof(FTimeStamp)));
  FBufferSize := PInteger(FSharedMemory.Allocate(sizeof(FBufferSize)));
  FBuffer := FSharedMemory.Allocate(0); //All remaining
  FMaxBufferSize := FSharedMemory.LastAllocationSize;

  if FInitialiser then
  begin
    FBufferSize^ := 0;
    FDataAvailableEvent.Lock
  end;
end;

destructor TGlobalInterface.Destroy;
begin
  if assigned(FWriteLock) then FWriteLock.Free;
  if assigned(FDataAvailableEvent) then FDataAvailableEvent.Free;
  if assigned(FWriterBusyEvent) then FWriterBusyEvent.Free;
  if assigned(FReadReadyEvent) then FReadReadyEvent.Free;
  if assigned(FReadFinishedEvent) then FReadFinishedEvent.Free;
  if assigned(FSharedMemory) then FSharedMemory.Free;
  inherited Destroy;
end;

procedure TGlobalInterface.IncMonitorCount;
begin
{$IFDEF USE_SV5_IPC}
  sem_op(cMonitorCounter,1);
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
  InterlockedIncrement(FMonitorCount^)
{$ENDIF}
end;

procedure TGlobalInterface.DecMonitorCount;
begin
{$IFDEF USE_SV5_IPC}
  sem_op(cMonitorCounter,-1,IPC_NOWAIT);
{$ENDIF}
{$IFDEF USE_WINDOWS_IPC}
   InterlockedDecrement(FMonitorCount^)
{$ENDIF}
end;

procedure TGlobalInterface.SendTrace(TraceObject: TTraceObject);
begin
  FTraceDataType^ := Integer(TraceObject.FDataType);
  FTimeStamp^ := TraceObject.FTimeStamp;
  FBufferSize^ := Min(Length(TraceObject.FMsg), MaxBufferSize);
  Move(TraceObject.FMsg[1], FBuffer^, FBufferSize^);
end;

procedure TGlobalInterface.ReceiveTrace(TraceObject: TTraceObject);
begin
  SetString(TraceObject.FMsg, FBuffer, FBufferSize^);
  TraceObject.FDataType := TTraceFlag(FTraceDataType^);
  TraceObject.FTimeStamp := TDateTime(FTimeStamp^);
end;


var
  FWriterThread : TWriterThread;
  FReaderThread : TReaderThread;
  _MonitorHook: TIBSQLMonitorHook;
  bDone: Boolean;
  CS : TCriticalSection;
  
{ TIBCustomSQLMonitor }

constructor TIBCustomSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTraceFlags := [tfqPrepare .. tfMisc];
  if not (csDesigning in ComponentState)  then
  begin
    {$IFDEF USE_POSTMESSAGE}
    FHWnd := AllocateHWnd(MonitorWndProc);
    {$ENDIF}
    MonitorHook.RegisterMonitor(self);
  end;
  FEnabled := true;
end;

destructor TIBCustomSQLMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled and assigned(_MonitorHook) then
      MonitorHook.UnregisterMonitor(self);
    {$IFDEF USE_POSTMESSAGE}
    DeallocateHwnd(FHWnd);
    {$ENDIF}
  end;
  inherited Destroy;
end;

{$IFDEF USE_POSTMESSAGE}
procedure TIBCustomSQLMonitor.MonitorWndProc(var Message: TMessage);
var
  st : TTraceObject;
begin
  case Message.Msg of
    WM_IBSQL_SQL_EVENT:
    begin
      st := TTraceObject(Message.LParam);
      if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
      st.Free;
    end;
    CM_RELEASE :
      Free;
    else
      DefWindowProc(FHWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;
{$ENDIF}

procedure TIBCustomSQLMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TIBCustomSQLMonitor.ReleaseObject;
begin
  Free
end;

procedure TIBCustomSQLMonitor.ReceiveMessage(Msg: TObject);
var
  st: TTraceObject;
begin
  st := (Msg as TTraceObject);
  if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
  st.Free;
  {$IFDEF WINDOWS}
  Application.ProcessMessages
  {$ENDIF}
end;

procedure TIBCustomSQLMonitor.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if not (csDesigning in ComponentState) then
      if FEnabled then
        Monitorhook.RegisterMonitor(self)
      else
        MonitorHook.UnregisterMonitor(self);
  end;
end;

{ TIBSQLMonitorHook }

constructor TIBSQLMonitorHook.Create;
begin
  inherited Create;
  FTraceFlags := [tfQPrepare..tfMisc];
  FEnabled := false;
end;

destructor TIBSQLMonitorHook.Destroy;
begin
  if assigned(FGlobalInterface) then FGlobalInterface.Free;
  inherited Destroy;
end;

procedure TIBSQLMonitorHook.DBConnect(db: TIBDatabase);
var
  st : String;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Connect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

procedure TIBSQLMonitorHook.DBDisconnect(db: TIBDatabase);
var
  st: String;
begin
  if (Self = nil) then exit;
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Disconnect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

function TIBSQLMonitorHook.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TIBSQLMonitorHook.GetMonitorCount: Integer;
begin
  Result := FGlobalInterface.MonitorCount
end;

function TIBSQLMonitorHook.GetTraceFlags: TTraceFlags;
begin
  Result := FTraceFlags;
end;

procedure TIBSQLMonitorHook.RegisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
begin
   //writeln('Register Monitor');
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
 if not Assigned(FReaderThread) then
    FReaderThread := TReaderThread.Create(FGlobalInterface);
  FReaderThread.AddMonitor(SQLMonitor);
end;

procedure TIBSQLMonitorHook.ReleaseMonitor(Arg: TIBCustomSQLMonitor);
begin
{$IFDEF USE_POSTMESSAGE}
  FWriterThread.ReleaseMonitor(Arg.Handle);
{$ELSE}
  FWriterThread.ReleaseMonitor(Arg);
{$ENDIF}
end;

procedure TIBSQLMonitorHook.SendMisc(Msg: String);
begin
  if FEnabled then
  begin
    WriteSQLData(Msg, tfMisc);
  end;
end;

procedure TIBSQLMonitorHook.ServiceAttach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Attach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceDetach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Detach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceQuery(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Query]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceStart(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Start]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
  if (not FEnabled) and (Assigned(FWriterThread)) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

procedure TIBSQLMonitorHook.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value
end;

procedure TIBSQLMonitorHook.ForceRelease;
begin
    if Assigned(FReaderThread) then
    begin
      FReaderThread.Terminate;
      if not Assigned(FWriterThread) then
        FWriterThread := TWriterThread.Create(FGlobalInterface);
      FWriterThread.WriteSQLData(' ', tfMisc);
    end;
end;

procedure TIBSQLMonitorHook.SQLExecute(qry: TIBSQL);
var
  st: String;
  i: Integer;
begin
  if FEnabled then
  begin
    if not ((tfQExecute in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags)) ) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Execute] ' + qry.SQL.Text; {do not localize}
    if qry.Params.Count > 0 then begin
      for i := 0 to qry.Params.Count - 1 do begin
        st := st + CRLF + '  ' + qry.Params[i].Name + ' = '; 
        try
          if qry.Params[i].IsNull then
            st := st + '<NULL>'; {do not localize}
          st := st + qry.Params[i].AsString;
        except
          st := st + '<' + SCantPrintValue + '>';
        end;
      end;
    end;
    WriteSQLData(st, tfQExecute);
  end;
end;

procedure TIBSQLMonitorHook.SQLFetch(qry: TIBSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQFetch in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Fetch] ' + qry.SQL.Text; {do not localize}
    if (qry.EOF) then
      st := st + CRLF + '  ' + SEOFReached;
    WriteSQLData(st, tfQFetch);
  end;
end;

procedure TIBSQLMonitorHook.SQLPrepare(qry: TIBSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQPrepare in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Prepare] ' + qry.SQL.Text + CRLF; {do not localize}
    st := st + '  Plan: ' + qry.Plan; {do not localize}
    WriteSQLData(st, tfQPrepare);
  end;
end;

procedure TIBSQLMonitorHook.TRCommit(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit (Hard commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRCommitRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit retaining (Soft commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRRollback(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRRollbackRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback retaining (Soft rollback)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRStart(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Start transaction]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.UnregisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
var
  Created : Boolean;
begin
//writeln('Unregister Monitor');
  if assigned(FReaderThread) then
  begin
    FReaderThread.RemoveMonitor(SQLMonitor);
    if FReaderThread.FMonitors.Count = 0 then
    begin
      FReaderThread.Terminate;

      { There is a possibility of a reader thread, but no writer one.
        When in that situation, the reader needs to be released after
        the terminate is set.  To do that, create a Writer thread, send
        the release code (a string of ' ' and type tfMisc) and then free
        it up. }
      
      Created := false;
      if not Assigned(FWriterThread) then
      begin
        FWriterThread := TWriterThread.Create(FGlobalInterface);
        Created := true;
      end;
      FWriterThread.WriteSQLData(' ', tfMisc);
      //writeln('Wait for read Terminate');
      FReaderThread.WaitFor;
      if assigned(FReaderThread.FatalException) then
        IBError(ibxeThreadFailed,['Reader',Exception(FReaderThread.FatalException).Message]);
      //writeln('Freeing Reader Thread');
      FreeAndNil(FReaderThread);
      //writeln('Reader Thread Freed');
      if Created then
      begin
        FWriterThread.Terminate;
        //writeln('Wait for write Terminate');
        FWriterThread.WaitFor;
        if assigned(FWriterThread.FatalException) then
          IBError(ibxeThreadFailed,['Writer',Exception(FWriterThread.FatalException).Message]);
        FreeAndNil(FWriterThread);
      end;
    end;
  end;
  //writeln('Unregister done')
end;

procedure TIBSQLMonitorHook.WriteSQLData(Text: String;
  DataType: TTraceFlag);
begin
 //writeln('Write SQL Data: '+Text);
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
  Text := CRLF + '[Application: ' + Application.Title + ']' + CRLF + Text; {do not localize}
  if not Assigned(FWriterThread) then
    FWriterThread := TWriterThread.Create(FGLobalInterface);
  FWriterThread.WriteSQLData(Text, DataType);
end;

{ TWriterThread }

constructor TWriterThread.Create(GlobalInterface: TGlobalInterface);

begin
  inherited Create(true);
  //writeln('Write Object Created');
  FGlobalInterface := GlobalInterface;
  FMsgs := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
  FMsgAvailable := TEventObject.Create(FGlobalInterface.Sa,true,false,cWriteMessageAvailable);
  Resume;
end;

destructor TWriterThread.Destroy;
begin
  if assigned(FMsgs) then FMsgs.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FMsgAvailable) then FMsgAvailable.Free;
  inherited Destroy;
end;

procedure TWriterThread.Execute;
begin
//writeln('Write Thread starts');
 try
  { Place thread code here }
  while ((not Terminated) and (not bDone)) or
        (FMsgs.Count <> 0) do
  begin
    FMsgAvailable.WaitFor(cMsgWaitTime);
    { Any one listening? }
    if FGlobalInterface.MonitorCount = 0 then
    begin
      if FMsgs.Count <> 0 then
      begin
        //writeln('Write Thread Drop Message');
        RemoveFromList;
      end;
    end
    else
      { Anything to process? }
      if FMsgs.Count <> 0 then
      begin
       { If the current queued message is a release release the object }
        if FMsgs.Items[0] is TReleaseObject then
        begin
          //writeln('Post Release');
          {$IFDEF USE_POSTMESSAGE}
          PostMessage(TReleaseObject(FMsgs.Items[0]).FHandle, CM_RELEASE, 0, 0);
          {$ELSE}
          Synchronize(PostRelease);
          {$ENDIF}
        end
        else
        { Otherwise write the TraceObject to the buffer }
        begin
          WriteToBuffer;
        end;
      end
      else
      begin
        FCriticalSection.Enter;
        try
          if FMsgs.Count = 0 then
          FMsgAvailable.ResetEvent
        finally
          FCriticalSection.Leave
        end;
      end;
  end;
 except on E: Exception do
   begin
     //writeln('Write Thread raised Exception: ' + E.Message);
     raise
   end
  end;
  //writeln('Write Thread Ends');
end;

procedure TWriterThread.WriteSQLData(Msg : String; DataType: TTraceFlag);
begin
  FCriticalSection.Enter;
  try
    FMsgs.Add(TTraceObject.Create(Msg, DataType));
  finally
    FCriticalSection.Leave;
  end;
  FMsgAvailable.SetEvent
end;

procedure TWriterThread.BeginWrite;
begin
//writeln('Begin Write');
  with FGlobalInterface do
  begin
    ReadReadyEvent.PassThroughGate;    {Wait for readers to become ready }
    WriterBusyEvent.Lock;     {Set Busy State}
  end;
//writeln('Begin Write Complete');
end;

procedure TWriterThread.EndWrite;
begin
  //writeln('End Write');
  with FGlobalInterface do
  begin
    DataAvailableEvent.Unlock;   { Signal Data Available. }
    ReadFinishedEvent.PassThroughGate; {Wait for readers to finish }
    DataAvailableEvent.Lock;  {reset Data Available }
    WriterBusyEvent.Unlock;      {Signal not Busy }
  end;
  //writeln('End Write Complete');
  end;

procedure TWriterThread.WriteToBuffer;
var I, len: integer;
    Temp: TTraceObject;
begin
  //writeln('Write to Buffer');
  FGlobalInterface.WriteLock.Lock;
  try
    { If there are no monitors throw out the message
      The alternative is to have messages queue up until a
      monitor is ready.}

    if FGlobalInterface.MonitorCount = 0 then
      RemoveFromList
    else
    begin
      i := 1;
      len := Length(TTraceObject(FMsgs[0]).FMsg);
      if len <= FGlobalInterface.MaxBufferSize then
      begin
        BeginWrite;
        try
          FGlobalInterface.SendTrace(TTraceObject(FMsgs[0]))
        finally
          RemoveFromList;
          EndWrite
        end;
      end
      else
      try
        while len > 0 do
        begin
          //writeln('Sending Partial Message, len = ',len);
          Temp := TTraceObject.Create(TTraceObject(FMsgs[0]),i,Min(len,FGlobalInterface.MaxBufferSize));
          try
            BeginWrite;
            FGlobalInterface.SendTrace(Temp);
            Inc(i,FGlobalInterface.MaxBufferSize);
            Dec(len,FGlobalInterface.MaxBufferSize);
          finally
            Temp.Free;
            EndWrite
          end
        end;
      finally
        RemoveFromList;
      end
    end;
  finally
    FGlobalInterface.WriteLock.Unlock;
  end;
  //writeln('Done Write');
end;

procedure TWriterThread.RemoveFromList;
begin
  //writeln('Write Thread: Remove object From List');
  FCriticalSection.Enter;
  try
    FMsgs.Remove(FMsgs[0]); { Pop the written item }
  finally
    FCriticalSection.Leave;
  end;
end;

{$IFDEF USE_POSTMESSAGE}
procedure TWriterThread.ReleaseMonitor(Handle: THandle);
begin
  FMsgs.Add(TReleaseObject.Create(Handle));
end;
{$ELSE}
procedure TWriterThread.PostRelease;
var Monitor: TIBCustomSQLMonitor;
begin
  Monitor := TReleaseObject(FMsgs.Items[0]).FMonitor;
  Monitor.ReleaseObject
end;

procedure TWriterThread.ReleaseMonitor(Arg : TIBCustomSQLMonitor);
begin
  FMsgs.Add(TReleaseObject.Create(Arg));
end;
{$ENDIF}

{ TTraceObject }

constructor TTraceObject.Create(Msg : String; DataType: TTraceFlag);
begin
  FMsg := Msg;
  FDataType := DataType;
  FTimeStamp := Now;
end;

constructor TTraceObject.Create(obj: TTraceObject);
begin
  FMsg := obj.FMsg;
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
end;

constructor TTraceObject.Create(obj: TTraceObject; MsgOffset, MsgLen: integer);
begin
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
  FMsg := copy(obj.FMsg,MsgOffset,MsgLen)
end;

{ TReleaseObject }

{$IFDEF USE_POSTMESSAGE}
constructor TReleaseObject.Create(Handle: THandle);
begin
  FHandle := Handle
end;

{$ELSE}
constructor TReleaseObject.Create(Monitor : TIBCustomSQLMonitor);
begin
  FMonitor := Monitor;
end;
{$ENDIF}

{ ReaderThread }

procedure TReaderThread.AddMonitor(Arg: TIBCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    if FMonitors.IndexOf(Arg) < 0 then
      FMonitors.Add(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

procedure TReaderThread.AlertMonitors;
var i : Integer;
    FTemp : TTraceObject;
    Monitor: TIBCustomSQLMonitor;
begin
  for i := 0 to FMonitors.Count - 1 do
  begin
     //writeln('Sending Message to Monitor ' +IntToStr(i));
     FTemp := TTraceObject.Create(st);
     Monitor := TIBCustomSQLMonitor(FMonitors[i]);
     Monitor.ReceiveMessage(FTemp);
  end;
end;

procedure TReaderThread.BeginRead;
begin
//writeln('Begin Read');
  with FGlobalInterface do
  begin
    WriterBusyEvent.PassthroughGate;     { Wait for Writer not busy}
    ReadFinishedEvent.Lock;          { Prepare Read Finished Gate}
    ReadReadyEvent.Unlock;                { Signal read ready  }
    //writeln('Read Ready Unlocked');
    DataAvailableEvent.PassthroughGate;  { Wait for a Data Available }
  end;
//writeln('Begin Read Complete');
end;

constructor TReaderThread.Create(GlobalInterface: TGlobalInterface);
begin
  inherited Create(true);
  FGlobalInterface := GlobalInterface;
  st := TTraceObject.Create('', tfMisc);
  FGlobalInterface.IncMonitorCount;
  FMonitors := TObjectList.Create(false);
  FCriticalSection := TCriticalSection.Create;
  //writeln('Reader Thread Created');
  FGlobalInterface.ReadReadyEvent.Lock;           { Initialise Read Ready}
  Resume;
end;

destructor TReaderThread.Destroy;
begin
//writeln('Reader Thread Destory');
  FGlobalInterface.ReadReadyEvent.UnLock;
  if assigned(FGlobalInterface) and (FGlobalInterface.MonitorCount > 0) then
     FGlobalInterface.DecMonitorCount;
  FMonitors.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  st.Free;
  inherited Destroy;
end;

procedure TReaderThread.EndRead;
begin
//writeln('End Read');
  FGlobalInterface.ReadReadyEvent.Lock;           { reset Read Ready}
  FGlobalInterface.ReadFinishedEvent.Unlock; {Signal Read completed }
  //writeln('End Read Complete');
end;

procedure TReaderThread.Execute;
{$IFDEF USE_POSTMESSAGE}
var
  i : Integer;
  FTemp : TTraceObject;
{$ENDIF}
begin
//writeln('Read Thread Starts');
  { Place thread code here }
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData;
    if (st.FMsg <> '') and
       not ((st.FMsg = ' ') and (st.FDataType = tfMisc)) then
    begin
      //writeln('Sending Message to Monitors');
      {$IFDEF USE_POSTMESSAGE}
      for i := 0 to FMonitors.Count - 1 do
      begin
        FTemp := TTraceObject.Create(st);
        PostMessage(TIBCustomSQLMonitor(FMonitors[i]).Handle,
                    WM_IBSQL_SQL_EVENT,
                    0,
                    LPARAM(FTemp));
      end;
      {$ELSE}
      Synchronize(AlertMonitors);
      {$ENDIF}
    end;
  end;
  //writeln('Read Thread Ends');
end;

procedure TReaderThread.ReadSQLData;
begin
  st.FMsg := '';
  BeginRead;
  if not bDone then
  try
    FGlobalInterface.ReceiveTrace(st)
  finally
    EndRead;
  end;
end;

procedure TReaderThread.RemoveMonitor(Arg: TIBCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    FMonitors.Remove(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

{ Misc methods }

function MonitorHook: IIBSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
  begin
    CS.Enter;
    if (_MonitorHook = nil) and (not bDone) then
    begin
      _MonitorHook := TIBSQLMonitorHook.Create;
      _MonitorHook._AddRef;
    end;
    CS.Leave;
  end;
  result := _MonitorHook;
end;

procedure EnableMonitoring;
begin
  MonitorHook.Enabled := True;
end;

procedure DisableMonitoring;
begin
  MonitorHook.Enabled := False;
end;

function MonitoringEnabled: Boolean;
begin
  result := MonitorHook.Enabled;
end;

procedure CloseThreads;
begin
//writeln('Closed Threads Called');
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Terminate;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end;
  if Assigned(FWriterThread) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

initialization
  CS := TCriticalSection.Create;
  _MonitorHook := nil;
  FWriterThread := nil;
  FReaderThread := nil;
  bDone := False;
{$IFDEF USE_SV5_IPC}
  if FpGetEnv('FBSQL_IPCFILENAME') <> nil then
    IPCFileName := strpas(FpGetEnv('FBSQL_IPCFILENAME'))
  else
    IPCFileName := '/tmp/' + IPCFileName + '.' + strpas(FpGetEnv('USER'));
{$ENDIF}

finalization
  //writeln('Entered Finalisation');
  try
    { Write an empty string to force the reader to unlock during termination }
    bDone := True;
    if Assigned(_MonitorHook) then
      _MonitorHook.ForceRelease;
    CloseThreads;
    if Assigned(_MonitorHook) then
      _MonitorHook._Release;

  finally
    _MonitorHook := nil;
    if assigned(CS) then CS.Free;
  end;
end.
