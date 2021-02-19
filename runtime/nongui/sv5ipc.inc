{Used by IBIPC and implements System V IPC}

uses IBMessages, ipc, Errors, baseunix;

const
  IPCFileName: string = 'FB.SQL.MONITOR1_0';
  cNumberOfSemaphores = 10;
  cMutexSemaphore = 0;
  cMonitorCounter = 1;
  cReadReadyEventSemaphore = 2;
  cReadFinishedEventSemaphore = 4;
  cDataAvailableEventSemaphore = 6;
  cWriterBusyEventSemaphore = 8;
  cDefaultTimeout = 1000; {seconds }

{$IF FPC_FULLVERSION = 30000 }
{Fix regression in FPC 3.0.0 ipc.pp unit. Expected to be fixed in fpc 3.0.2}
{$IF defined(darwin) }
SEM_GETNCNT = 3;   { Return the value of sempid (READ)  }
SEM_GETPID  = 4;   { Return the value of semval (READ)  }
SEM_GETVAL  = 5;   { Return semvals into arg.array (READ)  }
SEM_GETALL  = 6;   { Return the value of semzcnt (READ)  }
SEM_GETZCNT = 7;   { Set the value of semval to arg.val (ALTER)  }
SEM_SETVAL  = 8;   { Set semvals from arg.array (ALTER)  }
SEM_SETALL  = 9;
{$ENDIF}
{$ENDIF}

function GetLastErrno: cint;
begin
  Result := fpgetErrno
end;

type
  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TIpcCommon }

  TIpcCommon = class(TInterfacedObject)
  protected
    FInitialiser: boolean;  static;
    FSemaphoreSetID: cint;  static;
    FSharedMemoryID: cint;  static;
    function sem_op(SemNum, op: integer; flags: cshort = 0): cint;
    function sem_timedop(SemNum, op: integer; timeout_secs: integer; flags: cshort = 0): cint;
    function GetSemValue(SemNum: integer): cint;
    procedure SemInit(SemNum, AValue: cint);
  public
    function GetSa: PSecurityAttributes;
    property Sa : PSecurityAttributes read GetSa;
  end;

  { TSharedMemory }

  {
    The shared memory segment is used for interprocess communication and
    holds both a message buffer and a number of shared variables. Shared
    memory is allocated to each shared variable using the Allocate function.
    An underlying assumption is that each process using the shared memory
    calls "Allocate" in the same order and for the same memory sizes.

    Linux:

    The Linux implementation uses Linux shared memory. IPC_PRIVATE is used
    to allocate the memory and the resulting memory id is written to a
    well known file. By default this is in the current user's home directory,
    but this can be over-ridden to specify a globally unique filename.

    Access to the shared memory is restricted to the current user/group.
    Note that the Linux semaphore set is also created with the shared memory.
  }

  TSharedMemory = class(TIpcCommon,ISharedMemory)
  private
    FBuffer: PByte;
    FLastAllocationSize: integer;
    FUnused: integer;
    FBufptr: PByte;
    FIPCFileName: AnsiString;
    procedure DropSharedMemory;
    procedure GetSharedMemory(MemSize: integer);
  public
    constructor Create(MemSize: integer);
    destructor Destroy; override;
    function Allocate(Size: integer): PByte;
    function GetLastAllocationSize: integer;
    property LastAllocationSize: integer read GetLastAllocationSize;
  end;

  {TMutex}

  TMutex = class(TIpcCommon,IMutex)
  private
    FMutexSemaphore: cint;
    FLockCount: integer;
  public
    constructor Create(SemNumber: cint);
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

  TSingleLockGate = class(TIpcCommon,ISingleLockGate)
  private
    FSharedMemory: ISharedMemory;
    FSemaphore: cint;
    FMutex: cint;
    FSignalledState: PInteger;
    FWaitingThreads: PInteger;
    function GetWaitingThreads: integer;
  public
    constructor Create(SemNum: cint; sm: ISharedMemory);
    property WaitingThreads: integer read GetWaitingThreads;
  public
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

    Always initialised to the Unlocked state
  }

  TMultilockGate = class(TIpcCommon,IMultiLockGate)
  private
    FSharedMemory: ISharedMemory;
    FOnGateTimeout: TNotifyEvent;
    FSemaphore: cint;
    FMutex: cint;
    FLockCount: PInteger;
    function GetLockCount: integer;
  public
    constructor Create(SemNum: cint; sm: ISharedMemory);
    procedure Lock;
    procedure Unlock;
    procedure PassthroughGate;
    function GetOnGateTimeout: TNotifyEvent;
    procedure SetOnGateTimeout(AValue: TNotifyEvent);
    property LockCount: integer read GetLockCount;
    property OnGateTimeout: TNotifyEvent read GetOnGateTimeout write SetOnGateTimeout;
  end;

  { TIPCInterface }

  TIPCInterface = class(TIpcCommon,IIPCInterface)
  private
    FMaxBufferSize: integer;
    FSharedMemory: ISharedMemory;
    FWriteLock: IMutex;
    FBuffer: PByte;
    FTraceDataType,
    FBufferSize: PInteger;
    FTimeStamp: PDateTime;
    FMsgNumber: PInteger;
    FReadReadyEvent: IMultiLockGate;
    FReadFinishedEvent: IMultiLockGate;
    FDataAvailableEvent: ISingleLockGate;
    FWriterBusyEvent: ISingleLockGate;
  public
    constructor Create;
    procedure IncMonitorCount;
    procedure DecMonitorCount;
    procedure SendTrace(TraceObject: TTraceObject);
    procedure ReceiveTrace(TraceObject: TTraceObject);
    function GetDataAvailableEvent: ISingleLockGate;
    function GetWriterBusyEvent: ISingleLockGate;
    function GetReadReadyEvent: IMultiLockGate;
    function GetReadFinishedEvent: IMultiLockGate;
    function GetWriteLock: IMutex;
    function GetMonitorCount: integer;
    function GetSharedMemory: ISharedMemory;
    function GetMaxBufferSize: integer;
    property DataAvailableEvent: ISingleLockGate read GetDataAvailableEvent;
    property WriterBusyEvent: ISingleLockGate read GetWriterBusyEvent;
    property ReadReadyEvent: IMultiLockGate read GetReadReadyEvent;
    property ReadFinishedEvent: IMultiLockGate read GetReadFinishedEvent;
    property WriteLock: IMutex read GetWriteLock;
    property MonitorCount: integer read GetMonitorCount;
    property SharedMemory: ISharedMemory read GetSharedMemory;
    property MaxBufferSize: integer read GetMaxBufferSize;
  end;

{ TSharedMemory }

procedure TSharedMemory.GetSharedMemory(MemSize: integer);
var F: cint;
begin
  if GetEnvironmentVariable('FBSQL_IPCFILENAME') <> '' then
    FIPCFileName := GetEnvironmentVariable('FBSQL_IPCFILENAME')
  else
    FIPCFileName := GetTempDir(true) + IPCFileName + '.' + GetEnvironmentVariable('USER');

    {Get the Shared Memory and Semaphore IDs from the Global File if it exists
     or create them and the file otherwise }

    repeat
      F := fpOpen(FIPCFileName, O_WrOnly or O_Creat or O_Excl);
      if F < 0 then
      begin
        if fpgetErrno = ESysEEXIST {EEXIST} then
        begin
          { looks like it already exists}
          Sleep(100);
          F := fpOpen(FIPCFileName,O_RdOnly);
          if (F < 0) and (fpgetErrno = ESysENOENT {ENOENT}) then
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
    DeleteFile(FIPCFileName);
  end;
end;

constructor TSharedMemory.Create(MemSize: integer);
begin
  inherited Create;
  FInitialiser := false;
  GetSharedMemory(MemSize);
  FBuffer := shmat(FSharedMemoryID,nil,0);
  if PtrInt(FBuffer) = -1 then
    IBError(ibxeCannotCreateSharedResource,[StrError(Errno)]);
  FBufPtr := FBuffer;
  FUnused := MemSize
end;

destructor TSharedMemory.Destroy;
begin
  shmdt(FBuffer);
  DropSharedMemory;
  inherited Destroy;
end;

function TSharedMemory.Allocate(Size: integer): PByte;
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

function TSharedMemory.GetLastAllocationSize: integer;
begin
  Result := FLastAllocationSize;
end;

{ TIpcCommon }

function TIpcCommon.GetSa: PSecurityAttributes;
begin
  Result := nil
end;

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
{$IF declared(semtimedop)}
    Result := semtimedop(FSemaphoreSetID,@sembuf,1,@timeout);
{$ELSE}
    Result := semop(FSemaphoreSetID,@sembuf,1);    {May hang on race condition}
{$IFEND}
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

  { TMutex }

constructor TMutex.Create(SemNumber: cint);
begin
  inherited Create;
  FMutexSemaphore := SemNumber;
  if FInitialiser then
    SemInit(FMutexSemaphore,1)
end;

{ Obtain ownership of the Mutex and prevent other threads from accessing protected resource }

procedure TMutex.Lock;
begin
  //writeln('Lock: Entering Mutex ',FMutexSemaphore,' LockCount=',FLockCount,' State = ',GetSemValue(FMutexSemaphore));
  if FLockCount = 0 then
    sem_op(FMutexSemaphore,-1);
  Inc(FLockCount);
  //writeln('Lock: Mutex Exit');
end;

{Give up ownership of the Mutex and allow other threads access }

procedure TMutex.Unlock;
begin
  //writeln('UnLock: Entering Mutex, LockCount=',FLockCount);
  if FLockCount = 0 then Exit;
  Dec(FLockCount);
  if FLockCount = 0 then
    sem_op(FMutexSemaphore,1);
  //writeln('UnLock: Mutex Exit',' State = ',GetSemValue(FMutexSemaphore));
end;

{ TSingleLockGate }

function TSingleLockGate.GetWaitingThreads: integer;
begin
  Result := FWaitingThreads^
end;

constructor TSingleLockGate.Create(SemNum: cint; sm: ISharedMemory);
begin
  inherited Create;
  FSharedMemory := sm;
  FSignalledState := PInteger(FSharedMemory.Allocate(sizeof(FSignalledState)));
  FWaitingThreads := PInteger(FSharedMemory.Allocate(sizeof(FWaitingThreads)));
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

procedure TSingleLockGate.PassthroughGate;
begin
  if FSignalledState^ = 0 then
  begin
    sem_op(FMutex,-1,0); //Acquire Mutex
    Inc(FWaitingThreads^);
    sem_op(FMutex,1,0); //Release Mutex
    //writeln(ClassName + ': Wait State Entered ',FSemaphore,' = ',GetSemValue(FSemaphore));
    sem_op(FSemaphore,-1,0); //Enter Wait
    //writeln(ClassName + ': Wait State Ends ',FSemaphore);
  end;
end;

procedure TSingleLockGate.Unlock;
begin
  if FSignalledState^ = 0 then
  begin
    FSignalledState^ := 1;
    sem_op(FMutex,-1,0); //Acquire Mutex
    {$IFDEF DEBUG}writeln(ClassName + ': Unlocking' ,FSemaphore);{$ENDIF}
    sem_op(FSemaphore,FWaitingThreads^,0);
    FWaitingThreads^ := 0;
    sem_op(FMutex,1,0); //Release Mutex
  end;
end;

procedure TSingleLockGate.Lock;
begin
  if FSignalledState^ = 1 then
  begin
    {$IFDEF DEBUG}writeln(ClassName + ': Locking Gate ',FSemaphore);{$ENDIF}
    SemInit(FSemaphore,0);
    FSignalledState^ := 0;
  end;
end;

{ TMultilockGate }

constructor TMultilockGate.Create(SemNum: cint; sm: ISharedMemory);
begin
  inherited Create;
  FSemaphore := SemNum;
  FMutex := SemNum + 1;
  FSharedMemory := sm;
  FLockCount := PInteger(FSharedMemory.Allocate(sizeof(FLockCount)));
  if FInitialiser then
  begin
    FLockCount^ := 0;
    SemInit(FSemaphore,1);
    SemInit(FMutex,1);
  end;
end;

function TMultilockGate.GetLockCount: integer;
begin
  Result := FLockCount^
end;

function TMultilockGate.GetOnGateTimeout: TNotifyEvent;
begin
  Result := FOnGateTimeout;
end;

procedure TMultilockGate.SetOnGateTimeout(AValue: TNotifyEvent);
begin
  FOnGateTimeout := AValue;
end;

procedure TMultilockGate.Lock;
begin
    sem_op(FMutex,-1,0); //Acquire Mutex
    if FLockCount^ = 0 then
    begin
      {$IFDEF DEBUG}writeln(ClassName,': Locking ',FSemaphore);{$ENDIF}
      SemInit(FSemaphore,0);
    end;
    Inc(FLockCount^);
    sem_op(FMutex,1,0); //Release Mutex
end;

procedure TMultilockGate.Unlock;
begin
    sem_op(FMutex,-1,0); //Acquire Mutex
    Dec(FLockCount^);
    if FLockCount^ <= 0 then
    begin
      {$IFDEF DEBUG}writeln(ClassName,': UnLocking ',FSemaphore);{$ENDIF}
      SemInit(FSemaphore,1);
      FLockCount^ := 0
    end;
    sem_op(FMutex,1,0); //Release Mutex
end;

procedure TMultilockGate.PassthroughGate;
begin
  if FLockCount^ = 0 then
    Exit;
  {$IFDEF DEBUG}writeln(ClassName,': Waiting on ',FSemaphore);{$ENDIF}
  while sem_timedop(FSemaphore,-1,cDefaultTimeout) < 0 do
  {looks like we lost a reader}
  begin
    {$IFDEF DEBUG}writeln(ClassName,': reader lost timeout');{$ENDIF}
    if FLockCount^ > 0 then
    begin
      UnLock;
      if assigned(FOnGateTimeout) then
        OnGateTimeout(self)
    end
  end;
  sem_op(FSemaphore,1);
  {$IFDEF DEBUG}writeln(ClassName,': Wait done on ',FSemaphore);{$ENDIF}
end;


{ TIPCInterface }

function TIPCInterface.GetMonitorCount: integer;
begin
  Result := GetSemValue(cMonitorCounter)
end;

function TIPCInterface.GetSharedMemory: ISharedMemory;
begin
  Result := FSharedMemory;
end;

function TIPCInterface.GetMaxBufferSize: integer;
begin
  Result := FMaxBufferSize;
end;

constructor TIPCInterface.Create;
begin
  inherited Create;
  FSharedMemory := TSharedMemory.Create(cMonitorHookSize);

  FWriteLock := TMutex.Create(cMutexSemaphore);

  FDataAvailableEvent := TSingleLockGate.Create(cDataAvailableEventSemaphore,FSharedMemory);
  FWriterBusyEvent := TSingleLockGate.Create(cWriterBusyEventSemaphore,FSharedMemory);
  FReadReadyEvent := TMultiLockGate.Create(cReadReadyEventSemaphore,FSharedMemory);
  FReadFinishedEvent := TMultiLockGate.Create(cReadFinishedEventSemaphore,FSharedMemory);

  if FInitialiser then
    SemInit(cMonitorCounter,0);
  FTraceDataType := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FTimeStamp := PDateTime(FSharedMemory.Allocate(sizeof(TDateTime)));
  FBufferSize := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FMsgNumber := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FBuffer := FSharedMemory.Allocate(0); //All remaining
  FMaxBufferSize := FSharedMemory.LastAllocationSize;

  if FInitialiser then
  begin
    FBufferSize^ := 0;
    FDataAvailableEvent.Lock;
    FMsgNumber^ := 0;
  end;
end;

procedure TIPCInterface.IncMonitorCount;
begin
  sem_op(cMonitorCounter,1);
end;

procedure TIPCInterface.DecMonitorCount;
begin
  sem_op(cMonitorCounter,-1,IPC_NOWAIT);
end;

procedure TIPCInterface.SendTrace(TraceObject: TTraceObject);
begin
  FTraceDataType^ := Integer(TraceObject.FDataType);
  FTimeStamp^ := TraceObject.FTimeStamp;
  if Length(TraceObject.FMsg) > MaxBufferSize then
    FBufferSize^ := MaxBufferSize
  else
    FBufferSize^ := Length(TraceObject.FMsg);
  FMsgNumber^ := TraceObject.FMsgNumber;
  Move(TraceObject.FMsg[1], FBuffer^, FBufferSize^);
end;

procedure TIPCInterface.ReceiveTrace(TraceObject: TTraceObject);
begin
  SetString(TraceObject.FMsg, PAnsiChar(FBuffer), FBufferSize^);
  TraceObject.FDataType := TTraceFlag(FTraceDataType^);
  TraceObject.FTimeStamp := TDateTime(FTimeStamp^);
  TraceObject.FMsgNumber := FMsgNumber^;
end;

function TIPCInterface.GetDataAvailableEvent: ISingleLockGate;
begin
  Result := FDataAvailableEvent;
end;

function TIPCInterface.GetWriterBusyEvent: ISingleLockGate;
begin
  Result := FWriterBusyEvent;
end;

function TIPCInterface.GetReadReadyEvent: IMultiLockGate;
begin
  Result := FReadReadyEvent;
end;

function TIPCInterface.GetReadFinishedEvent: IMultiLockGate;
begin
  Result := FReadFinishedEvent;
end;

function TIPCInterface.GetWriteLock: IMutex;
begin
  Result := FWriteLock;
end;



