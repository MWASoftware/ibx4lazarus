{Used by ISQLMonitor and implements System V IPC}

const
  IPCFileName: string = 'FB.SQL.MONITOR1_0';
  cNumberOfSemaphores = 10;
  cMutexSemaphore = 0;
  cMonitorCounter = 1;
  cReadReadyEventSemaphore = 2;
  cReadFinishedEventSemaphore = 4;
  cDataAvailableEventSemaphore = 6;
  cWriterBusyEventSemaphore = 8;
  cDefaultTimeout = 1; { 1 seconds }

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

type
  TGlobalInterface = class;
  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TIpcCommon }

  TIpcCommon = class
   private
    function GetSa: PSecurityAttributes;
 protected
    FInitialiser: boolean;  static;
    FSemaphoreSetID: cint;  static;
    FSharedMemoryID: cint;  static;
    function sem_op(SemNum, op: integer; flags: cshort = 0): cint;
    function sem_timedop(SemNum, op: integer; timeout_secs: integer; flags: cshort = 0): cint;
    function GetSemValue(SemNum: integer): cint;
    procedure SemInit(SemNum, AValue: cint);
  public
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

  TSharedMemory = class(TIpcCommon)
  private
    FBuffer: PChar;
    FLastAllocationSize: integer;
    FUnused: integer;
    FBufptr: PChar;
    procedure DropSharedMemory;
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

  TSingleLockGate = class(TIpcCommon)
  private
    FOwner: TGlobalInterface;
    FSemaphore: cint;
    FMutex: cint;
    FSignalledState: PInteger;
    FWaitingThreads: PInteger;
    function GetWaitingThreads: integer;
  public
    constructor Create(SemNum: cint; AOwner: TGlobalInterface);
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

  TMultilockGate = class(TIpcCommon)
  private
    FOnGateTimeout: TNotifyEvent;
    FOwner: TGlobalInterface;
    FSemaphore: cint;
    FMutex: cint;
    FLockCount: PInteger;
    function GetLockCount: integer;
  public
    constructor Create(SemNum: cint; AOwner: TGlobalInterface);
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

procedure TSharedMemory.GetSharedMemory(MemSize: integer);
var F: cint;
begin
    {Get the Shared Memory and Semaphore IDs from the Global File if it exists
     or create them and the file otherwise }

    repeat
      F := fpOpen(IPCFileName, O_WrOnly or O_Creat or O_Excl);
      if F < 0 then
      begin
        if fpgetErrno = ESysEEXIST {EEXIST} then
        begin
          { looks like it already exists}
          Sleep(100);
          F := fpOpen(IPCFileName,O_RdOnly);
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
    DeleteFile(IPCFileName);
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
{$IFDEF HAS_SEMTIMEDOP}
    Result := semtimedop(FSemaphoreSetID,@sembuf,1,@timeout);
{$ELSE}
    Result := semop(FSemaphoreSetID,@sembuf,1);    {May hang on race condition}
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
    //writeln(ClassName + ': Unlocking' ,FSemaphore);
    sem_op(FSemaphore,FWaitingThreads^,0);
    FWaitingThreads^ := 0;
    sem_op(FMutex,1,0); //Release Mutex
  end;
end;

procedure TSingleLockGate.Lock;
begin
  if FSignalledState^ = 1 then
  begin
    //writeln(ClassName + ': Locking Gate ',FSemaphore);
    SemInit(FSemaphore,0);
    FSignalledState^ := 0;
  end;
end;

{ TMultilockGate }

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

function TMultilockGate.GetLockCount: integer;
begin
  Result := FLockCount^
end;

procedure TMultilockGate.Lock;
begin
    sem_op(FMutex,-1,0); //Acquire Mutex
    if FLockCount^ = 0 then
    begin
      //writeln(ClassName,': Locking ',FSemaphore);
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
      //writeln(ClassName,': UnLocking ',FSemaphore);
      SemInit(FSemaphore,1);
      FLockCount^ := 0
    end;
    sem_op(FMutex,1,0); //Release Mutex
end;

procedure TMultilockGate.PassthroughGate;
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
end;


{ TGlobalInterface }

function TGlobalInterface.GetMonitorCount: integer;
begin
  Result := GetSemValue(cMonitorCounter)
end;

constructor TGlobalInterface.Create;
begin
  inherited Create;
  FSharedMemory := TSharedMemory.Create(cMonitorHookSize);

  FWriteLock := TMutex.Create(cMutexSemaphore);

  FDataAvailableEvent := TSingleLockGate.Create(cDataAvailableEventSemaphore,self);
  FWriterBusyEvent := TSingleLockGate.Create(cWriterBusyEventSemaphore,self);
  FReadReadyEvent := TMultiLockGate.Create(cReadReadyEventSemaphore,self);
  FReadFinishedEvent := TMultiLockGate.Create(cReadFinishedEventSemaphore,self);

  if FInitialiser then
    SemInit(cMonitorCounter,0);
  FTraceDataType := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FTimeStamp := PDateTime(FSharedMemory.Allocate(sizeof(TDateTime)));
  FBufferSize := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
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
  sem_op(cMonitorCounter,1);
end;

procedure TGlobalInterface.DecMonitorCount;
begin
  sem_op(cMonitorCounter,-1,IPC_NOWAIT);
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



