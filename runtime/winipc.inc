const
  MonitorHookNames: array[0..5] of String = (
    'FB.SQL.MONITOR.Mutex1_0',
    'FB.SQL.MONITOR.SharedMem1_0',
    'FB.SQL.MONITOR.WriteEvent1_0',
    'FB.SQL.MONITOR.WriteFinishedEvent1_0',
    'FB.SQL.MONITOR.ReadEvent1_0',
    'FB.SQL.MONITOR.ReadFinishedEvent1_0'
  );
  cDefaultTimeout = 1000; { 1 seconds }

type
  TGlobalInterface = class;

  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TIpcCommon }

  TIpcCommon = class
  private
    function GetSa: PSecurityAttributes;
  protected
    class var FInitialiser: boolean;
    FSa : TSecurityAttributes;
  private
    Sd : TSecurityDescriptor;
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

  }

  TSharedMemory = class(TIpcCommon)
  private
    FBuffer: PChar;
    FLastAllocationSize: integer;
    FUnused: integer;
    FBufptr: PChar;
    FSharedBuffer: THandle;
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
    FMutex: THandle;
  public
    constructor Create(MutexName: string);
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


    Always initialised to the Unlocked state
  }

  TSingleLockGate = class(TIpcCommon)
  private
    FOwner: TGlobalInterface;
    FEvent: THandle;
  public
    constructor Create(EventName: string; AOwner: TGlobalInterface);
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
    FEvent: THandle;
    FLockCount: PInteger;
    FMutex: TMutex;
   function GetLockCount: integer;
  public
    constructor Create(EventName: string; AOwner: TGlobalInterface);
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
    FMonitorCount: PInteger;
    procedure HandleGateTimeout(Sender: TObject);
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

constructor TSharedMemory.Create(MemSize: integer);
begin
  inherited Create;
  FInitialiser := false;
  GetSharedMemory(MemSize);
  FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if FBuffer = nil then
      IBError(ibxeCannotCreateSharedResource, [GetLastError]);
  FBufPtr := FBuffer;
  FUnused := MemSize
end;

destructor TSharedMemory.Destroy;
begin
  UnmapViewOfFile(FBuffer);
  CloseHandle(FSharedBuffer);
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
  Result := @FSa
end;

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


  { TMutex }

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

destructor TMutex.Destroy;
begin
  CloseHandle(FMutex);
  inherited Destroy;
end;

{ Obtain ownership of the Mutex and prevent other threads from accessing protected resource }

procedure TMutex.Lock;
begin
  WaitForSingleObject(FMutex, INFINITE);
end;

{Give up ownership of the Mutex and allow other threads access }

procedure TMutex.Unlock;
begin
  ReleaseMutex(FMutex);
end;

{ TSingleLockGate }
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

destructor TSingleLockGate.Destroy;
begin
  CloseHandle(FEvent);
  inherited Destroy;
end;


procedure TSingleLockGate.PassthroughGate;
begin
  WaitForSingleObject(FEvent,INFINITE)
end;

procedure TSingleLockGate.Unlock;
begin
  SetEvent(FEvent) //Event State set to "signaled"
end;

procedure TSingleLockGate.Lock;
begin
  ResetEvent(FEvent) //Event State set to "unsignaled"
end;

{ TMultilockGate }

constructor TMultilockGate.Create(EventName: string; AOwner: TGlobalInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FLockCount := PInteger(FOwner.SharedMemory.Allocate(sizeof(FLockCount)));
  FMutex := TMutex.Create(EventName + '.Mutex');
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

destructor TMultilockGate.Destroy;
begin
  if assigned(FMutex) then FMutex.Free;
  CloseHandle(FEvent);
  inherited Destroy;
end;

function TMultilockGate.GetLockCount: integer;
begin
  Result := FLockCount^
end;

procedure TMultilockGate.Lock;
begin
  FMutex.Lock;
  try
    Inc(FLockCount^);
    ResetEvent(FEvent);
  finally
    FMutex.Unlock;
  end;
  //writeln('Lock '+IntToStr(FLockCount^));
end;

procedure TMultilockGate.Unlock;
begin
  //writeln('Start UnLock '+IntToStr(FLockCount^));
  FMutex.Lock;
  try
    Dec(FLockCount^);
    if FLockCount^ <= 0 then
    begin
       SetEvent(FEvent);
       FLockCount^ := 0
    end;
  finally
    FMutex.Unlock;
  end;
  //writeln('UnLock '+IntToStr(FLockCount^));
end;

procedure TMultilockGate.PassthroughGate;
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
end;


{ TGlobalInterface }

function TGlobalInterface.GetMonitorCount: integer;
begin
  Result := FMonitorCount^
end;

procedure TGlobalInterface.HandleGateTimeout(Sender: TObject);
begin
  //writeln(ClassName+': Gate TimeOut');
  DecMonitorCount
end;

constructor TGlobalInterface.Create;
begin
  inherited Create;
  FSharedMemory := TSharedMemory.Create(cMonitorHookSize);

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
  InterlockedIncrement(FMonitorCount^)
end;

procedure TGlobalInterface.DecMonitorCount;
begin
   InterlockedDecrement(FMonitorCount^)
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



