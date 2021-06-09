{Used by IBIPC and implements System V IPC}

uses IBMessages;

const
  MonitorHookNames: array[0..5] of String = (
    'FB.SQL.MONITOR.Mutex1_0',
    'FB.SQL.MONITOR.SharedMem1_0',
    'FB.SQL.MONITOR.WriteEvent1_0',
    'FB.SQL.MONITOR.WriteFinishedEvent1_0',
    'FB.SQL.MONITOR.ReadEvent1_0',
    'FB.SQL.MONITOR.ReadFinishedEvent1_0'
  );
  cDefaultTimeout = 10000; {milli seconds }

type
  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TIpcCommon }

  TIpcCommon = class(TInterfacedObject)
  protected
    FSa : TSecurityAttributes;
  private
    Sd : TSecurityDescriptor;
  public
    constructor Create;
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

    Windows:

    The Windows implementation uses Windows shared memory. This is identified
    by a global name known to every process. There is no security with
    the windows implementation and the shared memory can be read by
    any active process.

  }

  ISharedMemory = interface
  ['{db77bdd4-233a-4c9c-9212-dd7945e2e57c}']
  function IsInitialiser: boolean;
  function Allocate(Size: integer): PByte;
  function GetLastAllocationSize: integer;
  property LastAllocationSize: integer read GetLastAllocationSize;
  end;


  TSharedMemory = class(TIpcCommon,ISharedMemory)
  private
    FBuffer: PByte;
    FLastAllocationSize: integer;
    FUnused: integer;
    FBufptr: PByte;
    FSharedBuffer: THandle;
    FInitialiser: boolean;
    procedure GetSharedMemory(MemSize: integer);
  public
    constructor Create(MemSize: integer);
    destructor Destroy; override;
    function Allocate(Size: integer): PByte;
    function GetLastAllocationSize: integer;
    function IsInitialiser: boolean;
    property LastAllocationSize: integer read GetLastAllocationSize;
  end;

  {TMutex}

  TMutex = class(TIpcCommon, IMutex)
  private
    FMutex: THandle;
  public
    constructor Create(MutexName: string; IsInitialiser: boolean);
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

  TSingleLockGate = class(TIpcCommon,ISingleLockGate)
  private
    FEvent: THandle;
  public
    constructor Create(EventName: string; IsInitialiser: boolean);
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

  TMultilockGate = class(TIpcCommon, IMultilockGate)
  private
    FSharedMemory: ISharedMemory;
    FOnGateTimeout: TNotifyEvent;
    FEvent: THandle;
    FLockCount: PInteger;
    FMutex: TMutex;
  public
    constructor Create(EventName: string; sm: ISharedMemory);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    procedure PassthroughGate;
    function GetLockCount: integer;
    function GetOnGateTimeout: TNotifyEvent;
    procedure SetOnGateTimeout(aValue: TNotifyEvent);
    property LockCount: integer read GetLockCount;
    property OnGateTimeout: TNotifyEvent read GetOnGateTimeout write SetOnGateTimeout;
  end;

  { TIPCInterface }

  TIPCInterface = class(TIpcCommon, IIPCInterface)
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
    FMonitorCount: PInteger;
    procedure HandleGateTimeout(Sender: TObject);
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
    function GetMaxBufferSize: integer;
    property DataAvailableEvent: ISingleLockGate read GetDataAvailableEvent;
    property WriterBusyEvent: ISingleLockGate read GetWriterBusyEvent;
    property ReadReadyEvent: IMultiLockGate read GetReadReadyEvent;
    property ReadFinishedEvent: IMultiLockGate read GetReadFinishedEvent;
    property WriteLock: IMutex read GetWriteLock;
    property MonitorCount: integer read GetMonitorCount;
    property MaxBufferSize: integer read GetMaxBufferSize;
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

function TSharedMemory.IsInitialiser: boolean;
begin
  Result := FInitialiser;
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

constructor TMutex.Create(MutexName: string; IsInitialiser: boolean);
begin
  inherited Create;
  if IsInitialiser then
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
constructor TSingleLockGate.Create(EventName: string; IsInitialiser: boolean);
begin
  inherited Create;
  if IsInitialiser then
    FEvent := CreateEvent(sa, true, true, PAnsiChar(EventName))
  else
    FEvent := OpenEvent(EVENT_ALL_ACCESS, true, PAnsiChar(EventName));

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

constructor TMultilockGate.Create(EventName: string; sm: ISharedMemory);
begin
  inherited Create;
  FSharedMemory := sm;
  FLockCount := PInteger(FSharedMemory.Allocate(sizeof(FLockCount)));
  FMutex := TMutex.Create(EventName + '.Mutex',FSharedMemory.IsInitialiser);
  if FSharedMemory.IsInitialiser then
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

function TMultilockGate.GetOnGateTimeout: TNotifyEvent;
begin
  Result := FOnGateTimeout;
end;

procedure TMultilockGate.SetOnGateTimeout(aValue: TNotifyEvent);
begin
  FOnGateTimeout := AValue;
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


{ TIPCInterface }

function TIPCInterface.GetMonitorCount: integer;
begin
  Result := FMonitorCount^
end;

function TIPCInterface.GetMaxBufferSize: integer;
begin
  Result := FMaxBufferSize;
end;

procedure TIPCInterface.HandleGateTimeout(Sender: TObject);
begin
  //writeln(ClassName+': Gate TimeOut');
  DecMonitorCount
end;

constructor TIPCInterface.Create;
begin
  inherited Create;
  FSharedMemory := TSharedMemory.Create(cMonitorHookSize);

  FWriteLock := TMutex.Create(PChar(MonitorHookNames[0]),FSharedMemory.IsInitialiser);
  FDataAvailableEvent := TSingleLockGate.Create(MonitorHookNames[2],FSharedMemory.IsInitialiser);
  FWriterBusyEvent := TSingleLockGate.Create(MonitorHookNames[3],FSharedMemory.IsInitialiser);
  FReadReadyEvent := TMultiLockGate.Create(MonitorHookNames[4],FSharedMemory);
  FReadReadyEvent.OnGateTimeout  := HandleGateTimeout;
  FReadFinishedEvent := TMultiLockGate.Create(MonitorHookNames[5],FSharedMemory);
  FReadFinishedEvent.OnGateTimeout  := HandleGateTimeout;

  FMonitorCount := PInteger(FSharedMemory.Allocate(sizeof(FMonitorCount)));

  if FSharedMemory.IsInitialiser then
    FMonitorCount^ := 0;
  FTraceDataType := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FTimeStamp := PDateTime(FSharedMemory.Allocate(sizeof(TDateTime)));
  FBufferSize := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FMsgNumber := PInteger(FSharedMemory.Allocate(sizeof(Integer)));
  FBuffer := FSharedMemory.Allocate(0); //All remaining
  FMaxBufferSize := FSharedMemory.LastAllocationSize;

  if FSharedMemory.IsInitialiser then
  begin
    FBufferSize^ := 0;
    FDataAvailableEvent.Lock;
    FMsgNumber^ := 0;
  end;
end;

procedure TIPCInterface.IncMonitorCount;
begin
  InterlockedIncrement(FMonitorCount^)
end;

procedure TIPCInterface.DecMonitorCount;
begin
   InterlockedDecrement(FMonitorCount^)
end;

procedure TIPCInterface.SendTrace(TraceObject: TTraceObject);
begin
  FTraceDataType^ := Integer(TraceObject.FDataType);
  FTimeStamp^ := TraceObject.FTimeStamp;
  FBufferSize^ := Min(Length(TraceObject.FMsg), MaxBufferSize);
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



