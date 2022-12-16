unit IBBufferPool;

{$mode Delphi}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, DB, IB, IBBlob, IBExternals;

type
  { The TIBSimpleBufferPool provides basic buffer management for IBX. The pool consists
    of one or more memeory blocks organised as a bi-directional linked list. They do
    not have to be the same size and the first is typically a small block with
    subsequent blocks larger in size. This allows for efficient memory allocation
    for small datasets whilst efficiently extending the pool for larger datasets.

    Each block comprises a "start header", one or more fixed size "buffers" and a trailing end
    heaer. Each type is distinguished by the first byte value. Additionally, the
    first byte of the first buffer in a block is separately identified. This
    allows for a block to be parsed in either direction with the end header terminating
    forward parsing while the first buffer identifier terminates parsing in the
    backwards direction.

    The end header consists of no more than a pointer back to the start header, while
    the start header includes the previous and next block pointers, the number of buffers
    in the block and the number of buffers in use.

    Each buffer is allocated a sequential record number starting from one. A simple
    TList based index allows for random access to the block containing a buffer
    identifies by record number with the requested buffer then located as an
    offset from the start header.
  }

  { TIBSimpleBufferPool }

  TIBSimpleBufferPool = class
  private type
    THeaderTypes = (htEmptySlot=0, htStart, htEnd, htFirstBuffer, htBuffer);
    PStartHeader = ^TStartHeader;
    TStartHeader = record
      HeaderType: THeaderTypes;
      PreviousBlock: PByte;
      NextBlock: PByte;
      MaxBuffers: integer;
      BuffersInUse: integer;
      FirstRecNo: cardinal;  {1- based}
    end;

    PEndHeader = ^TEndHeader;
    TEndHeader = record
      HeaderType: THeaderTypes;
      StartHeader: PByte;
    end;

    PBufferHeader = ^TBufferHeader;
    TBufferHeader = record
      HeaderType: THeaderTypes;
      RecNo: cardinal;
    end;

  strict private
    FFirstBlock: PByte;
    FLastBlock: PByte;
    FBufferSize: integer;  {user buffer size i.e. not including header}
    FBuffersPerBlock: integer;
    FFirstBlockBuffers: integer;
    FBufferIndex: TList;
    FLastBuffer: PByte;
    FName: string;
    function AllocBlock(buffers: integer): PByte;
    procedure CheckBuffersAvailable;
  protected
    procedure CheckValidBuffer(P:PByte); virtual;
    function AddBuffer: PByte; virtual;
    procedure Clear; virtual;
    function GetFirst: PByte; virtual;
    function GetLast: PByte; virtual;
    function GetBuffer(RecNo: cardinal): PByte; virtual;
    function GetNextBuffer(aBuffer: PByte): PByte; virtual;
    function GetPriorBuffer(aBuffer: PByte): PByte; virtual;
  public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    destructor Destroy; override;
    function GetRecNo(aBuffer: PByte): cardinal; virtual;
    function Empty: boolean;
    property BuffersPerBlock: integer read FBuffersPerBlock write FBuffersPerBlock;
    property Name: string read FName;
  end;

  PIBDBKey = ^TIBDBKey;
  TIBDBKey = record
    DBKey: array[0..7] of Byte;
    end;


  {
    The TIBBufferPool builds on TIBSimpleBufferPool and adds the ability to
    insert buffers before and after a given buffer and to mark a buffer as
    deleted.

    In order to avoid large memory to memory copies, a previousBuffer pointer
    is added to each buffer header. InsertBefore is thus simply achieved by
    adding a buffer to the pool and inserting it into the backwards chain.
    GetPriorBuffer is then amended to follow the previous pointer.

    InsertAfter is implemented similarly. However, those appended to the pool
    have to be identified as appended instead of simply inserted. This is because
    GetNextBuffer is implemented using the inherited method while skipping
    inserted and deleted buffers. That is the inherited method is called repeatedly
    until an appended buffer is returned or EOF is reached.

    Deletion is implemetned by marking a buffer as deleted and adjusting the previous
    pointer of the next buffer in sequence
  }
  { TIBBufferPool }

  TIBBufferPool = class(TIBSimpleBufferPool)
  private type
    TRecordStatus = (rsAppended,rsInserted,rsDeleted);
    PRecordData = ^TRecordData;
    TRecordData = record
      rdBookmarkFlag: TBookmarkFlag;
      rdDBKey: TIBDBKey;
      rdStatus: TRecordStatus;
      rdPreviousBuffer: PByte;
    end;
  strict private
    FFirstRecord: PByte;
    FLastRecord: PByte;
    function GetNext(P: PByte): PByte;
    function GetRecDBkey(aBuffer: PByte): TIBDBKey;
    procedure SetRecDBKey(aBuffer: PByte; AValue: TIBDBKey);
    function InternalGetNextBuffer(aBuffer: PByte): PByte;
  protected
    procedure CheckValidBuffer(P:PByte); override;
  public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    procedure Clear; override;
    function GetFirst: PByte; override;
    function GetLast: PByte; override;
    function GetBuffer(RecNo: cardinal): PByte; override;
    function GetNextBuffer(aBuffer: PByte): PByte; override;
    function GetPriorBuffer(aBuffer: PByte): PByte; override;
    function GetRecNo(aBuffer: PByte): cardinal; override;
    function InsertBefore(aBuffer: PByte): PByte; virtual;
    function InsertAfter(aBuffer: PByte): PByte; virtual;
    function Append: PByte;
    procedure Delete(aBuffer: PByte);
    function GetBookmarkFlag(aBuffer: PByte): TBookmarkFlag; virtual;
    procedure SetBookmarkFlag(aBuffer: PByte; aBookmarkFlag: TBookmarkFlag); virtual;
    property RecDBKey[aBuffer: PByte]:TIBDBKey  read GetRecDBkey write SetRecDBKey;
  end;

  {
    TIIBOldBufferPool is used to support cached updates. When a record is updated
    a new buffer is allocated from this pool. The caller may then copy the
    record data into this buffer. The buffer header includes the cached update
    status and a pointer back to the buffer containing the updated data.

    Insertions and Deletions do not need a data copy. This only applies to updated
    records.

    Forward and backwards iterators are provided. The Forward iterator may be used
    to apply updates while the backwards iterator may be used to cancel updates. In
    each case the update status is used to determine the actual and the pointer to
    the data buffer is used to access the updated data.
  }

  TCachedUpdateStatus = (
                     cusUnmodified=0, cusModified, cusInserted,
                     cusDeleted, cusUninserted
                    );

  TIBUpdateRecordTypes = set of TCachedUpdateStatus;

  TIIBOldBufferPool = class(TIBSimpleBufferPool)
   private type
     PRecordData = ^TRecordData;
     TRecordData = record
       rdStatus: TCachedUpdateStatus;
       rdDataBuffer: PByte;
     end;

   public type
     TIterator = procedure(status: TCachedUpdateStatus; DataBuffer, OldBuffer: PByte) of object;

   public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    function Append(status: TCachedUpdateStatus; DataBuffer: PByte): PByte;
    function GetBuffer(RecNo: cardinal): PByte; override;
    function GetRecNo(aBuffer: PByte): cardinal; override;
    procedure SetStatus(aBuffer: PByte; status: TCachedUpdateStatus);
    procedure ForwardIterator(iterator: TIterator);
    procedure BackwardsIterator(iterator: TIterator);
  end;

  {
    TIBCursorBase provides common functions for uni-directional, bi-directional
    and bi-directional with cached updates cursors.

    The IB Cursor classes support a common interface that is used to satisfy the
    TDataset abstract methods used in buffer management including AllocRecordBuffer
    and GetRecord. The opaque pointer to a buffer returned to TDataset is an
    integer index to an array of pointers to actual buffer. This approach is used to
    avoid in memory copies every time the dataset is scolled.
  }

  TIBCursorBase = class(TInterfacedObject)
  private type
    type
      {Wrapper class to support array cache and event handling}

      { TIBArray }

      TIBArray = class
      private
        FArray: IArray;
        FRecNo: integer;
        FField: TField;
        procedure EventHandler(Sender: IArray; Reason: TArrayEventReason);
      public
        constructor Create(aField: TField; anArray: IArray);
        destructor Destroy; override;
        property ArrayIntf: IArray read FArray;
      end;

      TArrayDataArray = array [0..0] of TIBArray;
      PArrayDataArray = ^TArrayDataArray;

      TBlobDataArray = array[0..0] of TIBBlobStream;
      PBlobDataArray = ^TBlobDataArray;

      PFieldData = ^TFieldData;
      TFieldData = record
        fdIsNull: Boolean;
        fdDataLength: Short;
      end;

      PColumnData = ^TColumnData;
      TColumnData = record
        fdDataType: Short;
        fdDataScale: Short;
        fdNullable: Boolean;
        fdDataSize: Short;
        fdDataOfs: Integer;
        fdCodePage: TSystemCodePage;
        fdIsComputed: boolean;
        fdRefreshRequired: boolean;
      end;

      PFieldColumns = ^TFieldColumns;
      TFieldColumns =  array[1..1] of TColumnData;

      THackedField = class(TField); {Used to access protected method TField.DataChange}

  strict private
    FBuffers: TBufferArray;
    FBufferCount: integer;
    FBufferIndex: PtrUint;
    FRecordBufferSize: Integer;
    FRecordCount: Integer;
    FRecordSize: Integer;
    FSelectStmt: IStatement;
    FFieldColumns: PFieldColumns;
    FColumnCount: integer;
    FCalcFieldsOffset: Integer;
    FCalcFieldsSize: integer;
    FBlobFieldCount: Longint;
    FBlobCacheOffset: Integer;
    FBlobStreamList: TList;
    FArrayList: TList;
    FArrayFieldCount: integer;
    FArrayCacheOffset: integer;
    procedure ComputeRecordSize;
    function GetBuffers(index: TRecordBuffer): PByte;
  protected
    function InternalAllocRecordBuffer: PByte; virtual; abstract;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); virtual; abstract;
    property Buffers[index: TRecordBuffer]:PByte read GetBuffers;
  public
    constructor Create(selectStmt: IStatement; aCalcFieldsSize, aBlobFieldCount, aArrayFieldCount: integer);
    destructor Destroy; override;
    procedure Clear; virtual;

    {TDataset Interface}
    function AllocRecordBuffer: TRecordBuffer; virtual;
    procedure SetBufListSize(aValue: Longint);

  public
    property RecordSize: integer read FRecordSize;
    property RecordBufferSize: integer read FRecordBufferSize;
    property ColumnCount: integer read FColumnCount;
    property RecordCount: integer read FRecordCount;
    property CalcFieldsSize: integer read FCalcFieldsSize;
    property BlobFieldCount: integer read FBlobFieldCount;
    property ArrayFieldCount: integer read FArrayFieldCount;
    end;


implementation

uses IBMessages;

{ TIBCursorBase.TIBArray }

procedure TIBCursorBase.TIBArray.EventHandler(Sender: IArray;
  Reason: TArrayEventReason);
begin
  case Reason of
  arChanging:
    if FRecNo <> FField.Dataset.RecNo then
      IBError(ibxeNotCurrentArray,[nil]);

  arChanged:
    THackedField(FField).DataChanged;
  end;
end;

constructor TIBCursorBase.TIBArray.Create(aField: TField; anArray: IArray);
begin
  inherited Create;
  FField := aField;
  FArray := anArray;
  FRecNo := FField.Dataset.RecNo;
  FArray.AddEventHandler(EventHandler);
end;

destructor TIBCursorBase.TIBArray.Destroy;
begin
  FArray.RemoveEventHandler(EventHandler);
  inherited Destroy;
end;

{ TIBCursorBase }

procedure TIBCursorBase.ComputeRecordSize;

  function RecordDataLength(n: Integer): Long;
  begin
    Result := (n - 1) * SizeOf(TFieldData);
  end;

begin
  { Initialize offsets, buffer sizes, etc... }
  FColumnCount := FSelectStmt.MetaData.Count;
  FRecordSize := RecordDataLength(FSelectStmt.MetaData.Count);
  GetMem(FFieldColumns,sizeof(TFieldColumns) * (FSelectStmt.MetaData.Count));
  FCalcFieldsOffset := FRecordSize;
  FBlobCacheOffset := FCalcFieldsOffset + CalcFieldsSize;
  FArrayCacheOffset := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TIBBlobStream)));
  FRecordBufferSize := FArrayCacheOffset + (ArrayFieldCount * sizeof(IArray));

end;

function TIBCursorBase.GetBuffers(index: TRecordBuffer): PByte;
begin
  Result := PByte(FBuffers[PtrUInt(index)-1]);
end;

constructor TIBCursorBase.Create(selectStmt: IStatement; aCalcFieldsSize,
  aBlobFieldCount, aArrayFieldCount: integer);
begin
  inherited Create;
  FSelectStmt := selectStmt;
  FCalcFieldsSize := aCalcFieldsSize;
  FBlobFieldCount := aBlobFieldCount;
  FArrayFieldCount := aArrayFieldCount;
  ComputeRecordSize;
end;

destructor TIBCursorBase.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIBCursorBase.Clear;
begin
  FreeMem(FBuffers);
  FBuffers := nil;
  FBufferIndex := 0;
  FBufferCount := 0;
  FreeMem(FFieldColumns);
  FFieldColumns := nil;
  FCalcFieldsSize := 0;
  FBlobFieldCount := 0;
  FArrayFieldCount := 0;
  FRecordCount := 0;
  FColumnCount := 0;
  FRecordSize := 0;
  FCalcFieldsOffset := 0;
  FBlobCacheOffset := 0;
  FArrayCacheOffset := 0;
  FRecordBufferSize := 0;
end;

function TIBCursorBase.AllocRecordBuffer: TRecordBuffer;
begin
  if FBufferIndex > FBufferCount then
    IBError(ibxeBuffersExceeded,[FBufferIndex,FBufferCount]);
  Result := TRecordBuffer(FBufferIndex);
  Inc(FBufferIndex);
end;

procedure TIBCursorBase.SetBufListSize(aValue: Longint);
var i: integer;
begin
  {if decreasing the free up buffers}
  for i := aValue to FBufferCount - 1 do
    InternalFreeRecordBuffer(PByte(FBuffers[i]));

  ReAllocMem(FBuffers,aValue*SizeOf(TRecordBuffer));

  {if increasing then allocate buffers}
  for i := FBufferCount to aValue - 1 do
    FBuffers[i] := TRecordBuffer(InternalAllocRecordBuffer);

  FBufferCount := aValue;
end;

{ TIBSimpleBufferPool }

function TIBSimpleBufferPool.AllocBlock(buffers: integer): PByte;
var blockSize: integer;
    userBufferAreaSize: integer;
begin
  userBufferAreaSize := buffers * (FBufferSize + sizeof(TBufferHeader));
  blockSize := sizeof(TStartHeader) + userBufferAreaSize + sizeof(TEndHeader);
  Result := GetMem(blockSize);
  if Result <> nil then
  begin
    FillChar(Result^,blockSize,0);
    with PStartHeader(Result)^ do
    begin
      HeaderType := htStart;
      {add to end of list}
      PreviousBlock := FLastBlock;
      NextBlock := nil;
      MaxBuffers := buffers;
      BuffersInUse := 0;
      FirstRecNo := 1;
      if PreviousBlock <> nil then
      begin
        FirstRecNo := PStartHeader(PreviousBlock)^.FirstRecNo + PStartHeader(PreviousBlock)^.BuffersInUse;
        PStartHeader(PreviousBlock)^.NextBlock := Result;
      end;
    end;
    with PEndHeader(Result + sizeof(TStartHeader) + userBufferAreaSize)^ do
    begin
      HeaderType := htEnd;
      StartHeader := Result;
    end;
    FLastBlock := Result;
    FBufferIndex.Add(Result);
  end
  else
    OutofMemoryError;
end;

procedure TIBSimpleBufferPool.CheckValidBuffer(P: PByte);
begin
  {note: P points to header and not the user data}
  if not (PBufferHeader(P)^.HeaderType in [htFirstBuffer,htBuffer]) then
   IBError(ibxeNotABuffer,[FName]);
end;

procedure TIBSimpleBufferPool.CheckBuffersAvailable;
begin
  if FFirstBlock = nil then
     IBError(ibxeEmptyBufferPool,[FName]);
end;

constructor TIBSimpleBufferPool.Create(aName: string; bufSize,
  aBuffersPerBlock, firstBlockBuffers: integer);
begin
  inherited Create;
  FName := aName;
  FBufferSize := bufSize;
  if (buffersPerBlock <= 1) or (firstBlockBuffers <= 1) then
     IBError(ibxeNotEnoughBuffers,[FName]);
  FBuffersPerBlock := aBuffersPerBlock;
  FFirstBlockBuffers := firstBlockBuffers;
  FBufferIndex := TList.Create;
end;

destructor TIBSimpleBufferPool.Destroy;
begin
  Clear;
  if FBufferIndex <> nil then FBufferIndex.Free;
  inherited Destroy;
end;

function TIBSimpleBufferPool.AddBuffer: PByte;
begin
  Result := nil;
  if FFirstBlock = nil then
    FFirstBlock := AllocBlock(FFirstBlockBuffers);

  with PStartHeader(FLastBlock)^ do
    if BuffersInUse = MaxBuffers then
       AllocBlock(FBuffersPerBlock); {Add a Block and set FLastBlock to newly added block}

  with PStartHeader(FLastBlock)^ do
  begin
    Result := FLastBlock + sizeof(TStartHeader) + BuffersInUse * (FBufferSize + sizeof(TBufferHeader));
    with PBufferHeader(Result)^ do
    begin
      if BuffersInUse = 0 then
         HeaderType := htFirstBuffer
      else
        HeaderType := htBuffer;
      RecNo := FirstRecNo + BuffersInUse;
      Inc(BuffersInUse);
    end;
  end;
  FLastBuffer := Result;
  Inc(Result,sizeof(TBufferHeader)); {start of user data}
end;

procedure TIBSimpleBufferPool.Clear;
var P, P1: PByte;
begin
  P := FFirstBlock;
  while P <> nil do
  begin
    P1 := PStartHeader(P)^.NextBlock;
    FreeMem(P);
    P := P1;
  end;
  FFirstBlock := nil;
  FLastBlock := nil;
  FLastBuffer := nil;
  FBufferIndex.Clear;
end;

function TIBSimpleBufferPool.GetFirst: PByte;
begin
  CheckBuffersAvailable;
  Result := FFirstBlock + sizeof(TStartHeader);
  CheckValidBuffer(Result);
  Inc(Result,sizeof(TBufferHeader))
end;

function TIBSimpleBufferPool.GetLast: PByte;
begin
  CheckBuffersAvailable;
  Result := FLastBuffer;
end;

function TIBSimpleBufferPool.GetBuffer(RecNo: cardinal): PByte;
var  i: integer;
begin
  Result := nil;
  for i := 0 to FBufferIndex.Count - 1 do
  begin
    with PStartHeader(FBufferIndex[i]) ^ do
      if (BuffersInUse > 0 ) and (RecNo < FirstRecNo + BuffersInUse) then
      begin
        Result := FBufferIndex[i] + sizeof(TStartHeader) +
           (RecNo - FirstRecNo) * (sizeof(TBufferHeader) + FBufferSize) +
           sizeof(TBufferHeader); {adjust to start of user buffer}
        break;
      end;
  end;
end;

{Returns either pointer to next user buffer or nil if EOF}

function TIBSimpleBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then  {get first buffer if available}
  begin
    Result := GetFirst;
    Exit;
  end;

  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  Inc(P,sizeof(TBufferHeader)+FBufferSize);
  case PBufferHeader(P)^.HeaderType of
  htFirstBuffer,htBuffer:
    Result := P + sizeof(TBufferHeader);

  htEmptyslot:
    ; {No more buffers}

  htEnd:
    {get first buffer in next block if available}
    begin
      P := PStartHeader(PEndHeader(P)^.StartHeader)^.NextBlock;
      if (P <> nil) and (PStartHeader(P)^.BuffersInUse <> 0) then
      begin
        Result := P + sizeof(TStartHeader);
        CheckValidBuffer(Result);
        Inc(Result, sizeof(TBufferHeader));
      end;
    end;

  else
    IBError(ibxeUnrecognisedHeaderType,[ord(PBufferHeader(P)^.HeaderType)]);
  end
end;

{returns either pointer to previous user buffer or nil if BOF}

function TIBSimpleBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then  {get first buffer if available}
  begin
    Result := GetFirst;
    Exit;
  end;


  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  if PBufferHeader(P)^.HeaderType = htFirstBuffer then
  begin
    P := PStartHeader(P- sizeof(TStartHeader))^.PreviousBlock;
    if P = nil then Exit;
    Result :=  P + sizeof(TStartHeader) + (PStartHeader(P)^.BuffersInUse - 1)*(sizeof(TBufferHeader) + FBufferSize);
  end
  else
    Result := P - FBufferSize;
  CheckValidBuffer(Result);
end;

function TIBSimpleBufferPool.GetRecNo(aBuffer: PByte): cardinal;
var P: PByte;
begin
  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  Result := PBufferHeader(P)^.RecNo;
end;

function TIBSimpleBufferPool.Empty: boolean;
begin
  Result := FFirstBlock = nil;
end;

{ TIBBufferPool }

procedure TIBBufferPool.CheckValidBuffer(P: PByte);
begin
  Dec(P,sizeof(TRecordData));
  inherited CheckValidBuffer(P);
end;

constructor TIBBufferPool.Create(aName: string; bufSize, aBuffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create(aName,bufSize + sizeof(TRecordData), aBuffersPerBlock, firstBlockBuffers);
end;

procedure TIBBufferPool.Clear;
begin
  inherited Clear;
  FFirstRecord := nil;
  FLastRecord := nil;
end;

function TIBBufferPool.GetFirst: PByte;
begin
  Result := FFirstRecord;
  Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetLast: PByte;
begin
  Result := FLastRecord;
  Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetRecDBkey(aBuffer: PByte): TIBDBKey;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := PRecordData(aBuffer)^.rdDBKey;
end;

procedure TIBBufferPool.SetRecDBKey(aBuffer: PByte; AValue: TIBDBKey);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  PRecordData(aBuffer)^.rdDBKey := AValue;
end;

function TIBBufferPool.InternalGetNextBuffer(aBuffer: PByte): PByte;
{aBuffer points to TRecordData}
begin
  Result := aBuffer;

  case  PRecordData(aBuffer)^.rdStatus of
  rsAppended:
    repeat {look for the next undeleted buffer with a previous pointer to this buffer}
      Result := GetNext(Result);
    until (Result = nil) or ( PRecordData(Result)^.rdPreviousBuffer = aBuffer);

  rsDeleted:
    IBError(ibxeRecordisDeleted,[inherited GetRecNo(aBuffer)]);

  rsInserted:
    begin
      Result := GetNext(aBuffer);
      if (Result <> nil) and (PRecordData(Result)^.rdPreviousBuffer <> aBuffer) then {otherwise found it}
      begin
        Result := aBuffer;
        {Go back to insertion point}
        repeat
          Result := PRecordData(Result)^.rdPreviousBuffer;
        until (Result = nil) or (PRecordData(Result)^.rdStatus = rsAppended);

        if Result <> nil then {now back at the point where the buffer(s) were
                               inserted.}
        begin
          {find the next appended buffer}
          repeat
            Result := GetNext(Result);
          until (Result = nil) or (PRecordData(Result)^.rdStatus = rsAppended);

          {now work backwards to find the next buffer in the sequence}
          while (Result <> nil) and (PRecordData(Result)^.rdPreviousBuffer <> aBuffer) do
            {look backwards for the next undeleted buffer with a previous pointer to this buffer}
            Result := PRecordData(Result)^.rdPreviousBuffer;
        end;
      end;
    end;
  end;

end;

function TIBBufferPool.GetBuffer(RecNo: cardinal): PByte;
begin
  Result := inherited GetBuffer(RecNo);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetNext(P: PByte): PByte;
begin
  repeat
    Result := inherited GetNextBuffer(P);
  until (Result = nil) or (PRecordData(Result)^.rdStatus <> rsDeleted);
end;

function TIBBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := InternalGetNextBuffer(aBuffer);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := PRecordData(aBuffer)^.rdPreviousBuffer
end;

function TIBBufferPool.GetRecNo(aBuffer: PByte): cardinal;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := inherited GetRecNo(aBuffer);
end;

function TIBBufferPool.InsertBefore(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdBookmarkFlag := bfInserted;
    rdStatus := rsInserted;
    rdPreviousBuffer := PRecordData(aBuffer)^.rdPreviousBuffer;
  end;
  PRecordData(aBuffer)^.rdPreviousBuffer := Result;
  if aBuffer = FFirstRecord then
    FFirstRecord := Result;
  Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.InsertAfter(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdPreviousBuffer := aBuffer;
    rdBookmarkFlag := bfCurrent;
    if aBuffer = FLastRecord then
    begin
      rdStatus := rsAppended;
      FLastRecord := Result;
    end
    else
    begin
      rdStatus := rsInserted;
      {assumes InternalGetNextBuffer can never return nil given aBuffer is not last}
      PRecordData(InternalGetNextBuffer(aBuffer))^.rdPreviousBuffer := Result;
    end;
  end;
  Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.Append: PByte;
begin
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdPreviousBuffer := FLastRecord;
    rdBookmarkFlag := bfCurrent;
    rdStatus := rsAppended;
    FLastRecord := Result;
    if FFirstRecord = nil then
      FFirstRecord := Result;
  end;
  Inc(Result,sizeof(TRecordData));
end;

procedure TIBBufferPool.Delete(aBuffer: PByte);
var NextBuffer: PByte;
begin
  NextBuffer := GetNextBuffer(aBuffer); {checks valid buffer}
  Dec(aBuffer,sizeof(TRecordData));
  with PRecordData(aBuffer)^ do
  begin
    if NextBuffer <> nil then
      PRecordData(NextBuffer - sizeof(TRecordData))^.rdPreviousBuffer := rdPreviousBuffer;

    if FLastRecord = aBuffer then
      FLastRecord := rdPreviousBuffer;

    if FFirstRecord = aBuffer then
      FFirstRecord := NextBuffer;

    rdStatus := rsDeleted;
  end;
end;

function TIBBufferPool.GetBookmarkFlag(aBuffer: PByte): TBookmarkFlag;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := PRecordData(aBuffer)^.rdBookmarkFlag;
end;

procedure TIBBufferPool.SetBookmarkFlag(aBuffer: PByte;
  aBookmarkFlag: TBookmarkFlag);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  PRecordData(aBuffer)^.rdBookmarkFlag := aBookmarkFlag;
end;

{ TIIBOldBufferPool }

constructor TIIBOldBufferPool.Create(aName: string; bufSize, aBuffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create(aName,bufSize + sizeof(TRecordData),aBuffersPerBlock,
    firstBlockBuffers);
end;

function TIIBOldBufferPool.Append(status: TCachedUpdateStatus; DataBuffer: PByte
  ): PByte;
begin
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdStatus := status;
    rdDataBuffer := DataBuffer;
  end;
  Inc(Result,sizeof(TRecordData));
end;

function TIIBOldBufferPool.GetBuffer(RecNo: cardinal): PByte;
begin
  Result:=inherited GetBuffer(RecNo);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIIBOldBufferPool.GetRecNo(aBuffer: PByte): cardinal;
begin
  Result := inherited GetRecNo(aBuffer - sizeof(TRecordData));
end;

procedure TIIBOldBufferPool.SetStatus(aBuffer: PByte;
  status: TCachedUpdateStatus);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  PRecordData(aBuffer)^.rdStatus := status;
end;

procedure TIIBOldBufferPool.ForwardIterator(iterator: TIterator);
var buf: PByte;
begin
  buf := GetFirst;
  while (buf <> nil) do
    with PRecordData(buf)^ do
    begin
      iterator(rdStatus,rdDataBuffer,buf + sizeof(TRecordData));
      buf := GetNextBuffer(buf);
    end;
end;

procedure TIIBOldBufferPool.BackwardsIterator(iterator: TIterator);
var buf: PByte;
begin
  buf := GetLast;
  while (buf <> nil) do
    with PRecordData(buf)^ do
    begin
      iterator(rdStatus,rdDataBuffer,buf + sizeof(TRecordData));
      buf := GetPriorBuffer(buf);
    end;
end;

end.

