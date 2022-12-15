unit IBBufferPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

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

  private
    FFirstBlock: PByte;
    FLastBlock: PByte;
    FBlockSize: integer;
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
  public
    constructor Create(aName: string; bufSize, buffersPerBlock, firstBlockBuffers: integer);
    destructor Destroy; override;
    function AddBuffer: PByte; virtual;
    procedure Clear;
    function GetFirst: PByte; virtual;
    function GetLast: PByte; virtual;
    function GetBuffer(RecNo: cardinal): PByte; virtual;
    function GetNextBuffer(aBuffer: PByte): PByte; virtual;
    function GetPriorBuffer(aBuffer: PByte): PByte; virtual;
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
  private
    FFirstBuffer: PByte;
    FLastBuffer: PByte;
  protected
    procedure CheckValidBuffer(P:PByte); override;
  public
    constructor Create(aName: string; bufSize, buffersPerBlock, firstBlockBuffers: integer);
    function AddBuffer: PByte; override;
    function GetFirst: PByte; override;
    function GetLast: PByte; override;
    function GetRecDBkey(aBuffer: PByte): TIBDBKey;
    function GetBuffer(RecNo: cardinal): PByte; override;
    function GetNextBuffer(aBuffer: PByte): PByte; override;
    function GetPriorBuffer(aBuffer: PByte): PByte; override;
    function GetRecNo(aBuffer: PByte): cardinal; override;
    function InsertBefore(aBuffer: PByte): PByte; virtual;
    function InsertAfter(aBuffer: PByte): PByte; virtual;
    function Append: PByte;
    function GetBookmarkFlag(aBuffer: PByte): TBookmarkFlag; virtual;
    procedure SetBookmarkFlag(aBuffer: PByte; aBookmarkFlag: TBookmarkFlag); virtual;
  end;



implementation

uses IBMessages;

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

constructor TIBSimpleBufferPool.Create(aName: string; bufSize, buffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create;
  FName := aName;
  FBufferSize := bufSize;
  if (buffersPerBlock <= 1) or (firstBlockBuffers <= 1) then
     IBError(ibxeNotEnoughBuffers,[FName]);
  FBuffersPerBlock := buffersPerBlock;
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

constructor TIBBufferPool.Create(aName: string; bufSize, buffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create(aName,bufSize + sizeof(TRecordData), buffersPerBlock, firstBlockBuffers);
end;

function TIBBufferPool.AddBuffer: PByte;
begin
  IBError(ibxeNotSupported,[]);
end;

function TIBBufferPool.GetFirst: PByte;
begin
  Result := FFirstBuffer;
end;

function TIBBufferPool.GetLast: PByte;
begin
  Result := FLastBuffer;
end;

function TIBBufferPool.GetRecDBkey(aBuffer: PByte): TIBDBKey;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := PRecordData(aBuffer)^.rdDBKey;
end;

function TIBBufferPool.GetBuffer(RecNo: cardinal): PByte;
begin
  Result := inherited GetBuffer(RecNo);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  Result := aBuffer;
  case  PRecordData(aBuffer)^.rdStatus of
  rsAppended:
    repeat {look for the next undeleted buffer with a previous pointer to this buffer}
      Result := inherited GetNextBuffer(Result);
    until (Result = nil) or ((PRecordData(Result)^.rdStatus <> rsDeleted)
                             and (PRecordData(Result)^.rdPreviousBuffer = aBuffer));

  rsDeleted:
    IBError(ibxeRecordisDeleted,[inherited GetRecNo(aBuffer)]);

  rsInserted:
    {First look forward to find the next undeleted buffer with a previous pointer
    to this buffer. Terminates when an appended buffer is found. Then walk
    backwards to first previous appended buffer and then walk forwards}
    begin
      repeat
        Result := inherited GetNextBuffer(Result);
        if Result <> nil then
        begin
          case PRecordData(Result)^.rdStatus:
          rsDeleted:
            ; {ignore}

          rsAppended:
            break; {exit repeat loop}

          rsInserted:
            if PRecordData(Result)^.rdPreviousBuffer = aBuffer then
              break;
          end;
        end ;
      until Result = nil;
  end;


  Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  Result:=inherited GetPriorBuffer(aBuffer);
end;

function TIBBufferPool.GetRecNo(aBuffer: PByte): cardinal;
begin
  Result:=inherited GetRecNo(aBuffer);
end;

function TIBBufferPool.InsertBefore(aBuffer: PByte): PByte;
begin

end;

function TIBBufferPool.InsertAfter(aBuffer: PByte): PByte;
begin

end;

function TIBBufferPool.Append: PByte;
begin

end;

function TIBBufferPool.GetBookmarkFlag(aBuffer: PByte): TBookmarkFlag;
begin

end;

procedure TIBBufferPool.SetBookmarkFlag(aBuffer: PByte;
  aBookmarkFlag: TBookmarkFlag);
begin

end;

end.

