unit IBBufferPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type

  { TIBBufferPool }

  TIBBufferPool = class
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
    FBufferSize: integer;
    FBuffersPerBlock: integer;
    FFirstBlockBuffers: integer;
    function AllocBlock(buffers: integer): PByte;
    procedure CheckValidBuffer(P:PByte);
    procedure CheckBuffersAvailable;
  public
    constructor Create(bufSize, buffersPerBlock, firstBlockBuffers: integer);
    destructor Destroy; override;
    function AddBuffer: PByte; virtual;
    procedure Clear;
    function GetFirst: PByte; virtual;
    function GetLast: PByte; virtual;
    function GetBuffer(RecNo: cardinal): PByte; virtual;
    function GetNextBuffer(aBuffer: PByte): PByte; virtual;
    function GetPriorBuffer(aBuffer: PByte): PByte; virtual;
    function GetRecNo(aBuffer: PByte): cadinal; virtual;
    property BuffersPerBlock: integer read FBuffersPerBlock write FBuffersPerBlock;
  end;

  TIBOrderedBufferPool = class(TIBBufferPool)
  private type
    PRecordData = ^TRecordData;
    TRecordData = record
      rdBookmarkFlag: TBookmarkFlag;
      rdDBKey: TIBDBKey;
      rdInline: boolean;
      rdPreviousBuffer: PByte;
    end;
  public
    function AddBuffer: PByte; override;
    function GetFirst: PByte; override;
    function GetLast: PByte; override;
    function GetBuffer(RecNo: cardinal): PByte; override;
    function GetNextBuffer(aBuffer: PByte): PByte; override;
    function GetPriorBuffer(aBuffer: PByte): PByte; override;
    function GetRecNo(aBuffer: PByte): cadinal; override;
    function InsertBefore(aBuffer: PByte): PByte; virtual;
    function InsertAfter(aBuffer: PByte): PByte; virtual;
    function Append: PByte;
  end;



implementation

uses IBMessages;

{ TIBBufferPool }

function TIBBufferPool.AllocBlock(buffers: integer): PByte;
var blockSize: integer;
begin
  blockSize := sizeof(TStartHeader) + sizeof(TEndHeader) + buffers * (FBufSize + sizeof(TBufferHeader));
  Result := GetMem(blockSize);
  if Result then
  begin
    FillChar(Result^,blockSize,0);
    with PStartHeader(Result)^ do
    begin
      HeaderType := htStart;
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
    with PEndHeader(Result + blockSize - sizeof(TEndHeader)^ do
    begin
      HeaderType := htEnd;
      StartHeader := Result;
    end;
    FLastBlock := Result;
  end
  else
    OutofMemory;
end;

procedure TIBBufferPool.CheckValidBuffer(P: PByte);
begin
  {note: P points to header and not the user data}
  if not (PBufferHeader(P)^.HeaderType in [htFirstBuffer,htBuffer]) then
   IBError(ibxeNotABuffer,[]);
end;

procedure TIBBufferPool.CheckBuffersAvailable;
begin
  if FFirstBlock = nil then
     IBError(ibxeEmptyBufferPool,[]);
end;

constructor TIBBufferPool.Create(bufSize, buffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create;
  FBufferSize := bufSize;
  if (buffersPerBlock <= 1) or (firstBlockBuffers <= 1) then
     IBError(ibxeNotEnoughBuffers,[]);
  FBuffersPerBlock := buffersPerBlock;
  FFirstBlockBuffers := firstBlockBuffers;
end;

destructor TIBBufferPool.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIBBufferPool.AddBuffer: PByte;
begin
  Result := nil;
  if FFirstBlock = nil then
    FFirstBlock := AllocBlock(FFirstBlockBuffers);

  with PStartHeader(FLastBlock)^ do
    if BuffersInUse = MaxBuffers then
       AllocBlock(FBuffersPerBlock); {Add a Block and set FLastBlock to newly added block}

  with PStartHeader(FLastBlock)^ do
  begin
    Result := FLastBlock + sizeof(TStartHeader) + BuffersInUse * (FBufSize + sizeof(TBufferHeader));
    with PBufferHeader(Result)^ do
    begin
      if BuffersInUse = 0 then
         HeaderType := htStartBuffer;
      else
        HeaderType := htBuffer;
      RecNo := FirstRecNo + BuffersInUse;
      Inc(BuffersInUse);
    end;
  end;
  Inc(Result,sizeof(TBufferHeader)); {start of user data}
end;

procedure TIBBufferPool.Clear;
var P, P1: PByte;
begin
  P := FFirstBlock;
  while P <> nil then
  begin
    P1 := PStartHeader(P)^.NextBlock.
    FreeMem(P);
    P := P1;
  end;
  FFirstBlock := nil;
  FLastBlock := nil;
end;

function TIBBufferPool.GetFirst: PByte;
begin
  CheckBuffersAvailable;
  Result := FFirstBlock + sizeof(TStartHeader);
  if PBufferHeader(Result)^.HeaderType = htStartBuffer then
    Inc(Result,sizeof(TBufferHeader))
  else
    Result := nil;
end;

function TIBBufferPool.GetLast: PByte;
begin
  CheckBuffersAvailable;
  Result := FLastBlock + (PStartHeader(FLastBlock)^.BuffersInUse - 1)*(sizeof(TBufferHeader) + FBufSize);
  if PBufferHeader(Result)^.HeaderType in [htStartBuffer,htBuffer] then
    Inc(Result,sizeof(TBufferHeader))
  else
    Result := nil;
end;

function TIBBufferPool.GetBuffer(RecNo: cardinal): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;
  P := FFirstBlock;
  while (Result = nil) and (P <> nil) do
  with PStartHeader(P) ^ do
  begin
    if (BuffersInUse > 0 ) and (RecNo < FirstRecNo + BuffersInUse) then
      Result := P + sizeof(TStartHeader) + ((RecNo - FirstRecNo) * (sizeof(TBufferHeader) + FBufSize) + sizeof(TBufferHeader);
    else
      P := NextBlock;
  end;
end;

function TIBBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then  {get first buffer if available}
  begin
    Result := FFirstBlock + sizeof(TStartHeader);
    CheckValidBuffer(Result);
    Exit;
  end;

  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  Inc(P,sizeof(TBufferHeader)+FBufSize);
  case PBufferHeader(P)^.HeaderType of
  htStartBuffer..htEndBuffer:
    Result := P + sizeof(TBufferHeader);

  htEmpty:
    ; {No more buffers}

  htEnd:
    begin
      P := PStartHeader(PEndHeader(P)^.StartHeader)^.NextBlock;
      if (P <> nil) and PStartHeader(P)^.BuffersInUse <> 0) then
      begin
        Result := P + sizeof(TStartHeader);
        CheckValidBuffer(Result);
        Inc(Result, sizeof(TBufferHeader);
      end;
    end;

  else
    IBError(ibxeUnrecognisedHeaderType);
  end
end;

function TIBBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then
    IBError(ibxeNoPriorBuffers,[]);

  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  if PBufferHeader(P)^.HeaderType = htStartBuffer then
  begin
    P := P - sizeof(TStartHeader);
    P := PStartHeader(P)^.PreviousBlock;
    if P <> nil then
    begin
      Result :=  P + sizeof(TStartHeader) + PStartHeader(P)^.(BuffersInUse - 1)*(sizeof(TBufferHeader) + FBufSize);
      CheckValidBuffer(Result);
    end;
  end
  else
    Result := P - FBufSize;
end;

function TIBBufferPool.GetRecNo(aBuffer: PByte): cadinal;
var P: PByte;
begin
  P := aBuffer - sizeof(TBufferHeader);
  CheckValidBuffer(P);
  Result := PBufferHeader(P)^.RecNo;
end;

end.

