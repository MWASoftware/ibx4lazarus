unit IBBufferPool;

{$mode Delphi}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, DB, IB, IBBlob, IBExternals, FmtBCD;

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
      rdStatus: TRecordStatus;
      rdPreviousBuffer: PByte;
    end;
  strict private
    FFirstRecord: PByte;
    FLastRecord: PByte;
    function GetNext(P: PByte): PByte;
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

    PIBBufferedDateTimeWithTimeZone = ^TIBBufferedDateTimeWithTimeZone;
    TIBBufferedDateTimeWithTimeZone = packed record
      Timestamp: TDateTime;
      dstOffset: smallint;
      TimeZoneID: ISC_USHORT;
    end;

  TIBDSBlobStream = class(TStream)
  private type
    THackedDataset = class(TDataset); {used to call DataEvent}
  private
    FHasWritten: boolean;
  protected
    FField: TField;
    FBlobStream: TIBBlobStream;
    function  GetSize: Int64; override;
  public
    constructor Create(AField: TField; ABlobStream: TIBBlobStream;
                       Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property BlobStream: TIBBlobStream read FBlobStream;
  end;

    IIBCursor = interface
    ['{909621f7-e7fe-4b39-a8c5-f25c40a71c12}']
      function AllocRecordBuffer: TRecordBuffer;
      procedure SetBufListSize(aValue: Longint);
      function CreateBlobStream(aBufID: TRecordBuffer; Field: TField; Mode: TBlobStreamMode): TStream;
      function GetArray(aBufID: TRecordBuffer; Field: TField): IArray;
      procedure SetArrayIntf(aBufID: TRecordBuffer; AnArray: IArray; Field: TField);
      function GetRecDBkey(aBufID: TRecordBuffer): TIBDBKey;
      function GetFieldData(aBufID: TRecordBuffer; field: TField; outBuffer: PByte): boolean;
      procedure SetFieldData(aBufID: TRecordBuffer; field: TField; inBuffer: PByte);
      procedure SetSQLParams(aBufID: TRecordBuffer; params: ISQLParams);
      procedure UpdateRecordFromQuery(aBufID: TRecordBuffer; QryResults: IResults);
      function NeedRefresh: boolean;
      function GetRecord(aBufID: TRecordBuffer; GetMode: TGetMode;
                         DoCheck: Boolean): TGetResult;
      procedure SetCurrentRecord(Index: Longint);
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

      TColumnMetadata = record
        fdSQLColIndex: Integer;
        fdDataType: Short;
        fdDataScale: Short;
        fdNullable: Boolean;
        fdDataSize: Short;
        fdDataOfs: Integer;
        fdCodePage: TSystemCodePage;
        fdIsComputed: boolean;
        fdRefreshRequired: boolean;
        fdArrayIndex: Integer; {used for Blob and Array columns}
        fdAliasName: AnsiString;
      end;

      THackedField = class(TField); {Used to access protected method TField.DataChange}

  strict private
    FBuffers: array of PByte;
    FCalcFields: array of PByte;
    FBufferCount: integer;
    FBufferIndex: PtrUint;
    FRecordBufferSize: Integer;
    FRecordCount: Integer;
    FRecordSize: Integer;
    FColumnMetaData: array of TColumnMetadata;
    FColumnCount: integer;
    FCalcFieldsSize: integer;
    FBlobFieldCount: Longint;
    FBlobCacheOffset: Integer;
    FBlobStreamList: TList;
    FArrayList: TList;
    FArrayFieldCount: integer;
    FArrayCacheOffset: integer;
    FDBKeyFieldColumn: integer;
    FFieldNo2ColumnMap: array of integer;
    FNullColBitmapOffset: integer;
    FCursor: IResultSet;
    FDefaultTZDate: TDateTime;
    procedure SetupBufferStructure(metadata: IMetadata; aFields: TFields;
      ComputedFieldNames: TStrings);
    procedure ClearBlobCache;
    procedure ClearArrayCache;
    procedure ClearRecordCache(aBuffer: PByte);
    procedure CopyCursorDataToBuffer(QryResults: IResults; ColIndex: integer;
      destBuff: PByte);
    function InternalGetIsNull(Buff: PByte; ColIndex: integer): boolean;
    procedure InternalSetIsNull(Buff: PByte; ColIndex: integer; IsNull: boolean);
    procedure SaveBlobsAndArrays(Buff: PByte);
  protected
    function ColIndexByName(aName: AnsiString): integer;
    procedure FetchCurrentRecord(destBuffer: PByte);
    procedure FieldChanged(aBuffer: PByte; aField: TField); virtual;
    function GetBuffer(index: TRecordBuffer): PByte; inline;
    function GetCalcFields(index: TRecordBuffer): PByte; inline;
    function GetOldBuffer(aBuffer: PByte): PByte; virtual; abstract;
    function FieldNo2ColumnIndex(aField: TField): integer; inline;
    function InternalAllocRecordBuffer: PByte; virtual; abstract;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); virtual; abstract;
    property Buffers[index: TRecordBuffer]:PByte read GetBuffer;
    property Cursor: IResultSet read FCursor;
  public
    constructor Create(cursor: IResultSet; aFields: TFields; ComputedFieldNames: TStrings;
        aCalcFieldsSize: integer; aDefaultTZDate: TDateTime);
    destructor Destroy; override;

    {TDataset Interface}
    function AllocRecordBuffer: TRecordBuffer; virtual;
    procedure SetBufListSize(aValue: Longint);
    function CreateBlobStream(aBufID: TRecordBuffer; Field: TField; Mode: TBlobStreamMode): TStream;
    function GetArray(aBufID: TRecordBuffer; Field: TField): IArray;
    procedure SetArrayIntf(aBufID: TRecordBuffer; AnArray: IArray; Field: TField);
    function GetRecDBkey(aBufID: TRecordBuffer): TIBDBKey;
    function GetFieldData(aBufID: TRecordBuffer; field: TField; outBuffer: PByte): boolean;
    procedure SetFieldData(aBufID: TRecordBuffer; field: TField; inBuffer: PByte);
    procedure SetSQLParams(aBufID: TRecordBuffer; params: ISQLParams);
    procedure UpdateRecordFromQuery(aBufID: TRecordBuffer; QryResults: IResults);
    function NeedRefresh: boolean;
  public
    property RecordSize: integer read FRecordSize;
    property RecordBufferSize: integer read FRecordBufferSize;
    property ColumnCount: integer read FColumnCount;
    property RecordCount: integer read FRecordCount;
    property CalcFieldsSize: integer read FCalcFieldsSize;
    property BlobFieldCount: integer read FBlobFieldCount;
    property ArrayFieldCount: integer read FArrayFieldCount;
  end;

  {
    TIBUniDirectionalCursor provides a simple forwards only cursor with edit support
  }

  TIBUniDirectionalCursor = class(TIBCursorBase, IIBCursor)
  private type
    PRecordData = ^TRecordData;

    { TRecordData }

    TRecordData = record
      rdBookmarkFlag: TBookmarkFlag;
      rdRecNo: cardinal;
    end;

    TCursorState = (csBrowse,csEdit);

  private
    FCurrentRecNo: cardinal;
    FOldBuffer: PByte;
    FState: TCursorState;
    procedure CheckState;
  protected
    function GetOldBuffer(aBuffer: PByte): PByte; override;
    function InternalAllocRecordBuffer: PByte; override;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); override;
  public
    constructor Create(cursor: IResultSet; aFields: TFields; ComputedFieldNames: TStrings;
        aCalcFieldsSize: integer; aDefaultTZDate: TDateTime);
    destructor Destroy; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    function GetRecord(aBufID: TRecordBuffer; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult;
    procedure SetCurrentRecord(Index: Longint);
    procedure Edit;
    procedure EditingDone;
    procedure CancelEdit;
  end;


implementation

uses IBMessages, IBCustomDataSet;

{ TIBUniDirectionalCursor }

procedure TIBUniDirectionalCursor.CheckState;
begin
  if FState <> csBrowse then
    IBError(ibxeUniCursorState,[]);
end;

function TIBUniDirectionalCursor.GetOldBuffer(aBuffer: PByte): PByte;
begin
  Result := FOldBuffer;
end;

function TIBUniDirectionalCursor.InternalAllocRecordBuffer: PByte;
begin
  Result :=  GetMem(RecordBufferSize + sizeof(TRecordData);
  if Result = nil then
    OutOfMemoryError;
  FillChar(Result^,RecordBufferSize + sizeof(TRecordData),0);
  Inc(Result,sizeof(TRecordData));
end;

procedure TIBUniDirectionalCursor.InternalFreeRecordBuffer(aBuffer: PByte);
begin
  if aBuffer = nil then
  begin
    Dec(aBuffer,sizeof(TRecordData));
    FreeMem(aBuffer);
  end;
end;

constructor TIBUniDirectionalCursor.Create(cursor: IResultSet;
  aFields: TFields; ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
  aDefaultTZDate: TDateTime);
begin
  inherited Create(cursor,aFields,ComputedFieldNames, aCalcFieldsSize, aDefaultTZDate);
  FOldBuffer := InternalAllocRecordBuffer;
  FCurrentRecNo := 1;
end;

destructor TIBUniDirectionalCursor.Destroy;
begin
  InternalFreeRecordBuffer(FOldBuffer);
  inherited Destroy;
end;

function TIBUniDirectionalCursor.AllocRecordBuffer: TRecordBuffer;
begin
  Result:=inherited AllocRecordBuffer;
end;

function TIBUniDirectionalCursor.GetRecord(aBufID: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var Buff: PByte;
begin
  CheckState;
  Result := grError;
  Buff := GetBuffer(aBufID);
  if Buff = nil then Exit;

  case GetMode of
  gmCurrent:
    begin
      if Cursor.IsEof then
        Result := grEOF
      else
      begin
        if PRecordData(Buff-sizeof(TRecordData))^.rdRecNo <> FCurrentRecNo then
        begin
          FetchCurrentRecord(Buff);
          PRecordData(Buff-sizeof(TRecordData))^.rdRecNo := FCurrentRecNo;
        end;
        Result := grOK;
      end;
    end;

    gmNext:
      begin
        if Cursor.IsEof or not Cursor.FetchNext then
          Result := grEOF
        else
        begin
          FetchCurrentRecord(Buff);
          Inc(FCurrentRecord);
          PRecordData(Buff-sizeof(TRecordData))^.rdRecNo := FCurrentRecNo;
          Result := grOK;
        end;
      end;
  end;
end;

procedure TIBUniDirectionalCursor.SetCurrentRecord(Index: Longint);
begin
  CheckState;
  if Index < FCurrentRecNo then
    IBError(ibxeUniDirectional,[FCurrentRecNo, index]);
  while FCurrentRecNo < Index do
  begin
    Inc(FCurrentRecNo);
    if not Cursor.FetchNext then
      IBError(ibxBeyondEof,[]);
  end;
end;

procedure TIBUniDirectionalCursor.Edit;
begin
  CheckState;
  FState := csEdit;
  SaveCurrentBuffer;
end;

procedure TIBUniDirectionalCursor.EditingDone;
begin
  if FState = csEdit then
  begin
    FState := csBrowse;
    FillChar(FOldBuffer-sizeof(TRecordDate),RecordBufferSize+ sizeof(TRecodData),0);
  end;
end;

procedure TIBUniDirectionalCursor.CancelEdit;
begin
  if FState = csEdit then
  begin
    FState := csBrowse;
    RestoreCurrentBuffer;
  end;
end;

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

{
  A record buffer is structured into

  1. a little endian bitmap giving each column's null status. 0 => null, 1 => not null
  2. Space for each field's column data determined from the metadate datasize.
     Note: for VarChar the column data is sizeof(short) longer to allow for a length
     indicator
  3. Additional space for the blob and array caches,

  The Column Metadata and Field No. mapping is also established. The interesting
  metadata is copied from the IStatement into the FColumnMetadata dynamic array, and which
  also includes the SQL Column Index. This latter mapping is necessary because an
  IStatement column is ignored if it is not reference from the list of fields, or
  it's aliasname is not the IBX specical field sDBkeyAlias (used by TIBTable).

  Likewise a mapping between the Field.FieldNo (a 1 -based sequence nu. and the
  corresponding index into the 0 -based FColumnMetadata dynamic array. This is
  not just because of the different base, but because the use of IBX special field can
  result in and FColumnMetadata that is not linked to a TField.
}

procedure TIBCursorBase.SetupBufferStructure(metadata: IMetadata;
  aFields: TFields; ComputedFieldNames: TStrings);
var i: integer;
    colMetadata: IColumnMetaData;
    field: TField;
    ColUsed: boolean;
    ColMetaDataIndex: integer;
begin
  FArrayFieldCount := 0;
  FBlobFieldCount := 0;
  FDBKeyFieldColumn := -1;
  ColMetaDataIndex := 0;
  SetLength(FFieldNo2ColumnMap,aFields.Count+1); {Note: FieldNos are 1-based - index 0 is not used}

  { Initialize offsets, buffer sizes, etc... }
  FColumnCount := metadata.Count;
  SetLength(FColumnMetadata,FColumnCount);
  FRecordSize := 0;

  {Now determine how much space needs to be reserved for each column and column metadata}
  for i := 0 to FColumnCount - 1 do
  with FColumnMetadata[ColMetaDataIndex] do
  begin
    fdSQLColIndex := i;
    colMetadata := metadata[i];
    ColUsed := false;

    if colMetadata.GetAliasName = sDBkeyAlias then {special case for TIBTable support}
    begin
      FDBKeyFieldColumn := ColMetaDataIndex;
      ColUsed := true;
    end;

    field := aFields.FindField(colMetadata.GetAliasName);
    if field <> nil then
    begin
      FFieldNo2ColumnMap[field.FieldNo] := ColMetaDataIndex;
      Colused := true;
    end;

    if not Colused then continue;

    fdDataType := colMetadata.GetSQLType;
    if fdDataType = SQL_BLOB then
      fdDataScale := 0
    else
      fdDataScale := colMetadata.getScale;
    fdNullable := colMetadata.getIsNullable;
    fdDataSize := colMetadata.GetSize;
    fdCodePage := CP_NONE;
    fdAliasName := colMetadata.GetAliasName;
    fdIsComputed := ComputedFieldNames.IndexOf(fdAliasName) <> -1;

    case fdDataType of
    SQL_TIMESTAMP,
    SQL_TYPE_DATE,
    SQL_TYPE_TIME:
      fdDataSize := SizeOf(TDateTime);
    SQL_TIMESTAMP_TZ,
    SQL_TIMESTAMP_TZ_EX,
    SQL_TIME_TZ,
    SQL_TIME_TZ_EX:
      fdDataSize := SizeOf(TIBBufferedDateTimeWithTimeZone);
    SQL_SHORT, SQL_LONG:
    begin
      if (fdDataScale = 0) then
        fdDataSize := SizeOf(Integer)
      else
      if (fdDataScale >= (-4)) then
        fdDataSize := SizeOf(Currency)
      else
        fdDataSize := SizeOf(Double);
    end;
    SQL_INT64:
    begin
      if (fdDataScale = 0) then
        fdDataSize := SizeOf(Int64)
      else
      if (fdDataScale >= (-4)) then
        fdDataSize := SizeOf(Currency)
      else
        fdDataSize := SizeOf(Double);
    end;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
      fdDataSize := SizeOf(Double);
    SQL_BOOLEAN:
      fdDataSize := SizeOf(wordBool);
    SQL_VARYING,
    SQL_TEXT:
      fdCodePage := colMetadata.getCodePage;
    SQL_BLOB:
      begin
        fdArrayIndex := FBlobFieldCount;
        Inc(FBlobFieldCount);
        fdCodePage := colMetadata.getCodePage;
      end;
    SQL_DEC16,
    SQL_DEC34,
    SQL_DEC_FIXED,
    SQL_INT128:
      fdDataSize := sizeof(tBCD);
    SQL_Array:
      begin
        fdArrayIndex := FArrayFieldCount;
        Inc(FArrayFieldCount);
      end;
    end;
    fdDataOfs := FRecordSize;
    if fdDataType = SQL_VARYING then
      Inc(FRecordSize, fdDataSize + sizeof(short))
    else
      Inc(FRecordSize, fdDataSize);
    Inc(ColMetaDataIndex);
  end;

  FColumnCount := ColMetaDataIndex;
  {Reserve space for null column bitmap}
  FNullColBitmapOffset := FRecordSize;
  Inc(FRecordSize, ((FColumnCount - 1) div 8) + 1);


  {Add in space for offsets}
  FBlobCacheOffset := FRecordSize;
  FArrayCacheOffset := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TIBBlobStream)));
  {FRecordBufferSize is how much space needs to be reserved}
  FRecordBufferSize := FArrayCacheOffset + (ArrayFieldCount * sizeof(TIBArray));
end;

function TIBCursorBase.GetBuffer(index: TRecordBuffer): PByte;
begin
  Result := FBuffers[PtrUInt(index)-1];
end;

function TIBCursorBase.GetCalcFields(index: TRecordBuffer): PByte;
begin
  Result := FCalcFields[PtrUInt(index)-1];
end;

function TIBCursorBase.FieldNo2ColumnIndex(aField: TField): integer;
begin
  if (aField.FieldNo < 1) or (aField.FieldNo > Length(FFieldNo2ColumnMap)) then
    IBError(ibxeBadFieldNo,[aField.FieldNo, Length(FFieldNo2ColumnMap)-1]);

  Result  := FFieldNo2ColumnMap[aField.FieldNo];
end;

procedure TIBCursorBase.ClearBlobCache;
var  i: Integer;
begin
  for i := 0 to FBlobStreamList.Count - 1 do
  begin
    TIBBlobStream(FBlobStreamList[i]).Free;
    FBlobStreamList[i] := nil;
  end;
  FBlobStreamList.Pack;
end;

procedure TIBCursorBase.ClearArrayCache;
var  i: Integer;
begin
  for i := 0 to FArrayList.Count - 1 do
  begin
    TIBArray(FArrayList[i]).Free;
    FArrayList[i] := nil;
  end;
  FArrayList.Pack;
end;

procedure TIBCursorBase.ClearRecordCache(aBuffer: PByte);
var pbd: PBlobDataArray;
    pda: PArrayDataArray;
    i: Integer;
begin
  pbd := PBlobDataArray(aBuffer + FBlobCacheOffset);
  pda := PArrayDataArray(aBuffer + FArrayCacheOffset);
  for i := 0 to BlobFieldCount - 1 do
    pbd^[i] := nil;
  for i := 0 to ArrayFieldCount - 1 do
    pda^[i] := nil;
end;

procedure TIBCursorBase.CopyCursorDataToBuffer(QryResults: IResults;
  ColIndex: integer; destBuff: PByte);
var LocalData: PByte;
    ColData: ISQLData;
    IsNull: boolean;
    DataLength: Short;
    BufPtr: PByte;
begin
  QryResults.GetData(ColIndex,IsNull,DataLength,LocalData);
  with FColumnMetaData[ColIndex] do
  begin
    InternalSetIsNull(destBuff,ColIndex,IsNull);
    BufPtr := destBuff + fdDataOfs;
    if IsNull then
      FillChar(BufPtr^,fdDataSize,0)
    else
    begin
      ColData := FCursor[fdSQLColIndex];
      case fdDataType of  {Get Formatted data for column types that need formatting}
        SQL_TYPE_DATE,
        SQL_TYPE_TIME,
        SQL_TIMESTAMP:
          {This is an IBX native format and not the TDataset approach. See also GetFieldData}
          PDateTime(BufPtr)^ := ColData.AsDateTime;

        SQL_TIMESTAMP_TZ,
        SQL_TIMESTAMP_TZ_EX:
        begin
          with PIBBufferedDateTimeWithTimeZone(Bufptr)^ do
            ColData.GetAsDateTime(Timestamp,dstOffset,TimeZoneID);
        end;

        SQL_TIME_TZ,
        SQL_TIME_TZ_EX:
        begin
          with PIBBufferedDateTimeWithTimeZone(Bufptr)^ do
            ColData.GetAsTime(Timestamp, dstOffset,TimeZoneID, FDefaultTZDate);
        end;
        SQL_SHORT, SQL_LONG:
        begin
          if (fdDataScale = 0) then
            PInteger(BufPtr)^ := ColData.AsLong
          else
          if (fdDataScale >= (-4)) then
            PCurrency(BufPtr)^ := ColData.AsCurrency
          else
           PDouble(BufPtr)^ := ColData.AsDouble;
        end;
        SQL_INT64:
        begin
          if (fdDataScale = 0) then
            PInt64(BufPtr)^ := ColData.AsInt64
          else
          if (fdDataScale >= (-4)) then
            PCurrency(BufPtr)^ := ColData.AsCurrency
          else
            PDouble(BufPtr)^ := ColData.AsDouble;
        end;

        SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          PDouble(BufPtr)^ := ColData.AsDouble;

        SQL_BOOLEAN:
          system.PBoolean(BufPtr)^ := ColData.AsBoolean;

        SQL_DEC16,
        SQL_DEC34,
        SQL_DEC_FIXED,
        SQL_INT128:
          pBCD(BufPtr)^ := ColData.GetAsBCD;

      else
        begin
          if fdDataType = SQL_VARYING then
          begin
            PShort(BufPtr)^ := DataLength;
            Move(LocalData^, (BufPtr + sizeof(Short))^, DataLength);
          end
          else
            Move(LocalData^, BufPtr^, fdDataSize)
        end;
      end; {case}
    end;
  end;
end;

function TIBCursorBase.InternalGetIsNull(Buff: PByte; ColIndex: integer
  ): boolean;
var pBitmap: PByte;
    mask: byte;
begin
  pBitmap := Buff + FNullColBitmapOffset + ColIndex div 8;
  mask := $01 shl (ColIndex mod 8);
  Result := (pBitmap^ and mask) = 0; {bit is 0 => null}
end;

procedure TIBCursorBase.InternalSetIsNull(Buff: PByte; ColIndex: integer;
  IsNull: boolean);
var pBitmap: PByte;
    mask: byte;
begin
  pBitmap := Buff + FNullColBitmapOffset + ColIndex div 8;
  mask := $01 shl (ColIndex mod 8);
  if IsNull then
    pBitmap^ := pBitmap^ and not mask {unset bit}
  else
    pBitmap^ := pBitmap^ or mask;     {set bit}
end;

procedure TIBCursorBase.SaveBlobsAndArrays(Buff: PByte);
var  pbd: PBlobDataArray;
     pda: PArrayDataArray;
     i: integer;
begin
  pbd := PBlobDataArray(Buff + FBlobCacheOffset);
  pda := PArrayDataArray(Buff + FArrayCacheOffset);

  for i := 0 to FColumnCount - 1 do
  with FColumnMetadata[i] do
  begin
    if fdDataType = SQL_Blob then
    begin
      if pbd^[fdArrayIndex] <> nil then
      begin
        pbd^[fdArrayIndex].Finalize;
        PISC_QUAD(Buff + fdDataOfs)^ := pbd^[fdArrayIndex].BlobID;
        InternalSetIsNull(Buff,i, pbd^[fdArrayIndex].Size = 0);
        fdRefreshRequired := true;
      end;
    end
    else
    if fdDataType = SQL_ARRAY then
    begin
      if pda^[fdArrayIndex] <> nil then
      begin
        PISC_QUAD(Buff + fdDataOfs)^ := pda^[fdArrayIndex].ArrayIntf.GetArrayID;
        InternalSetIsNull(Buff,i, pda^[fdArrayIndex].ArrayIntf.IsEmpty);
        fdRefreshRequired := true;
      end;
    end;
  end;
end;

function TIBCursorBase.ColIndexByName(aName: AnsiString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to FColumnCount - 1 do
    if FColumnMetaData[i].fdAliasName = aName then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TIBCursorBase.FetchCurrentRecord(destBuffer: PByte);
var  i: Integer;
begin
   { Make sure blob and array caches are empty }
  ClearRecordCache(destBuffer);

  if FDBKeyFieldColumn <> -1 then
      CopyCursorDataToBuffer(FCursor,FDBKeyFieldColumn,destBuffer);
  for i := 0 to FColumnCount - 1 do
    CopyCursorDataToBuffer(FCursor,i,destBuffer);
end;

procedure TIBCursorBase.FieldChanged(aBuffer: PByte; aField: TField);
begin
  THackedField(aField).DataChanged;
end;

constructor TIBCursorBase.Create(cursor: IResultSet; aFields: TFields;
  ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
  aDefaultTZDate: TDateTime);
begin
  inherited Create;
  FBlobStreamList := TList.Create;
  FArrayList := TList.Create;
  FCursor := cursor;
  FCalcFieldsSize := aCalcFieldsSize;
  FDefaultTZDate := aDefaultTZDate;
  SetupBufferStructure(cursor.GetStatement.MetaData,aFields,ComputedFieldNames);
end;

destructor TIBCursorBase.Destroy;
begin
  ClearBlobCache;
  ClearArrayCache;
  FBlobStreamList.Free;
  FArrayList.Free;
  SetLength(FColumnMetadata,0);
  SetBufListSize(0);
  SetLength(FCalcFields,0);
  inherited Destroy;
end;

{Note bufferindex starrs at one to avoid confusion with a nil pointer}

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
  Inc(aValue); {always allocate one more than requested}

  {if decreasing the free up buffers}
  for i := aValue to FBufferCount - 1 do
  begin
    InternalFreeRecordBuffer(PByte(FBuffers[i]));
    FreeMem(FCalcFields[i]);
  end;

  SetLength(FBuffers,aValue);
  SetLength(FCalcFields,aValue);

  {if increasing then allocate buffers}
  for i := FBufferCount to aValue - 1 do
  begin
    FBuffers[i] := InternalAllocRecordBuffer;
    FCalcFields[i] := GetMem(FCalcFieldsSize);
  end;

  FBufferCount := aValue;
end;

{Field.offset is a zero based integer indexing the blob field
 Field.FieldNo is a one based field index accross all of a record's fields}

function TIBCursorBase.CreateBlobStream(aBufID: TRecordBuffer; Field: TField;
  Mode: TBlobStreamMode): TStream;
var pdb: PBlobDataArray;
    fs: TIBBlobStream;
    Buff: PByte;
    ColMetadata: TColumnMetadata;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
  begin
    fs := TIBBlobStream.Create;
    fs.Mode := bmReadWrite;
    fs.Database := (Field.Dataset as TIBCustomDataset).Database;
    fs.Transaction := (Field.Dataset as TIBCustomDataset).Transaction;
    fs.SetField(Field);
    FBlobStreamList.Add(Pointer(fs));
    Result := TIBDSBlobStream.Create(Field, fs, Mode);
    Exit;
  end;

  ColMetadata := FColumnMetaData[FieldNo2ColumnIndex(Field)];
  pdb := PBlobDataArray(Buff + FBlobCacheOffset);
  if pdb^[ColMetadata.fdArrayIndex] = nil then {not yet assigned}
  begin
    fs := TIBBlobStream.Create;;
    fs.Mode := bmReadWrite;
    fs.Database :=  (Field.Dataset as TIBCustomDataset).Database;
    fs.Transaction :=  (Field.Dataset as TIBCustomDataset).Transaction;
    fs.SetField(Field);
    fs.BlobID := PISC_QUAD(Buff + ColMetaData.fdDataOfs)^;
    pdb^[ColMetadata.fdArrayIndex] := fs;
    FBlobStreamList.Add(Pointer(fs));
    {note move to descendant class
    if (CachedUpdates) then
    begin
      bTr := not Transaction.InTransaction;
      bDB := not Database.Connected;
      if bDB then
        Database.Open;
      if bTr then
        Transaction.StartTransaction;
      fs.Seek(0, soFromBeginning);
      if bTr then
        Transaction.Commit;
      if bDB then
        Database.Close;
    end;}
  end else
    fs := pdb^[ColMetadata.fdArrayIndex];
  Result := TIBDSBlobStream.Create(Field, fs, Mode);
end;

function TIBCursorBase.GetArray(aBufID: TRecordBuffer; Field: TField
  ): IArray;
var Buff: PByte;
    pda: PArrayDataArray;
    ColIndex: integer;
    ColMetadata: TColumnMetadata;
    ar: TIBArray;
begin
  Buff := GetBuffer(aBufID);
  with Field.Dataset as TIBCustomDataset do
  begin
    if Buff = nil then
      Result := Database.Attachment.CreateArray(Transaction.TransactionIntf,
                                (Field as TIBArrayField).RelationName,Field.FieldName)
    else
    begin
      ColIndex := FieldNo2ColumnIndex(Field);
      ColMetadata := FColumnMetaData[ColIndex];
      pda := PArrayDataArray(Buff + FArrayCacheOffset);
      if pda^[ColMetadata.fdArrayIndex] = nil then
      begin
        if InternalGetIsNull(Buff,ColIndex) then
          Result := Database.Attachment.CreateArray(Transaction.TransactionIntf,
                                  (Field as TIBArrayField).RelationName,Field.FieldName)
        else
          Result := Database.Attachment.OpenArray(Transaction.TransactionIntf,
                              (Field as TIBArrayField).RelationName,Field.FieldName,
                              PISC_QUAD(Buff + ColMetaData.fdDataOfs)^);
        ar := TIBArray.Create(Field,Result);
        pda^[ColMetadata.fdArrayIndex] := ar;
        FArrayList.Add(ar);
            {note move to descendant class
        if (CachedUpdates) then
        begin
          bTr := not Transaction.InTransaction;
          bDB := not Database.Connected;
          if bDB then
            Database.Open;
          if bTr then
            Transaction.StartTransaction;
           pda^[Field.FCacheOffset].ArrayIntf.PreLoad;
          if bTr then
            Transaction.Commit;
          if bDB then
            Database.Close;
        end;   }
      end
      else
        Result := pda^[ColMetadata.fdArrayIndex].ArrayIntf;
    end;
  end;
end;

procedure TIBCursorBase.SetArrayIntf(aBufID: TRecordBuffer; AnArray: IArray;
  Field: TField);
var Buff: PByte;
    pda: PArrayDataArray;
    ColIndex: integer;
    ColMetadata: TColumnMetadata;
    ar: TIBArray;
    IsNull: boolean;
begin
  Buff := GetBuffer(aBufID);
  if Buff <> nil then
  begin
    ColIndex := FieldNo2ColumnIndex(Field);
    ColMetadata := FColumnMetaData[ColIndex];
    IsNull := AnArray = nil;
    InternalSetIsNull(Buff,ColIndex,IsNull);
    pda := PArrayDataArray(Buff + FArrayCacheOffset);
    if pda^[ColMetadata.fdArrayIndex] = nil then
    begin
      if not IsNull then
      begin
        ar := TIBArray.Create(Field,AnArray);
        pda^[ColMetadata.fdArrayIndex] := ar;
        FArrayList.Add(ar);
      end
      else
        pda^[ColMetadata.fdArrayIndex].FArray := AnArray;
    end;
    FieldChanged(Buff,Field);
  end;
end;

function TIBCursorBase.GetRecDBkey(aBufID: TRecordBuffer): TIBDBKey;
var Buff: PByte;
begin
  FillChar(Result,sizeof(TIBDBKey),0);
  if FDBKeyFieldColumn >= 0 then
  with FColumnMetaData[FDBKeyFieldColumn] do
  begin
    Buff := GetBuffer(aBufID);
    if (Buff <> nil) and (fdDataSize <= 8) then
      Result := PIBDBKEY(Buff + fdDataOfs)^;
  end;
end;

function TIBCursorBase.GetFieldData(aBufID: TRecordBuffer; field: TField;
  outBuffer: PByte): boolean;
var Buff: PByte;
    ColIndex: integer;
    Data: PByte;
    len: Short;
begin
  Result := false;
  if field.FieldNo < 0 then  {Calculated Field}
  begin
    Buff := GetCalcFields(aBufID);
    if Buff = nil then Exit;

    Inc(Buff, field.Offset); {For CalcFields, TField.offset is the buffer offset}
    Result := Boolean(Buff[0]);

    if Result then
      Move(Buff[1], outBuffer^, field.DataSize);
  end
  else
  begin
    Buff := GetBuffer(aBufID);
    if Buff = nil then Exit;

    ColIndex := FieldNo2ColumnIndex(field);
    Result := not InternalGetIsNull(Buff,ColIndex);
    if Result then
    with FColumnMetaData[ColIndex] do
    begin
      Data := Buff + fdDataOfs;
      if fdDataType = SQL_VARYING then
      begin
        len := PShort(Data)^;
        if len <= field.DataSize then
        begin
          Move(Data^, outBuffer^, len);
          PAnsiChar(outBuffer)[len] := #0;
        end
        else
          IBError(ibxeFieldSizeError,[field.FieldName,field.DataSize,len])
      end
      else
      if fdDataSize <= Field.DataSize then
        Move(Data^, outBuffer^, fdDataSize)
      else
        IBError(ibxeFieldSizeError,[field.FieldName,field.DataSize,fdDataSize])
    end;
  end;

end;

procedure TIBCursorBase.SetFieldData(aBufID: TRecordBuffer; field: TField;
  inBuffer: PByte);
var Buff: PByte;
    ColIndex: integer;
    Data: PByte;
    DataSize: Short;
    len: Short;
    IsNull: boolean;
begin
  if field.FieldNo < 0 then {calaculated field}
  begin
    Buff := GetCalcFields(aBufID);
    if Buff = nil then Exit;

    Inc(Buff, field.Offset); {For CalcFields, TField.offset is the buffer offset}
    IsNull := LongBool(inBuffer);
    Boolean(Buff[0]) := IsNull;
    if not IsNull then
      Move(inBuffer^, Buff[1], Field.DataSize);
  end
  else
  begin
    Buff := GetBuffer(aBufID);
    if Buff = nil then Exit;

    ColIndex := FieldNo2ColumnIndex(field);
    Field.Validate(inBuffer);
    IsNull := (inBuffer = nil) or
              (Field is TStringField) and (PAnsiChar(inBuffer)^ = #0);
    InternalSetIsNull(Buff,Colindex,IsNull);
    if not IsNull then
    with FColumnMetadata[ColIndex] do
    begin
      Data := Buff + fdDataOfs;
      DataSize := fdDataSize;
      FillChar(Data^,DataSize,0);
      if fdDataType = SQL_VARYING then
      begin
        len := StrLen(PAnsiChar(inBuffer));
        PShort(Data)^ := len;
        Inc(Data,sizeof(Short));
        Dec(DataSize,sizeof(Short));
      end;

      if DataSize >= field.DataSize then
         Move(inBuffer^, Data^,DataSize)
       else
         IBError(ibxeDBBufferTooSmall,[DataSize,field.FieldName,field.DataSize]);

      FieldChanged(Buff,field);
    end;
  end;
end;

procedure TIBCursorBase.SetSQLParams(aBufID: TRecordBuffer; params: ISQLParams);
const
  sOldPrefix = 'OLD_';
  sNewPrefix = 'NEW';

var Buff: PByte;
    OldBuffer: PByte;
    i: integer;
    Param: ISQLParam;
    ParamName: AnsiString;
    srcBuffer: PByte;
    ColIndex: integer;
    Data: PByte;
    DataLength: Short;
    st: RawByteString;
    pda: PArrayDataArray;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  SaveBlobsAndArrays(Buff);

  OldBuffer := GetOldBuffer(Buff);
  for i := 0 to Params.GetCount - 1 do
  begin
     Param := params[i];
     ParamName := Param.Name;

     {Determine source buffer}
     if pos(sOldPrefix,ParamName) = 1 then
     begin
       system.Delete(ParamName,1,length(sOldPrefix));
       srcBuffer := OldBuffer;
     end
     else
     if pos(sNewPrefix,ParamName) = 1 then
     begin
       system.Delete(ParamName,1,length(sNewPrefix));
       srcBuffer := Buff;
     end
     else
      srcBuffer := Buff;

     ColIndex := ColIndexByName(ParamName);
     if ColIndex = -1 then
       IBError(ibxeUnknownParamName,[ParamName]);

     Param.IsNull := InternalGetIsNull(srcBuffer,ColIndex);
     if not Param.IsNull then
     with FColumnMetaData[ColIndex] do
     begin
       pda := PArrayDataArray(srcBuffer + FArrayCacheOffset);
       Data := srcBuffer + fdDataOfs;
       case fdDataType of
         SQL_TEXT:
           begin
             SetString(st, PAnsiChar(data), fdDataSize);
             SetCodePage(st,fdCodePage,false);
             Param.AsString := st;
           end;
         SQL_VARYING:
         begin
           DataLength := PShort(Data)^;
           Inc(Data,sizeof(Short));
           SetString(st, PAnsiChar(Data), DataLength);
           SetCodePage(st,fdCodePage,false);
           Param.AsString := st;
         end;
       SQL_FLOAT, SQL_DOUBLE, SQL_D_FLOAT:
         Param.AsDouble := PDouble(Data)^;
       SQL_SHORT, SQL_LONG:
       begin
         if fdDataScale = 0 then
           Param.AsLong := PLong(Data)^
         else
         if fdDataScale >= (-4) then
           Param.AsCurrency := PCurrency(Data)^
         else
           Param.AsDouble := PDouble(Data)^;
       end;
       SQL_INT64:
       begin
         if fdDataScale = 0 then
           Param.AsInt64 := PInt64(Data)^
         else
         if fdDataScale >= (-4) then
           Param.AsCurrency := PCurrency(Data)^
         else
           Param.AsDouble := PDouble(Data)^;
       end;
       SQL_BLOB, SQL_QUAD:
         Param.AsQuad := PISC_QUAD(Data)^;
       SQL_ARRAY:
         begin
           if pda^[fdArrayIndex] = nil then
             Param.AsQuad := PISC_QUAD(Data)^
           else
             Param.AsArray := pda^[fdArrayIndex].ArrayIntf;
         end;
       SQL_TYPE_DATE,
       SQL_TYPE_TIME,
       SQL_TIMESTAMP:
       {This is an IBX native format and not the TDataset approach. See also SetFieldData}
         Param.AsDateTime := PDateTime(Data)^;
       SQL_TIMESTAMP_TZ_EX,
       SQL_TIMESTAMP_TZ:
         with PIBBufferedDateTimeWithTimeZone(Data)^ do
           Param.SetAsDateTime(Timestamp,TimeZoneID);
       SQL_TIME_TZ_EX,
       SQL_TIME_TZ:
         with PIBBufferedDateTimeWithTimeZone(Data)^ do
           Param.SetAsTime(Timestamp,FDefaultTZDate,TimeZoneID);
       SQL_BOOLEAN:
         Param.AsBoolean := PWordBool(Data)^;
       SQL_DEC16,
       SQL_DEC34,
       SQL_DEC_FIXED,
       SQL_INT128:
         Param.AsBCD := pBCD(Data)^;
       else
         IBError(ibxeUnknownSQLType,[fdDataType]);
       end;
     end;
  end;
end;

{This method is called either using a Row Refresh query or an update/insert query
 with a returning clause}

procedure TIBCursorBase.UpdateRecordFromQuery(aBufID: TRecordBuffer;
  QryResults: IResults);
var Buff: PByte;
    ColIndex: integer;
    i: integer;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);
  ClearRecordCache(Buff);

  for i := 0 to QryResults.Count - 1  do
  begin
    ColIndex := ColIndexByName(QryResults[i].GetAliasName);
    with FColumnMetaData[i] do
    begin
      CopyCursorDataToBuffer(QryResults, fdSQLColIndex,Buff);
      fdRefreshRequired := false;
    end;
  end;
end;

function TIBCursorBase.NeedRefresh: boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to FColumnCount - 1 do
    if FColumnMetadata[i].fdRefreshRequired then
    begin
      Result := true;
      Exit;
    end;
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

{ TIBDSBlobStream }

function TIBDSBlobStream.GetSize: Int64;
begin
  Result := FBlobStream.BlobSize;
end;

constructor TIBDSBlobStream.Create(AField: TField; ABlobStream: TIBBlobStream;
                                    Mode: TBlobStreamMode);
begin
  FField := AField;
  FBlobStream := ABlobStream;
  FBlobStream.Seek(0, soFromBeginning);
  if (Mode = bmWrite) then
  begin
    FBlobStream.Truncate;
    THackedDataset(FField.DataSet).DataEvent(deFieldChange,0);
    TBlobField(FField).Modified := true;
    FHasWritten := true;
  end;
end;

destructor TIBDSBlobStream.Destroy;
begin
  if FHasWritten then
     THackedDataset(FField.DataSet).DataEvent(deFieldChange, PtrInt(FField));
  inherited Destroy;
end;

function TIBDSBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  result := FBlobStream.Read(Buffer, Count);
end;

function TIBDSBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := FBlobStream.Seek(Offset, Origin);
end;

procedure TIBDSBlobStream.SetSize(NewSize: Longint);
begin
  FBlobStream.SetSize(NewSize);
end;

function TIBDSBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not (FField.DataSet.State in [dsEdit, dsInsert]) then
    IBError(ibxeNotEditing, [nil]);
  THackedDataset(FField.DataSet).DataEvent(deFieldChange, PtrInt(FField));
  TBlobField(FField).Modified := true;
  result := FBlobStream.Write(Buffer, Count);
  FHasWritten := true;
end;

end.

