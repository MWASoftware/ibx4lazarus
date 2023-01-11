unit IBBufferedCursors;

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

  PIBRecordNumber = ^TIBRecordNumber;

  TIBRecordNumber = cardinal;

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
      FirstRecNo: TIBRecordNumber;  {1- based}
    end;

    PEndHeader = ^TEndHeader;
    TEndHeader = record
      HeaderType: THeaderTypes;
      StartHeader: PByte;
    end;

    PBufferHeader = ^TBufferHeader;
    TBufferHeader = record
      HeaderType: THeaderTypes;
      RecNo: TIBRecordNumber;
    end;

  strict private
    FFirstBlock: PByte;
    FLastBlock: PByte;
    FBufferSize: integer;  {user buffer size i.e. not including header}
    FBuffersPerBlock: integer;
    FFirstBlockBuffers: integer;
    FBufferIndex: TList;
    FLastBuffer: PByte;
    FCurrent: PByte;
    FName: string;
    FRecordCount: TIBRecordNumber;
    function AllocBlock(buffers: integer): PByte;
    procedure CheckBuffersAvailable;
    procedure InternalCheckValidBuffer(P:PByte);
  protected
    procedure CheckValidBuffer(P:PByte); virtual;
    function AddBuffer: PByte; virtual;
    procedure Clear; virtual;
    function GetFirst: PByte; virtual;
    function GetLast: PByte; virtual;
    function GetBuffer(RecNo: TIBRecordNumber): PByte; virtual;
    function GetNextBuffer(aBuffer: PByte): PByte; virtual;
    function GetPriorBuffer(aBuffer: PByte): PByte; virtual;
  public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    destructor Destroy; override;
    function Append: PByte;
    function GetRecNo(aBuffer: PByte): TIBRecordNumber; virtual;
    function GetRecordCount: TIBRecordNumber;
    function Empty: boolean;
    property BuffersPerBlock: integer read FBuffersPerBlock write FBuffersPerBlock;
    property Name: string read FName;
    property RecordCount: TIBRecordNumber read FRecordCount;
    property BufferSize: integer read FBufferSize;
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
    TRecordStatus = (rsAppended,rsInserted,rsInsertDeleted,rsAppendDeleted);
    PRecordData = ^TRecordData;
    TRecordData = record
      rdStatus: TRecordStatus;
      rdPreviousBuffer: PByte;
    end;
  strict private
    FFirstRecord: PByte;
    FLastRecord: PByte;
    FInsertedRecords: integer;
    FDeletedRecords: integer;
    function InternalGetNextBuffer(aBuffer: PByte; IncludeDeleted: boolean): PByte;
  protected
    procedure CheckValidBuffer(P:PByte); override;
  public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    procedure Clear; override;
    function GetFirst: PByte; override;
    function GetLast: PByte; override;
    function GetBuffer(RecNo: TIBRecordNumber): PByte; override;
    function GetNextBuffer(aBuffer: PByte): PByte; override;
    function GetPriorBuffer(aBuffer: PByte): PByte; override;
    function GetRecNo(aBuffer: PByte): TIBRecordNumber; override;
    function InsertBefore(aBuffer: PByte): PByte; virtual;
    function InsertAfter(aBuffer: PByte): PByte; virtual;
    function Append: PByte;
    function Delete(aBuffer: PByte): PByte;
    procedure UnDelete(aBuffer: PByte);
    property InsertedRecords: integer read FInsertedRecords;
    property DeletedRecords: integer read FDeletedRecords;
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

  { TIBOldBufferPool }

  TIBOldBufferPool = class(TIBSimpleBufferPool)
   private type
     PRecordData = ^TRecordData;
     TRecordData = record
       rdStatus: TCachedUpdateStatus;
       rdDataBuffer: PByte;
     end;

   protected
     procedure CheckValidBuffer(P:PByte); override;

   public type
     TIterator = procedure(status: TCachedUpdateStatus; DataBuffer, OldBuffer: PByte) of object;

   public
    constructor Create(aName: string; bufSize, aBuffersPerBlock, firstBlockBuffers: integer);
    function Append(DataBuffer: PByte): PByte;
    function GetBuffer(RecNo: TIBRecordNumber): PByte; override;
    function GetRecNo(aBuffer: PByte): TIBRecordNumber; override;
    procedure SetStatus(aBuffer: PByte; status: TCachedUpdateStatus);
    procedure SetDataBuffer(aBuffer: PByte; aDataBuffer: PByte);
    procedure ForwardIterator(iterator: TIterator);
    procedure BackwardsIterator(iterator: TIterator);
  end;

    PIBBufferedDateTimeWithTimeZone = ^TIBBufferedDateTimeWithTimeZone;
    TIBBufferedDateTimeWithTimeZone = packed record
      Timestamp: TDateTime;
      dstOffset: smallint;
      TimeZoneID: ISC_USHORT;
    end;

  { TIBDSBlobStream }

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
      procedure FreeRecordBuffer(var Buffer: TRecordBuffer);
      function CreateBlobStream(aBufID: TRecordBuffer; Field: TField; Mode: TBlobStreamMode): TStream;
      function GetArray(aBufID: TRecordBuffer; Field: TField): IArray;
      procedure SetArrayIntf(aBufID: TRecordBuffer; AnArray: IArray; Field: TField);
      function GetRecDBkey(aBufID: TRecordBuffer): TIBDBKey;
      function GetFieldData(aBufID: TRecordBuffer; field: TField; outBuffer: PByte): boolean;
      procedure SetFieldData(aBufID: TRecordBuffer; field: TField; inBuffer: PByte);
      procedure SetSQLParams(aBufID: TRecordBuffer; params: ISQLParams);
      procedure UpdateRecordFromQuery(aBufID: TRecordBuffer; QryResults: IResults);
      function NeedRefresh(aBufID: TRecordBuffer): boolean;
      function GetBookmarkFlag(aBufID: TRecordBuffer): TBookmarkFlag;
      procedure SetBookmarkFlag(aBufID: TRecordBuffer; aBookmarkFlag: TBookmarkFlag);
      function GetRecord(aBufID: TRecordBuffer; GetMode: TGetMode;
                         DoCheck: Boolean): TGetResult;
      procedure GotoFirst;
      procedure GotoLast;
      function GotoRecordNumber(RecNo: TIBRecordNumber): boolean;
      function GetRecNo(aBufID: TRecordBuffer): TIBRecordNumber;
      function GetCurrentRecNo: TIBRecordNumber;
      function GetRecordCount: TIBRecordNumber;
      function GetRecordSize: word;
      procedure SetCurrentRecord(aBufID: TRecordBuffer);
      procedure SaveBuffer(aBufID: TRecordBuffer);
      procedure RestoreBuffer(aBufID: TRecordBuffer);
      procedure EditingDone(aBufID: TRecordBuffer; UpdateStatus: TCachedUpdateStatus);
      procedure InsertBeforeCurrent(aBufID: TRecordBuffer);
      procedure InsertAfterCurrent(aBufID: TRecordBuffer);
      procedure DeleteCurrent;

      function GetInsertedRecords: integer;
      function GetDeletedRecords: integer;
      function UpdatesPending: boolean;
      function GetCachedUpdatesEnabled:boolean;
      procedure SetCacbedUpdatesEnabled(enabled: boolean);
      procedure SwapDataBuffer(buf1, buf2: TRecordBuffer);
      function GetAliasName(FieldNo: integer): AnsiString;
      procedure InitRecord(aBufID: TRecordBuffer);
      function AtBOF: boolean;
      function AtEOF: boolean;
    end;

    {
    TIBCursorBase provides common functions for uni-directional, bi-directional
    and bi-directional with cached updates cursors.

    The IB Cursor classes support a common interface that is used to satisfy the
    TDataset abstract methods used in buffer management including AllocRecordBuffer
    and GetRecord.

    The opaque pointer to a buffer returned to TDataset is a pointer to an internal
    data structure including a pointer to the actual buffer and a pointer to a separate
    buffer for cached updates. This approach is used to avoid in memory copies every
    time the dataset is scolled. It also avoids the cached rows buffers having to
    include space for the calculated fields.
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
        fdSQLColIndex: Integer;  {Corresponding element index in ISQLData}
        fdDataType: Short;
        fdDataScale: Short;
        fdNullable: Boolean;
        fdDataSize: Short;
        fdDataOfs: Integer;
        fdCodePage: TSystemCodePage;
        fdIsComputed: boolean;
        fdArrayIndex: Integer; {used for Blob and Array columns}
        fdAliasName: AnsiString;
      end;

      THackedField = class(TField); {Used to access to protected method TField.DataChange}

      PDisplayBuffer = ^TDisplaybuffer;
      TDisplayBuffer = record
        dbBookmarkFlag: TBookmarkFlag;
        dbBuffer: PByte;
        dbCalcFields: PByte;
      end;

  strict private
    FRecordBufferSize: Integer;            {Calculated size in bytes for each record buffer}
    FRecordCount: Integer;                 {No. of records held in buffer pool. Total in dataset with Cursor.IsEof}
    FColumnMetaData: array of TColumnMetadata; {Metadata extracted from cursor + per column info.
                                                Note: only includes columns required i.e. there is
                                                a corresponding field for the column or is the DBKey}
    FColumnCount: integer;                 {size of metadata array}
    FCalcFieldsSize: integer;              {size in bytes of calculated fields buffer}
    FBlobFieldCount: Longint;              {Number of blob fields in each record}
    FBlobCacheOffset: Integer;             {start of blob field cache in each record buffer}
    FBlobStreamList: TList;                {Keeps track of TIBBlobStream objects created}
    FArrayList: TList;                     {Keeps track of TIBArry objects created}
    FArrayFieldCount: integer;             {Number of array fields in each record}
    FArrayCacheOffset: integer;            {start of array cache in each record buffer}
    FDBKeyFieldColumn: integer;            {FColumnMetadata index of DBKey with alias sDBKeyAIias}
    FFieldNo2ColumnMap: array of integer;  {TField.FieldNo to FColumnMetadata index map}
    FNullColBitmapOffset: integer;         {start of NullColumn Bitmap in each record buffer}
    FRefreshRequiredBitmapOffset: integer; {start of Refresh Requried Bitmap in each record buffer}
    FRefreshRequiredSize: integer;         {number of bytes in RefreshRequired bitmap}
    FCursor: IResultSet;                   {The Cursor}
    FDefaultTZDate: TDateTime;             {Default Time Zone time}
    FName: string;                         {Local cursor name - set by creator}

    procedure SetupBufferStructure(metadata: IMetadata; aFields: TFields;
      ComputedFieldNames: TStrings);
    procedure ClearBlobCache;
    procedure ClearArrayCache;
    procedure CopyCursorDataToBuffer(QryResults: IResults; ColIndex: integer;
      destBuff: PByte);
    function InternalGetIsNull(Buff: PByte; ColIndex: integer): boolean;
    procedure InternalSetIsNull(Buff: PByte; ColIndex: integer; IsNull: boolean);
    procedure SetRefreshRequired(Buff: PByte; ColIndex: integer; RefreshRequired: boolean);
    procedure SaveBlobsAndArrays(Buff: PByte);
  protected
    FCurrentRecord: PByte;
    FCurrentRecordStatus: (csBOF, csRowBuffer, csEOF);
    procedure ClearRecordCache(aBuffer: PByte);
    procedure CopyBuffers(src, dest: PByte); inline;
    function ColIndexByName(aName: AnsiString): integer;
    procedure FetchCurrentRecord(destBuffer: PByte);
    procedure FieldChanged(aBuffer: PByte; aField: TField); virtual;
    function GetBuffer(aBufID: TRecordBuffer): PByte; inline;
    procedure SetBuffer(aBufID: TRecordBuffer; aBuffer: PByte); inline;
    function GetCalcFields(aBufID: TRecordBuffer): PByte; inline;
    function GetOldBufferFor(aBuffer: PByte): PByte; virtual; abstract;
    function FieldNo2ColumnIndex(aField: TField): integer; inline;
    function InternalAllocRecordBuffer: PByte; virtual; abstract;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); virtual; abstract;
    function InternalGetRecNo(aBuffer: PByte): TIBRecordNumber; virtual; abstract;
  protected
    property Buffers[index: TRecordBuffer]:PByte read GetBuffer;
    property Cursor: IResultSet read FCursor;
  public type
    TIterator = procedure(status: TCachedUpdateStatus; DataBuffer: PByte) of object;
  public
    constructor Create(aName: string; aCursor: IResultSet; aFields: TFields; ComputedFieldNames: TStrings;
        aCalcFieldsSize: integer; aDefaultTZDate: TDateTime);
    destructor Destroy; override;

    {TDataset Interface}
    function AllocRecordBuffer: TRecordBuffer;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer);
    procedure SetCurrentRecord(aBufID: TRecordBuffer);
    function CreateBlobStream(aBufID: TRecordBuffer; Field: TField; Mode: TBlobStreamMode): TStream;  virtual;
    function GetArray(aBufID: TRecordBuffer; Field: TField): IArray;
    procedure SetArrayIntf(aBufID: TRecordBuffer; AnArray: IArray; Field: TField);
    function GetRecDBkey(aBufID: TRecordBuffer): TIBDBKey;
    function GetFieldData(aBufID: TRecordBuffer; field: TField; outBuffer: PByte): boolean;
    procedure SetFieldData(aBufID: TRecordBuffer; field: TField; inBuffer: PByte);
    procedure SetSQLParams(aBufID: TRecordBuffer; params: ISQLParams);
    procedure UpdateRecordFromQuery(aBufID: TRecordBuffer; QryResults: IResults);
    function NeedRefresh(aBufID: TRecordBuffer): boolean;
    function GetBookmarkFlag(aBufID: TRecordBuffer): TBookmarkFlag;
    procedure SetBookmarkFlag(aBufID: TRecordBuffer; aBookmarkFlag: TBookmarkFlag);
    function GetRecordSize: integer;
    function GetCurrentRecNo: TIBRecordNumber;
    procedure SwapDataBuffer(buf1, buf2: TRecordBuffer);
    function GetAliasName(FieldNo: integer): AnsiString;
    procedure InitRecord(aBufID: TRecordBuffer); virtual;
    function AtBOF: boolean;
    function AtEOF: boolean;
  public
    property RecordBufferSize: integer read FRecordBufferSize;
    property ColumnCount: integer read FColumnCount;
    property RecordCount: integer read FRecordCount;
    property CalcFieldsSize: integer read FCalcFieldsSize;
    property BlobFieldCount: integer read FBlobFieldCount;
    property ArrayFieldCount: integer read FArrayFieldCount;
    property Name: string read FName;
  end;

  { TIBEditableCursor adds the means the save a copy of the current buffer during
    a row edit and to restore it later. This may either be a single save buffer
    or, if cached updates are enabled, a save buffer allocated from the old buffer pool.}

  TIBEditableCursor = class(TIBCursorBase)
  private
    const OldBuffersPerBlock = 1024;
  strict private
    FApplyUpdates: TIterator;
    FSaveBuffer: PByte;
    FOldBufferCache: TIBOldBufferPool;
    FCachedUpdatesEnabled: boolean;
    procedure ApplyUpdatesIterator(status: TCachedUpdateStatus; DataBuffer, OldBuffer: PByte);
  protected
    FOldBuffer: PByte;  {Points to current save buffer}
    FSavedRecNo: TIBRecordNumber; {record number of saved buffer if any}
    procedure DoCancelUpdates; virtual; abstract;
    function GetOldBufferFor(aBuffer: PByte): PByte; override;
    procedure InitCachedUpdates; virtual;
    procedure ReleaseSaveBuffer(aBufID: TRecordBuffer; UpdateStatus: TCachedUpdateStatus);
    property OldBufferCache: TIBOldBufferPool read FOldBufferCache;
  public
    constructor Create(aName: string; aCursor: IResultSet; aFields: TFields; ComputedFieldNames: TStrings;
      aCalcFieldsSize: integer; aDefaultTZDate: TDateTime);
    destructor Destroy; override;
    procedure EditingDone(aBufID: TRecordBuffer; UpdateStatus: TCachedUpdateStatus); virtual;
    procedure SaveBuffer(aBufID: TRecordBuffer);
    procedure RestoreBuffer(aBufID: TRecordBuffer);
    procedure ApplyUpdates(iterator: TIterator);
    procedure CancelUpdates;
    function UpdatesPending: boolean;
    function GetCachedUpdatesEnabled:boolean;
    procedure SetCacbedUpdatesEnabled(enabled: boolean);
  end;

  {
    TIBUniDirectionalCursor provides a simple forwards only cursor with edit support.

  }

  TIBUniDirectionalCursor = class(TIBEditableCursor, IIBCursor)
  private const
    DataBuffersPerBlock = 1024;
  private type
    PRecordData = ^TRecordData;

    { TRecordData }

    TRecordData = record
      rdRecNo: TIBRecordNumber;
    end;

  private
    FRecordCount: TIBRecordNumber;
    FInserting: boolean;
    FInsertedRecords: integer;
    FDeletedRecords: integer;
    FDataBufferCache: TIBSimpleBufferPool;
  protected
    procedure DoCancelUpdates; override;
    function InternalAllocRecordBuffer: PByte; override;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); override;
    function InternalGetRecNo(aBuffer : PByte) : TIBRecordNumber; override;
    procedure InitCachedUpdates; override;
  public
    destructor Destroy; override;
    function GetRecord(aBufID: TRecordBuffer; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult;
    function GetRecNo(aBufID: TRecordBuffer): TIBRecordNumber;
    function GetRecordCount: TIBRecordNumber;
    procedure GotoFirst;
    procedure GotoLast;
    function GotoRecordNumber(RecNo: TIBRecordNumber): boolean;
    procedure EditingDone(aBufID: TRecordBuffer; UpdateStatus: TCachedUpdateStatus); override;
    procedure InsertBeforeCurrent(aBufID: TRecordBuffer);
    procedure InsertAfterCurrent(aBufID: TRecordBuffer);
    procedure DeleteCurrent;
    function GetInsertedRecords: integer;
    function GetDeletedRecords: integer;
    procedure InitRecord(aBufID: TRecordBuffer); override;
  end;

  {TIBBiDirectionalCursor provides a buffered dataset that can be scrolled in either direction}

  TIBBiDirectionalCursor = class(TIBEditableCursor,IIBCursor)
  private
    FBufferPool: TIBBufferPool;
    procedure CancelUpdatesIterator(status: TCachedUpdateStatus; DataBuffer, OldBuffer: PByte);
  protected
    procedure DoCancelUpdates; override;
    function InternalAllocRecordBuffer: PByte; override;
    procedure InternalFreeRecordBuffer(aBuffer: PByte); override;
    function InternalGetRecNo(aBuffer: PByte): TIBRecordNumber; override;
  public
    constructor Create(aName: string; aCursor: IResultSet; aFields: TFields;
      ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
      aDefaultTZDate: TDateTime; aBuffersPerBlock, aFirstBlockBuffers: integer);
    destructor Destroy; override;
    function GetRecord(aBufID: TRecordBuffer; GetMode: TGetMode;
                     DoCheck: Boolean): TGetResult;
    function GetRecNo(aBufID: TRecordBuffer): TIBRecordNumber;
    procedure GotoFirst;
    procedure GotoLast;
    function GotoRecordNumber(RecNo: TIBRecordNumber): boolean;
    function GetRecordCount: TIBRecordNumber;
    procedure InsertBeforeCurrent(aBufID: TRecordBuffer);
    procedure InsertAfterCurrent(aBufID: TRecordBuffer);
    procedure DeleteCurrent;
    function GetInsertedRecords: integer;
    function GetDeletedRecords: integer;
    procedure InitRecord(aBufID: TRecordBuffer);  override;
  end;

implementation

uses IBMessages, IBCustomDataSet, IBInternals;

{ TIBEditableCursor }

procedure TIBEditableCursor.ApplyUpdatesIterator(status: TCachedUpdateStatus;
  DataBuffer, OldBuffer: PByte);
begin
  FOldBuffer := OldBuffer;
  FSavedRecNo :=  InternalGetRecNo(DataBuffer);
  try
    FApplyUpdates(status,DataBuffer);
  finally
    FOldBuffer := nil;
    FSavedRecNo := 0;
  end;
end;

function TIBEditableCursor.GetOldBufferFor(aBuffer: PByte): PByte;
begin
  if InternalGetRecNo(aBuffer) = FSavedRecNo then
    Result := FOldBuffer
  else
    Result := nil;
end;

procedure TIBEditableCursor.InitCachedUpdates;
begin
  if FOldBufferCache = nil then
    FOldBufferCache := TIBOldBufferPool.Create(Name + ': Old Buffer Cache', RecordBufferSize,
                                               OldBuffersPerBlock, OldBuffersPerBlock);
end;

constructor TIBEditableCursor.Create(aName: string; aCursor: IResultSet;
  aFields: TFields; ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
  aDefaultTZDate: TDateTime);
begin
  inherited Create(aName, aCursor,aFields,ComputedFieldNames, aCalcFieldsSize, aDefaultTZDate);
  FSaveBuffer := GetMem(RecordBufferSize);
  if FSaveBuffer = nil then
     OutOfMemoryError;
end;

destructor TIBEditableCursor.Destroy;
begin
  if FSaveBuffer <> nil then
    FreeMem(FSaveBuffer);
  if FOldBufferCache <> nil then
    FOldBufferCache.Free;
 inherited Destroy;
end;

procedure TIBEditableCursor.EditingDone(aBufID: TRecordBuffer;
  UpdateStatus: TCachedUpdateStatus);
begin
  if FSavedRecNo <> 0 then
    ReleaseSaveBuffer(aBufID, UpdateStatus);
end;

procedure TIBEditableCursor.SaveBuffer(aBufID: TRecordBuffer);
var Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  if FSavedRecNo <> 0 then
    IBError(ibxeSaveBufferNotReleased,[InternalGetRecNo(Buff),FSavedRecNo]);

  if FCachedUpdatesEnabled then
    FOldBuffer := FOldBufferCache.Append(Buff)
  else
    FOldBuffer := FSaveBuffer;

  CopyBuffers(Buff,FOldBuffer);
  FSavedRecNo := InternalGetRecNo(Buff);
end;

procedure TIBEditableCursor.RestoreBuffer(aBufID: TRecordBuffer);
var Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  if InternalGetRecNo(Buff) <> FSavedRecNo then
    IBError(ibxeUnableToRestore,[InternalGetRecNo(Buff),FSavedRecNo]);

  CopyBuffers(FOldBuffer,Buff);
  if FCachedUpdatesEnabled then
    FOldBufferCache.SetStatus(FOldBuffer,cusUnModified);
  FOldBuffer := nil;
  FSavedRecNo := 0;
end;

procedure TIBEditableCursor.ReleaseSaveBuffer(aBufID: TRecordBuffer;
  UpdateStatus: TCachedUpdateStatus);
var Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  if FSavedRecNo <> InternalGetRecNo(Buff) then
    IBError(ibxeUnableToReleaseSaveBuffer,[InternalGetRecNo(Buff),FSavedRecNo]);

  if FCachedUpdatesEnabled then
    FOldBufferCache.SetStatus(FOldBuffer,UpdateStatus);
  FOldBuffer := nil;
  FSavedRecNo := 0;
end;

procedure TIBEditableCursor.ApplyUpdates(iterator: TIterator);
begin
  if FSavedRecNo <> 0 then
    IBError(ibxeSaveBufferNotReleased,[0,FSavedRecNo]);

  if not FCachedUpdatesEnabled or not UpdatesPending then  Exit;

  FApplyUpdates := iterator;
  FOldBuffer := nil;
  FSavedRecNo := 0;
  FOldBufferCache.ForwardIterator(ApplyUpdatesIterator);
  FOldBufferCache.Clear;
end;

procedure TIBEditableCursor.CancelUpdates;
begin
  if FSavedRecNo <> 0 then
    IBError(ibxeSaveBufferNotReleased,[0,FSavedRecNo]);

  if not FCachedUpdatesEnabled or not UpdatesPending then  Exit;

  FOldBuffer := nil;
  FSavedRecNo := 0;
  DoCancelUpdates;
  FOldBufferCache.Clear;
end;

function TIBEditableCursor.UpdatesPending: boolean;
begin
  Result := FOldBufferCache.RecordCount > 0;
end;

function TIBEditableCursor.GetCachedUpdatesEnabled: boolean;
begin
  Result := FCachedUpdatesEnabled;
end;

procedure TIBEditableCursor.SetCacbedUpdatesEnabled(enabled: boolean);
begin
  if enabled <> FCachedUpdatesEnabled then
  begin
    if not enabled and UpdatesPending then
       IBError(ibxeUpdatesNotAppled,[]);
    if enabled  then
      InitCachedUpdates;
    FCachedUpdatesEnabled := enabled;
  end;
end;

{ TIBBiDirectionalCursor }

procedure TIBBiDirectionalCursor.CancelUpdatesIterator(
  status: TCachedUpdateStatus; DataBuffer, OldBuffer: PByte);
begin
  FOldBuffer := OldBuffer;
  FSavedRecNo :=  InternalGetRecNo(DataBuffer);
  try
    case status of
      cusInserted:
        FBufferPool.Delete(DataBuffer);

      cusDeleted:
        FBufferPool.UnDelete(DataBuffer);

      cusModified:
        CopyBuffers(FOldBuffer,DataBuffer);
    end;
  finally
    FOldBuffer := nil;
    FSavedRecNo := 0;
  end;
end;

procedure TIBBiDirectionalCursor.DoCancelUpdates;
begin
  OldBufferCache.BackwardsIterator(CancelUpdatesIterator);
end;

function TIBBiDirectionalCursor.InternalGetRecNo(aBuffer: PByte): TIBRecordNumber;
begin
  Result := FBufferPool.GetRecNo(aBuffer);
end;

function TIBBiDirectionalCursor.InternalAllocRecordBuffer: PByte;
begin
  Result := nil; {see GetRecord}
end;

procedure TIBBiDirectionalCursor.InternalFreeRecordBuffer(aBuffer: PByte);
begin
  // Do nothing
end;

constructor TIBBiDirectionalCursor.Create(aName: string; aCursor: IResultSet;
  aFields: TFields; ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
  aDefaultTZDate: TDateTime; aBuffersPerBlock, aFirstBlockBuffers: integer);
begin
  inherited Create(aName,aCursor,aFields,ComputedFieldNames, aCalcFieldsSize, aDefaultTZDate);
  FBufferPool := TIBBufferPool.Create(aName+ ': BiDirectional record cache',
                      RecordBufferSize,aBuffersPerBlock, aFirstBlockBuffers);
end;

destructor TIBBiDirectionalCursor.Destroy;
begin
  if FBufferPool <> nil then
    FBufferPool.Free;
  inherited Destroy;
end;

function TIBBiDirectionalCursor.GetRecord(aBufID: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;

  function ReadNext: TGetResult;
  begin
    if not Cursor.IsEof and Cursor.FetchNext then
    begin
      FCurrentRecord :=  FBufferPool.Append;
      FetchCurrentRecord(FCurrentRecord);
      FCurrentRecordStatus := csRowBuffer;
      Result := grOK;
    end
    else
    begin
      FCurrentRecordStatus := csEOF;
      Result := grEOF;
    end;
  end;

begin
  Result := grError;
  case GetMode of
  gmPrior:
    case FCurrentRecordStatus of
    csBOF:
      Result := grBOF;
    csRowBuffer:
      begin
        FCurrentRecord := FBufferPool.GetPriorBuffer(FCurrentRecord);
        if FCurrentRecord = nil then
        begin
          FCurrentRecordStatus := csBOF;
          Result := grBOF
        end
        else
          Result := grOK;
      end;
    csEOF:
      begin
        FCurentRecord := FBufferPool.GetLast;
        if FCurrentRecord = nil then
          Result := grEOF
        else
        begin
          FCurrentRecordStatus := csRowBuffer;
          Result := grOK;
        end;
      end;
    end;

  gmCurrent:
    case FCurrentRecordStatus of
    csBOF:
      Result := grBOF;
    csRowBuffer:
      Result := grOK;
    csEOF:
      Result := grEOF;
    end;

  gmNext:
    case FCurrentRecordStatus of
    csBOF:
      begin
        FCurrentRecord := FBufferPool.First;
        if (FCurrentRecord = nil) then
          Result := ReadNext
        else
          Result := grOK;
      end;
    csRowBuffer:
      begin
        FCurrentRecord := FBufferPool.GetNextBuffer(FCurrentRecord);
        if (FCurrentRecord = nil) then
          Result := ReadNext
        else
          Result := grOK;
      end;
    csEOF:
      Result := grEOF;
    end;
  end;

  case Result of
  grOK:
    begin
      SetBuffer(aBufID,FCurrentRecord);
      SetBookmarkFlag(aBufID,bfCurrent);
    end;
  grBOF:
    begin
      SetBuffer(aBufID,nil);
      SetBookmarkFlag(aBufID,bfBOF);
    end;
  grEOF:
    begin
      SetBuffer(aBufID,nil);
      SetBookmarkFlag(aBufID,bfEOF);
    end;
  end;
end;

function TIBBiDirectionalCursor.GetRecNo(aBufID: TRecordBuffer): TIBRecordNumber;
var Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);
  Result := InternalGetRecNo(Buff);
end;

procedure TIBBiDirectionalCursor.GotoFirst;
begin
  FCurrentRecord := FBufferPool.GetFirst;
  if FCurrentRecord = nil then
  begin
    if Cursor.IsEOF then
      FCurrentRecordStatus := csEOF
    else
    begin
      FCurrentRecord := FBufferPool.Append;
      FetchCurrentRecord(FCurrentRecord);
      FCurrentRecordStatus := csRowBuffer;
    end;
  end;

end;

procedure TIBBiDirectionalCursor.GotoLast;
begin
  FCurrentRecord := FBufferPool.GetLast;
  if (FCurrentRecord = nil) and Cursor.IsEOF then
    FCurrentRecordStatus := csEOF
  else
  begin
    FCurrentRecordStatus := csRowBuffer;
    while Cursor.FetchNext do
    begin
      FCurrentRecord := FBufferPool.Append;
      FetchCurrentRecord(FCurrentRecord);
    end;
  end;
end;

function TIBBiDirectionalCursor.GotoRecordNumber(RecNo: TIBRecordNumber): boolean;
begin
  if FBufferPool.GetRecordCount >= RecNo then
    FCurrentRecord := FBufferPool.GetBuffer(RecNo)
  else
  begin
    FCurrentRecord := FBufferPool.GetLast;
    while (FBufferPool.GetRecNo(FCurrentRecord) < RecNo) and Cursor.FetchNext do
    begin
      FCurrentRecord := FBufferPool.Append;
      FetchCurrentRecord(FCurrentRecord);
    end;
  end;
  Result :=  FBufferPool.GetRecNo(FCurrentRecord) = RecNo;
end;

function TIBBiDirectionalCursor.GetRecordCount: TIBRecordNumber;
begin
  Result := FBufferPool.GetRecordCount - FBufferPool.DeletedRecords;
end;

procedure TIBBiDirectionalCursor.InsertBeforeCurrent(aBufID: TRecordBuffer);
begin
  case FCurrentRecordStatus of
  csBOF:
    IBError(ibxeInsertBeforeBOF,[]);
  csRowBuffer:
    FCurrentRecord := FBufferPool.InsertBefore(FCurrentRecord);
  csEOF:
    FCurrentRecord := FBufferPool.Append;
  end;
  SetBuffer(aBufID, FCurrentRecord);
  FCurrentRecordStatus := csRowBuffer;
end;

procedure TIBBiDirectionalCursor.InsertAfterCurrent(aBufID: TRecordBuffer);
begin
  case FCurrentRecordStatus of
  csBOF:
    FCurrentRecord := FBufferPool.InsertBefore(FBufferPool.GetFirst);
  csRowBuffer:
    FCurrentRecord := FBufferPool.InsertAfter(FCurrentRecord);
  csEOF:
    IBError(ibxeInsertBeyondEOF,[]);
  end;

  SetBuffer(aBufID,FCurrentRecord);
  FCurrentRecordStatus := csRowBuffer
end;

procedure TIBBiDirectionalCursor.DeleteCurrent;
begin
  case FCurrentRecordStatus of
  csBOF:
    IBError(ibxeDeleteAtBOF,[]);
  csEOF:
    IBError(ibxeDeleteBeyondEOF,[]);
  csRowBuffer:
    FCurrentRecord := FBufferPool.Delete(FCurrentRecord);
  end;
end;

function TIBBiDirectionalCursor.GetInsertedRecords: integer;
begin
  Result := FBufferPool.InsertedRecords;
end;

function TIBBiDirectionalCursor.GetDeletedRecords: integer;
begin
  Result := FBufferPool.DeletedRecords;
end;

procedure TIBBiDirectionalCursor.InitRecord(aBufID: TRecordBuffer);
begin
  inherited InitRecord(aBufID);
  if aBufID <> nil then
    PDisplayBuffer(aBufID)^.dbBuffer := nil;
end;

{ TIBUniDirectionalCursor }

function TIBUniDirectionalCursor.InternalGetRecNo(aBuffer: PByte) : TIBRecordNumber;
begin
  if aBuffer = nil then
    Result := 0
  else
    Result := PRecordData(aBuffer - sizeof(TRecordData))^.rdRecNo;
 end;

procedure TIBUniDirectionalCursor.InitCachedUpdates;
begin
  inherited InitCachedUpdates;
  if FDataBufferCache = nil then
    FDataBufferCache := TIBSimpleBufferPool.Create(Name + ': Data Buffer Cacne',
           RecordBufferSize,
           DataBuffersPerBlock,DataBuffersPerBlock);
end;

destructor TIBUniDirectionalCursor.Destroy;
begin
  if FDataBufferCache <> nil then
    FDataBufferCache.Free;
  inherited Destroy;
end;

procedure TIBUniDirectionalCursor.DoCancelUpdates;
begin

end;

function TIBUniDirectionalCursor.InternalAllocRecordBuffer: PByte;
begin
  Result :=  GetMem(RecordBufferSize + sizeof(TRecordData));
  if Result = nil then
    OutOfMemoryError;
  FillChar(Result^,RecordBufferSize + sizeof(TRecordData),0);
  Inc(Result,sizeof(TRecordData));
end;

procedure TIBUniDirectionalCursor.InternalFreeRecordBuffer(aBuffer: PByte);
begin
  if aBuffer <> nil then
  begin
    Dec(aBuffer,sizeof(TRecordData));
    FreeMem(aBuffer);
  end;
end;

function TIBUniDirectionalCursor.GetRecord(aBufID: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var Buff: PByte;
begin
  Result := grError;
  Buff := GetBuffer(aBufID);
  if Buff = nil then Exit;

  ClearRecordCache(Buff);
  case GetMode of
  gmCurrent:
    begin
      case FCurrentRecordStatus of
      csBOF: ; {do nothing - returns grError}
      csEOF:
        Result := grEOF;
      csRowBuffer:
        begin
          if Buff <> FCurrentRecord then
          begin
            FetchCurrentRecord(Buff);
            PRecordData(Buff-sizeof(TRecordData))^.rdRecNo := FRecordCount;
            FCurrentRecord := Buff;
          end;
          Result := grOK;
        end;
      end;
    end;

    gmNext:
      begin
        case FCurrentRecordStatus of
        csEOF:
          Result := grEOF;
        csBOF, csRowBuffer:
          begin
            if Cursor.IsEOF then
              Result := grEOF
            else
            begin
              Cursor.FetchNext;
              FetchCurrentRecord(Buff);
              Inc(FRecordCount);
              PRecordData(Buff-sizeof(TRecordData))^.rdRecNo := FRecordCount;
              FCurrentRecord := Buff;
              FCurrentRecordStatus := csRowBuffer
              Result := grOK;
            end;
          end;
        end;
      end;
   end;

  case Result of
  grOK:
      SetBookmarkFlag(aBufID,bfCurrent);
  grBOF:
      SetBookmarkFlag(aBufID,bfBOF);
  grEOF:
      SetBookmarkFlag(aBufID,bfEOF);
  end;

end;

function TIBUniDirectionalCursor.GetRecNo(aBufID : TRecordBuffer) : TIBRecordNumber;
var Buff: PByte;
begin
  Result := 0;
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);
  Result := InternalGetRecNo(Buff);
end ;

function TIBUniDirectionalCursor.GetRecordCount: TIBRecordNumber;
begin
  Result := FRecordCount - FDeletedRecords;
end;

procedure TIBUniDirectionalCursor.GotoFirst;
begin
  if FRecordCount <> 1 then
    IBError(ibxeUniDirectional,[FRecordCount,1]);
end;

procedure TIBUniDirectionalCursor.GotoLast;
begin
  while not Cursor.IsEOF do
  begin
    Cursor.FetchNext;
    Inc(FRecordCount);
  end;
  FCurrentRecord := nil;
  FCurrentRecordStatus := csRowBuffer;
end;

function TIBUniDirectionalCursor.GotoRecordNumber(RecNo: TIBRecordNumber
  ): boolean;
begin
    while (FRecordCount < RecNo) and Cursor.FetchNext do
      Inc(FRecordCount);
  Result := FRecordCount = RecNo;
end;

procedure TIBUniDirectionalCursor.EditingDone(aBufID: TRecordBuffer;
  UpdateStatus: TCachedUpdateStatus);
var Buff: PByte;
    DataBuffer: PByte;
begin
  if FOldBuffer <> nil then
  begin
    Buff := GetBuffer(aBufID);
    if Buff = nil then
      IBError(ibxeBufferNotSet, [nil]);

    DataBuffer := FDataBufferCache.Append;
    CopyBuffers(Buff,DataBuffer); {cache current data buffer}
    OldBufferCache.SetDataBuffer(FOldBuffer,DataBuffer);
  end;
  inherited EditingDone(aBufID, UpdateStatus);
  FInserting := false;
end;

procedure TIBUniDirectionalCursor.InsertBeforeCurrent(aBufID: TRecordBuffer);
var CurRecNo: TIBRecordNumber;
    Buff: PByte;
begin
  if FCurrentRecordStatus = csEOF then
  begin
    Buff := GetBuffer(aBufID);
    if Buff = nil then
      IBError(ibxeBufferNotSet, [nil]);
    Inc(FRecordNumber);
    PRecordData(Buff - sizeof(TRecordData))^.rdRecNo := FRecordNumber;
    FCurrentRecord := Buff;
    FCurrentRecordStatus := csRowBuffer;
    FInserting := true;
    Inc(FInsertedRecords);
  end
  else
  begin
    CurRecNo := InternalGetRecNo(FCurrentRecord);
    IBError(ibxeUniDirectional,[CurRecNo,CurRecNo - 1]);
  end;
end;

procedure TIBUniDirectionalCursor.InsertAfterCurrent(aBufID: TRecordBuffer);
var Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  Inc(FRecordCount);
  PRecordData(Buff - sizeof(TRecordData))^.rdRecNo := FRecordNumber;
  FCurrentRecord := Buff;
  FCurrentRecordStatus := csRowBuffer;
  FInserting := true;
  Inc(FInsertedRecords);
end;

procedure TIBUniDirectionalCursor.DeleteCurrent;
begin
   FCurrentRecord := nil;
   if Cursor.IsEof then
     FCurrentRecordStatus := csEOF
   else
   begin
     FCurrentRecordStatus := csRowBuffer;
     if not FInserting then
     begin
       if not Cursor.FetchNext then
         FCurrentRecordStatus := csEOF
     end
   end;
   Inc(FDeletedRecords);
end;

function TIBUniDirectionalCursor.GetInsertedRecords: integer;
begin
  Result := FInsertedRecords
end;

function TIBUniDirectionalCursor.GetDeletedRecords: integer;
begin
  Result := FDeletedRecords;
end;

procedure TIBUniDirectionalCursor.InitRecord(aBufID: TRecordBuffer);
var Buff: PByte;
begin
  inherited InitRecord(aBufID);
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);
  ClearRecordCache(Buff);
  Fillchar(Buff^,RecordBufferSize,0);
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

  1. Space for each field's column data determined from the metadate datasize.
     Note: for VarChar the column data is sizeof(short) longer to allow for a length
     indicator.
  2. Buffer Header for per record local data
  3. a little endian bitmap giving each column's null status. 0 => null, 1 => not null
  4. Another bitmap to keep track of each colmn's refresh required status.
  5. Additional space for the blob and array caches,

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
    RecordSize: integer;
begin
  FArrayFieldCount := 0;
  FBlobFieldCount := 0;
  FDBKeyFieldColumn := -1;
  ColMetaDataIndex := 0;
  SetLength(FFieldNo2ColumnMap,aFields.Count+1); {Note: FieldNos are 1-based - index 0 is not used}

  { Initialize offsets, buffer sizes, etc... }
  FColumnCount := metadata.Count;
  SetLength(FColumnMetadata,FColumnCount);
  RecordSize := 0;

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
    fdDataOfs := RecordSize;
    if fdDataType = SQL_VARYING then
      Inc(RecordSize, fdDataSize + sizeof(short))
    else
      Inc(RecordSize, fdDataSize);
    Inc(ColMetaDataIndex);
  end;

  FColumnCount := ColMetaDataIndex; {set to number of columns in use}

  {Reserve space for null column bitmap}
  FNullColBitmapOffset := RecordSize;
  Inc(RecordSize, ((FColumnCount - 1) div 8) + 1);

  {Reserve space for Refresh Required bit map}
  FRefreshRequiredBitmapOffset := RecordSize;
  FRefreshRequiredSize := ((FColumnCount - 1) div 8) + 1;
  Inc(RecordSize, FRefreshRequiredSize);

  {Add in space for offsets}
  FBlobCacheOffset := RecordSize;
  FArrayCacheOffset := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TIBBlobStream)));
  {FRecordBufferSize is how much space needs to be reserved}
  FRecordBufferSize := FArrayCacheOffset + (ArrayFieldCount * sizeof(TIBArray));
end;

function TIBCursorBase.GetBuffer(aBufID: TRecordBuffer): PByte;
begin
  Result := PDisplayBuffer(aBufID)^.dbBuffer;
end;

procedure TIBCursorBase.SetBuffer(aBufID: TRecordBuffer; aBuffer: PByte);
begin
  PDisplayBuffer(aBufID)^.dbBuffer := aBuffer;
end;

function TIBCursorBase.GetCalcFields(aBufID: TRecordBuffer): PByte;
begin
  Result := PDisplayBuffer(aBufID)^.dbCalcFields;
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
  FillChar((aBuffer)^,FBlobCacheOffset,0);
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

procedure TIBCursorBase.SetRefreshRequired(Buff: PByte; ColIndex: integer;
  RefreshRequired: boolean);
var pBitmap: PByte;
    mask: byte;
begin
  pBitmap := Buff + FRefreshRequiredBitmapOffset + ColIndex div 8;
  mask := $01 shl (ColIndex mod 8);
  if RefreshRequired then
    pBitmap^ := pBitmap^ or mask           {set bit}
  else
    pBitmap^ := pBitmap^ and not mask;     {unset bit}
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
        SetRefreshRequired(Buff,i,true);
      end
      else
         InternalSetIsNull(Buff,i, true);
   end
    else
    if fdDataType = SQL_ARRAY then
    begin
      if pda^[fdArrayIndex] <> nil then
      begin
        PISC_QUAD(Buff + fdDataOfs)^ := pda^[fdArrayIndex].ArrayIntf.GetArrayID;
        InternalSetIsNull(Buff,i, pda^[fdArrayIndex].ArrayIntf.IsEmpty);
        SetRefreshRequired(Buff,i,true);
      end;
    end
    else
       InternalSetIsNull(Buff,i, true);
  end;
end;

procedure TIBCursorBase.CopyBuffers(src, dest: PByte);
begin
  Move(src^,dest^,RecordBufferSize);
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

  if Cursor.IsEOF then
    IBError(ibxeCursorAtEOF,[]);

  if FDBKeyFieldColumn <> -1 then
      CopyCursorDataToBuffer(FCursor,FDBKeyFieldColumn,destBuffer);
  for i := 0 to FColumnCount - 1 do
    CopyCursorDataToBuffer(FCursor,i,destBuffer);
end;

procedure TIBCursorBase.FieldChanged(aBuffer: PByte; aField: TField);
var i: integer;
begin
  THackedField(aField).DataChanged;

  {Once a field has changed, we need to make sure that all computed values are updated
   the next time the record is saved}

  for i := 0 to FColumnCount - 1 do
    if FColumnMetaData[i].fdIsComputed then
      SetRefreshRequired(aBuffer,i,true);
end;

constructor TIBCursorBase.Create(aName: string; aCursor: IResultSet;
  aFields: TFields; ComputedFieldNames: TStrings; aCalcFieldsSize: integer;
  aDefaultTZDate: TDateTime);
begin
  inherited Create;
  FBlobStreamList := TList.Create;
  FArrayList := TList.Create;
  FName := aName;
  FCursor := aCursor;
  FCalcFieldsSize := aCalcFieldsSize;
  FDefaultTZDate := aDefaultTZDate;
  SetupBufferStructure(cursor.GetStatement.MetaData,aFields,ComputedFieldNames);
  FCurrentRecord := nil;
  FCurrentRecordStatus := csEOF;
end;

destructor TIBCursorBase.Destroy;
begin
  ClearBlobCache;
  ClearArrayCache;
  FBlobStreamList.Free;
  FArrayList.Free;
  SetLength(FColumnMetadata,0);
  inherited Destroy;
end;

{Note bufferindex starts at one to avoid confusion with a nil pointer.
 The "buffer" is an opaque pointer - actually an integer index to the
 FBuffers array.}

function TIBCursorBase.AllocRecordBuffer: TRecordBuffer;
begin
  Result := GetMem(sizeof(TDisplayBuffer));
  if Result = nil then
    OutofMemoryError;
  with PDisplayBuffer(Result)^ do
  begin
    dbBookmarkFlag := bfCurrent;
    dbBuffer := InternalAllocRecordBuffer;
    dbCalcFields := GetMem(FCalcFieldsSize);
    if dbCalcFields = nil then
      OutOfMemoryError;
  end;
end;

procedure TIBCursorBase.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  if Buffer <> nil then
  begin
    with PDisplayBuffer(Buffer)^ do
    begin
      InternalFreeRecordBuffer(dbBuffer);
      FreeMem(dbCalcFields);
    end;
    FreeMem(Buffer);
  end;
  Buffer := nil;
end;

procedure TIBCursorBase.SetCurrentRecord(aBufID: TRecordBuffer);
var Buff: PByte;
begin
  case PDisplayBuffer(aBufID)^.dbBookmarkFlag of
    bfBOF:
      begin
        FCurrentRecord = nil;
        FCurrentRecordStatus := csBOF;
      end;
    bfEOF:
      begin
        FCurrentRecord = nil;
        FCurrentRecordStatus := csEOF;
      end;
    else
    begin
      Buff := GetBuffer(aBufID);
      if Buff = nil then
        IBError(ibxeBufferNotSet, [nil]);
      FCurrentRecord := Buff;
      FCurrentRecordStatus := csRowBuffer;
    end;
  end,
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
  end
  else
  begin
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
     end
    else
      fs := pdb^[ColMetadata.fdArrayIndex];
  end;
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
          Inc(Data,sizeof(short));
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

  OldBuffer := GetOldBufferFor(Buff);
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
     begin
       srcBuffer := Buff;
       if pos(sNewPrefix,ParamName) = 1 then
         system.Delete(ParamName,1,length(sNewPrefix));
     end;

     ColIndex := ColIndexByName(ParamName);
     if ColIndex = -1 then
       continue;

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
      SetRefreshRequired(Buff,ColIndex,false);
    end;
  end;
end;

function TIBCursorBase.NeedRefresh(aBufID : TRecordBuffer) : boolean;
var i: integer;
    Buff: PByte;
begin
  Buff := GetBuffer(aBufID);
  if Buff = nil then
    IBError(ibxeBufferNotSet, [nil]);

  Result := false;
  for i := 0 to FRefreshRequiredSize - 1 do
    if PByte(Buff + FRefreshRequiredBitmapOffset + i)^ <> 0 then
    begin
      Result := true;
      Exit;
    end;
end;

function TIBCursorBase.GetBookmarkFlag(aBufID: TRecordBuffer): TBookmarkFlag;
begin
  Result := PDisplayBuffer(aBufID)^.dbBookmarkFlag;;
end;

procedure TIBCursorBase.SetBookmarkFlag(aBufID: TRecordBuffer;
  aBookmarkFlag: TBookmarkFlag);
begin
  PDisplayBuffer(aBufID)^.dbBookmarkFlag := aBookmakrFlag,
end;

function TIBCursorBase.GetRecordSize: integer;
begin
  Result := sizeof(TDisplayBuffer);
end;

function TIBCursorBase.GetCurrentRecNo: TIBRecordNumber;
begin
  case FCurrentRecordStatus of
  csBOF:
    Result := 0;
  csEOF:
    Result := FRecordCount;
  csRowBuffer:
    Result := InternalGetRecNo(FCurrentRecord);
  end;
end;

procedure TIBCursorBase.SwapDataBuffer(buf1, buf2: TRecordBuffer);
var TmpBuf: PByte;
begin
  TmpBuf := PDisplayBuffer(Buf1)^.dbBuffer;
  PDisplayBuffer(Buf1)^.dbBuffer := PDisplayBuffer(Buf2)^.dbBuffer;
  PDisplayBuffer(Buf2)^.dbBuffer := TmpBuf;
end;

function TIBCursorBase.GetAliasName(FieldNo: integer): AnsiString;
begin
  if (FieldNo < 1) or (FieldNo > Length(FFieldNo2ColumnMap)) then
    IBError(ibxeBadFieldNo,[FieldNo, Length(FFieldNo2ColumnMap)-1]);

  Result  := FColumnMetaData[ FFieldNo2ColumnMap[FieldNo] ].fdAliasName;
end;

procedure TIBCursorBase.InitRecord(aBufID: TRecordBuffer);
begin
  with PDisplayBuffer(aBufID)^ do
  begin
    dbBookmarkFlag := bfInserted;
    Fillchar(dbCalcFields^,CalcFieldsSize,0);
  end;
end;

function TIBCursorBase.AtBOF: boolean;
begin
  Result := FCurrentRecordStatus = csBOF;
end;

function TIBCursorBase.AtEOF: boolean;
begin
  Result := (FCurrentRecordStatus - csEOF) and Cursor.IsEof;
end;


{ TIBSimpleBufferPool }

function TIBSimpleBufferPool.AllocBlock(buffers: integer): PByte;
var blockSize: integer;
    userBufferAreaSize: integer;
begin
  userBufferAreaSize := buffers * (BufferSize + sizeof(TBufferHeader));
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
  Dec(P,sizeof(TBufferHeader));
  InternalCheckValidBuffer(P);
end;

procedure TIBSimpleBufferPool.CheckBuffersAvailable;
begin
  if FFirstBlock = nil then
     IBError(ibxeEmptyBufferPool,[FName]);
end;

procedure TIBSimpleBufferPool.InternalCheckValidBuffer(P: PByte);
begin
  if not (PBufferHeader(P)^.HeaderType in [htFirstBuffer,htBuffer]) then
   IBError(ibxeNotABuffer,[FName]);
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

function TIBSimpleBufferPool.Append: PByte;
begin
  Result := AddBuffer;
end;

function TIBSimpleBufferPool.AddBuffer: PByte;
begin
  Result := nil;
  if FFirstBlock = nil then
    FFirstBlock := AllocBlock(FFirstBlockBuffers);

  with PStartHeader(FLastBlock)^ do
    if BuffersInUse >= MaxBuffers then
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
  Inc(FRecordCount);
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
  FCurrent := nil;
  FBufferIndex.Clear;
  FRecordCount := 0;
end;

function TIBSimpleBufferPool.GetFirst: PByte;
begin
  CheckBuffersAvailable;
  Result := FFirstBlock + sizeof(TStartHeader);
  InternalCheckValidBuffer(Result);
  FCurrent := Result;
  Inc(Result,sizeof(TBufferHeader))
end;

function TIBSimpleBufferPool.GetLast: PByte;
begin
  CheckBuffersAvailable;
  Result := FLastBuffer;
  InternalCheckValidBuffer(Result);
  FCurrent := Result;
  Inc(Result,sizeof(TBufferHeader))
end;

function TIBSimpleBufferPool.GetBuffer(RecNo: TIBRecordNumber): PByte;
var  i: integer;
begin
  Result := nil;
  CheckBuffersAvailable;

  for i := 0 to FBufferIndex.Count - 1 do
  begin
    with PStartHeader(FBufferIndex[i]) ^ do
      if (BuffersInUse > 0 ) and (RecNo < FirstRecNo + BuffersInUse) then
      begin
        Result := FBufferIndex[i] + sizeof(TStartHeader) +
           (RecNo - FirstRecNo) * (sizeof(TBufferHeader) + FBufferSize);
        break;
      end;
  end;

  if Result <> nil then
  begin
    InternalCheckValidBuffer(Result);
    FCurrent := Result;
    Inc(Result, sizeof(TBufferHeader));
  end;
end;

{Returns either pointer to next user buffer or nil if EOF}

function TIBSimpleBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then {Implicit request for current buffer}
    Result := FCurrent
  else
  begin
    P := aBuffer - sizeof(TBufferHeader);
    InternalCheckValidBuffer(P);
    Inc(P,sizeof(TBufferHeader)+FBufferSize);
    case PBufferHeader(P)^.HeaderType of
    htFirstBuffer,htBuffer:
      Result := P ;

    htEmptyslot:
      ; {No more buffers}

    htEnd:
      {get first buffer in next block if available}
      begin
        P := PStartHeader(PEndHeader(P)^.StartHeader)^.NextBlock;
        if (P <> nil) and (PStartHeader(P)^.BuffersInUse <> 0) then
          Result := P + sizeof(TStartHeader);
      end;

    else
      IBError(ibxeUnrecognisedHeaderType,[ord(PBufferHeader(P)^.HeaderType)]);
    end;
  end;
  if Result <> nil then
  begin
    InternalCheckValidBuffer(Result);
    FCurrent := Result;
    Inc(Result, sizeof(TBufferHeader));
  end;
end;

{returns either pointer to previous user buffer or nil if BOF}

function TIBSimpleBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
var P: PByte;
begin
  Result := nil;
  CheckBuffersAvailable;

  if aBuffer = nil then   {Implicit request for current buffer}
    Result := FCurrent
  else
  begin
    P := aBuffer - sizeof(TBufferHeader);
    InternalCheckValidBuffer(P);
    if PBufferHeader(P)^.HeaderType = htFirstBuffer then
    begin
      P := PStartHeader(P- sizeof(TStartHeader))^.PreviousBlock;
      if (P <> nil) and (PStartHeader(P)^.BuffersInUse <> 0) then
        Result :=  P + sizeof(TStartHeader) +
                  (PStartHeader(P)^.BuffersInUse - 1)*(sizeof(TBufferHeader) + FBufferSize);
    end
    else
      Result := P - FBufferSize - sizeof(TBufferHeader);
  end;

  if Result <> nil then
  begin
    InternalCheckValidBuffer(Result);
    FCurrent := Result;
    Inc(Result, sizeof(TBufferHeader));
  end;
end;

function TIBSimpleBufferPool.GetRecNo(aBuffer: PByte): TIBRecordNumber;
var P: PByte;
begin
  P := aBuffer - sizeof(TBufferHeader);
  InternalCheckValidBuffer(P);
  Result := PBufferHeader(P)^.RecNo;
end;

function TIBSimpleBufferPool.GetRecordCount: TIBRecordNumber;
begin
  Result := FRecordCount;
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
  if (Result <> nil) and (PRecordData(Result)^.rdStatus in [rsAppendDeleted,rsInsertDeleted]) then
    Result := InternalGetNextBuffer(Result,false);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetLast: PByte;
begin
  Result := FLastRecord;
  while (Result <> nil) and (PRecordData(Result)^.rdStatus in [rsAppendDeleted,rsInsertDeleted]) do
    Result := PRecordData(Result)^.rdPreviousBuffer;
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

{InternalGetNextBuffer skips over deleted records and tries to find the next
 record that points back to aBuffer}

function TIBBufferPool.InternalGetNextBuffer(aBuffer: PByte;
  IncludeDeleted: boolean): PByte;
{aBuffer points to TRecordData}
var P: PByte;
begin
  Result := aBuffer;
  repeat
    P := Result;
    case  PRecordData(aBuffer)^.rdStatus of

    {records are always appended in sequence but the sequence may be interrupted by
     inserted out of sequence records. These should generally be ignored but could
     be the next in sequence - so have to check them}

    rsAppended, rsAppendDeleted:
      repeat {look for the next undeleted buffer with a previous pointer to this buffer}
        Result := inherited GetNextBuffer(Result);
      until (Result = nil) or ( PRecordData(Result)^.rdPreviousBuffer = P);

    {Inserted records are typically out of sequence, but can be part of a local
     sequence of inserted buffers.

     1. lookahead until either the next in sequence is found or the local sequence
        breaks.
     2.
     }

    rsInserted, rsInsertDeleted:
      begin
        Result := inherited GetNextBuffer(P);
        if (Result <> nil) and (PRecordData(Result)^.rdPreviousBuffer <> P) then {otherwise found it}
        begin
          Result := P;
          {Go back to insertion point}
          repeat
            Result := PRecordData(Result)^.rdPreviousBuffer;
          until (Result = nil) or (PRecordData(Result)^.rdStatus in [rsAppended, rsAppendDeleted]);

          if Result <> nil then {now back at the point where the buffer(s) were
                                 inserted.}
          begin
            {find the next appended buffer}
            repeat
              Result := inherited GetNextBuffer(Result);
            until (Result = nil) or (PRecordData(Result)^.rdStatus in [rsAppended, rsAppendDeleted]);

            {now work backwards to find the next buffer in the sequence}
            while (Result <> nil) and (PRecordData(Result)^.rdPreviousBuffer <> P) do
              {look backwards for the next buffer with a previous pointer to this buffer}
              Result := PRecordData(Result)^.rdPreviousBuffer;
          end;
        end;
      end;
    end;
  until (Result = nil) or IncludeDeleted or (PRecordData(Result)^.rdStatus in [rsAppended,rsInserted]);
end;

function TIBBufferPool.GetBuffer(RecNo: TIBRecordNumber): PByte;
begin
  Result := inherited GetBuffer(RecNo);
  if PRecordData(Result)^.rdStatus in [rsInsertDeleted, rsAppendDeleted] then
    IBError(ibxeRecordisDeleted,[RecNo]);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetNextBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := InternalGetNextBuffer(aBuffer,false);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetPriorBuffer(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  repeat
    Result := PRecordData(aBuffer)^.rdPreviousBuffer
  until (Result = nil) or (PRecordData(Result)^.rdStatus in [rsAppended,rsInserted]);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBBufferPool.GetRecNo(aBuffer: PByte): TIBRecordNumber;
begin
  if aBuffer = nil then
    Result := 0
  else
  begin
    Dec(aBuffer,sizeof(TRecordData));
    CheckValidBuffer(aBuffer);
    if PRecordData(aBuffer)^.rdStatus in [rsInsertDeleted, rsAppendDeleted] then
      Result := 0;
    Result := inherited GetRecNo(aBuffer);
  end;
end;

function TIBBufferPool.InsertBefore(aBuffer: PByte): PByte;
begin
  if Empty then
    Result := Append
  else
  begin
    Dec(aBuffer,sizeof(TRecordData));
    CheckValidBuffer(aBuffer);
    Result := AddBuffer;
    with PRecordData(Result)^ do
    begin
      rdStatus := rsInserted;
      rdPreviousBuffer := PRecordData(aBuffer)^.rdPreviousBuffer;
    end;
    PRecordData(aBuffer)^.rdPreviousBuffer := Result;
    if aBuffer = FFirstRecord then
      FFirstRecord := Result;
  end;
  Inc(Result,sizeof(TRecordData));
  Inc(FInsertedRecords);
end;

function TIBBufferPool.InsertAfter(aBuffer: PByte): PByte;
begin
  if Empty then
     Result := Append
  else
  begin
    Dec(aBuffer,sizeof(TRecordData));
    CheckValidBuffer(aBuffer);
    Result := AddBuffer;
    with PRecordData(Result)^ do
    begin
      rdPreviousBuffer := aBuffer;
      if aBuffer = FLastRecord then
      begin
        rdStatus := rsAppended;
        FLastRecord := Result;
      end
      else
      begin
        rdStatus := rsInserted;
        {assumes InternalGetNextBuffer can never return nil given aBuffer is not last}
        PRecordData(InternalGetNextBuffer(aBuffer,true))^.rdPreviousBuffer := Result;
      end;
    end;
  end;
  Inc(Result,sizeof(TRecordData));
  Inc(FInsertedRecords);
end;

function TIBBufferPool.Append: PByte;
begin
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdPreviousBuffer := FLastRecord;
    rdStatus := rsAppended;
    FLastRecord := Result;
    if FFirstRecord = nil then
      FFirstRecord := Result;
  end;
  Inc(Result,sizeof(TRecordData));
  Inc(FInsertedRecords);
end;

function TIBBufferPool.Delete(aBuffer: PByte): PByte;
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  Result := GetNextBuffer(aBuffer);
  case PRecordData(aBuffer)^.rdStatus of
  rsInserted:
    PRecordData(aBuffer)^.rdStatus := rsInsertDeleted;
  rsAppended:
    PRecordData(aBuffer)^.rdStatus := rsAppendDeleted;
  end;
  Inc(FDeletedRecords);
end;

procedure TIBBufferPool.UnDelete(aBuffer: PByte);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  case PRecordData(aBuffer)^.rdStatus of
  rsInsertDeleted:
    PRecordData(aBuffer)^.rdStatus := rsInserted;
  rsAppendDeleted:
    PRecordData(aBuffer)^.rdStatus := rsAppended;
  end;
  Dec(FDeletedRecords);
end;

{ TIBOldBufferPool }

procedure TIBOldBufferPool.CheckValidBuffer(P: PByte);
begin
  Dec(P,sizeof(TRecordData));
  inherited CheckValidBuffer(P);
end;

constructor TIBOldBufferPool.Create(aName: string; bufSize, aBuffersPerBlock,
  firstBlockBuffers: integer);
begin
  inherited Create(aName,bufSize + sizeof(TRecordData),aBuffersPerBlock,
    firstBlockBuffers);
end;

function TIBOldBufferPool.Append(DataBuffer: PByte): PByte;
begin
  Result := AddBuffer;
  with PRecordData(Result)^ do
  begin
    rdStatus := cusUnModified;
    rdDataBuffer := DataBuffer;
  end;
  Inc(Result,sizeof(TRecordData));
end;

function TIBOldBufferPool.GetBuffer(RecNo: TIBRecordNumber): PByte;
begin
  Result := inherited GetBuffer(RecNo);
  if Result <> nil then
    Inc(Result,sizeof(TRecordData));
end;

function TIBOldBufferPool.GetRecNo(aBuffer: PByte): TIBRecordNumber;
begin
  Result := inherited GetRecNo(aBuffer - sizeof(TRecordData));
end;

procedure TIBOldBufferPool.SetStatus(aBuffer: PByte;
  status: TCachedUpdateStatus);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  PRecordData(aBuffer)^.rdStatus := status;
end;

procedure TIBOldBufferPool.SetDataBuffer(aBuffer: PByte; aDataBuffer: PByte);
begin
  Dec(aBuffer,sizeof(TRecordData));
  CheckValidBuffer(aBuffer);
  PRecordData(aBuffer)^.rdDataBuffer := aDataBuffer;
end;

procedure TIBOldBufferPool.ForwardIterator(iterator: TIterator);
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

procedure TIBOldBufferPool.BackwardsIterator(iterator: TIterator);
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

