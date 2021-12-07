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
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2015                                          }
{                                                                        }
{************************************************************************}

unit IBCustomDataSet;

{$R-}

{$IFDEF FPC}
{$Mode Delphi}
{$codepage UTF8}
{$ENDIF}

{$IFDEF DELPHI}
{$DEFINE TDBDFIELD_IS_BCD}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ENDIF}
{$IFDEF UNIX}
  unix,
{$ENDIF}
  SysUtils, Classes, IBDatabase, IBExternals, IBInternals, IB,  IBSQL, Db,
  IBUtils, IBBlob, IBSQLParser, IBDatabaseInfo;

type
  TIBCustomDataSet = class;
  TIBDataSet = class;

  { TIBDataSetUpdateObject }

  TIBDataSetUpdateObject = class(TComponent)
  private
    FRefreshSQL: TStrings;
    procedure SetRefreshSQL(value: TStrings);
  protected
    function GetDataSet: TIBCustomDataSet; virtual; abstract;
    procedure SetDataSet(ADataSet: TIBCustomDataSet); virtual; abstract;
    procedure Apply(UpdateKind: TUpdateKind; buff: PChar); virtual; abstract;
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
    procedure InternalSetParams(Params: ISQLParams; buff: PChar); overload;
    procedure InternalSetParams(Query: TIBSQL; buff: PChar); overload;
    procedure UpdateRecordFromQuery(UpdateKind: TUpdateKind; QryResults: IResults; Buffer: PChar);
    property DataSet: TIBCustomDataSet read GetDataSet write SetDataSet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount,
                                 DeleteCount: integer): boolean; virtual;
  published
    property RefreshSQL: TStrings read FRefreshSQL write SetRefreshSQL;
  end;

  TIBArrayField = class;

  { TIBArray }

  {Wrapper class to support array cache in TIBCustomDataSet and event handling}

  TIBArray = class
  private
    FArray: IArray;
    FRecNo: integer;
    FField: TIBArrayField;
    procedure EventHandler(Sender: IArray; Reason: TArrayEventReason);
  public
    constructor Create(aField: TIBArrayField; anArray: IArray);
    destructor Destroy; override;
    property ArrayIntf: IArray read FArray;
  end;

  { TIBArrayField }

  TIBArrayField = class(TField)
  private
    FArrayBounds: TArrayBounds;
    FArrayDimensions: integer;
    FRelationName: string;
    FCacheOffset: word;
    function GetArrayID: TISC_QUAD;
    function GetArrayIntf: IArray;
    procedure SetArrayIntf(AValue: IArray);
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    function GetAsString: string; override;
    function GetDataSize: Integer; override;
    procedure Bind(Binding: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CreateArray: IArray;
    property ArrayID: TISC_QUAD read GetArrayID;
    property ArrayIntf: IArray read GetArrayIntf write SetArrayIntf;
    property ArrayDimensions: integer read FArrayDimensions write FArrayDimensions;
    property ArrayBounds: TArrayBounds read FArrayBounds write FArrayBounds;
  end;

  { TIBStringField allows us to have strings longer than 8196 }

  TIBStringField = class(TStringField)
  private
    FCharacterSetName: RawByteString;
    FCharacterSetSize: integer;
    FAutoFieldSize: boolean;
    FCodePage: TSystemCodePage;
    FDataSize: integer;
  protected
    procedure Bind(Binding: Boolean); override;
    function GetDataSize: Integer; override;
  public
    constructor Create(aOwner: TComponent); override;
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue(var Value: string): Boolean;
    procedure SetAsString(const Value: string); override;
    property CharacterSetName: RawByteString read FCharacterSetName write FCharacterSetName;
    property CharacterSetSize: integer read FCharacterSetSize write FCharacterSetSize;
    property CodePage: TSystemCodePage read FCodePage write FCodePage;
  published
    property AutoFieldSize: boolean read FAutoFieldSize write FAutoFieldSize default true;
  end;

  { TIBBCDField }
  {  Actually, there is no BCD involved in this type,
     instead it deals with currency types.
     In IB, this is an encapsulation of Numeric (x, y)
     where x < 18 and y <= 4.
     Note: y > 4 will default to Floats
  }
  TIBBCDField = class(TBCDField)
  private
    FIdentityColumn: boolean;
  protected
    procedure Bind(Binding: Boolean); override;
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property IdentityColumn: boolean read FIdentityColumn;
  published
    property Size default 8;
  end;

  {The following integer field types extend the built in versions to enable IBX appplications
   to check for an Identity column}

  { TIBSmallintField }

  TIBSmallintField = class(TSmallintField)
  private
    FIdentityColumn: boolean;
  protected
    procedure Bind(Binding: Boolean); override;
  public
    property IdentityColumn: boolean read FIdentityColumn;
  end;

  { TIBIntegerField }

  TIBIntegerField = class(TIntegerField)
  private
    FIdentityColumn: boolean;
  protected
    procedure Bind(Binding: Boolean); override;
  public
    property IdentityColumn: boolean read FIdentityColumn;
  end;

  { TIBLargeIntField }

  TIBLargeIntField = class(TLargeIntField)
  private
    FIdentityColumn: boolean;
  protected
    procedure Bind(Binding: Boolean); override;
  public
    property IdentityColumn: boolean read FIdentityColumn;
  end;

  {TIBMemoField}
  {Allows us to show truncated text in DBGrids and anything else that uses
   DisplayText}

   TIBMemoField = class(TMemoField)
   private
     FCharacterSetName: RawByteString;
     FCharacterSetSize: integer;
     FDisplayTextAsClassName: boolean;
     function GetTruncatedText: string;
   protected
     procedure Bind(Binding: Boolean); override;
     function GetAsString: string; override;
     function GetDefaultWidth: Longint; override;
     procedure GetText(var AText: string; ADisplayText: Boolean); override;
     procedure SetAsString(const AValue: string); override;
   public
     constructor Create(AOwner: TComponent); override;
     property CharacterSetName: RawByteString read FCharacterSetName write FCharacterSetName;
     property CharacterSetSize: integer read FCharacterSetSize write FCharacterSetSize;
   published
     property DisplayTextAsClassName: boolean read FDisplayTextAsClassName
                                            write FDisplayTextAsClassName;
   private
     FCodePage: TSystemCodePage;
     FFCodePage: TSystemCodePage;
   public
     property CodePage: TSystemCodePage read FFCodePage write FFCodePage;
   end;

   PIBBufferedDateTimeWithTimeZone = ^TIBBufferedDateTimeWithTimeZone;
   TIBBufferedDateTimeWithTimeZone = packed record
     Timestamp: TDateTime;
     dstOffset: smallint;
     TimeZoneID: ISC_USHORT;
   end;

   { TIBDateTimeField }

   {It seems wrong to make this a subclass of TTimeField and not TDateTimField.
    However, the rationale is backwards compatibility for applications that
    may want to coerce a TField to a TTimeField. If this is to work then
    TIBTimeField has to descend from TTimeField. Hence the declation. As
    TTimeField also descends from TDateTimeField this should not result in any
    problems - unless someone makes a drastic change to TTimeField.}

   TIBDateTimeField = class(TTimeField)
   private
     FHasTimeZone: boolean;
     FTimeZoneServices: ITimeZoneServices;
     function GetTimeZoneServices: ITimeZoneServices;
     function GetDateTimeBuffer(var aBuffer: TIBBufferedDateTimeWithTimeZone): boolean;
     function GetTimeZoneID: TFBTimeZoneID;
     function GetTimeZoneName: string;
     procedure SetTimeZoneID(aValue: TFBTimeZoneID);
     procedure SetTimeZoneName(AValue: string);
   protected
     procedure Bind(Binding: Boolean); override;
     function GetAsDateTime: TDateTime; override;
     function GetAsVariant: variant; override;
     function GetDataSize: Integer; override;
     procedure GetText(var theText: string; ADisplayText: Boolean); override;
     procedure SetAsDateTime(AValue: TDateTime); override;
     procedure SetAsString(const AValue: string); override;
     procedure SetVarValue(const AValue: Variant); override;
   public
     constructor Create(AOwner: TComponent); override;
     function GetAsDateTimeTZ(var aDateTime: TDateTime; var dstOffset: smallint;
                              var aTimeZoneID: TFBTimeZoneID): boolean; overload;
     function GetAsDateTimeTZ(var aDateTime: TDateTime; var dstOffset: smallint;
                              var aTimeZone: string): boolean; overload;
     function GetAsUTCDateTime: TDateTime;
     procedure SetAsDateTimeTZ(aDateTime: TDateTime; aTimeZoneID: TFBTimeZoneID); overload;
     procedure SetAsDateTimeTZ(aDateTime: TDateTime; aTimeZone: string); overload;
     property TimeZoneName: string read GetTimeZoneName write SetTimeZoneName;
     property TimeZoneID: TFBTimeZoneID read GetTimeZoneID;
   published
     property HasTimeZone: boolean read FHasTimeZone;
   end;

   { TIBTimeField }

   TIBTimeField = class(TIBDateTimeField)
   public
     constructor Create(AOwner: TComponent); override;
   end;

  { TIBDataLink }

  TIBDataLink = class(TDetailDataLink)
  private
    FDataSet: TIBCustomDataSet;
    FDelayTimerValue: integer;
    FTimer: IIBTimerInf;
    procedure HandleRefreshTimer(Sender: TObject);
    procedure SetDelayTimerValue(AValue: integer);
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataSet: TIBCustomDataSet);
    destructor Destroy; override;
    property DelayTimerValue: integer {in Milliseconds}
            read FDelayTimerValue write SetDelayTimerValue;
  end;

  TIBGeneratorApplyOnEvent = (gaeOnNewRecord,gaeOnPostRecord);

  { TIBGenerator }

  TIBGenerator = class(TPersistent)
  private
    FOwner: TIBCustomDataSet;
    FApplyOnEvent: TIBGeneratorApplyOnEvent;
    FFieldName: string;
    FGeneratorName: string;
    FIncrement: integer;
    FQuery: TIBSQL;
    function GetDatabase: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetGeneratorName(AValue: string);
    procedure SetIncrement(const AValue: integer);
    procedure SetTransaction(AValue: TIBTransaction);
    procedure SetQuerySQL;
  protected
    function GetNextValue: integer;
  public
    constructor Create(Owner: TIBCustomDataSet);
    destructor Destroy; override;
    procedure Apply;
    property Owner: TIBCustomDataSet read FOwner;
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
  published
    property Generator: string read FGeneratorName write SetGeneratorName;
    property Field: string read FFieldName write FFieldName;
    property Increment: integer read FIncrement write SetIncrement default 1;
    property ApplyOnEvent: TIBGeneratorApplyOnEvent read FApplyOnEvent write FApplyOnEvent;
  end;

  {TIBControlLink - Allows IB Aware controls to react to dataset state changes}

  TIBControlLink = class
  private
    FTIBDataSet: TIBCustomDataSet;
    procedure SetIBDataSet(AValue: TIBCustomDataSet);
  protected
    procedure UpdateSQL(Sender: TObject); virtual;
    procedure UpdateParams(Sender: TObject); virtual;
  public
    destructor Destroy; override;
    property IBDataSet: TIBCustomDataSet read FTIBDataSet write SetIBDataSet;
  end;

  TIBAutoCommit = (acDisabled, acCommitRetaining);

  TIBUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApply, uaApplied);

  TIBUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
                                 UpdateKind: TUpdateKind; var TheUpdateAction: TIBUpdateAction)
                                 of object;
  TIBUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
                                   var UpdateAction: TIBUpdateAction) of object;

  TDataSetCloseAction = (dcDiscardChanges, dcSaveChanges);

  TOnValidatePost = procedure (Sender: TObject; var CancelPost: boolean) of object;

  TOnDeleteReturning = procedure (Sender: TObject; QryResults: IResults) of object;

  { TIBCustomDataSet }

  TIBCustomDataSet = class(TDataset)
  private
    const
      BufferCacheSize    =  1000;  { Allocate cache in this many record chunks}
      UniCache           =  2;     { Uni-directional cache is 2 records big }

      {Buffer cache constants for record selection}
      FILE_BEGIN = 0;
      FILE_CURRENT = 1;
      FILE_END = 2;

      {internal type declarations}
    type
      TArrayDataArray = array [0..0] of TIBArray;
      PArrayDataArray = ^TArrayDataArray;

      TBlobDataArray = array[0..0] of TIBBlobStream;
      PBlobDataArray = ^TBlobDataArray;

      TCachedUpdateStatus = (
                         cusUnmodified, cusModified, cusInserted,
                         cusDeleted, cusUninserted
                        );
      TIBUpdateRecordTypes = set of TCachedUpdateStatus;

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
      end;

      PFieldColumns = ^TFieldColumns;
      TFieldColumns =  array[1..1] of TColumnData;

  protected
    type
      TIBDBKey = record
        DBKey: array[0..7] of Byte;
      end;
      PIBDBKey = ^TIBDBKey;

    TRecordData = record
      rdBookmarkFlag: TBookmarkFlag;
      rdFieldCount: Short;
      rdRecordNumber: Integer;
      rdCachedUpdateStatus: TCachedUpdateStatus;
      rdUpdateStatus: TUpdateStatus;
      rdSavedOffset: DWORD;
      rdDBKey: TIBDBKey;
      rdFields: array[1..1] of TFieldData;
    end;
    PRecordData = ^TRecordData;

  private
    FAllowAutoActivateTransaction: Boolean;
    FArrayFieldCount: integer;
    FArrayCacheOffset: integer;
    FAutoCommit: TIBAutoCommit;
    FCaseSensitiveParameterNames: boolean;
    FDefaultTZDate: TDateTime;
    FEnableStatistics: boolean;
    FGenerateParamNames: Boolean;
    FGeneratorField: TIBGenerator;
    FNeedsRefresh: Boolean;
    FForcedRefresh: Boolean;
    FDidActivate: Boolean;
    FBase: TIBBase;
    FBlobCacheOffset: Integer;
    FBlobStreamList: TList;
    FArrayList: TList;
    FBufferChunks: Integer;
    FBufferCache,
    FOldBufferCache: PChar;
    FBufferChunkSize,
    FCacheSize,
    FOldCacheSize: Integer;
    FFilterBuffer: PChar;
    FBPos,
    FOBPos,
    FBEnd,
    FOBEnd: DWord;
    FCachedUpdates: Boolean;
    FCalcFieldsOffset: Integer;
    FCurrentRecord: Long;
    FDeletedRecords: Long;
    FModelBuffer,
    FOldBuffer: PChar;
    FOnDeleteReturning: TOnDeleteReturning;
    FOnValidatePost: TOnValidatePost;
    FOpen: Boolean;
    FInternalPrepared: Boolean;
    FQDelete,
    FQInsert,
    FQRefresh,
    FQSelect,
    FQModify: TIBSQL;
    FDatabaseInfo: TIBDatabaseInfo;
    FRecordBufferSize: Integer;
    FRecordCount: Integer;
    FRecordSize: Integer;
    FDataSetCloseAction: TDataSetCloseAction;
    FTZTextOption: TTZTextOptions;
    FSQLFiltered: boolean;
    FSQLFilterParams: TStrings;
    FUniDirectional: Boolean;
    FUpdateMode: TUpdateMode;
    FUpdateObject: TIBDataSetUpdateObject;
    FParamCheck: Boolean;
    FUpdatesPending: Boolean;
    FUpdateRecordTypes: TIBUpdateRecordTypes;
    FMappedFieldPosition: array of Integer;
    FDataLink: TIBDataLink;

    FBeforeDatabaseDisconnect,
    FAfterDatabaseDisconnect,
    FDatabaseFree: TNotifyEvent;
    FOnUpdateError: TIBUpdateErrorEvent;
    FOnUpdateRecord: TIBUpdateRecordEvent;
    FBeforeTransactionEnd,
    FAfterTransactionEnd,
    FTransactionFree: TNotifyEvent;
    FAliasNameMap: array of string;
    FAliasNameList: array of string;
    FBaseSQLSelect: TStrings;
    FParser: TSelectSQLParser;
    FCloseAction: TTransactionAction;
    FInTransactionEnd: boolean;
    FIBLinks: TList;
    FFieldColumns: PFieldColumns;
    FBufferUpdatedOnQryReturn: boolean;
    FSelectCount: integer;
    FInsertCount: integer;
    FUpdateCount: integer;
    FDeleteCount: integer;
    procedure ColumnDataToBuffer(QryResults: IResults; ColumnIndex,
      FieldIndex: integer; Buffer: PChar);
    procedure InitModelBuffer(Qry: TIBSQL; Buffer: PChar);
    function GetSelectStmtIntf: IStatement;
    procedure SetCaseSensitiveParameterNames(AValue: boolean);
    procedure SetDefaultTZDate(AValue: TDateTime);
    procedure SetSQLFiltered(AValue: boolean);
    procedure SetSQLFilterParams(AValue: TStrings);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TIBDataSetUpdateObject);

    function AdjustCurrentRecord(Buffer: Pointer; GetMode: TGetMode): TGetResult;
    procedure AdjustRecordOnInsert(Buffer: Pointer);
    function CanEdit: Boolean;
    function CanInsert: Boolean;
    function CanDelete: Boolean;
    function CanRefresh: Boolean;
    procedure CheckEditState;
    procedure ClearBlobCache;
    procedure ClearArrayCache;
    procedure ClearIBLinks;
    procedure CopyRecordBuffer(Source, Dest: Pointer);
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    procedure DoAfterDatabaseDisconnect(Sender: TObject);
    procedure DoDatabaseFree(Sender: TObject);
    procedure DoBeforeTransactionEnd(Sender: TObject; Action: TTransactionAction);
    procedure DoAfterTransactionEnd(Sender: TObject);
    procedure DoTransactionFree(Sender: TObject);
    procedure DoDeleteReturning(QryResults: IResults);
    procedure FetchCurrentRecordToBuffer(Qry: TIBSQL; RecordNumber: Integer;
                                         Buffer: PChar);
    function GetDatabase: TIBDatabase;
    function GetDeleteSQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetSQLParams: ISQLParams;
    function GetRefreshSQL: TStrings;
    function GetSelectSQL: TStrings;
    function GetStatementType: TIBSQLStatementTypes;
    function GetModifySQL: TStrings;
    function GetTransaction: TIBTransaction;
    function GetParser: TSelectSQLParser;
    procedure HandleSQLFilterParamsChanged(Sender: TObject);
    procedure InternalDeleteRecord(Qry: TIBSQL; Buff: Pointer); virtual;
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
                            Options: TLocateOptions): Boolean; virtual;
    procedure InternalPostRecord(Qry: TIBSQL; Buff: Pointer); virtual;
    procedure InternalRevertRecord(RecordNumber: Integer); virtual;
    function IsVisible(Buffer: PChar): Boolean;
    procedure RegisterIBLink(Sender: TIBControlLink);
    procedure UnRegisterIBLink(Sender: TIBControlLink);
    procedure SaveOldBuffer(Buffer: PChar);
    procedure SetBufferChunks(Value: Integer);
    procedure SetDatabase(Value: TIBDatabase);
    procedure SetDeleteSQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetInternalSQLParams(Params: ISQLParams; Buffer: Pointer);
    procedure SetRefreshSQL(Value: TStrings);
    procedure SetSelectSQL(Value: TStrings);
    procedure SetModifySQL(Value: TStrings);
    procedure SetTransaction(Value: TIBTransaction);
    procedure SetUpdateRecordTypes(Value: TIBUpdateRecordTypes);
    procedure SetUniDirectional(Value: Boolean);
    procedure UpdateRecordFromQuery(QryResults: IResults; Buffer: PChar);
    procedure RefreshParams;
    function AdjustPosition(FCache: PChar; Offset: DWORD;
                            Origin: Integer): DWORD;
    procedure ReadCache(FCache: PChar; Offset: DWORD; Origin: Integer;
                       Buffer: PChar);
    procedure ReadRecordCache(RecordNumber: Integer; Buffer: PChar;
                              ReadOldBuffer: Boolean);
    procedure WriteCache(FCache: PChar; Offset: DWORD; Origin: Integer;
                        Buffer: PChar);
    procedure WriteRecordCache(RecordNumber: Integer; Buffer: PChar);
    function InternalGetRecord(Buffer: PChar; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult; virtual;

  protected
    function GetMasterDetailDelay: integer; virtual;
    procedure SetMasterDetailDelay(AValue: integer); virtual;
    procedure ActivateConnection;
    function ActivateTransaction: Boolean;
    procedure DeactivateTransaction;
    procedure CheckDatasetClosed;
    procedure CheckDatasetOpen;
    function CreateParser: TSelectSQLParser; virtual;
    procedure FieldDefsFromQuery(SourceQuery: TIBSQL);
    function GetActiveBuf: PChar;
    procedure InternalBatchInput(InputObject: TIBBatchInput); virtual;
    procedure InternalBatchOutput(OutputObject: TIBBatchOutput); virtual;
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecQuery; virtual;
    procedure InternalRefreshRow; virtual;
    procedure InternalSetParamsFromCursor; virtual;
    procedure CheckNotUniDirectional;
    procedure SQLChanging(Sender: TObject); virtual;
    procedure SQLChanged(Sender: TObject); virtual;

    { IProviderSupport }
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
    function PsGetTableName: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;

    { TDataSet support }
    procedure InternalInsert; override;
    procedure InitRecord(Buffer: PChar); override;
    procedure Disconnect; virtual;
    function ConstraintsStored: Boolean;
    procedure ClearCalcFields(Buffer: PChar); override;
    function AllocRecordBuffer: PChar; override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterInsert; override;
    procedure DoBeforeClose; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetDBAliasName(FieldNo: integer): string;
    function GetFieldDefFromAlias(aliasName: string): TFieldDef;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    procedure InternalAutoCommit;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    function InternalGetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalSetFieldData(Field: TField; Buffer: Pointer); virtual;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure Loaded; override;
    procedure ReQuery;
    procedure ResetBufferCache;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetDataSource(Value: TDataSource);
    procedure SetGenerateParamNames(AValue: Boolean); virtual;
    procedure SetFieldData(Field : TField; Buffer : Pointer); override;
    procedure SetFieldData(Field : TField; Buffer : Pointer;
      NativeFormat : Boolean); overload; override;
    procedure SetRecNo(Value: Integer); override;

  protected
    {Likely to be made public by descendant classes}
    property AutoCommit: TIBAutoCommit read FAutoCommit write FAutoCommit default acDisabled;
    property SQLParams: ISQLParams read GetSQLParams;
    property Params: ISQLParams read GetSQLParams;
    property InternalPrepared: Boolean read FInternalPrepared;
    property QDelete: TIBSQL read FQDelete;
    property QInsert: TIBSQL read FQInsert;
    property QRefresh: TIBSQL read FQRefresh;
    property QSelect: TIBSQL read FQSelect;
    property QModify: TIBSQL read FQModify;
    property StatementType: TIBSQLStatementTypes read GetStatementType;
    property SelectStmtHandle: IStatement read GetSelectStmtIntf;
    property Parser: TSelectSQLParser read GetParser;
    property BaseSQLSelect: TStrings read FBaseSQLSelect;

    {Likely to be made published by descendant classes}
    property CaseSensitiveParameterNames: boolean read FCaseSensitiveParameterNames
                                                  write SetCaseSensitiveParameterNames;
    property BufferChunks: Integer read FBufferChunks write SetBufferChunks;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
    property UniDirectional: Boolean read FUniDirectional write SetUniDirectional default False;
    property GeneratorField: TIBGenerator read FGeneratorField write FGeneratorField;
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property RefreshSQL: TStrings read GetRefreshSQL write SetRefreshSQL;
    property SelectSQL: TStrings read GetSelectSQL write SetSelectSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property TZTextOption: TTZTextOptions read FTZTextOption write FTZTextOption;
    property SQLFiltered: boolean read FSQLFiltered write SetSQLFiltered;
    property SQLFilterParams: TStrings read FSQLFilterParams write SetSQLFilterParams;

    property BeforeDatabaseDisconnect: TNotifyEvent read FBeforeDatabaseDisconnect
                                                 write FBeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect: TNotifyEvent read FAfterDatabaseDisconnect
                                                write FAfterDatabaseDisconnect;
    property DatabaseFree: TNotifyEvent read FDatabaseFree
                                        write FDatabaseFree;
    property BeforeTransactionEnd: TNotifyEvent read FBeforeTransactionEnd
                                             write FBeforeTransactionEnd;
    property AfterTransactionEnd: TNotifyEvent read FAfterTransactionEnd
                                            write FAfterTransactionEnd;
    property TransactionFree: TNotifyEvent read FTransactionFree
                                           write FTransactionFree;
    property OnValidatePost: TOnValidatePost read FOnValidatePost write FOnValidatePost;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates;
    function CachedUpdateStatus: TCachedUpdateStatus;
    procedure CancelUpdates;
    function GetFieldPosition(AliasName: string): integer;
    procedure FetchAll;
    function LocateNext(const KeyFields: string; const KeyValues: Variant;
                        Options: TLocateOptions): Boolean;
    procedure RecordModified(Value: Boolean);
    procedure RevertRecord;
    procedure Undelete;
    procedure ResetParser; virtual;
    function HasParser: boolean;

    { TDataSet support methods }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetArray(Field: TIBArrayField): IArray;
    procedure SetArrayIntf(AnArray: IArray; Field: TIBArrayField);
    function GetCurrentRecord(Buffer: PChar): Boolean; override;
    function GetFieldData(Field : TField; Buffer : Pointer) : Boolean; overload; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; (*override;*)
    function GetFieldData(Field : TField; Buffer : Pointer;
      NativeFormat : Boolean) : Boolean; overload; override;
    property GenerateParamNames: Boolean read FGenerateParamNames write SetGenerateParamNames;
    function Locate(const KeyFields: string; const KeyValues: Variant;
                    Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
                    const ResultFields: string): Variant; override;
    function UpdateStatus: TUpdateStatus; override;
    function IsSequenced: Boolean; override;
    procedure Post; override;
    function ParamByName(ParamName: String): ISQLParam;
    function FindParam(ParamName: String): ISQLParam;
    property ArrayFieldCount: integer read FArrayFieldCount;
    property DatabaseInfo: TIBDatabaseInfo read FDatabaseInfo;
    property UpdateObject: TIBDataSetUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read FUpdatesPending;
    property UpdateRecordTypes: TIBUpdateRecordTypes read FUpdateRecordTypes
                                                      write SetUpdateRecordTypes;
    property MasterDetailDelay: integer read GetMasterDetailDelay write SetMasterDetailDelay;
    property DataSetCloseAction: TDataSetCloseAction
               read FDataSetCloseAction write FDataSetCloseAction;
    property DefaultTZDate: TDateTime read FDefaultTZDate write SetDefaultTZDate;

  public
    {Performance Statistics}
    function GetRowsAffected(var SelectCount, InsertCount, UpdateCount, DeleteCount: integer): boolean;
    function GetPerfStatistics(var stats: TPerfCounters): boolean;
    property EnableStatistics: boolean read FEnableStatistics write FEnableStatistics;

  published
    property AllowAutoActivateTransaction: Boolean read FAllowAutoActivateTransaction
                 write FAllowAutoActivateTransaction;
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Transaction: TIBTransaction read GetTransaction
                                          write SetTransaction;
    property ForcedRefresh: Boolean read FForcedRefresh
                                    write FForcedRefresh default False;
    property AutoCalcFields;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnUpdateError: TIBUpdateErrorEvent read FOnUpdateError
                                                 write FOnUpdateError;
    property OnUpdateRecord: TIBUpdateRecordEvent read FOnUpdateRecord
                                                   write FOnUpdateRecord;
    property OnDeleteReturning: TOnDeleteReturning read FOnDeleteReturning
                                                   write FOnDeleteReturning;
  end;

  { TIBParserDataSet }

  TIBParserDataSet = class(TIBCustomDataSet)
  protected
    procedure DoBeforeOpen; override;
  public
    property Parser;
  end;

  TIBDataSet = class(TIBParserDataSet)
  private
    function GetPrepared: Boolean;

  protected
    procedure SetFiltered(Value: Boolean); override;
    procedure InternalOpen; override;

  public
    procedure Prepare;
    procedure UnPrepare;
    procedure BatchInput(InputObject: TIBBatchInput);
    procedure BatchOutput(OutputObject: TIBBatchOutput);
    procedure ExecSQL;

  public
    property Params;
    property Prepared : Boolean read GetPrepared;
    property QDelete;
    property QInsert;
    property QRefresh;
    property QSelect;
    property QModify;
    property StatementType;
    property SelectStmtHandle;
    property BaseSQLSelect;

  published
    { TIBCustomDataSet }
    property AutoCommit;
    property BufferChunks;
    property CachedUpdates;
    property CaseSensitiveParameterNames;
    property EnableStatistics;
    property DeleteSQL;
    property InsertSQL;
    property RefreshSQL;
    property SelectSQL;
    property ModifySQL;
    property GeneratorField;
    property GenerateParamNames;
    property MasterDetailDelay;
    property ParamCheck;
    property UniDirectional;
    property Filtered;
    property DataSetCloseAction;
    property TZTextOption;
    property DefaultTZDate;
    property SQLFiltered;
    property SQLFilterParams;

    property BeforeDatabaseDisconnect;
    property AfterDatabaseDisconnect;
    property DatabaseFree;
    property BeforeTransactionEnd;
    property AfterTransactionEnd;
    property TransactionFree;

    { TIBDataSet }
    property Active;
    property AutoCalcFields;
    property DataSource read GetDataSource write SetDataSource;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnValidatePost;
    property OnDeleteReturning;
  end;

  { TIBDSBlobStream }
  TIBDSBlobStream = class(TStream)
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
  end;

  {Extended Field Def for character set info}

  { TIBFieldDef }

  TIBFieldDef = class(TFieldDef)
  private
    FArrayBounds: TArrayBounds;
    FArrayDimensions: integer;
    FCharacterSetName: RawByteString;
    FCharacterSetSize: integer;
    FCodePage: TSystemCodePage;
    FHasTimeZone: boolean;
    FIdentityColumn: boolean;
    FRelationName: string;
    FDataSize: integer;
  published
    property CharacterSetName: RawByteString read FCharacterSetName write FCharacterSetName;
    property CharacterSetSize: integer read FCharacterSetSize write FCharacterSetSize;
    property CodePage: TSystemCodePage read FCodePage write FCodePage;
    property DataSize: integer read FDataSize write FDataSize;
    property RelationName: string read FRelationName write FRelationName;
    property ArrayDimensions: integer read FArrayDimensions write FArrayDimensions;
    property ArrayBounds: TArrayBounds read FArrayBounds write FArrayBounds;
    property IdentityColumn: boolean read FIdentityColumn write FIdentityColumn default false;
    property HasTimeZone: boolean read FHasTimeZone write FHasTimeZone default false;
  end;

const
  DefaultFieldClasses: array[TFieldType] of TFieldClass = (
    nil,                { ftUnknown }
    TIBStringField,     { ftString }
    TIBSmallintField,   { ftSmallint }
    TIBIntegerField,    { ftInteger }
    TWordField,         { ftWord }
    TBooleanField,      { ftBoolean }
    TFloatField,        { ftFloat }
    TCurrencyField,     { ftCurrency }
    TIBBCDField,        { ftBCD }
    TDateField,         { ftDate }
    TIBTimeField,       { ftTime }
    TIBDateTimeField,   { ftDateTime }
    TBytesField,        { ftBytes }
    TVarBytesField,     { ftVarBytes }
    TAutoIncField,      { ftAutoInc }
    TBlobField,         { ftBlob }
    TIBMemoField,       { ftMemo }
    TGraphicField,      { ftGraphic }
    TBlobField,         { ftFmtMemo }
    TBlobField,         { ftParadoxOle }
    TBlobField,         { ftDBaseOle }
    TBlobField,         { ftTypedBinary }
    nil,                { ftCursor }
    TStringField,       { ftFixedChar }
    nil,                { ftWideString }
    TIBLargeIntField,   { ftLargeInt }
    nil,                { ftADT }
    TIBArrayField,      { ftArray }
    nil,                { ftReference }
    nil,                { ftDataSet }
    TBlobField,         { ftOraBlob }
    TMemoField,         { ftOraClob }
    TVariantField,      { ftVariant }
    nil,                { ftInterface }
    nil,                { ftIDispatch }
    TGuidField,         { ftGuid }
    TIBDateTimeField,   { ftTimestamp }
    TFmtBCDField,       { ftFMTBcd }
    nil,                { ftFixedWideChar }
    nil                 { ftWideMemo }
{$IF declared(ftOraTimeStamp)}
    {These six extra elements were added to the FPC fixes_3_2 branch in Q3 2021}
    ,
    nil,                {ftOraTimeStamp}
    nil,                {ftOraInterval}
    nil,                {ftLongWord}
    nil,                {ftShortint}
    nil,                {ftByte}
    nil                 {ftExtended}
{$IFEND}
    );
(*var
  CreateProviderProc: function(DataSet: TIBCustomDataSet): IProvider = nil;*)

implementation

uses Variants, FmtBCD, LazUTF8, IBMessages, IBQuery, DateUtils, dbconst;

type

  TFieldNode = class(TObject)
  protected
    FieldName : String;
    COMPUTED_BLR : Boolean;
    DEFAULT_VALUE : boolean;
    IDENTITY_COLUMN : boolean;
    NextField : TFieldNode;
  end;

  TRelationNode = class(TObject)
  protected
    RelationName : String;
    FieldNodes : TFieldNode;
    NextRelation : TRelationNode;
  end;


  {  Copied from LCLProc in order to avoid LCL dependency

    Ensures the covenient look of multiline string
    when displaying it in the single line
    * Replaces CR and LF with spaces
    * Removes duplicate spaces
  }
  function TextToSingleLine(const AText: string): string;
  var
    str: string;
    i, wstart, wlen: Integer;
  begin
    str := Trim(AText);
    wstart := 0;
    wlen := 0;
    i := 1;
    while i < Length(str) - 1 do
    begin
      if (str[i] in [' ', #13, #10]) then
      begin
        if (wstart = 0) then
        begin
          wstart := i;
          wlen := 1;
        end else
          Inc(wlen);
      end else
      begin
        if wstart > 0 then
        begin
          str[wstart] := ' ';
          Delete(str, wstart+1, wlen-1);
          Dec(i, wlen-1);
          wstart := 0;
        end;
      end;
      Inc(i);
    end;
    Result := str;
  end;

{ TIBDateTimeField }

function TIBDateTimeField.GetTimeZoneName: string;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    Result := GetTimeZoneServices.TimeZoneID2TimeZoneName(aBuffer.TimeZoneID)
  else
    Result := '';
end;

function TIBDateTimeField.GetTimeZoneServices: ITimeZoneServices;
begin
  if (FTimeZoneServices = nil) and
     (DataSet <> nil) and ((DataSet as TIBCustomDataSet).Database <> nil)
      and ((DataSet as TIBCustomDataSet).Database.attachment <> nil) then
    FTimeZoneServices := (DataSet as TIBCustomDataSet).Database.attachment.GetTimeZoneServices;
  Result := FTimeZoneServices;
end;

function TIBDateTimeField.GetDateTimeBuffer(
  var aBuffer: TIBBufferedDateTimeWithTimeZone): boolean;
begin
  Result := HasTimeZone;
  if Result then
    Result := GetData(@aBuffer,False);
end;

function TIBDateTimeField.GetTimeZoneID: TFBTimeZoneID;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    Result := aBuffer.TimeZoneID
  else
    Result := TimeZoneID_GMT;
end;

procedure TIBDateTimeField.SetTimeZoneID(aValue: TFBTimeZoneID);
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    SetAsDateTimeTZ(aBuffer.Timestamp,aValue)
end;

procedure TIBDateTimeField.SetTimeZoneName(AValue: string);
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    SetAsDateTimeTZ(aBuffer.Timestamp,aValue)
end;

procedure TIBDateTimeField.Bind(Binding: Boolean);
var IBFieldDef: TIBFieldDef;
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
  begin
    IBFieldDef := FieldDef as TIBFieldDef;
    FHasTimeZone := IBFieldDef.HasTimeZone;
  end;
end;

function TIBDateTimeField.GetAsDateTime: TDateTime;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    Result := aBuffer.Timestamp
  else
    Result := inherited GetAsDateTime;
end;

function TIBDateTimeField.GetAsVariant: variant;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    with aBuffer do
      Result := VarArrayOf([Timestamp,dstOffset,TimeZoneID])
  else
    Result := inherited GetAsVariant;
end;

function TIBDateTimeField.GetDataSize: Integer;
begin
  if HasTimeZone then
    Result := sizeof(TIBBufferedDateTimeWithTimeZone)
  else
    Result := inherited GetDataSize;
end;

procedure TIBDateTimeField.GetText(var theText: string; ADisplayText: Boolean);
var aBuffer: TIBBufferedDateTimeWithTimeZone;
    F: string;
begin
  if Dataset = nil then
    DatabaseErrorFmt(SNoDataset,[FieldName]);

  if GetDateTimeBuffer(aBuffer) then
    {$if declared(DefaultFormatSettings)}
    with DefaultFormatSettings do
    {$else}
    {$if declared(FormatSettings)}
    with FormatSettings do
    {$ifend}
    {$ifend}
  begin
    if ADisplayText and (Length(DisplayFormat) <> 0) then
      F := DisplayFormat
    else
      Case DataType of
       ftTime : F := LongTimeFormat;
       ftDate : F := ShortDateFormat;
      else
       F := ShortDateFormat + ' ' + LongTimeFormat;
      end;

    with aBuffer do
    case (DataSet as TIBCustomDataSet).TZTextOption of
    tzOffset:
      TheText := FBFormatDateTime(F,timestamp) + ' ' + FormatTimeZoneOffset(dstOffset);
    tzGMT:
      TheText := FBFormatDateTime(F,IncMinute(Timestamp,-dstOffset));
    tzOriginalID:
      TheText := FBFormatDateTime(F,timestamp) + ' ' + GetTimeZoneServices.TimeZoneID2TimeZoneName(TimeZoneID);
    end;
  end
  else
    inherited GetText(theText, ADisplayText);
end;

procedure TIBDateTimeField.SetAsDateTime(AValue: TDateTime);
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    SetAsDateTimeTZ(AValue,aBuffer.TimeZoneID)
  else
    inherited SetAsDateTime(AValue)
end;

procedure TIBDateTimeField.SetAsString(const AValue: string);
var aDateTime: TDateTime;
    aTimeZone: AnsiString;
begin
  if AValue = '' then
    Clear
  else
  if ParseDateTimeTZString(AValue,aDateTime,aTimeZone,DataType=ftTime) then
  begin
    if not HasTimeZone or (aTimeZone = '') then
      SetAsDateTime(aDateTime)
    else
      SetAsDateTimeTZ(aDateTime,aTimeZone);
  end
  else
    IBError(ibxeBadDateTimeTZString,[AValue]);
end;

procedure TIBDateTimeField.SetVarValue(const AValue: Variant);
begin
  if HasTimeZone and VarIsArray(AValue)then
      SetAsDateTimeTZ(AValue[0],string(AValue[2]))
  else
    inherited SetVarValue(AValue);
end;

constructor TIBDateTimeField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDateTime);
end;

function TIBDateTimeField.GetAsDateTimeTZ(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimeZoneID: TFBTimeZoneID): boolean;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  Result := GetDateTimeBuffer(aBuffer);
  if Result then
  begin
    aDateTime := aBuffer.Timestamp;
    dstOffset := aBuffer.dstOffset;
    aTimeZoneID := aBuffer.TimeZoneID;
  end
  else
    aDateTime := inherited GetAsDateTime
end;

function TIBDateTimeField.GetAsDateTimeTZ(var aDateTime: TDateTime;
  var dstOffset: smallint; var aTimeZone: string): boolean;
var aTimeZoneID: TFBTimeZoneID;
begin
  Result := GetAsDateTimeTZ(aDateTime,dstOffset,aTimeZoneID);
  if Result then
    aTimeZone := GetTimeZoneServices.TimeZoneID2TimeZoneName(aTimeZoneID);
end;

function TIBDateTimeField.GetAsUTCDateTime: TDateTime;
var aBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if GetDateTimeBuffer(aBuffer) then
    Result := IncMinute(aBuffer.timestamp,-aBuffer.dstOffset)
  else
    Result := inherited GetAsDateTime;
end;

procedure TIBDateTimeField.SetAsDateTimeTZ(aDateTime: TDateTime;
  aTimeZoneID: TFBTimeZoneID);
var DateTimeBuffer: TIBBufferedDateTimeWithTimeZone;
begin
  if HasTimeZone then
  begin
    DateTimeBuffer.Timestamp := aDateTime;
    DateTimeBuffer.dstOffset := GetTimeZoneServices.GetEffectiveOffsetMins(aDateTime,aTimeZoneID);
    DateTimeBuffer.TimeZoneID := aTimeZoneID;
    SetData(@DateTimeBuffer,False);
  end
  else
    inherited SetAsDateTime(aDateTime);
end;

procedure TIBDateTimeField.SetAsDateTimeTZ(aDateTime: TDateTime;
  aTimeZone: string);
begin
  if HasTimeZone then
    SetAsDateTimeTZ(aDateTime,GetTimeZoneServices.TimeZoneName2TimeZoneID(aTimeZone))
  else
    inherited SetAsDateTime(aDateTime);
end;

{ TIBTimeField }

constructor TIBTimeField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftTime);
end;

{ TIBParserDataSet }

procedure TIBParserDataSet.DoBeforeOpen;
var i: integer;
begin
  if assigned(FParser) then
     FParser.RestoreClauseValues;
  if SQLFiltered then
    for i := 0 to SQLFilterParams.Count - 1 do
      Parser.Add2WhereClause(SQLFilterParams[i]);
  for i := 0 to FIBLinks.Count - 1 do
    TIBControlLink(FIBLinks[i]).UpdateSQL(self);
  inherited DoBeforeOpen;
  for i := 0 to FIBLinks.Count - 1 do
    TIBControlLink(FIBLinks[i]).UpdateParams(self);
end;

{ TIBLargeIntField }

procedure TIBLargeIntField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
     FIdentityColumn := (FieldDef as TIBFieldDef).IdentityColumn;
end;

{ TIBIntegerField }

procedure TIBIntegerField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
     FIdentityColumn := (FieldDef as TIBFieldDef).IdentityColumn;
end;

{ TIBSmallintField }

procedure TIBSmallintField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
     FIdentityColumn := (FieldDef as TIBFieldDef).IdentityColumn;
end;

{ TIBArray }

procedure TIBArray.EventHandler(Sender: IArray; Reason: TArrayEventReason);
begin
  case Reason of
  arChanging:
    if FRecNo <> FField.Dataset.RecNo then
      IBError(ibxeNotCurrentArray,[nil]);

  arChanged:
    FField.DataChanged;
  end;
end;

constructor TIBArray.Create(aField: TIBArrayField; anArray: IArray);
begin
  inherited Create;
  FField := aField;
  FArray := anArray;
  FRecNo := FField.Dataset.RecNo;
  FArray.AddEventHandler(EventHandler);
end;

destructor TIBArray.Destroy;
begin
  FArray.RemoveEventHandler(EventHandler);
  inherited Destroy;
end;

{ TIBArrayField }

function TIBArrayField.GetArrayIntf: IArray;
begin
  Result := TIBCustomDataSet(DataSet).GetArray(self);
end;

function TIBArrayField.GetArrayID: TISC_QUAD;
begin
  GetData(@Result);
end;

procedure TIBArrayField.SetArrayIntf(AValue: IArray);
begin
  TIBCustomDataSet(DataSet).SetArrayIntf(AValue,self);
  DataChanged;
end;

class procedure TIBArrayField.CheckTypeSize(AValue: Longint);
begin
  //Ignore
end;

function TIBArrayField.GetAsString: string;
begin
  Result := '(Array)';
end;

function TIBArrayField.GetDataSize: Integer;
begin
  Result := sizeof(TISC_QUAD);
end;

procedure TIBArrayField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding then
  begin
    FCacheOffset := TIBCustomDataSet(DataSet).ArrayFieldCount;
    Inc(TIBCustomDataSet(DataSet).FArrayFieldCount);
    if FieldDef <> nil then
    begin
      FRelationName := TIBFieldDef(FieldDef).FRelationName;
      FArrayDimensions := TIBFieldDef(FieldDef).ArrayDimensions;
      FArrayBounds :=  TIBFieldDef(FieldDef).ArrayBounds;
    end;
  end;
end;

constructor TIBArrayField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftArray);
end;

function TIBArrayField.CreateArray: IArray;
begin
with DataSet as TIBCustomDataSet do
  Result := Database.Attachment.CreateArray(Transaction.TransactionIntf,FRelationName,FieldName);
end;

{ TIBMemoField }

function TIBMemoField.GetTruncatedText: string;
begin
   Result := GetAsString;

   if Result <> '' then
   begin
       case CharacterSetSize of
       1:
         if DisplayWidth = 0 then
           Result := TextToSingleLine(Result)
         else
         if Length(Result) > DisplayWidth then {Show truncation with elipses}
           Result := TextToSingleLine(system.copy(Result,1,DisplayWidth-3)) + '...';

       {2: case 2 ignored. This should be handled by TIBWideMemo}

       3, {Assume UNICODE_FSS is really UTF8}
       4: {Include GB18030 - assuming UTF8 routines work for this codeset}
         if DisplayWidth = 0 then
           {$if declared(Utf8EscapeControlChars)}
           Result := Utf8EscapeControlChars(TextToSingleLine(Result))
           {$else}
           Result := ValidUTF8String(TextToSingleLine(Result))
           {$endif}
         else
         if UTF8Length(Result) > DisplayWidth then {Show truncation with elipses}
           {$if declared(Utf8EscapeControlChars)}
           Result := Utf8EscapeControlChars(TextToSingleLine(UTF8Copy(Result,1,DisplayWidth-3))) + '...';
           {$else}
           Result := ValidUTF8String(TextToSingleLine(UTF8Copy(Result,1,DisplayWidth-3))) + '...';
           {$endif}
       end;
   end
end;

procedure TIBMemoField.Bind(Binding: Boolean);
var IBFieldDef: TIBFieldDef;
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
  begin
    IBFieldDef := FieldDef as TIBFieldDef;
    CharacterSetSize := IBFieldDef.CharacterSetSize;
    CharacterSetName := IBFieldDef.CharacterSetName;
    CodePage := IBFieldDef.CodePage;
  end;
end;

function TIBMemoField.GetAsString: string;
var s: RawByteString;
begin
  s := inherited GetAsString;
  SetCodePage(s,CodePage,false);
  if (CodePage <> CP_NONE) and (CodePage <> CP_UTF8) then
    SetCodePage(s,CP_UTF8,true);  {LCL only accepts UTF8}
  Result := s;
end;

function TIBMemoField.GetDefaultWidth: Longint;
begin
  if DisplayTextAsClassName then
    Result := inherited
  else
    Result := 128;
end;

procedure TIBMemoField.GetText(var AText: string; ADisplayText: Boolean);
begin
  if ADisplayText then
  begin
    if not DisplayTextAsClassName and (CharacterSetName <> '') then
      AText := GetTruncatedText
    else
      inherited GetText(AText, ADisplayText);
  end
  else
    AText := GetAsString;
end;

procedure TIBMemoField.SetAsString(const AValue: string);
var s: RawByteString;
begin
  s := AValue;
  if StringCodePage(s) <> CodePage then
    SetCodePage(s,CodePage,CodePage<>CP_NONE);
  inherited SetAsString(s);
end;

constructor TIBMemoField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BlobType := ftMemo;
  FCodePage := CP_NONE;
end;

{ TIBControlLink }

destructor TIBControlLink.Destroy;
begin
  IBDataSet := nil;
  inherited Destroy;
end;

procedure TIBControlLink.UpdateParams(Sender: TObject);
begin

end;

procedure TIBControlLink.UpdateSQL(Sender: TObject);
begin

end;

procedure TIBControlLink.SetIBDataSet(AValue: TIBCustomDataSet);
begin
  if FTIBDataSet = AValue then Exit;
  if IBDataSet <> nil then
    IBDataSet.UnRegisterIBLink(self);
  FTIBDataSet := AValue;
  if IBDataSet <> nil then
    IBDataSet.RegisterIBLink(self);
end;


{ TIBStringField}

procedure TIBStringField.Bind(Binding: Boolean);
var IBFieldDef: TIBFieldDef;
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
  begin
    IBFieldDef := FieldDef as TIBFieldDef;
    CharacterSetSize := IBFieldDef.CharacterSetSize;
    CharacterSetName := IBFieldDef.CharacterSetName;
    FDataSize := IBFieldDef.DataSize;
    if AutoFieldSize then
      Size := IBFieldDef.Size;
    CodePage := IBFieldDef.CodePage;
  end;
end;

function TIBStringField.GetDataSize: Integer;
begin
  Result := FDataSize;
end;

constructor TIBStringField.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCharacterSetSize := 1;
  FCodePage := CP_NONE;
  FAutoFieldSize := true;
end;

class procedure TIBStringField.CheckTypeSize(Value: Integer);
begin
  { don't check string size. all sizes valid }
end;

function TIBStringField.GetAsString: string;
begin
  if not GetValue(Result) then Result := '';
end;

function TIBStringField.GetAsVariant: Variant;
var
  S: string;
begin
  if GetValue(S) then Result := S else Result := Null;
end;

function TIBStringField.GetValue(var Value: string): Boolean;
var
  Buffer: PChar;
  s: RawByteString;
begin
  Buffer := nil;
  IBAlloc(Buffer, 0, DataSize + 1); {allow for trailing #0}
  try
    Result := GetData(Buffer);
    if Result then
    begin
      s := strpas(Buffer);
      SetCodePage(s,CodePage,false);
      if (CodePage <> CP_NONE) and (CodePage <> CP_UTF8) then
        SetCodePage(s,CP_UTF8,true);  {LCL only accepts UTF8}

      if (CodePage = CP_UTF8) and (UTF8Length(s) > Size) then
        {truncate to max. number of UTF8 characters - usually a problem with
         fixed width columns right padded with white space}
        Value := UTF8Copy(s,1,Size)
      else
        Value := s;

//      writeln(FieldName,': ', StringCodePage(Value),', ',Value);
      if Transliterate and (Value <> '') then
        DataSet.Translate(PChar(Value), PChar(Value), False);
    end
  finally
    FreeMem(Buffer);
  end;
end;

procedure TIBStringField.SetAsString(const Value: string);
var
  Buffer: PChar;
  s: RawByteString;
begin
  Buffer := nil;
  IBAlloc(Buffer, 0, DataSize + 1); {allow for trailing #0}
  try
    s := Value;
    if StringCodePage(s) <> CodePage then
      SetCodePage(s,CodePage,CodePage<>CP_NONE);
    StrLCopy(Buffer, PChar(s), DataSize);
    if Transliterate then
      DataSet.Translate(Buffer, Buffer, True);
    SetData(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;


{ TIBBCDField }

constructor TIBBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 8;
end;

procedure TIBBCDField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding and (FieldDef <> nil) then
     FIdentityColumn := (FieldDef as TIBFieldDef).IdentityColumn;
end;

class procedure TIBBCDField.CheckTypeSize(Value: Integer);
begin
{ No need to check as the base type is currency, not BCD }
end;

function TIBBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then
    Result := 0;
end;

function TIBBCDField.GetAsString: string;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := CurrToStr(C)
  else
    Result := '';
end;

function TIBBCDField.GetAsVariant: Variant;
var
  C: System.Currency;
begin
  if GetValue(C) then
    Result := C
  else
    Result := Null;
end;

function TIBBCDField.GetDataSize: Integer;
begin
{$IFDEF TBCDFIELD_IS_BCD}
  Result := 8;
{$ELSE}
  Result := inherited GetDataSize
{$ENDIF}
end;

{ TIBDataLink }

constructor TIBDataLink.Create(ADataSet: TIBCustomDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
  if assigned(IBGUIInterface) then
  begin
    FTimer := IBGUIInterface.CreateTimer;
    if FTimer <> nil then
    begin
      FTimer.Enabled := false;
      FTimer.Interval := 0;
      FTimer.OnTimer := HandleRefreshTimer;
    end;
  end;
  FDelayTimerValue := 0;
end;

destructor TIBDataLink.Destroy;
begin
  FDataSet.FDataLink := nil;
  inherited Destroy;
end;

procedure TIBDataLink.HandleRefreshTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  if FDataSet.Active then
    FDataSet.RefreshParams;
end;

procedure TIBDataLink.SetDelayTimerValue(AValue: integer);
begin
  if FDelayTimerValue = AValue then Exit;
  if assigned(FTimer) then
    FTimer.Enabled := false;
  FDelayTimerValue := AValue;
end;

procedure TIBDataLink.ActiveChanged;
begin
  if DetailDataSet.Active and DataSet.Active then
    FDataSet.RefreshParams;
end;


function TIBDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TIBDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataSet.Active then
  begin
    if assigned(FTimer) and (FDelayTimerValue > 0) then
    with FTimer do
    begin
      FTimer.Enabled := false;
      FTimer.Interval := FDelayTimerValue;
      FTimer.Enabled := true;
    end
    else
      FDataSet.RefreshParams;
  end;
end;

procedure TIBDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

{ TIBCustomDataSet }

constructor TIBCustomDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := TIBBase.Create(Self);
  FDatabaseInfo := TIBDatabaseInfo.Create(self);
  FIBLinks := TList.Create;
  FCurrentRecord := -1;
  FDeletedRecords := 0;
  FUniDirectional := False;
  FBufferChunks := BufferCacheSize;
  FBlobStreamList := TList.Create;
  FArrayList := TList.Create;
  FGeneratorField := TIBGenerator.Create(self);
  FDataLink := TIBDataLink.Create(Self);
  FQDelete := TIBSQL.Create(Self);
  FQDelete.OnSQLChanging := SQLChanging;
  FQDelete.GoToFirstRecordOnExecute := True;
  FQInsert := TIBSQL.Create(Self);
  FQInsert.OnSQLChanging := SQLChanging;
  FQInsert.GoToFirstRecordOnExecute := true;
  FQRefresh := TIBSQL.Create(Self);
  FQRefresh.OnSQLChanging := SQLChanging;
  FQRefresh.GoToFirstRecordOnExecute := False;
  FQSelect := TIBSQL.Create(Self);
  FQSelect.OnSQLChanging := SQLChanging;
  FQSelect.OnSQLChanged := SQLChanged;
  FQSelect.GoToFirstRecordOnExecute := False;
  FQModify := TIBSQL.Create(Self);
  FQModify.OnSQLChanging := SQLChanging;
  FQModify.GoToFirstRecordOnExecute := True;  {In Firebird 5, Update..Returning returns a cursor}
  FUpdateRecordTypes := [cusUnmodified, cusModified, cusInserted];
  FParamCheck := True;
  FGenerateParamNames := False;
  FForcedRefresh := False;
  FAutoCommit:= acDisabled;
  FDataSetCloseAction := dcDiscardChanges;
  {Bookmark Size is Integer for IBX}
  BookmarkSize := SizeOf(Integer);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.AfterDatabaseDisconnect := DoAfterDatabaseDisconnect;
  FBase.OnDatabaseFree := DoDatabaseFree;
  FBase.BeforeTransactionEnd := DoBeforeTransactionEnd;
  FBase.AfterTransactionEnd := DoAfterTransactionEnd;
  FBase.OnTransactionFree := DoTransactionFree;
  if AOwner is TIBDatabase then
    Database := TIBDatabase(AOwner)
  else
    if AOwner is TIBTransaction then
      Transaction := TIBTransaction(AOwner);
  FBaseSQLSelect := TStringList.Create;
  FTZTextOption := tzOffset;
  FDefaultTZDate := EncodeDate(2020,1,1);
  FSQLFilterParams := TStringList.Create;
  TStringList(FSQLFilterParams).OnChange :=  HandleSQLFilterParamsChanged;
end;

destructor TIBCustomDataSet.Destroy;
begin
  if Active then Active := false;
  if assigned(FGeneratorField) then FGeneratorField.Free;
  FDataLink.Free;
  FBase.Free;
  ClearBlobCache;
  ClearIBLinks;
  FIBLinks.Free;
  FBlobStreamList.Free;
  FArrayList.Free;
  FreeMem(FBufferCache);
  FBufferCache := nil;
  FreeMem(FOldBufferCache);
  FOldBufferCache := nil;
  FCacheSize := 0;
  FOldCacheSize := 0;
  FMappedFieldPosition := nil;
  if assigned(FBaseSQLSelect) then FBaseSQLSelect.Free;
  if assigned(FParser) then FParser.Free;
  if assigned(FSQLFilterParams) then FSQLFilterParams.Free;
  inherited Destroy;
end;

function TIBCustomDataSet.AdjustCurrentRecord(Buffer: Pointer; GetMode: TGetMode):
                                             TGetResult;
begin
  while not IsVisible(Buffer) do
  begin
    if GetMode = gmPrior then
    begin
      Dec(FCurrentRecord);
      if FCurrentRecord = -1 then
      begin
        result := grBOF;
        exit;
      end;
      ReadRecordCache(FCurrentRecord, Buffer, False);
    end
    else begin
      Inc(FCurrentRecord);
      if (FCurrentRecord = FRecordCount) then
      begin
        if (not FQSelect.EOF) and FQSelect.Next  then
        begin
          FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
          Inc(FRecordCount);
        end
        else begin
          result := grEOF;
          exit;
        end;
      end
      else
        ReadRecordCache(FCurrentRecord, Buffer, False);
    end;
  end;
  result := grOK;
end;

procedure TIBCustomDataSet.ApplyUpdates;
var
  CurBookmark: TBookmark;
  Buffer: PRecordData;
  CurUpdateTypes: TIBUpdateRecordTypes;
  UpdateAction: TIBUpdateAction;
  UpdateKind: TUpdateKind;
  bRecordsSkipped: Boolean;

  procedure GetUpdateKind;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
        UpdateKind := ukModify;
      cusInserted:
        UpdateKind := ukInsert;
      else
        UpdateKind := ukDelete;
    end;
  end;

  procedure ResetBufferUpdateStatus;
  begin
    case Buffer^.rdCachedUpdateStatus of
      cusModified:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
      cusInserted:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usUnmodified;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
      cusDeleted:
      begin
        PRecordData(Buffer)^.rdUpdateStatus := usDeleted;
        PRecordData(Buffer)^.rdCachedUpdateStatus := cusUnmodified;
      end;
    end;
    WriteRecordCache(PRecordData(Buffer)^.rdRecordNumber, Pointer(Buffer));
  end;

  procedure UpdateUsingOnUpdateRecord;
  begin
    try
      FOnUpdateRecord(Self, UpdateKind, UpdateAction);
    except
      on E: Exception do
      begin
        UpdateAction := uaFail;
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EDatabaseError(E), UpdateKind, UpdateAction);
      end;
    end;
  end;

  procedure UpdateUsingUpdateObject;
  begin
    try
      FUpdateObject.Apply(UpdateKind,PChar(Buffer));
      UpdateAction := uaApplied;
    except
      on E: Exception do
      begin
        UpdateAction := uaFail;
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EDatabaseError(E), UpdateKind, UpdateAction);
      end;
    end;
  end;

  procedure UpdateUsingInternalquery;
  begin
    try
      case Buffer^.rdCachedUpdateStatus of
        cusModified:
          InternalPostRecord(FQModify, Buffer);
        cusInserted:
          InternalPostRecord(FQInsert, Buffer);
        cusDeleted:
          InternalDeleteRecord(FQDelete, Buffer);
      end;
      UpdateAction := uaApplied;
    except
      on E: Exception do begin
        UpdateAction := uaFail;
        if (E is EDatabaseError) and Assigned(FOnUpdateError) then
          FOnUpdateError(Self, EDatabaseError(E), UpdateKind, UpdateAction);
      end;
    end;
  end;

begin
  if State in [dsEdit, dsInsert] then
    Post;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  DisableControls;
  CurBookmark := Bookmark;
  CurUpdateTypes := FUpdateRecordTypes;
  FUpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
  try
    First;
    bRecordsSkipped := False;
    while not EOF do
    begin
      Buffer := PRecordData(GetActiveBuf);
      GetUpdateKind;
      UpdateAction := uaApply;
      if (Assigned(FOnUpdateRecord)) then
        UpdateUsingOnUpdateRecord;
      if UpdateAction = uaApply then
      begin
        if Assigned(FUpdateObject) then
          UpdateUsingUpdateObject
        else
          UpdateUsingInternalquery;
      end;

      case UpdateAction of
        uaFail:
          IBError(ibxeUserAbort, [nil]);
        uaAbort:
          SysUtils.Abort;
        uaApplied:
          ResetBufferUpdateStatus;
        uaSkip:
          bRecordsSkipped := True;
        uaRetry:
          Continue;
      end;

      Next;
    end;
    FUpdatesPending := bRecordsSkipped;
  finally
    FUpdateRecordTypes := CurUpdateTypes;
    Bookmark := CurBookmark;
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.InternalBatchInput(InputObject: TIBBatchInput);
begin
  FQSelect.BatchInput(InputObject);
end;

procedure TIBCustomDataSet.InternalBatchOutput(OutputObject: TIBBatchOutput);
var
  Qry: TIBSQL;
begin
  Qry := TIBSQL.Create(Self);
  try
    Qry.Database := FBase.Database;
    Qry.Transaction := FBase.Transaction;
    Qry.SQL.Assign(FQSelect.SQL);
    Qry.BatchOutput(OutputObject);
  finally
    Qry.Free;
  end;
end;

procedure TIBCustomDataSet.CancelUpdates;
var
  CurUpdateTypes: TIBUpdateRecordTypes;
begin
  if State in [dsEdit, dsInsert] then
    Post;
  if FCachedUpdates and FUpdatesPending then
  begin
    DisableControls;
    CurUpdateTypes := UpdateRecordTypes;
    UpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
    try
      First;
      while not EOF do
      begin
        if UpdateStatus = usInserted then
          RevertRecord
        else
        begin
          RevertRecord;
          Next;
        end;
      end;
    finally
      UpdateRecordTypes := CurUpdateTypes;
      First;
      FUpdatesPending := False;
      EnableControls;
    end;
  end;
end;

function TIBCustomDataSet.GetFieldPosition(AliasName: string): integer;
var i: integer;
    Prepared: boolean;
begin
  Result := 0;
  Prepared := FInternalPrepared;
  if not Prepared then
    InternalPrepare;
  try
    for i := 0 to Length(FAliasNameList) - 1 do
      if FAliasNameList[i] = AliasName then
      begin
        Result := i + 1;
        Exit
      end;
  finally
    if not Prepared then
      InternalUnPrepare;
  end;
end;

procedure TIBCustomDataSet.ActivateConnection;
begin
  if not Assigned(Database) then
    IBError(ibxeDatabaseNotAssigned, [nil]);
  if not Assigned(Transaction) then
    IBError(ibxeTransactionNotAssigned, [nil]);
  if not Database.Connected then Database.Open;
end;

function TIBCustomDataSet.ActivateTransaction: Boolean;
begin
  Result := False;
  if AllowAutoActivateTransaction or (csDesigning in ComponentState) then
  begin
    if not Assigned(Transaction) then
      IBError(ibxeTransactionNotAssigned, [nil]);
    if not Transaction.Active then
    begin
      Result := True;
      Transaction.StartTransaction;
      FDidActivate := True;
    end;
  end;
end;

procedure TIBCustomDataSet.DeactivateTransaction;
var
  i: Integer;
begin
  if not Assigned(Transaction) then
    IBError(ibxeTransactionNotAssigned, [nil]);
  with Transaction do
  begin
    for i := 0 to SQLObjectCount - 1 do
    begin
      if (SQLObjects[i] <> nil) and ((SQLObjects[i]).owner is TDataSet) then
      begin
        if TDataSet(SQLObjects[i].owner).Active then
        begin
          FDidActivate := False;
          exit;
        end;
      end;
    end;
  end;
  FInternalPrepared := False;
  if Transaction.InTransaction then
    Transaction.Commit;
  FDidActivate := False;
end;

procedure TIBCustomDataSet.CheckDatasetClosed;
begin
  if FOpen then
    IBError(ibxeDatasetOpen, [nil]);
end;

procedure TIBCustomDataSet.CheckDatasetOpen;
begin
  if not FOpen then
    IBError(ibxeDatasetClosed, [nil]);
end;

function TIBCustomDataSet.CreateParser: TSelectSQLParser;
begin
  Result := TSelectSQLParser.Create(self,FBaseSQLSelect);
  Result.OnSQLChanging := SQLChanging
end;

procedure TIBCustomDataSet.CheckNotUniDirectional;
begin
  if UniDirectional then
    IBError(ibxeDataSetUniDirectional, [nil]);
end;

procedure TIBCustomDataSet.AdjustRecordOnInsert(Buffer: Pointer);
begin
  with PRecordData(Buffer)^ do
    if (State = dsInsert) and (not Modified) then
    begin
      rdRecordNumber := FRecordCount;
      FCurrentRecord := FRecordCount;
    end;
end;

function TIBCustomDataSet.CanEdit: Boolean;
var
  Buff: PRecordData;
begin
  Buff := PRecordData(GetActiveBuf);
  result := (Trim(FQModify.SQL.Text) <> '') or
    (Assigned(FUpdateObject) and (Trim(FUpdateObject.GetSQL(ukModify).Text) <> '')) or
    ((Buff <> nil) and (Buff^.rdCachedUpdateStatus = cusInserted) and
      (FCachedUpdates));
end;

function TIBCustomDataSet.CanInsert: Boolean;
begin
  result := (Trim(FQInsert.SQL.Text) <> '') or
    (Assigned(FUpdateObject) and (Trim(FUpdateObject.GetSQL(ukInsert).Text) <> ''));
end;

function TIBCustomDataSet.CanDelete: Boolean;
begin
  if (Trim(FQDelete.SQL.Text) <> '') or
    (Assigned(FUpdateObject) and (Trim(FUpdateObject.GetSQL(ukDelete).Text) <> '')) then
    result := True
  else
    result := False;
end;

function TIBCustomDataSet.CanRefresh: Boolean;
begin
  result := (Trim(FQRefresh.SQL.Text) <> '') or
    (Assigned(FUpdateObject) and (Trim(FUpdateObject.RefreshSQL.Text) <> ''));
end;

procedure TIBCustomDataSet.CheckEditState;
begin
  case State of
    { Check all the wsEditMode types }
    dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter,
    dsNewValue, dsInternalCalc :
    begin
      if (State in [dsEdit]) and (not CanEdit) then
        IBError(ibxeCannotUpdate, [nil]);
      if (State in [dsInsert]) and (not CanInsert) then
        IBError(ibxeCannotInsert, [nil]);
    end;
  else
    IBError(ibxeNotEditing, [])
  end;
end;

procedure TIBCustomDataSet.ClearBlobCache;
var
  i: Integer;
begin
  for i := 0 to FBlobStreamList.Count - 1 do
  begin
    TIBBlobStream(FBlobStreamList[i]).Free;
    FBlobStreamList[i] := nil;
  end;
  FBlobStreamList.Pack;
end;

procedure TIBCustomDataSet.ClearArrayCache;
var
  i: Integer;
begin
  for i := 0 to FArrayList.Count - 1 do
  begin
    TIBArray(FArrayList[i]).Free;
    FArrayList[i] := nil;
  end;
  FArrayList.Pack;
end;

procedure TIBCustomDataSet.CopyRecordBuffer(Source, Dest: Pointer);
begin
  Move(Source^, Dest^, FRecordBufferSize);
end;

procedure TIBCustomDataSet.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  if Active then
    Active := False;
  InternalUnPrepare;
  if Assigned(FBeforeDatabaseDisconnect) then
    FBeforeDatabaseDisconnect(Sender);
end;

procedure TIBCustomDataSet.DoAfterDatabaseDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDatabaseDisconnect) then
    FAfterDatabaseDisconnect(Sender);
end;

procedure TIBCustomDataSet.DoDatabaseFree(Sender: TObject);
begin
  if Assigned(FDatabaseFree) then
    FDatabaseFree(Sender);
end;

procedure TIBCustomDataSet.DoBeforeTransactionEnd(Sender: TObject;
  Action: TTransactionAction);
begin
  FCloseAction := Action;
  FInTransactionEnd := true;
  try
    if Active then
      Active := False;
  finally
    FInTransactionEnd := false;
  end;
  if FQSelect <> nil then
    FQSelect.FreeHandle;
  if FQDelete <> nil then
    FQDelete.FreeHandle;
  if FQInsert <> nil then
    FQInsert.FreeHandle;
  if FQModify <> nil then
    FQModify.FreeHandle;
  if FQRefresh <> nil then
    FQRefresh.FreeHandle;
  InternalUnPrepare;
  if Assigned(FBeforeTransactionEnd) then
    FBeforeTransactionEnd(Sender);
end;

procedure TIBCustomDataSet.DoAfterTransactionEnd(Sender: TObject);
begin
  if Assigned(FAfterTransactionEnd) then
    FAfterTransactionEnd(Sender);
end;

procedure TIBCustomDataSet.DoTransactionFree(Sender: TObject);
begin
  if Assigned(FTransactionFree) then
    FTransactionFree(Sender);
end;

procedure TIBCustomDataSet.DoDeleteReturning(QryResults: IResults);
begin
  if assigned(FOnDeleteReturning) then
     OnDeleteReturning(self,QryResults);
end;

procedure TIBCustomDataSet.InitModelBuffer(Qry: TIBSQL; Buffer: PChar);
var i, j: Integer;
    FieldsLoaded: integer;
    p: PRecordData;
    colMetadata: IColumnMetaData;
begin
  p := PRecordData(Buffer);
  { Get record information }
  p^.rdBookmarkFlag := bfCurrent;
  p^.rdFieldCount := Qry.FieldCount;
  p^.rdRecordNumber := -1;
  p^.rdUpdateStatus := usUnmodified;
  p^.rdCachedUpdateStatus := cusUnmodified;
  p^.rdSavedOffset := $FFFFFFFF;

  { Load up the fields }
  FieldsLoaded := FQSelect.MetaData.Count;
  j := 1;
  for i := 0 to Qry.MetaData.Count - 1 do
  begin
    if (Qry = FQSelect) then
      j := i + 1
    else
    begin
      if FieldsLoaded = 0 then
        break;
      j := FQSelect.FieldIndex[Qry[i].Name] + 1;
      if j < 1 then
        continue
      else
        Dec(FieldsLoaded);
    end;
    if j > 0 then
    begin
      colMetadata := Qry.MetaData[i];
      with p^.rdFields[j], FFieldColumns^[j] do
      begin
        fdDataType := colMetadata.GetSQLType;
        if fdDataType = SQL_BLOB then
          fdDataScale := 0
        else
          fdDataScale := colMetadata.getScale;
        fdNullable := colMetadata.getIsNullable;
        fdIsNull := true;
        fdDataSize := colMetadata.GetSize;
        fdDataLength := 0;
        fdCodePage := CP_NONE;

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
        SQL_TEXT,
        SQL_BLOB:
          fdCodePage := colMetadata.getCodePage;
        SQL_DEC16,
        SQL_DEC34,
        SQL_DEC_FIXED,
        SQL_INT128:
          fdDataSize := sizeof(tBCD);
        end;
        fdDataOfs := FRecordSize;
        Inc(FRecordSize, fdDataSize);
      end;
    end;
  end;
end;

{Update Buffer Fields from Query Results}

procedure TIBCustomDataSet.UpdateRecordFromQuery(QryResults: IResults;
  Buffer: PChar);
var i, j: integer;
    pda: PArrayDataArray;
    pbd: PBlobDataArray;
begin
  { Make sure blob cache is empty }
  pbd := PBlobDataArray(Buffer + FBlobCacheOffset);
  pda := PArrayDataArray(Buffer + FArrayCacheOffset);
  for i := 0 to BlobFieldCount - 1 do
    pbd^[i] := nil;
  for i := 0 to ArrayFieldCount - 1 do
    pda^[i] := nil;

  for i := 0 to QryResults.Count - 1 do
  begin
    j := GetFieldPosition(QryResults[i].GetAliasName);
    if j > 0 then
    begin
      ColumnDataToBuffer(QryResults,i,j,Buffer);
      FBufferUpdatedOnQryReturn := true;
    end;
  end;
end;


{Move column data returned from query to row buffer}

procedure TIBCustomDataSet.ColumnDataToBuffer(QryResults: IResults;
               ColumnIndex, FieldIndex: integer; Buffer: PChar);
var
  LocalData: PByte;
  BufPtr: PByte;
  ColData: ISQLData;
begin
  LocalData := nil;
  with PRecordData(Buffer)^.rdFields[FieldIndex], FFieldColumns^[FieldIndex] do
  begin
    QryResults.GetData(ColumnIndex,fdIsNull,fdDataLength,LocalData);
    BufPtr := PByte(Buffer + fdDataOfs);
    if not fdIsNull then
    begin
      ColData := QryResults[ColumnIndex];
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
            ColData.GetAsTime(Timestamp, dstOffset,TimeZoneID, DefaultTZDate);
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
            Move(LocalData^, BufPtr^, fdDataLength)
          else
            Move(LocalData^, BufPtr^, fdDataSize)
        end;
      end; {case}
    end
    else {Null column}
    if fdDataType = SQL_VARYING then
      FillChar(BufPtr^,fdDataLength,0)
    else
      FillChar(BufPtr^,fdDataSize,0);
  end;
end;

function TIBCustomDataSet.GetMasterDetailDelay: integer;
begin
  Result := FDataLink.DelayTimerValue;
end;

{ Read the record from FQSelect.Current into the record buffer
  Then write the buffer to in memory cache }
procedure TIBCustomDataSet.FetchCurrentRecordToBuffer(Qry: TIBSQL;
  RecordNumber: Integer; Buffer: PChar);
var
  pbd: PBlobDataArray;
  pda: PArrayDataArray;
  i, j: Integer;
  FieldsLoaded: Integer;
  p: PRecordData;
begin
  if RecordNumber = -1 then
  begin
    InitModelBuffer(Qry,Buffer);
    Exit;
  end;
  p := PRecordData(Buffer);
  { Make sure blob cache is empty }
  pbd := PBlobDataArray(Buffer + FBlobCacheOffset);
  pda := PArrayDataArray(Buffer + FArrayCacheOffset);
  for i := 0 to BlobFieldCount - 1 do
    pbd^[i] := nil;
  for i := 0 to ArrayFieldCount - 1 do
    pda^[i] := nil;

  { Get record information }
  p^.rdBookmarkFlag := bfCurrent;
  p^.rdFieldCount := Qry.FieldCount;
  p^.rdRecordNumber := RecordNumber;
  p^.rdUpdateStatus := usUnmodified;
  p^.rdCachedUpdateStatus := cusUnmodified;
  p^.rdSavedOffset := $FFFFFFFF;

  { Load up the fields }
  FieldsLoaded := FQSelect.MetaData.Count;
  j := 1;
  for i := 0 to Qry.FieldCount - 1 do
  begin
    if (Qry = FQSelect) then
      j := i + 1
    else
    begin
      if FieldsLoaded = 0 then
        break;
      j := FQSelect.FieldIndex[Qry[i].Name] + 1;
      if j < 1 then
        continue
      else
        Dec(FieldsLoaded);
    end;
    with FQSelect.MetaData[j - 1] do
      if GetAliasname = 'IBX_INTERNAL_DBKEY' then {do not localize}
      begin
        if (GetSize <= 8) then
          p^.rdDBKey := PIBDBKEY(Qry[i].AsPointer)^;
        continue;
      end;
    if j > 0 then
      ColumnDataToBuffer(Qry.Current,i,j,Buffer);
  end;
  WriteRecordCache(RecordNumber, Buffer);
end;

function TIBCustomDataSet.GetActiveBuf: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        result := nil
      else
        result := ActiveBuffer;
    dsEdit, dsInsert:
      result := ActiveBuffer;
    dsCalcFields:
      result := CalcBuffer;
    dsFilter:
      result := FFilterBuffer;
    dsNewValue:
      result := ActiveBuffer;
    dsOldValue:
      if (PRecordData(ActiveBuffer)^.rdRecordNumber =
        PRecordData(FOldBuffer)^.rdRecordNumber) then
        result := FOldBuffer
      else
        result := ActiveBuffer;
  else if not FOpen then
    result := nil
  else
    result := ActiveBuffer;
  end;
end;

function TIBCustomDataSet.CachedUpdateStatus: TCachedUpdateStatus;
begin
  if Active then
    result := PRecordData(GetActiveBuf)^.rdCachedUpdateStatus
  else
    result := cusUnmodified;
end;

function TIBCustomDataSet.GetDatabase: TIBDatabase;
begin
  result := FBase.Database;
end;

function TIBCustomDataSet.GetDeleteSQL: TStrings;
begin
  result := FQDelete.SQL;
end;

function TIBCustomDataSet.GetInsertSQL: TStrings;
begin
  result := FQInsert.SQL;
end;

function TIBCustomDataSet.GetSQLParams: ISQLParams;
begin
  if not FInternalPrepared then
    InternalPrepare;
  result := FQSelect.Params;
end;

function TIBCustomDataSet.GetRefreshSQL: TStrings;
begin
  result := FQRefresh.SQL;
end;

function TIBCustomDataSet.GetSelectSQL: TStrings;
begin
  result := FQSelect.SQL;
end;

function TIBCustomDataSet.GetStatementType: TIBSQLStatementTypes;
begin
  result := FQSelect.SQLStatementType;
end;

function TIBCustomDataSet.GetModifySQL: TStrings;
begin
  result := FQModify.SQL;
end;

function TIBCustomDataSet.GetTransaction: TIBTransaction;
begin
  result := FBase.Transaction;
end;

procedure TIBCustomDataSet.InternalDeleteRecord(Qry: TIBSQL; Buff: Pointer);
begin
  if (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    FUpdateObject.Apply(ukDelete,Buff)
  else
  begin
    SetInternalSQLParams(FQDelete.Params, Buff);
    FQDelete.ExecQuery;
    if (FQDelete.FieldCount > 0)  then
      DoDeleteReturning(FQDelete.Current);
  end;
  with PRecordData(Buff)^ do
  begin
    rdUpdateStatus := usDeleted;
    rdCachedUpdateStatus := cusUnmodified;
  end;
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
end;

function TIBCustomDataSet.InternalLocate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  keyFieldList: TList;
  CurBookmark: TBookmark;
  fieldValue: Variant;
  lookupValues: array of variant;
  i, fieldCount: Integer;
  fieldValueAsString: string;
  lookupValueAsString: string;
begin
  keyFieldList := TList.Create;
  try
    GetFieldList(keyFieldList, KeyFields);
    fieldCount := keyFieldList.Count;
    CurBookmark := Bookmark;
    result := false;
    SetLength(lookupValues, fieldCount);
    if not EOF then
    begin
      for i := 0 to fieldCount - 1 do  {expand key values into lookupValues array}
      begin
        if VarIsArray(KeyValues) then
          lookupValues[i] := KeyValues[i]
        else
        if i > 0 then
          lookupValues[i] := NULL
        else
          lookupValues[0] := KeyValues;

        {convert to upper case is case insensitive search}
        if (TField(keyFieldList[i]).DataType = ftString) and
           not VarIsNull(lookupValues[i]) and (loCaseInsensitive in Options) then
            lookupValues[i] := UpperCase(lookupValues[i]);
      end;
    end;
    while not result and not EOF do   {search for a matching record}
    begin
      i := 0;
      result := true;
      while result and (i < fieldCount) do
      {see if all of the key fields matches}
      begin
        fieldValue := TField(keyFieldList[i]).Value;
        result := not (VarIsNull(fieldValue) xor VarIsNull(lookupValues[i]));
        if result and not VarIsNull(fieldValue) then
        begin
          try
            if TField(keyFieldList[i]).DataType = ftString then
            begin
              {strings need special handling because of the locate options that
               apply to them}
              fieldValueAsString := TField(keyFieldList[i]).AsString;
              lookupValueAsString := lookupValues[i];
              if (loCaseInsensitive in Options) then
                fieldValueAsString := UpperCase(fieldValueAsString);

              if (loPartialKey in Options) then
                result := result and (Pos(lookupValueAsString, fieldValueAsString) = 1)
              else
                result := result and (fieldValueAsString = lookupValueAsString);
            end
            else
              result := result and (lookupValues[i] =
                             VarAsType(fieldValue, VarType(lookupValues[i])));
          except on EVariantError do
            result := False;
          end;
        end;
        Inc(i);
      end;
      if not result then
          Next;
    end;
    if not result then
      Bookmark := CurBookmark
    else
      CursorPosChanged;
  finally
    keyFieldList.Free;
    SetLength(lookupValues,0)
  end;
end;

procedure TIBCustomDataSet.InternalPostRecord(Qry: TIBSQL; Buff: Pointer);
var
  i, j, k, arr: Integer;
  pbd: PBlobDataArray;
  pda: PArrayDataArray;
begin
  pbd := PBlobDataArray(PChar(Buff) + FBlobCacheOffset);
  pda := PArrayDataArray(PChar(Buff) + FArrayCacheOffset);
  j := 0; arr := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].IsBlob then
    begin
      k := FMappedFieldPosition[Fields[i].FieldNo -1];
      if pbd^[j] <> nil then
      begin
        pbd^[j].Finalize;
        PISC_QUAD(
          PChar(Buff) + FFieldColumns^[k].fdDataOfs)^ :=
          pbd^[j].BlobID;
        PRecordData(Buff)^.rdFields[k].fdIsNull := pbd^[j].Size = 0;
      end
      else
      begin
        PRecordData(Buff)^.rdFields[k].fdIsNull := true;
        with PISC_QUAD(PChar(Buff) + FFieldColumns^[k].fdDataOfs)^ do
        begin
          gds_quad_high := 0;
          gds_quad_low := 0;
        end;
      end;
      Inc(j);
    end
    else
    if Fields[i] is TIBArrayField then
    begin
      if pda^[arr] <> nil then
      begin
        k := FMappedFieldPosition[Fields[i].FieldNo -1];
        PISC_QUAD(
          PChar(Buff) + FFieldColumns^[k].fdDataOfs)^ :=  pda^[arr].ArrayIntf.GetArrayID;
        PRecordData(Buff)^.rdFields[k].fdIsNull := pda^[arr].ArrayIntf.IsEmpty;
      end;
      Inc(arr);
    end;
  FBufferUpdatedOnQryReturn := false;
  if Assigned(FUpdateObject) then
  begin
    if (Qry = FQDelete) then
      FUpdateObject.Apply(ukDelete,Buff)
    else if (Qry = FQInsert) then
      FUpdateObject.Apply(ukInsert,Buff)
    else
      FUpdateObject.Apply(ukModify,Buff);
    FUpdateObject.GetRowsAffected(FSelectCount, FInsertCount, FUpdateCount, FDeleteCount);
  end
  else begin
    SetInternalSQLParams(Qry.Params, Buff);
    Qry.ExecQuery;
    Qry.Statement.GetRowsAffected(FSelectCount, FInsertCount, FUpdateCount, FDeleteCount);
    if Qry.FieldCount > 0 then {Has RETURNING Clause}
      UpdateRecordFromQuery(Qry.Current,Buff);
  end;
  PRecordData(Buff)^.rdUpdateStatus := usUnmodified;
  PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
  SetModified(False);
  WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  if (FForcedRefresh or (FNeedsRefresh and not FBufferUpdatedOnQryReturn)) and CanRefresh then
    InternalRefreshRow;
end;

procedure TIBCustomDataSet.InternalRefreshRow;
var
  Buff: PChar;
  ofs: DWORD;
  Qry: TIBSQL;
begin
  FBase.SetCursor;
  try
    Buff := GetActiveBuf;
    if CanRefresh then
    begin
      if Buff <> nil then
      begin
        if (Assigned(FUpdateObject) and (Trim(FUpdateObject.RefreshSQL.Text) <> '')) then
        begin
          Qry := TIBSQL.Create(self);
          Qry.Database := Database;
          Qry.Transaction := Transaction;
          Qry.GoToFirstRecordOnExecute := False;
          Qry.SQL.Text := FUpdateObject.RefreshSQL.Text;
        end
        else
          Qry := FQRefresh;
        SetInternalSQLParams(Qry.Params, Buff);
        Qry.ExecQuery;
        try
          if (Qry.SQLStatementType = SQLExecProcedure) or Qry.Next then
          begin
            ofs := PRecordData(Buff)^.rdSavedOffset;
            FetchCurrentRecordToBuffer(Qry,
                                       PRecordData(Buff)^.rdRecordNumber,
                                       Buff);
            if FCachedUpdates and (ofs <> $FFFFFFFF) then
            begin
              PRecordData(Buff)^.rdSavedOffset := ofs;
              WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
              SaveOldBuffer(Buff);
            end;
          end;
        finally
          Qry.Close;
        end;
        if Qry <> FQRefresh then
          Qry.Free;
      end
    end
    else
      IBError(ibxeCannotRefresh, [nil]);
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalRevertRecord(RecordNumber: Integer);
var
  NewBuffer, OldBuffer: PRecordData;

begin
  NewBuffer := nil;
  OldBuffer := nil;
  NewBuffer := PRecordData(AllocRecordBuffer);
  OldBuffer := PRecordData(AllocRecordBuffer);
  try
    ReadRecordCache(RecordNumber, PChar(NewBuffer), False);
    ReadRecordCache(RecordNumber, PChar(OldBuffer), True);
    case NewBuffer^.rdCachedUpdateStatus of
      cusInserted:
      begin
        NewBuffer^.rdCachedUpdateStatus := cusUninserted;
        Inc(FDeletedRecords);
      end;
      cusModified,
      cusDeleted:
      begin
        if (NewBuffer^.rdCachedUpdateStatus = cusDeleted) then
          Dec(FDeletedRecords);
        CopyRecordBuffer(OldBuffer, NewBuffer);
      end;
    end;

    if State in dsEditModes then
      Cancel;

    WriteRecordCache(RecordNumber, PChar(NewBuffer));

    if (NewBuffer^.rdCachedUpdateStatus = cusUninserted ) then
      ReSync([]);
  finally
    FreeRecordBuffer(PChar(NewBuffer));
    FreeRecordBuffer(PChar(OldBuffer));
  end;
end;

{ A visible record is one that is not truly deleted,
  and it is also listed in the FUpdateRecordTypes set }

function TIBCustomDataSet.IsVisible(Buffer: PChar): Boolean;
begin
  result := True;
  if not (State = dsOldValue) then
    result :=
      (PRecordData(Buffer)^.rdCachedUpdateStatus in FUpdateRecordTypes) and
      (not ((PRecordData(Buffer)^.rdCachedUpdateStatus = cusUnmodified) and
        (PRecordData(Buffer)^.rdUpdateStatus = usDeleted)));
end;


function TIBCustomDataSet.LocateNext(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DisableControls;
  try
    result := InternalLocate(KeyFields, KeyValues, Options);
  finally
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.InternalPrepare;
begin
  if FInternalPrepared then
    Exit;
  FBase.SetCursor;
  try
    ActivateConnection;
    ActivateTransaction;
    FBase.CheckDatabase;
    FBase.CheckTransaction;
    if HasParser and not FParser.NotaSelectStmt and (FParser.SQLText <> FQSelect.SQL.Text) then
    begin
      FQSelect.OnSQLChanged := nil; {Do not react to change}
      try
        FQSelect.SQL.Text := FParser.SQLText;
      finally
        FQSelect.OnSQLChanged := SQLChanged;
      end;
    end;
//   writeln( FQSelect.SQL.Text);
    if FQSelect.SQL.Text <> '' then
    begin
      if not FQSelect.Prepared then
      begin
        FQSelect.GenerateParamNames := FGenerateParamNames;
        FQSelect.ParamCheck := ParamCheck;
        FQSelect.Prepare;
      end;
      FQDelete.GenerateParamNames := FGenerateParamNames;
      if (Trim(FQDelete.SQL.Text) <> '') and (not FQDelete.Prepared) then
        FQDelete.Prepare;
      FQInsert.GenerateParamNames := FGenerateParamNames;
      if (Trim(FQInsert.SQL.Text) <> '') and (not FQInsert.Prepared) then
        FQInsert.Prepare;
      FQRefresh.GenerateParamNames := FGenerateParamNames;
      if (Trim(FQRefresh.SQL.Text) <> '') and (not FQRefresh.Prepared) then
        FQRefresh.Prepare;
      FQModify.GenerateParamNames := FGenerateParamNames;
      if (Trim(FQModify.SQL.Text) <> '') and (not FQModify.Prepared) then
        FQModify.Prepare;
      FInternalPrepared := True;
      InternalInitFieldDefs;
    end else
      IBError(ibxeEmptyQuery, [nil]);
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.RecordModified(Value: Boolean);
begin
  SetModified(Value);
end;

procedure TIBCustomDataSet.RevertRecord;
var
  Buff: PRecordData;
begin
  if FCachedUpdates and FUpdatesPending then
  begin
    Buff := PRecordData(GetActiveBuf);
    InternalRevertRecord(Buff^.rdRecordNumber);
    ReadRecordCache(Buff^.rdRecordNumber, PChar(Buff), False);
    DataEvent(deRecordChange, 0);
  end;
end;

procedure TIBCustomDataSet.SaveOldBuffer(Buffer: PChar);
var
  OldBuffer: Pointer;
  procedure CopyOldBuffer;
  begin
    CopyRecordBuffer(Buffer, OldBuffer);
    if BlobFieldCount > 0 then
      FillChar(PChar(OldBuffer)[FBlobCacheOffset],
               BlobFieldCount * SizeOf(TIBBlobStream) + ArrayFieldCount * SizeOf(IArray),
               0);
  end;

begin
  if (Buffer <> nil) and (PRecordData(Buffer)^.rdRecordNumber >= 0) then
  begin
    OldBuffer := AllocRecordBuffer;
    try
      if (PRecordData(Buffer)^.rdSavedOffset = $FFFFFFFF) then
      begin
        PRecordData(Buffer)^.rdSavedOffset := AdjustPosition(FOldBufferCache, 0,
                                                             FILE_END);
        CopyOldBuffer;
          WriteCache(FOldBufferCache, 0, FILE_CURRENT, OldBuffer);
          WriteCache(FBufferCache, PRecordData(Buffer)^.rdRecordNumber * FRecordBufferSize,
                     FILE_BEGIN, Buffer);
      end
      else begin
        CopyOldBuffer;
        WriteCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, FILE_BEGIN,
                   OldBuffer);
      end;
    finally
      FreeRecordBuffer(PChar(OldBuffer));
    end;
  end;
end;

procedure TIBCustomDataSet.SetBufferChunks(Value: Integer);
begin
  if (Value <= 0) then
    FBufferChunks := BufferCacheSize
  else
    FBufferChunks := Value;
end;

procedure TIBCustomDataSet.SetDatabase(Value: TIBDatabase);
begin
  if (csLoading in ComponentState) or (FBase.Database <> Value) then
  begin
    CheckDatasetClosed;
    InternalUnPrepare;
    FBase.Database := Value;
    FQDelete.Database := Value;
    FQInsert.Database := Value;
    FQRefresh.Database := Value;
    FQSelect.Database := Value;
    FQModify.Database := Value;
    FDatabaseInfo.Database := Value;
    FGeneratorField.Database := Value;
  end;
end;

procedure TIBCustomDataSet.SetDeleteSQL(Value: TStrings);
begin
  if FQDelete.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQDelete.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetInsertSQL(Value: TStrings);
begin
  if FQInsert.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQInsert.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetInternalSQLParams(Params: ISQLParams; Buffer: Pointer);
var
  i, j, arr: Integer;
  cr, data: PByte;
  fn: string;
  st: RawByteString;
  OldBuffer: Pointer;
  Param: ISQLParam;
  pda: PArrayDataArray;
begin
  if (Buffer = nil) then
    IBError(ibxeBufferNotSet, [nil]);
  if (not FInternalPrepared) then
    InternalPrepare;
  OldBuffer := nil;
  try
    pda := PArrayDataArray(PChar(Buffer) + FArrayCacheOffset);
    arr := 0;
    for i := 0 to Params.GetCount - 1 do
    begin
      Param := Params[i];
      fn := Param.Name;
      if (Pos('OLD_', fn) = 1) then {mbcs ok}
      begin
        fn := Copy(fn, 5, Length(fn));
        if not Assigned(OldBuffer) then
        begin
          OldBuffer := AllocRecordBuffer;
          ReadRecordCache(PRecordData(Buffer)^.rdRecordNumber, OldBuffer, True);
        end;
        cr := OldBuffer;
      end
      else if (Pos('NEW_', fn) = 1) then {mbcs ok}
           begin
             fn := Copy(fn, 5, Length(fn));
             cr := Buffer;
            end
            else
             cr := Buffer;
      j := FQSelect.FieldIndex[fn] + 1;
      if (j > 0) then
        with PRecordData(cr)^,rdFields[j], FFieldColumns^[j] do
        begin
          if Param.name = 'IBX_INTERNAL_DBKEY' then {do not localize}
          begin
            PIBDBKey(Param.AsPointer)^ := rdDBKey;
            continue;
          end;
          if fdIsNull then
            Param.IsNull := True
          else begin
            Param.IsNull := False;
            data := cr + fdDataOfs;
            case fdDataType of
              SQL_TEXT, SQL_VARYING:
              begin
                SetString(st, PAnsiChar(data), fdDataLength);
                SetCodePage(st,fdCodePage,false);
                Param.AsString := st;
              end;
            SQL_FLOAT, SQL_DOUBLE, SQL_D_FLOAT:
              Param.AsDouble := PDouble(data)^;
            SQL_SHORT, SQL_LONG:
            begin
              if fdDataScale = 0 then
                Param.AsLong := PLong(data)^
              else
              if fdDataScale >= (-4) then
                Param.AsCurrency := PCurrency(data)^
              else
                Param.AsDouble := PDouble(data)^;
            end;
            SQL_INT64:
            begin
              if fdDataScale = 0 then
                Param.AsInt64 := PInt64(data)^
              else
              if fdDataScale >= (-4) then
                Param.AsCurrency := PCurrency(data)^
              else
                Param.AsDouble := PDouble(data)^;
            end;
            SQL_BLOB, SQL_QUAD:
              Param.AsQuad := PISC_QUAD(data)^;
            SQL_ARRAY:
              begin
                if pda[arr] = nil then
                  Param.AsQuad := PISC_QUAD(data)^
                else
                  Param.AsArray := pda[arr].ArrayIntf;
                Inc(arr);
              end;
            SQL_TYPE_DATE,
            SQL_TYPE_TIME,
            SQL_TIMESTAMP:
            {This is an IBX native format and not the TDataset approach. See also SetFieldData}
              Param.AsDateTime := PDateTime(data)^;
            SQL_TIMESTAMP_TZ_EX,
            SQL_TIMESTAMP_TZ:
              with PIBBufferedDateTimeWithTimeZone(data)^ do
                Param.SetAsDateTime(Timestamp,TimeZoneID);
            SQL_TIME_TZ_EX,
            SQL_TIME_TZ:
              with PIBBufferedDateTimeWithTimeZone(data)^ do
                Param.SetAsTime(Timestamp,DefaultTZDate,TimeZoneID);
            SQL_BOOLEAN:
              Param.AsBoolean := PWordBool(data)^;
            SQL_DEC16,
            SQL_DEC34,
            SQL_DEC_FIXED,
            SQL_INT128:
              Param.AsBCD := pBCD(data)^;
            else
              IBError(ibxeUnknownSQLType,[fdDataType]);
          end;
        end;
      end;
    end;
  finally
    if (OldBuffer <> nil) then
      FreeRecordBuffer(PChar(OldBuffer));
  end;
end;

procedure TIBCustomDataSet.SetRefreshSQL(Value: TStrings);
begin
  if FQRefresh.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQRefresh.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetSelectSQL(Value: TStrings);
begin
  if FQSelect.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQSelect.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetModifySQL(Value: TStrings);
begin
  if FQModify.SQL.Text <> Value.Text then
  begin
    Disconnect;
    FQModify.SQL.Assign(Value);
  end;
end;

procedure TIBCustomDataSet.SetTransaction(Value: TIBTransaction);
begin
  if (FBase.Transaction <> Value) then
  begin
    CheckDatasetClosed;
    FBase.Transaction := Value;
    FQDelete.Transaction := Value;
    FQInsert.Transaction := Value;
    FQRefresh.Transaction := Value;
    FQSelect.Transaction := Value;
    FQModify.Transaction := Value;
    FGeneratorField.Transaction := Value;
  end;
end;

procedure TIBCustomDataSet.SetUniDirectional(Value: Boolean);
begin
  CheckDatasetClosed;
  FUniDirectional := Value;
  inherited SetUniDirectional(Value);
end;

procedure TIBCustomDataSet.SetUpdateRecordTypes(Value: TIBUpdateRecordTypes);
begin
  FUpdateRecordTypes := Value;
  if Active then
    First;
end;

procedure TIBCustomDataSet.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.RegisterIBLink(Sender: TIBControlLink);
begin
  if FIBLinks.IndexOf(Sender) = -1 then
  begin
    FIBLinks.Add(Sender);
    if Active then
    begin
      Active := false;
      Active := true;
    end;
  end;
end;


procedure TIBCustomDataSet.SQLChanging(Sender: TObject);
begin
  Active := false;
{  if FOpen then
    InternalClose;}
  if FInternalPrepared then
    InternalUnPrepare;
  FieldDefs.Clear;
  FieldDefs.Updated := false;
end;

procedure TIBCustomDataSet.SQLChanged(Sender: TObject);
begin
  FBaseSQLSelect.assign(FQSelect.SQL);
end;

{ I can "undelete" uninserted records (make them "inserted" again).
  I can "undelete" cached deleted (the deletion hasn't yet occurred) }
procedure TIBCustomDataSet.Undelete;
var
  Buff: PRecordData;
begin
  CheckActive;
  Buff := PRecordData(GetActiveBuf);
  with Buff^ do
  begin
    if rdCachedUpdateStatus = cusUninserted then
    begin
      rdCachedUpdateStatus := cusInserted;
      Dec(FDeletedRecords);
    end
    else if (rdUpdateStatus = usDeleted) and
            (rdCachedUpdateStatus = cusDeleted) then
    begin
      rdCachedUpdateStatus := cusUnmodified;
      rdUpdateStatus := usUnmodified;
      Dec(FDeletedRecords);
    end;
    WriteRecordCache(rdRecordNumber, PChar(Buff));
  end;
end;

procedure TIBCustomDataSet.UnRegisterIBLink(Sender: TIBControlLink);
begin
  FIBLinks.Remove(Sender);
end;

function TIBCustomDataSet.UpdateStatus: TUpdateStatus;
begin
  if Active then
    if GetActiveBuf <> nil then
      result := PRecordData(GetActiveBuf)^.rdUpdateStatus
    else
      result := usUnmodified
  else
    result := usUnmodified;
end;

function TIBCustomDataSet.IsSequenced: Boolean;
begin
  Result := Assigned( FQSelect ) and FQSelect.EOF;
end;

function TIBCustomDataSet.ParamByName(ParamName: String): ISQLParam;
begin
  Result := FindParam(ParamName);
  if Result = nil then
    IBError(ibxeParameterNameNotFound,[ParamName]);
end;

function TIBCustomDataSet.FindParam(ParamName: String): ISQLParam;
begin
  ActivateConnection;
  ActivateTransaction;
  if not FInternalPrepared then
    InternalPrepare;
  Result := Params.ByName(ParamName);
end;

function TIBCustomDataSet.GetRowsAffected(var SelectCount, InsertCount,
  UpdateCount, DeleteCount: integer): boolean;
begin
  Result := Active;
  SelectCount := FSelectCount;
  InsertCount := FInsertCount;
  UpdateCount := FUpdateCount;
  DeleteCount := FDeleteCount;
end;

function TIBCustomDataSet.GetPerfStatistics(var stats: TPerfCounters): boolean;
begin
  Result := EnableStatistics and (FQSelect.Statement <> nil) and
     FQSelect.Statement.GetPerfStatistics(stats);
end;

{Beware: the parameter FCache is used as an identifier to determine which
 cache is being operated on and is not referenced in the computation.
 The result is an adjusted offset into the identified cache, either the
 Buffer Cache or the old Buffer Cache.}

function TIBCustomDataSet.AdjustPosition(FCache: PChar; Offset: DWORD;
                                        Origin: Integer): DWORD;
var
  OldCacheSize: Integer;
begin
  if (FCache = FBufferCache) then
  begin
    case Origin of
      FILE_BEGIN:    FBPos := Offset;
      FILE_CURRENT:  FBPos := FBPos + Offset;
      FILE_END:      FBPos := DWORD(FBEnd) + Offset;
    end;
    OldCacheSize := FCacheSize;
    while (FBPos >= DWORD(FCacheSize)) do
      Inc(FCacheSize, FBufferChunkSize);
    if FCacheSize > OldCacheSize then
      IBAlloc(FBufferCache, FCacheSize, FCacheSize);
    result := FBPos;
  end
  else begin
    case Origin of
      FILE_BEGIN:    FOBPos := Offset;
      FILE_CURRENT:  FOBPos := FOBPos + Offset;
      FILE_END:      FOBPos := DWORD(FOBEnd) + Offset;
    end;
    OldCacheSize := FOldCacheSize;
    while (FBPos >= DWORD(FOldCacheSize)) do
      Inc(FOldCacheSize, FBufferChunkSize);
    if FOldCacheSize > OldCacheSize then
      IBAlloc(FOldBufferCache, FOldCacheSize, FOldCacheSize);
    result := FOBPos;
  end;
end;

procedure TIBCustomDataSet.ReadCache(FCache: PChar; Offset: DWORD; Origin: Integer;
                                    Buffer: PChar);
var
  pCache: PChar;
  AdjustedOffset: DWORD;
  bOld: Boolean;
begin
  bOld := (FCache = FOldBufferCache);
  AdjustedOffset := AdjustPosition(FCache, Offset, Origin);
  if not bOld then
    pCache := FBufferCache + AdjustedOffset
  else
    pCache := FOldBufferCache + AdjustedOffset;
  Move(pCache^, Buffer^, DWORD(FRecordBufferSize));
  AdjustPosition(FCache, FRecordBufferSize, FILE_CURRENT);
end;

procedure TIBCustomDataSet.ReadRecordCache(RecordNumber: Integer; Buffer: PChar;
                                          ReadOldBuffer: Boolean);
begin
  if RecordNumber = -1 then
    Exit; {nothing to do}
  if FUniDirectional then
    RecordNumber := RecordNumber mod UniCache;
  if (ReadOldBuffer) then
  begin
    ReadRecordCache(RecordNumber, Buffer, False);
    if FCachedUpdates and
      (PRecordData(Buffer)^.rdSavedOffset <> $FFFFFFFF) then
      ReadCache(FOldBufferCache, PRecordData(Buffer)^.rdSavedOffset, FILE_BEGIN,
                Buffer)
    else
      if ReadOldBuffer and
         (PRecordData(FOldBuffer)^.rdRecordNumber = RecordNumber) then
         CopyRecordBuffer( FOldBuffer, Buffer )
  end
  else
    ReadCache(FBufferCache, RecordNumber * FRecordBufferSize, FILE_BEGIN, Buffer);
end;

procedure TIBCustomDataSet.WriteCache(FCache: PChar; Offset: DWORD; Origin: Integer;
                                     Buffer: PChar);
var
  pCache: PChar;
  AdjustedOffset: DWORD;
  bOld: Boolean;
  dwEnd: DWORD;
begin
  bOld := (FCache = FOldBufferCache);
  AdjustedOffset := AdjustPosition(FCache, Offset, Origin);
  if not bOld then
    pCache := FBufferCache + AdjustedOffset
  else
    pCache := FOldBufferCache + AdjustedOffset;
  Move(Buffer^, pCache^, FRecordBufferSize);
  dwEnd := AdjustPosition(FCache, FRecordBufferSize, FILE_CURRENT);
  if not bOld then
  begin
    if (dwEnd > FBEnd) then
      FBEnd := dwEnd;
  end
  else begin
    if (dwEnd > FOBEnd) then
      FOBEnd := dwEnd;
  end;
end;

procedure TIBCustomDataSet.WriteRecordCache(RecordNumber: Integer; Buffer: PChar);
begin
  if RecordNumber >= 0 then
  begin
    if FUniDirectional then
      RecordNumber := RecordNumber mod UniCache;
    WriteCache(FBufferCache, RecordNumber * FRecordBufferSize, FILE_BEGIN, Buffer);
  end;
end;

function TIBCustomDataSet.AllocRecordBuffer: PChar;
begin
  result := nil;
  IBAlloc(result, FRecordBufferSize, FRecordBufferSize);
  Move(FModelBuffer^, result^, FRecordBufferSize);
end;

function TIBCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  pb: PBlobDataArray;
  fs: TIBBlobStream;
  Buff: PChar;
  bTr, bDB: Boolean;
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  Buff := GetActiveBuf;
  if Buff = nil then
  begin
    fs := TIBBlobStream.Create;
    fs.Mode := bmReadWrite;
    fs.Database := Database;
    fs.Transaction := Transaction;
    fs.SetField(Field);
    FBlobStreamList.Add(Pointer(fs));
    result := TIBDSBlobStream.Create(Field, fs, Mode);
    exit;
  end;
  pb := PBlobDataArray(Buff + FBlobCacheOffset);
  if pb^[Field.Offset] = nil then
  begin
    AdjustRecordOnInsert(Buff);
    pb^[Field.Offset] := TIBBlobStream.Create;
    fs := pb^[Field.Offset];
    FBlobStreamList.Add(Pointer(fs));
    fs.Mode := bmReadWrite;
    fs.Database := Database;
    fs.Transaction := Transaction;
    fs.SetField(Field);
    fs.BlobID :=
      PISC_QUAD(@Buff[FFieldColumns^[FMappedFieldPosition[Field.FieldNo - 1]].fdDataOfs])^;
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
    end;
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Pointer(Buff));
  end else
    fs := pb^[Field.Offset];
  result := TIBDSBlobStream.Create(Field, fs, Mode);
end;

function TIBCustomDataSet.GetArray(Field: TIBArrayField): IArray;
var Buff: PChar;
    pda: PArrayDataArray;
    bTr, bDB: Boolean;
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  Buff := GetActiveBuf;
  if Buff = nil then
    Result := Database.Attachment.CreateArray(Transaction.TransactionIntf,
                              Field.FRelationName,Field.FieldName)
  else
  begin
    pda := PArrayDataArray(Buff + FArrayCacheOffset);
    if pda^[Field.FCacheOffset] = nil then
    begin
      AdjustRecordOnInsert(Buff);
      if Field.IsNull then
        Result := Database.Attachment.CreateArray(Transaction.TransactionIntf,
                                Field.FRelationName,Field.FieldName)
      else
        Result := Database.Attachment.OpenArray(Transaction.TransactionIntf,
                            Field.FRelationName,Field.FieldName,Field.ArrayID);
      pda^[Field.FCacheOffset] := TIBArray.Create(Field,Result);
      FArrayList.Add(pda^[Field.FCacheOffset]);
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
      end;
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Pointer(Buff));
    end
    else
      Result := pda^[Field.FCacheOffset].ArrayIntf;
  end;
end;

procedure TIBCustomDataSet.SetArrayIntf(AnArray: IArray; Field: TIBArrayField);
var Buff: PChar;
    pda: PArrayDataArray;
    MappedFieldPos: integer;
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  Buff := GetActiveBuf;
  if Buff <> nil then
  with PRecordData(Buff)^ do
  begin
    AdjustRecordOnInsert(Buff);
    MappedFieldPos := FMappedFieldPosition[Field.FieldNo - 1];
    if (MappedFieldPos > 0) and
       (MappedFieldPos <= rdFieldCount) then
    begin
      rdFields[MappedFieldPos].fdIsNull := AnArray = nil;
      pda := PArrayDataArray(Buff + FArrayCacheOffset);
      if pda^[Field.FCacheOffset] = nil then
      begin
        if not rdFields[MappedFieldPos].fdIsNull then
        begin
          pda^[Field.FCacheOffset] := TIBArray.Create(Field,AnArray);
          FArrayList.Add(pda^[Field.FCacheOffset]);
        end
      end
      else
        pda^[Field.FCacheOffset].FArray := AnArray;
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Pointer(Buff));
    end;
  end;
end;

function TIBCustomDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  CMPLess = -1;
  CMPEql  =  0;
  CMPGtr  =  1;
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, CMPLess),
                                                   (CMPGtr, CMPEql));
begin
  result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];

  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := CMPLess
    else
    if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := CMPGtr
    else
      Result := CMPEql;
  end;
end;

procedure TIBCustomDataSet.DoBeforeDelete;
var
  Buff: PRecordData;
begin
  if not CanDelete then
    IBError(ibxeCannotDelete, [nil]);
  Buff := PRecordData(GetActiveBuf);
  if FCachedUpdates and
    (Buff^.rdCachedUpdateStatus in [cusUnmodified]) then
    SaveOldBuffer(PChar(Buff));
  inherited DoBeforeDelete;
end;

procedure TIBCustomDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  FBase.DoAfterDelete(self);
  InternalAutoCommit;
end;

procedure TIBCustomDataSet.DoBeforeEdit;
var
  Buff: PRecordData;
begin
  Buff := PRecordData(GetActiveBuf);
  if not(CanEdit or (FQModify.SQL.Count <> 0) or
    (FCachedUpdates and Assigned(FOnUpdateRecord))) then
    IBError(ibxeCannotUpdate, [nil]);
  if FCachedUpdates and (Buff^.rdCachedUpdateStatus in [cusUnmodified, cusInserted]) then
    SaveOldBuffer(PChar(Buff));
  CopyRecordBuffer(GetActiveBuf, FOldBuffer);
  inherited DoBeforeEdit;
end;

procedure TIBCustomDataSet.DoAfterEdit;
begin
  inherited DoAfterEdit;
  FBase.DoAfterEdit(self);
end;

procedure TIBCustomDataSet.DoBeforeInsert;
begin
  if not CanInsert then
    IBError(ibxeCannotInsert, [nil]);
  inherited DoBeforeInsert;
end;

procedure TIBCustomDataSet.DoAfterInsert;
begin
  if GeneratorField.ApplyOnEvent = gaeOnNewRecord then
    GeneratorField.Apply;
  inherited DoAfterInsert;
  FBase.DoAfterInsert(self);
end;

procedure TIBCustomDataSet.DoBeforeClose;
begin
  inherited DoBeforeClose;
  if FInTransactionEnd and (FCloseAction = TARollback) then
     Exit;
  if State in [dsInsert,dsEdit] then
  begin
    if DataSetCloseAction = dcSaveChanges then
      Post;
      {Note this can fail with an exception e.g. due to
       database validation error. In which case the dataset remains open }
  end;
  if FCachedUpdates and FUpdatesPending and (DataSetCloseAction = dcSaveChanges) then
    ApplyUpdates;
end;

procedure TIBCustomDataSet.DoBeforePost;
begin
  inherited DoBeforePost;
  if (State = dsInsert) and
     (GeneratorField.ApplyOnEvent = gaeOnPostRecord) then
     GeneratorField.Apply
end;

procedure TIBCustomDataSet.DoAfterPost;
begin
  inherited DoAfterPost;
  FBase.DoAfterPost(self);
  InternalAutoCommit;
end;

procedure TIBCustomDataSet.FetchAll;
var
  CurBookmark: TBookmark;
begin
  FBase.SetCursor;
 try
    if FQSelect.EOF or not FQSelect.Open then
      exit;
    DisableControls;
    try
      CurBookmark := Bookmark;
      Last;
      Bookmark := CurBookmark;
    finally
      EnableControls;
    end;
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TIBCustomDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(PRecordData(Buffer)^.rdRecordNumber, Data^, BookmarkSize);
end;

function TIBCustomDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  result := PRecordData(Buffer)^.rdBookmarkFlag;
end;

function TIBCustomDataSet.GetCanModify: Boolean;
begin
  result := (FQInsert.SQL.Text <> '') or
    (FQModify.SQL.Text <> '') or
    (FQDelete.SQL.Text <> '') or
    (Assigned(FUpdateObject));
end;

function TIBCustomDataSet.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    ReadRecordCache(PRecordData(ActiveBuffer)^.rdRecordNumber, Buffer, False);
    result := True;
  end
  else
    result := False;
end;

function TIBCustomDataSet.GetDataSource: TDataSource;
begin
  if FDataLink = nil then
    result := nil
  else
    result := FDataLink.DataSource;
end;

 function TIBCustomDataSet.GetDBAliasName(FieldNo: integer): string;
begin
  Result := FAliasNameMap[FieldNo-1]
end;

 function TIBCustomDataSet.GetFieldDefFromAlias(aliasName: string): TFieldDef;
 var
   i: integer;
 begin
   Result := nil;
   for i := 0 to Length(FAliasNameMap) - 1 do
       if FAliasNameMap[i] = aliasName then
       begin
         Result := FieldDefs[i];
         Exit
       end;
 end;

function TIBCustomDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

function TIBCustomDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  result := GetFieldData(FieldByNumber(FieldNo), buffer);
end;

function TIBCustomDataSet.InternalGetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Buff: PChar;
  Data: PByte;
  CurrentRecord: PRecordData;
begin
  result := False;
  Buff := GetActiveBuf;
  if (Buff = nil) or
     (not IsVisible(Buff)) or not assigned(Field.DataSet) then
    exit;
  { The intention here is to stuff the buffer with the data for the
   referenced field for the current record }
  CurrentRecord := PRecordData(Buff);
  if (Field.FieldNo < 0) then
  begin
    Inc(Buff, FRecordSize + Field.Offset);
    result := Boolean(Buff[0]);
    if result and (Buffer <> nil) then
      Move(Buff[1], Buffer^, Field.DataSize);
  end
  else
  if (FMappedFieldPosition[Field.FieldNo - 1] > 0) and
     (FMappedFieldPosition[Field.FieldNo - 1] <= CurrentRecord^.rdFieldCount) then
  with CurrentRecord^.rdFields[FMappedFieldPosition[Field.FieldNo - 1]],
                         FFieldColumns^[FMappedFieldPosition[Field.FieldNo - 1]] do
  begin
    result := not fdIsNull;
    if result and (Buffer <> nil) then
      begin
        Data := PByte(Buff) + fdDataOfs;
        if (fdDataType = SQL_VARYING) or (fdDataType = SQL_TEXT) then
        begin
          if fdDataLength <= Field.DataSize then
          begin
            Move(Data^, Buffer^, fdDataLength);
            PChar(Buffer)[fdDataLength] := #0;
          end
          else
            IBError(ibxeFieldSizeError,[Field.FieldName])
        end
        else
        if fdDataLength <= Field.DataSize then
          Move(Data^, Buffer^, Field.DataSize)
        else
          IBError(ibxeFieldSizeError,[Field.FieldName,Field.DataSize,fdDataLength])
      end;
  end;
end;

{ GetRecNo and SetRecNo both operate off of 1-based indexes as
 opposed to 0-based indexes.
 This is because we want LastRecordNumber/RecordCount = 1 }

function TIBCustomDataSet.GetRecNo: Integer;
begin
  if GetActiveBuf = nil then
    result := 0
  else
    result := PRecordData(GetActiveBuf)^.rdRecordNumber + 1;
end;

function TIBCustomDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  Result := grOK;
  if Filtered and Assigned(OnFilterRecord) then
  begin
    Accept := False;
    SaveState := SetTempState(dsFilter);
    while not Accept do
    begin
      Result := InternalGetRecord(Buffer, GetMode, DoCheck);
      if Result <> grOK then
        break;
      FFilterBuffer := Buffer;
      try
        Accept := True;
        OnFilterRecord(Self, Accept);
        if not Accept and (GetMode = gmCurrent) then
          GetMode := gmPrior;
      except
//        FBase.HandleException(Self);
      end;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TIBCustomDataSet.InternalGetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  result := grError;
  case GetMode of
    gmCurrent: begin
      if (FCurrentRecord >= 0) then begin
        if FCurrentRecord < FRecordCount then
          ReadRecordCache(FCurrentRecord, Buffer, False)
        else begin
          while (not FQSelect.EOF) and FQSelect.Next  and
                (FCurrentRecord >= FRecordCount) do begin
            FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
            Inc(FRecordCount);
          end;
          FCurrentRecord := FRecordCount - 1;
          if (FCurrentRecord >= 0) then
            ReadRecordCache(FCurrentRecord, Buffer, False);
        end;
        result := grOk;
      end else
        result := grBOF;
    end;
    gmNext: begin
      result := grOk;
      if FCurrentRecord = FRecordCount then
        result := grEOF
      else if FCurrentRecord = FRecordCount - 1 then begin
        if (not FQSelect.EOF) then begin
          FQSelect.Next;
          Inc(FCurrentRecord);
        end;
        if (FQSelect.EOF) then begin
          result := grEOF;
        end else begin
          Inc(FRecordCount);
          FetchCurrentRecordToBuffer(FQSelect, FCurrentRecord, Buffer);
        end;
      end else if (FCurrentRecord < FRecordCount) then begin
        Inc(FCurrentRecord);
        ReadRecordCache(FCurrentRecord, Buffer, False);
      end;
    end;
    else { gmPrior }
    begin
      if (FCurrentRecord = 0) then begin
        Dec(FCurrentRecord);
        result := grBOF;
      end else if (FCurrentRecord > 0) and
                  (FCurrentRecord <= FRecordCount) then begin
        Dec(FCurrentRecord);
        ReadRecordCache(FCurrentRecord, Buffer, False);
        result := grOk;
      end else if (FCurrentRecord = -1) then
        result := grBOF;
    end;
  end;
  if result = grOk then
    result := AdjustCurrentRecord(Buffer, GetMode);
  if result = grOk then with PRecordData(Buffer)^ do begin
    rdBookmarkFlag := bfCurrent;
    GetCalcFields(Buffer);
  end else if (result = grEOF) then begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
  end else if (result = grBOF) then begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfBOF;
  end else if (result = grError) then begin
    CopyRecordBuffer(FModelBuffer, Buffer);
    PRecordData(Buffer)^.rdBookmarkFlag := bfEOF;
  end;;
end;

function TIBCustomDataSet.GetRecordCount: Integer;
begin
  result := FRecordCount - FDeletedRecords;
end;

function TIBCustomDataSet.GetRecordSize: Word;
begin
  result := FRecordBufferSize;
end;

procedure TIBCustomDataSet.InternalAutoCommit;
begin
  with Transaction do
    if InTransaction and (FAutoCommit = acCommitRetaining) then
    begin
      if CachedUpdates then ApplyUpdates;
      CommitRetaining;
    end;
end;

procedure TIBCustomDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  CheckEditState;
  begin
     { When adding records, we *always* append.
       Insertion is just too costly }
    AdjustRecordOnInsert(Buffer);
    with PRecordData(Buffer)^ do
    begin
      rdUpdateStatus := usInserted;
      rdCachedUpdateStatus := cusInserted;
    end;
    if not CachedUpdates then
      InternalPostRecord(FQInsert, Buffer)
    else begin
      WriteRecordCache(FCurrentRecord, Buffer);
      FUpdatesPending := True;
    end;
    Inc(FRecordCount);
    InternalSetToRecord(Buffer);
  end
end;

procedure TIBCustomDataSet.InternalCancel;
var
  Buff: PChar;
  CurRec: Integer;
  pda: PArrayDataArray;
  pbd: PBlobDataArray;
  i: integer;
begin
  inherited InternalCancel;
  Buff := GetActiveBuf;
  if Buff <> nil then
  begin
    pda := PArrayDataArray(Buff + FArrayCacheOffset);
    pbd := PBlobDataArray(Buff + FBlobCacheOffset);
    for i := 0 to ArrayFieldCount - 1 do
      pda^[i].ArrayIntf.CancelChanges;
    CurRec := FCurrentRecord;
    AdjustRecordOnInsert(Buff);
    if (State = dsEdit) then begin
      CopyRecordBuffer(FOldBuffer, Buff);
      for i := 0 to BlobFieldCount - 1 do
        pbd^[i] := nil;
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
    end else begin
      CopyRecordBuffer(FModelBuffer, Buff);
      PRecordData(Buff)^.rdUpdateStatus := usDeleted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusUnmodified;
      PRecordData(Buff)^.rdBookmarkFlag := bfEOF;
      FCurrentRecord := CurRec;
    end;
  end;
end;


procedure TIBCustomDataSet.InternalClose;
begin
  if FDidActivate then
    DeactivateTransaction;
  FQSelect.Close;
  ResetBufferCache;
  FreeRecordBuffer(FModelBuffer);
  FreeRecordBuffer(FOldBuffer);
  FCurrentRecord := -1;
  FOpen := False;
  FRecordSize := 0;
  FreeMem(FFieldColumns);
  FFieldColumns := nil;
  BindFields(False);
  ResetParser;
  if DefaultFields then DestroyFields;
end;

procedure TIBCustomDataSet.InternalDelete;
var
  Buff: PChar;
begin
  FBase.SetCursor;
  try
    Buff := GetActiveBuf;
    if CanDelete then
    begin
      if not CachedUpdates then
        InternalDeleteRecord(FQDelete, Buff)
      else
      begin
        with PRecordData(Buff)^ do
        begin
          if rdCachedUpdateStatus = cusInserted then
            rdCachedUpdateStatus := cusUninserted
          else begin
            rdUpdateStatus := usDeleted;
            rdCachedUpdateStatus := cusDeleted;
          end;
        end;
        WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
      end;
      Inc(FDeletedRecords);
      FUpdatesPending := True;
    end else
      IBError(ibxeCannotDelete, [nil]);
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalFirst;
begin
  FCurrentRecord := -1;
  if Unidirectional then GetNextRecord;
end;

procedure TIBCustomDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurrentRecord := PInteger(Bookmark)^;
end;

procedure TIBCustomDataSet.InternalHandleException;
begin
  FBase.HandleException(Self)
end;

procedure TIBCustomDataSet.InternalInitFieldDefs;
begin
  if not InternalPrepared then
  begin
    InternalPrepare;
    exit;
  end;
   FieldDefsFromQuery(FQSelect);
 end;

procedure TIBCustomDataSet.FieldDefsFromQuery(SourceQuery: TIBSQL);
const
  DefaultSQL = 'Select F.RDB$COMPUTED_BLR, ' + {do not localize}
               'F.RDB$DEFAULT_VALUE,  R.RDB$FIELD_NAME ' + {do not localize}
               'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
               'where R.RDB$RELATION_NAME = :RELATION ' +  {do not localize}
               'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+ {do not localize}
               'and ((not F.RDB$COMPUTED_BLR is NULL) or ' + {do not localize}
               '     (not F.RDB$DEFAULT_VALUE is NULL)) '; {do not localize}

  DefaultSQLODS12 = 'Select F.RDB$COMPUTED_BLR, ' + {do not localize}
               'F.RDB$DEFAULT_VALUE, R.RDB$FIELD_NAME, R.RDB$IDENTITY_TYPE ' + {do not localize}
               'from RDB$RELATION_FIELDS R, RDB$FIELDS F ' + {do not localize}
               'where R.RDB$RELATION_NAME = :RELATION ' +  {do not localize}
               'and R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+ {do not localize}
               'and ((not F.RDB$COMPUTED_BLR is NULL) or ' + {do not localize}
               '     (not F.RDB$DEFAULT_VALUE is NULL) or ' + {do not localize}
               '     ( not R.RDB$IDENTITY_TYPE is NULL))' ; {do not localize}

var
  FieldType: TFieldType;
  FieldSize: Word;
  FieldDataSize: integer;
  CharSetSize: integer;
  CharSetName: RawByteString;
  FieldCodePage: TSystemCodePage;
  FieldNullable : Boolean;
  i, FieldPosition, FieldPrecision: Integer;
  FieldAliasName, DBAliasName: string;
  aRelationName, FieldName: string;
  Query : TIBSQL;
  FieldIndex: Integer;
  FRelationNodes : TRelationNode;
  aArrayDimensions: integer;
  aArrayBounds: TArrayBounds;
  ArrayMetaData: IArrayMetaData;
  FieldHasTimeZone: boolean;

  function Add_Node(Relation, Field : String) : TRelationNode;
  var
    FField : TFieldNode;
  begin
    if FRelationNodes.RelationName = '' then
      Result := FRelationNodes
    else
    begin
      Result := TRelationNode.Create;
      Result.NextRelation := FRelationNodes;
    end;
    Result.RelationName := Relation;
    FRelationNodes := Result;
    Query.Params[0].AsString := Relation;
    Query.ExecQuery;
    while not Query.Eof do
    begin
      FField := TFieldNode.Create;
      FField.FieldName := TrimRight(Query.Fields[2].AsString);
      FField.DEFAULT_VALUE := not Query.Fields[1].IsNull;
      FField.COMPUTED_BLR := not Query.Fields[0].IsNull;
      FField.IDENTITY_COLUMN := (Query.FieldCount > 3) and not Query.Fields[3].IsNull;
      FField.NextField := Result.FieldNodes;
      Result.FieldNodes := FField;
      Query.Next;
    end;
    Query.Close;
  end;

  function Has_COMPUTED_BLR(Relation, Field : String) : Boolean;
  var
    FRelation : TRelationNode;
    FField : TFieldNode;
  begin
    FRelation := FRelationNodes;
    while Assigned(FRelation) and
         (FRelation.RelationName <> Relation) do
      FRelation := FRelation.NextRelation;
    if not Assigned(FRelation) then
      FRelation := Add_Node(Relation, Field);
    Result := false;
    FField := FRelation.FieldNodes;
    while Assigned(FField) do
      if FField.FieldName = Field then
      begin
        Result := Ffield.COMPUTED_BLR;
        Exit;
      end
      else
        FField := Ffield.NextField;
  end;

  function Has_DEFAULT_VALUE(Relation, Field : String) : Boolean;
  var
    FRelation : TRelationNode;
    FField : TFieldNode;
  begin
    FRelation := FRelationNodes;
    while Assigned(FRelation) and
         (FRelation.RelationName <> Relation) do
      FRelation := FRelation.NextRelation;
    if not Assigned(FRelation) then
      FRelation := Add_Node(Relation, Field);
    Result := false;
    FField := FRelation.FieldNodes;
    while Assigned(FField) do
      if FField.FieldName = Field then
      begin
        Result := Ffield.DEFAULT_VALUE;
        Exit;
      end
      else
        FField := Ffield.NextField;
  end;

  function Is_IDENTITY_COLUMN(Relation, Field : String) : Boolean;
  var
    FRelation : TRelationNode;
    FField : TFieldNode;
  begin
    FRelation := FRelationNodes;
    while Assigned(FRelation) and
         (FRelation.RelationName <> Relation) do
      FRelation := FRelation.NextRelation;
    if not Assigned(FRelation) then
      FRelation := Add_Node(Relation, Field);
    Result := false;
    FField := FRelation.FieldNodes;
    while Assigned(FField) do
      if FField.FieldName = Field then
      begin
        Result := Ffield.IDENTITY_COLUMN;
        Exit;
      end
      else
        FField := Ffield.NextField;
  end;

  Procedure FreeNodes;
  var
    FRelation : TRelationNode;
    FField : TFieldNode;
  begin
    while Assigned(FRelationNodes) do
    begin
      While Assigned(FRelationNodes.FieldNodes) do
      begin
        FField := FRelationNodes.FieldNodes.NextField;
        FRelationNodes.FieldNodes.Free;
        FRelationNodes.FieldNodes := FField;
      end;
      FRelation := FRelationNodes.NextRelation;
      FRelationNodes.Free;
      FRelationNodes := FRelation;
    end;
  end;

begin
  FRelationNodes := TRelationNode.Create;
  FNeedsRefresh := False;
  if not Database.InternalTransaction.InTransaction then
    Database.InternalTransaction.StartTransaction;
  Query := TIBSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    FieldDefs.BeginUpdate;
    FieldDefs.Clear;
    FieldIndex := 0;
    if (Length(FMappedFieldPosition) < SourceQuery.MetaData.Count) then
      SetLength(FMappedFieldPosition, SourceQuery.MetaData.Count);
    if FDatabaseInfo.ODSMajorVersion >= 12 then
      Query.SQL.Text := DefaultSQLODS12
    else
      Query.SQL.Text := DefaultSQL;
    Query.Prepare;
    SetLength(FAliasNameMap, SourceQuery.MetaData.Count);
    SetLength(FAliasNameList, SourceQuery.MetaData.Count);
    for i := 0 to SourceQuery.MetaData.GetCount - 1 do
      with SourceQuery.MetaData[i] do
      begin
        { Get the field name }
        FieldAliasName := GetName;
        DBAliasName := GetAliasname;
        aRelationName := getRelationName;
        FieldName := getSQLName;
        FAliasNameList[i] := DBAliasName;
        FieldSize := 0;
        FieldDataSize := GetSize;
        FieldPrecision := 0;
        FieldNullable := IsNullable;
        FieldHasTimeZone := false;
        CharSetSize := 0;
        CharSetName := '';
        FieldCodePage := CP_NONE;
        aArrayDimensions := 0;
        SetLength(aArrayBounds,0);
        case SQLType of
          { All VARCHAR's must be converted to strings before recording
           their values }
          SQL_VARYING, SQL_TEXT:
          begin
            if not Database.Attachment.CharSetWidth(getCharSetID,CharSetSize) then
              CharSetSize := 1;
            CharSetName := Database.Attachment.GetCharsetName(getCharSetID);
            Database.Attachment.CharSetID2CodePage(getCharSetID,FieldCodePage);
            FieldSize := FieldDataSize div CharSetSize;
            FieldType := ftString;
          end;
          { All Doubles/Floats should be cast to doubles }
          SQL_DOUBLE, SQL_FLOAT:
            FieldType := ftFloat;
          SQL_SHORT:
          begin
            if (getScale = 0) then
              FieldType := ftSmallInt
            else begin
              FieldType := ftBCD;
              FieldPrecision := 4;
              FieldSize := -getScale;
            end;
          end;
          SQL_LONG:
          begin
            if (getScale = 0) then
              FieldType := ftInteger
            else if (getScale >= (-4)) then
            begin
              FieldType := ftBCD;
              FieldPrecision := 9;
              FieldSize := -getScale;
            end
            else
            if Database.SQLDialect = 1 then
              FieldType := ftFloat
            else
            if (FieldCount > i) and (Fields[i] is TFloatField) then
              FieldType := ftFloat
            else
            begin
              FieldType := ftBCD;
              FieldPrecision := 9;
              FieldSize := -getScale;
            end;
          end;

          SQL_INT64:
          begin
            if (getScale = 0) then
              FieldType := ftLargeInt
            else if (getScale >= (-4)) then
            begin
              FieldType := ftBCD;
              FieldPrecision := 18;
              FieldSize := -getScale;
            end
            else
              FieldType := ftFloat;
          end;
          SQL_TIMESTAMP: FieldType := ftDateTime;
          SQL_TYPE_TIME: FieldType := ftTime;
          SQL_TYPE_DATE: FieldType := ftDate;
          SQL_TIMESTAMP_TZ,
          SQL_TIMESTAMP_TZ_EX:
            begin
              FieldType := ftDateTime;
              FieldHasTimeZone := true;
            end;
          SQL_TIME_TZ,
          SQL_TIME_TZ_EX:
            begin
              FieldType := ftTime;
              FieldHasTimeZone := true;
            end;
          SQL_BLOB:
          begin
            FieldSize := sizeof (TISC_QUAD);
            if (getSubtype = 1) then
            begin
              if not Database.Attachment.CharSetWidth(getCharSetID,CharSetSize) then
                CharSetSize := 1;
              CharSetName := Database.Attachment.GetCharsetName(getCharSetID);
              Database.Attachment.CharSetID2CodePage(getCharSetID,FieldCodePage);
              FieldType := ftMemo;
            end
            else
              FieldType := ftBlob;
          end;
          SQL_ARRAY:
          begin
            FieldSize := sizeof (TISC_QUAD);
            FieldType := ftArray;
            ArrayMetaData := GetArrayMetaData;
            if ArrayMetaData <> nil then
            begin
              aArrayDimensions := ArrayMetaData.GetDimensions;
              aArrayBounds := ArrayMetaData.GetBounds;
            end;
          end;
          SQL_BOOLEAN:
             FieldType:= ftBoolean;

          SQL_DEC16:
            begin
              FieldType := ftFmtBCD;
              FieldPrecision := 16;
              FieldSize := 4; {For conversions from currency type}
            end;

          SQL_DEC34:
          begin
            FieldType := ftFmtBCD;
            FieldPrecision := 34;
            FieldSize := 4; {For conversions from currency type}
          end;

          SQL_DEC_FIXED,
          SQL_INT128:
          begin
            FieldType := ftFmtBCD;
            FieldPrecision := 38;
            FieldSize := -getScale; {For conversions from currency type}
          end;

          else
            FieldType := ftUnknown;
        end;
        FieldPosition := i + 1;
        if (FieldType <> ftUnknown) and (FieldAliasName <> 'IBX_INTERNAL_DBKEY') then {do not localize}
        begin
          FMappedFieldPosition[FieldIndex] := FieldPosition;
          Inc(FieldIndex);
          with TIBFieldDef.Create(FieldDefs,'',FieldType,0,False,FieldDefs.Count+1) do
          begin
            Name := FieldAliasName;
            FAliasNameMap[FieldNo-1] := DBAliasName;
            Size := FieldSize;
            DataSize := FieldDataSize;
            Precision := FieldPrecision;
            Required := not FieldNullable;
            RelationName := aRelationName;
            InternalCalcField := False;
            CharacterSetSize := CharSetSize;
            CharacterSetName := CharSetName;
            CodePage := FieldCodePage;
            ArrayDimensions := aArrayDimensions;
            ArrayBounds := aArrayBounds;
            HasTimezone := FieldHasTimeZone;
            if (FieldName <> '') and (RelationName <> '') then
            begin
              IdentityColumn := Is_IDENTITY_COLUMN(RelationName, FieldName);
              if Has_COMPUTED_BLR(RelationName, FieldName) then
              begin
                Attributes := [faReadOnly];
                InternalCalcField := True;
                FNeedsRefresh := True;
              end
              else
              begin
                if Has_DEFAULT_VALUE(RelationName, FieldName) then
                begin
                  if not FieldNullable then
                    Attributes := [faRequired];
                end
                else
                  FNeedsRefresh := True;
              end;
            end;
          end;
        end;
      end;
  finally
    Query.free;
    FreeNodes;
    Database.InternalTransaction.Commit;
    FieldDefs.EndUpdate;
    FieldDefs.Updated := true;
  end;
end;

procedure TIBCustomDataSet.InternalInitRecord(Buffer: PChar);
begin
  CopyRecordBuffer(FModelBuffer, Buffer);
end;

procedure TIBCustomDataSet.InternalLast;
var
  Buffer: PChar;
begin
  if (FQSelect.EOF) then
    FCurrentRecord := FRecordCount
  else begin
    Buffer := AllocRecordBuffer;
    try
      while FQSelect.Next do
      begin
        FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
        Inc(FRecordCount);
      end;
      FCurrentRecord := FRecordCount;
    finally
      FreeRecordBuffer(Buffer);
    end;
  end;
end;

procedure TIBCustomDataSet.InternalSetParamsFromCursor;
var
  i: Integer;
  cur_param: ISQLParam;
  cur_field: TField;
  s: TStream;
begin
  if FQSelect.SQL.Text = '' then
    IBError(ibxeEmptyQuery, [nil]);
  if not FInternalPrepared then
    InternalPrepare;
  if (SQLParams.GetCount > 0) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    for i := 0 to SQLParams.GetCount - 1 do
    begin
      cur_field := DataSource.DataSet.FindField(SQLParams[i].Name);
      if (cur_field <> nil) then
      begin
        cur_param := SQLParams[i];
        if (cur_field.IsNull) then
          cur_param.IsNull := True
        else
        case cur_field.DataType of
          ftString:
            cur_param.AsString := cur_field.AsString;
          ftBoolean:
            cur_param.AsBoolean := cur_field.AsBoolean;
          ftSmallint, ftWord:
            cur_param.AsShort := cur_field.AsInteger;
          ftInteger:
            cur_param.AsLong := cur_field.AsInteger;
          ftLargeInt:
            cur_param.AsInt64 := cur_field.AsLargeInt;
          ftFloat, ftCurrency:
           cur_param.AsDouble := cur_field.AsFloat;
          ftBCD:
            cur_param.AsCurrency := cur_field.AsCurrency;
          ftDate:
            cur_param.AsDate := cur_field.AsDateTime;
          ftTime:
            if (cur_field is TIBDateTimeField) and TIBDateTimeField(cur_field).HasTimeZone
              and (cur_param.GetSQLType = SQL_TIME_TZ) then
              cur_param.SetAsTime(cur_Field.asDateTime,DefaultTZDate,TIBDateTimeField(cur_field).TimeZoneID)
            else
              cur_param.AsTime := cur_field.AsDateTime;
          ftDateTime:
          begin
            if (cur_field is TIBDateTimeField) and TIBDateTimeField(cur_field).HasTimeZone
              and (cur_param.GetSQLType = SQL_TIMESTAMP_TZ) then
              cur_param.SetAsDateTime(cur_field.AsDateTime,TIBDateTimeField(cur_field).TimeZoneID)
            else
              cur_param.AsDateTime := cur_field.AsDateTime;
          end;
          ftBlob, ftMemo:
          begin
            s := nil;
            try
              s := DataSource.DataSet.
                     CreateBlobStream(cur_field, bmRead);
              cur_param.AsBlob := TIBDSBlobStream(s).FBlobStream.Blob;
            finally
              s.free;
            end;
          end;
          ftArray:
            cur_param.AsArray := TIBArrayField(cur_field).ArrayIntf;
          ftFmtBCD:
            cur_param.AsBCD := TFmtBCDField(cur_field).AsBCD;
          else
            IBError(ibxeNotSupported, [nil]);
        end;
      end;
    end;
  end;
end;

procedure TIBCustomDataSet.ReQuery;
begin
  FQSelect.Close;
  ClearBlobCache;
  FCurrentRecord := -1;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FBPos := 0;
  FOBPos := 0;
  FBEnd := 0;
  FOBEnd := 0;
  FQSelect.Close;
  FQSelect.ExecQuery;
  FOpen := FQSelect.Open;
  First;
end;

procedure TIBCustomDataSet.ResetBufferCache;
begin
  ClearBlobCache;
  ClearArrayCache;
  FRecordCount := 0;
  FDeletedRecords := 0;
  FBPos := 0;
  FOBPos := 0;
  FCacheSize := 0;
  FOldCacheSize := 0;
  FBEnd := 0;
  FOBEnd := 0;
  FreeMem(FBufferCache);
  FBufferCache := nil;
  FreeMem(FOldBufferCache);
  FOldBufferCache := nil;
end;

procedure TIBCustomDataSet.InternalOpen;

  function RecordDataLength(n: Integer): Long;
  begin
    result := SizeOf(TRecordData) + ((n - 1) * SizeOf(TFieldData));
  end;

begin
  FBase.SetCursor;
  try
    ActivateConnection;
    ActivateTransaction;
    if FQSelect.SQL.Text = '' then
      IBError(ibxeEmptyQuery, [nil]);
    if not FInternalPrepared then
      InternalPrepare;
    if FQSelect.Statement <> nil then
      FQSelect.Statement.EnableStatistics(FEnableStatistics);
   if FQSelect.SQLStatementType = SQLSelect then
   begin
      if DefaultFields then
        CreateFields;
      FArrayFieldCount := 0;
      BindFields(True);
      FCurrentRecord := -1;
      FQSelect.ExecQuery;
      FOpen := FQSelect.Open;

      { Initialize offsets, buffer sizes, etc...
        1. Initially FRecordSize is just the "RecordDataLength".
        2. Allocate a "model" buffer and do a dummy fetch
        3. After the dummy fetch, FRecordSize will be appropriately
           adjusted to reflect the additional "weight" of the field
           data.
        4. Set up the FCalcFieldsOffset, FBlobCacheOffset, FArrayCacheOffset and FRecordBufferSize.
        5. Now, with the BufferSize available, allocate memory for chunks of records
        6. Re-allocate the model buffer, accounting for the new
           FRecordBufferSize.
        7. Finally, calls to AllocRecordBuffer will work!.
       }
      {Step 1}
      FRecordSize := RecordDataLength(FQSelect.FieldCount);
      {Step 2, 3}
      GetMem(FFieldColumns,sizeof(TFieldColumns) * (FQSelect.FieldCount));
      IBAlloc(FModelBuffer, 0, FRecordSize);
      InitModelBuffer(FQSelect, FModelBuffer);
      {Step 4}
      FCalcFieldsOffset := FRecordSize;
      FBlobCacheOffset := FCalcFieldsOffset + CalcFieldsSize;
      FArrayCacheOffset := (FBlobCacheOffset + (BlobFieldCount * SizeOf(TIBBlobStream)));
      FRecordBufferSize := FArrayCacheOffset + (ArrayFieldCount * sizeof(IArray));
      {Step 5}
      if UniDirectional then
        FBufferChunkSize := FRecordBufferSize * UniCache
      else
        FBufferChunkSize := FRecordBufferSize * BufferChunks;
      IBAlloc(FBufferCache, FBufferChunkSize, FBufferChunkSize);
      if FCachedUpdates or (csReading in ComponentState) then
        IBAlloc(FOldBufferCache, FBufferChunkSize, FBufferChunkSize);
      FBPos := 0;
      FOBPos := 0;
      FBEnd := 0;
      FOBEnd := 0;
      FCacheSize := FBufferChunkSize;
      FOldCacheSize := FBufferChunkSize;
      {Step 6}
      IBAlloc(FModelBuffer, RecordDataLength(FQSelect.FieldCount),
                             FRecordBufferSize);
      {Step 7}
      FOldBuffer := AllocRecordBuffer;
    end
    else
      FQSelect.ExecQuery;
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalPost;
var
  Qry: TIBSQL;
  Buff: PChar;
  bInserting: Boolean;
begin
  FBase.SetCursor;
  try
    Buff := GetActiveBuf;
    CheckEditState;
    AdjustRecordOnInsert(Buff);
    if (State = dsInsert) then
    begin
      bInserting := True;
      Qry := FQInsert;
      PRecordData(Buff)^.rdUpdateStatus := usInserted;
      PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
      WriteRecordCache(FRecordCount, Buff);
      FCurrentRecord := FRecordCount;
    end
    else begin
      bInserting := False;
      Qry := FQModify;
      if PRecordData(Buff)^.rdCachedUpdateStatus = cusUnmodified then
      begin
        PRecordData(Buff)^.rdUpdateStatus := usModified;
        PRecordData(Buff)^.rdCachedUpdateStatus := cusModified;
      end
      else if PRecordData(Buff)^.
                    rdCachedUpdateStatus = cusUninserted then
            begin
              PRecordData(Buff)^.rdCachedUpdateStatus := cusInserted;
              Dec(FDeletedRecords);
            end;
    end;
    if (not CachedUpdates) then
      InternalPostRecord(Qry, Buff)
    else begin
      WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
      FUpdatesPending := True;
    end;
    if bInserting then
      Inc(FRecordCount);
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalRefresh;
begin
  inherited InternalRefresh;
  InternalRefreshRow;
end;

procedure TIBCustomDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@(PRecordData(Buffer)^.rdRecordNumber));
end;

function TIBCustomDataSet.IsCursorOpen: Boolean;
begin
  result := FOpen;
end;

procedure TIBCustomDataSet.Loaded;
begin
  if assigned(FQSelect) then
    FBaseSQLSelect.assign(FQSelect.SQL);
  inherited Loaded;
end;

procedure TIBCustomDataSet.Post;
var CancelPost: boolean;
begin
  CancelPost := false;
  if assigned(FOnValidatePost) then
    OnValidatePost(self,CancelPost);
  if CancelPost then
    Cancel
  else
   inherited Post;
end;

function TIBCustomDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
                                 Options: TLocateOptions): Boolean;
var
  CurBookmark: TBookmark;
begin
  DisableControls;
  try
    CurBookmark := Bookmark;
    First;
    result := InternalLocate(KeyFields, KeyValues, Options);
    if not result then
      Bookmark := CurBookmark;
  finally
    EnableControls;
  end;
end;

function TIBCustomDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
                                 const ResultFields: string): Variant;
var
  fl: TList;
  CurBookmark: TBookmark;
begin
  DisableControls;
  fl := TList.Create;
  CurBookmark := Bookmark;
  try
    First;
    if InternalLocate(KeyFields, KeyValues, []) then
    begin
      if (ResultFields <> '') then
        result := FieldValues[ResultFields]
      else
        result := NULL;
    end
    else
      result := Null;
  finally
    Bookmark := CurBookmark;
    fl.Free;
    EnableControls;
  end;
end;

procedure TIBCustomDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if Data <> nil then
    PRecordData(Buffer)^.rdRecordNumber := PInteger(Data)^;
end;

procedure TIBCustomDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordData(Buffer)^.rdBookmarkFlag := Value;
end;

procedure TIBCustomDataSet.SetCachedUpdates(Value: Boolean);
begin
  if not Value and FCachedUpdates then
    CancelUpdates;
  if (not (csReading in ComponentState)) and Value then
    CheckDatasetClosed;
  FCachedUpdates := Value;
end;

procedure TIBCustomDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    IBError(ibxeCircularReference, [nil]);
  if FDataLink <> nil then
    FDataLink.DataSource := Value;
end;

procedure TIBCustomDataSet.InternalSetFieldData(Field: TField; Buffer: Pointer);
var
  Buff, TmpBuff: PChar;
  MappedFieldPos: integer;
begin
  Buff := GetActiveBuf;
  if Field.FieldNo < 0 then
  begin
    TmpBuff := Buff + FRecordSize + Field.Offset;
    Boolean(TmpBuff[0]) := LongBool(Buffer);
    if Boolean(TmpBuff[0]) then
      Move(Buffer^, TmpBuff[1], Field.DataSize);
    WriteRecordCache(PRecordData(Buff)^.rdRecordNumber, Buff);
  end
  else begin
    CheckEditState;
    with PRecordData(Buff)^ do
    begin
      { If inserting, Adjust record position }
      AdjustRecordOnInsert(Buff);
      MappedFieldPos := FMappedFieldPosition[Field.FieldNo - 1];
      if (MappedFieldPos > 0) and
         (MappedFieldPos <= rdFieldCount) then
      with rdFields[MappedFieldPos], FFieldColumns^[MappedFieldPos] do
      begin
        Field.Validate(Buffer);
        if (Buffer = nil) or
           (Field is TIBStringField) and (PChar(Buffer)[0] = #0) then
          fdIsNull := True
        else
        begin
          if fdDataSize >= Field.DataSize then
            Move(Buffer^, Buff[fdDataOfs],fdDataSize)
          else
            IBError(ibxeDBBufferTooSmall,[fdDataSize,Field.FieldName,Field.DataSize]);

          if (fdDataType = SQL_TEXT) or (fdDataType = SQL_VARYING) then
            fdDataLength := StrLen(PChar(Buffer));
          fdIsNull := False;
          if rdUpdateStatus = usUnmodified then
          begin
            if CachedUpdates then
            begin
              FUpdatesPending := True;
              if State = dsInsert then
                rdCachedUpdateStatus := cusInserted
              else if State = dsEdit then
                rdCachedUpdateStatus := cusModified;
            end;

            if State = dsInsert then
              rdUpdateStatus := usInserted
            else
              rdUpdateStatus := usModified;
          end;
          WriteRecordCache(rdRecordNumber, Buff);
          SetModified(True);
        end;
      end;
    end;
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, PtrInt(Field));
end;

procedure TIBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value < 1) then
    Value := 1
  else if Value > FRecordCount then
  begin
    InternalLast;
    Value := Min(FRecordCount, Value);
  end;
  if (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TIBCustomDataSet.Disconnect;
begin
 Close;
 InternalUnPrepare;
end;

procedure TIBCustomDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if not CanModify then
    IBError(ibxeCannotUpdate, [nil])
  else
    FUpdateMode := Value;
end;


procedure TIBCustomDataSet.SetUpdateObject(Value: TIBDataSetUpdateObject);
begin
  if Value <> FUpdateObject then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      if Assigned(FUpdateObject.DataSet) and
        (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

function TIBCustomDataSet.ConstraintsStored: Boolean;
begin
  Result := Constraints.Count > 0;
end;

procedure TIBCustomDataSet.ClearCalcFields(Buffer: PChar);
begin
 FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TIBCustomDataSet.ClearIBLinks;
var i: integer;
begin
  for i := FIBLinks.Count - 1 downto 0 do
    TIBControlLink(FIBLinks[i]).IBDataSet := nil;
end;


procedure TIBCustomDataSet.InternalUnPrepare;
begin
  if FInternalPrepared then
  begin
    CheckDatasetClosed;
    if FDidActivate then
      DeactivateTransaction;
    FieldDefs.Clear;
    FieldDefs.Updated := false;
    FInternalPrepared := False;
    Setlength(FAliasNameList,0);
  end;
end;

procedure TIBCustomDataSet.InternalExecQuery;
var
  DidActivate: Boolean;
begin
  DidActivate := False;
  FBase.SetCursor;
  try
    ActivateConnection;
    DidActivate := ActivateTransaction;
    if FQSelect.SQL.Text = '' then
      IBError(ibxeEmptyQuery, [nil]);
    if not FInternalPrepared then
      InternalPrepare;
    if FQSelect.SQLStatementType = SQLSelect then
    begin
      IBError(ibxeIsASelectStatement, [nil]);
    end
    else
      FQSelect.ExecQuery;
  finally
    if DidActivate then
      DeactivateTransaction;
    FBase.RestoreCursor;
  end;
end;

function TIBCustomDataSet.GetSelectStmtIntf: IStatement;
begin
  Result := FQSelect.Statement;
end;

procedure TIBCustomDataSet.SetCaseSensitiveParameterNames(AValue: boolean);
begin
  if FCaseSensitiveParameterNames = AValue then Exit;
  FCaseSensitiveParameterNames := AValue;
  if assigned(FQSelect) then
    FQSelect.CaseSensitiveParameterNames := AValue;
end;

procedure TIBCustomDataSet.SetDefaultTZDate(AValue: TDateTime);
begin
  FDefaultTZDate := DateOf(AValue);
end;

procedure TIBCustomDataSet.SetSQLFiltered(AValue: boolean);
begin
  if FSQLFiltered = AValue then Exit;
  FSQLFiltered := AValue;
  if Active then
  begin
    Active := false;
    Active := true;
  end;
end;

procedure TIBCustomDataSet.SetSQLFilterParams(AValue: TStrings);
begin
  if FSQLFilterParams = AValue then Exit;
  FSQLFilterParams.Assign(AValue);
end;

procedure TIBCustomDataSet.SetMasterDetailDelay(AValue: integer);
begin
  FDataLink.DelayTimerValue := AValue;
end;

function TIBCustomDataSet.GetParser: TSelectSQLParser;
begin
  if not assigned(FParser) then
    FParser := CreateParser;
  Result := FParser
end;

procedure TIBCustomDataSet.HandleSQLFilterParamsChanged(Sender: TObject);
begin
  Active := false;
end;

procedure TIBCustomDataSet.ResetParser;
begin
  if assigned(FParser) then
  begin
    FParser.Free;
    FParser := nil;
    FQSelect.OnSQLChanged := nil; {Do not react to change}
    try
      FQSelect.SQL.Assign(FBaseSQLSelect);
    finally
      FQSelect.OnSQLChanged := SQLChanged;
    end;
  end;
end;

function TIBCustomDataSet.HasParser: boolean;
begin
  Result := not (csDesigning in ComponentState) and (FParser <> nil)
end;

 procedure TIBCustomDataSet.SetGenerateParamNames(AValue: Boolean);
begin
  if FGenerateParamNames = AValue then Exit;
  FGenerateParamNames := AValue;
  Disconnect
end;

procedure TIBCustomDataSet.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  with PRecordData(Buffer)^ do
  begin
    rdUpdateStatus := TUpdateStatus(usInserted);
    rdBookMarkFlag := bfInserted;
    rdRecordNumber := -1;
  end;
end;

procedure TIBCustomDataSet.InternalInsert;
begin
  CursorPosChanged;
end;

{ TIBDataSet IProviderSupport }

procedure TIBCustomDataSet.PSEndTransaction(Commit: Boolean);
begin
  if Commit then
    Transaction.Commit else
    Transaction.Rollback;
end;

function TIBCustomDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: Pointer = nil): Integer;
var
  FQuery: TIBQuery;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TIBQuery.Create(nil);
    with TIBQuery(ResultSet^) do
    begin
      SQL.Text := ASQL;
      Params.Assign(AParams);
      Open;
      Result := RowsAffected;
    end;
  end
  else
  begin
    FQuery := TIBQuery.Create(nil);
    try
      FQuery.Database := Database;
      FQuery.Transaction := Transaction;
      FQuery.GenerateParamNames := True;
      FQuery.SQL.Text := ASQL;
      FQuery.Params.Assign(AParams);
      FQuery.ExecSQL;
      Result := FQuery.RowsAffected;
    finally
      FQuery.Free;
    end;
  end;
end;

function TIBCustomDataSet.PSGetQuoteChar: string;
begin
  if Database.SQLDialect = 3 then
    Result := '"' else
    Result := '';
end;

function TIBCustomDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode else
    PrevErr := 0;
  if E is EIBError then
    with EIBError(E) do
      Result := EUpdateError.Create(E.Message, '', SQLCode, PrevErr, E) else
      Result := inherited PSGetUpdateException(E, Prev);
end;

function TIBCustomDataSet.PSInTransaction: Boolean;
begin
  Result := Transaction.InTransaction;
end;

function TIBCustomDataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TIBCustomDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TIBCustomDataSet.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Close;
    Open;
  end;
end;

function TIBCustomDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TIBUpdateAction;
  SQL: string;
  Params: TParams;

  procedure AssignParams(DataSet: TDataSet; Params: TParams);
  var
    I: Integer;
    Old: Boolean;
    Param: TParam;
    PName: string;
    Field: TField;
    Value: Variant;
  begin
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0; {do not localize}
      if Old then System.Delete(PName, 1, 4);
      Field := DataSet.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then Param.AssignFieldValue(Field, Field.OldValue) else
      begin
        Value := Field.NewValue;
        if VarIsEmpty(Value) then Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;
  end;

begin
  Result := False;
  if Assigned(OnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    if Assigned(FOnUpdateRecord) then
    begin
      FOnUpdateRecord(Delta, UpdateKind, UpdateAction);
      Result := UpdateAction = uaApplied;
    end;
  end
  else if Assigned(FUpdateObject) then
  begin
    SQL := FUpdateObject.GetSQL(UpdateKind).Text;
    if SQL <> '' then
    begin
      Params := TParams.Create;
      try
        Params.ParseSQL(SQL, True);
        AssignParams(Delta, Params);
        if PSExecuteStatement(SQL, Params) = 0 then
          IBError(ibxeNoRecordsAffected, [nil]);
        Result := True;
      finally
        Params.Free;
      end;
    end;
  end;
end;

procedure TIBCustomDataSet.PSStartTransaction;
begin
  ActivateConnection;
  Transaction.StartTransaction;
end;

function TIBCustomDataSet.PsGetTableName: string;
begin
//  if not FInternalPrepared then
//    InternalPrepare;
  { It is possible for the FQSelectSQL to be unprepared
    with FInternalPreprepared being true (see DoBeforeTransactionEnd).
    So check the Prepared of the SelectSQL instead }
  if not FQSelect.Prepared then
    FQSelect.Prepare;
  Result := FQSelect.UniqueRelationName;
end;

procedure TIBDataSet.BatchInput(InputObject: TIBBatchInput);
begin
  InternalBatchInput(InputObject);
end;

procedure TIBDataSet.BatchOutput(OutputObject: TIBBatchOutput);
begin
  InternalBatchOutput(OutputObject);
end;

procedure TIBDataSet.ExecSQL;
begin
  InternalExecQuery;
end;

procedure TIBDataSet.Prepare;
begin
  InternalPrepare;
end;

procedure TIBDataSet.UnPrepare;
begin
  InternalUnPrepare;
end;

function TIBDataSet.GetPrepared: Boolean;
begin
  Result := InternalPrepared;
end;

procedure TIBDataSet.InternalOpen;
begin
  ActivateConnection;
  ActivateTransaction;
  InternalSetParamsFromCursor;
  Inherited InternalOpen;
end;

procedure TIBDataSet.SetFiltered(Value: Boolean);
begin
  if(Filtered <> Value) then
  begin
    inherited SetFiltered(value);
    if Active then
    begin
      Close;
      Open;
    end;
  end
  else
    inherited SetFiltered(value);
end;

function TIBCustomDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := false;
  if not Assigned(Bookmark) then
    exit;
  Result := PInteger(Bookmark)^ < FRecordCount;
end;

function TIBCustomDataSet.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
{$IFDEF TBCDFIELD_IS_BCD}
var
  lTempCurr : System.Currency;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    Result := InternalGetFieldData(Field, @lTempCurr);
    if Result then
      CurrToBCD(lTempCurr, TBCD(Buffer^), 32, Field.Size);
  end
  else
{$ELSE}
begin
{$ENDIF}
    Result := InternalGetFieldData(Field, Buffer);
end;

function TIBCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  {These datatypes use IBX conventions and not TDataset conventions}
  if (Field.DataType in [ftBCD,ftDateTime,ftDate,ftTime]) and not NativeFormat then
    Result := InternalGetFieldData(Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer);
{$IFDEF TDBDFIELD_IS_BCD}
var
  lTempCurr : System.Currency;
begin
  if (Field.DataType = ftBCD) and (Buffer <> nil) then
  begin
    BCDToCurr(TBCD(Buffer^), lTempCurr);
    InternalSetFieldData(Field, @lTempCurr);
  end
  else
{$ELSE}
begin
{$ENDIF}
    InternalSetFieldData(Field, Buffer);
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
begin
  {These datatypes use IBX conventions and not TDataset conventions}
  if (not NativeFormat) and (Field.DataType in [ftBCD,ftDateTime,ftDate,ftTime]) then
    InternalSetfieldData(Field, Buffer)
  else
    inherited SetFieldData(Field, buffer, NativeFormat);
end;

{ TIBDataSetUpdateObject }

constructor TIBDataSetUpdateObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshSQL := TStringList.Create;
end;

destructor TIBDataSetUpdateObject.Destroy;
begin
  FRefreshSQL.Free;
  inherited Destroy;
end;

function TIBDataSetUpdateObject.GetRowsAffected(
  var SelectCount, InsertCount, UpdateCount, DeleteCount: integer): boolean;
begin
  Result := true;
  SelectCount := 0;
  InsertCount := 0;
  UpdateCount := 0;
  DeleteCount := 0;
end;

procedure TIBDataSetUpdateObject.SetRefreshSQL(value: TStrings);
begin
  FRefreshSQL.Assign(Value);
end;

procedure TIBDataSetUpdateObject.InternalSetParams(Params: ISQLParams;
  buff: PChar);
begin
  if not Assigned(DataSet) then Exit;
  DataSet.SetInternalSQLParams(Params, buff);
end;

procedure TIBDataSetUpdateObject.InternalSetParams(Query: TIBSQL; buff: PChar);
begin
  InternalSetParams(Query.Params,buff);
end;

procedure TIBDataSetUpdateObject.UpdateRecordFromQuery(UpdateKind: TUpdateKind;
  QryResults: IResults; Buffer: PChar);
begin
  if not Assigned(DataSet) then Exit;
  case UpdateKind of
  ukModify, ukInsert:
    DataSet.UpdateRecordFromQuery(QryResults, Buffer);
  ukDelete:
    DataSet.DoDeleteReturning(QryResults);
  end;
end;

function TIBDSBlobStream.GetSize: Int64;
begin
  Result := FBlobStream.BlobSize;
end;

{ TIBDSBlobStream }
constructor TIBDSBlobStream.Create(AField: TField; ABlobStream: TIBBlobStream;
                                    Mode: TBlobStreamMode);
begin
  FField := AField;
  FBlobStream := ABlobStream;
  FBlobStream.Seek(0, soFromBeginning);
  if (Mode = bmWrite) then
  begin
    FBlobStream.Truncate;
    TIBCustomDataSet(FField.DataSet).RecordModified(True);
    TBlobField(FField).Modified := true;
    FHasWritten := true;
  end;
end;

destructor TIBDSBlobStream.Destroy;
begin
  if FHasWritten then
     TIBCustomDataSet(FField.DataSet).DataEvent(deFieldChange, PtrInt(FField));
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
  TIBCustomDataSet(FField.DataSet).RecordModified(True);
  TBlobField(FField).Modified := true;
  result := FBlobStream.Write(Buffer, Count);
  FHasWritten := true;
{  TIBCustomDataSet(FField.DataSet).DataEvent(deFieldChange, PtrInt(FField));
  Removed as this caused a seek to beginning of the blob stream thus corrupting
  the blob stream. Moved to the destructor i.e. called after blob written}
end;

{ TIBGenerator }

procedure TIBGenerator.SetIncrement(const AValue: integer);
begin
  if FIncrement = AValue then Exit;
  if AValue < 0 then
    IBError(ibxeNegativeGenerator,[]);
  FIncrement := AValue;
  SetQuerySQL;
end;

procedure TIBGenerator.SetTransaction(AValue: TIBTransaction);
begin
  FQuery.Transaction := AValue;
end;

procedure TIBGenerator.SetQuerySQL;
begin
  if (Database <> nil) and (FGeneratorName <> '') then
    FQuery.SQL.Text := Format('Select Gen_ID(%s,%d) From RDB$Database',
      [QuoteIdentifierIfNeeded(Database.SQLDialect,FGeneratorName),Increment]);
end;

function TIBGenerator.GetDatabase: TIBDatabase;
begin
  Result := FQuery.Database;
end;

function TIBGenerator.GetTransaction: TIBTransaction;
begin
  Result := FQuery.Transaction;
end;

procedure TIBGenerator.SetDatabase(AValue: TIBDatabase);
begin
  FQuery.Database := AValue;
  SetQuerySQL;
end;

procedure TIBGenerator.SetGeneratorName(AValue: string);
begin
  if FGeneratorName = AValue then Exit;
  FGeneratorName := AValue;
  SetQuerySQL;
end;

function TIBGenerator.GetNextValue: integer;
begin
  with FQuery do
  begin
    Transaction.Active := true;
    ExecQuery;
    try
      Result := Fields[0].AsInteger
    finally
      Close
    end;
  end;
end;

constructor TIBGenerator.Create(Owner: TIBCustomDataSet);
begin
  FOwner := Owner;
  FIncrement := 1;
  FQuery := TIBSQL.Create(nil);
end;

destructor TIBGenerator.Destroy;
begin
  if assigned(FQuery) then FQuery.Free;
  inherited Destroy;
end;


procedure TIBGenerator.Apply;
begin
  if assigned(Database) and assigned(Transaction) and
       (FGeneratorName <> '') and (FFieldName <> '') and Owner.FieldByName(FFieldName).IsNull then
    Owner.FieldByName(FFieldName).AsInteger := GetNextValue;
end;

initialization
  RegisterClasses([TIBArrayField,TIBStringField,TIBBCDField,
                   TIBSmallintField,TIBIntegerField,TIBLargeIntField,
                   TIBMemoField, TIBDateTimeField, TIBTimeField]);


end.
