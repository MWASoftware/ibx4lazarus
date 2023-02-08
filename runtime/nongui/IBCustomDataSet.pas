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
  SysUtils, Classes, IBDatabase, IBExternals, IBInternals, IB,  IBSQL, DB,
  IBUtils, IBBlob, IBSQLParser, IBDatabaseInfo, IBBufferedCursors;

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
    procedure Apply(UpdateKind: TUpdateKind; buff: TRecordBuffer); virtual; abstract;
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
    procedure InternalSetParams(Params: ISQLParams; buff: TRecordBuffer); overload;
    procedure InternalSetParams(Query: TIBSQL; buff: TRecordBuffer); overload;
    procedure UpdateRecordFromQuery(UpdateKind: TUpdateKind; QryResults: IResults; Buffer: TRecordBuffer);
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

  { TIBArrayField }

  TIBArrayField = class(TField)
  private
    FArrayBounds: TArrayBounds;
    FArrayDimensions: integer;
    FRelationName: string;
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
    property RelationName: string read FRelationName;
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


   { TIBDateTimeField }

   {It seems wrong to make this a subclass of TTimeField and not TDateTimField.
    However, the rationale is backwards compatibility for applications that
    may want to coerce a TField to a TTimeField. If this is to work then
    TIBTimeField has to descend from TTimeField. Hence the declation. As
    TTimeField also descends from TDateTimeField this should not result in any
    problems - unless someone makes a drastic change to TTimeField...}

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

  private
    FAllowAutoActivateTransaction: Boolean;
    FAutoCommit: TIBAutoCommit;
    FCaseSensitiveParameterNames: boolean;
    FDefaultTZDate: TDateTime;
    FEnableStatistics: boolean;
    FGenerateParamNames: Boolean;
    FGeneratorField: TIBGenerator;
    FForcedRefresh: Boolean;
    FDidActivate: Boolean;
    FBase: TIBBase;
    FBufferChunks: Integer;
    FCachedUpdates: Boolean;
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
    FDataSetCloseAction: TDataSetCloseAction;
    FTZTextOption: TTZTextOptions;
    FSQLFiltered: boolean;
    FSQLFilterParams: TStrings;
    FUniDirectional: Boolean;
    FUpdateMode: TUpdateMode;
    FUpdateObject: TIBDataSetUpdateObject;
    FParamCheck: Boolean;
    FUpdateRecordTypes: TIBUpdateRecordTypes;
    FDataLink: TIBDataLink;
    FBeforeDatabaseDisconnect,
    FAfterDatabaseDisconnect,
    FDatabaseFree: TNotifyEvent;
    FOnUpdateError: TIBUpdateErrorEvent;
    FOnUpdateRecord: TIBUpdateRecordEvent;
    FBeforeTransactionEnd,
    FAfterTransactionEnd,
    FTransactionFree: TNotifyEvent;
    FBaseSQLSelect: TStrings;
    FParser: TSelectSQLParser;
    FCloseAction: TTransactionAction;
    FInTransactionEnd: boolean;
    FIBLinks: TList;
    FSelectCount: integer;
    FInsertCount: integer;
    FUpdateCount: integer;
    FDeleteCount: integer;
    FComputedFieldNames: TStringList;
    FCursor: IIBCursor;
    FFilterBuffer: TRecordBuffer;
    FAliasNameMap: array of string;
    FAliasNameList: array of AnsiString;
    FCachedUpdatesBuffer: TRecordBuffer;
    procedure ApplyUpdatesIterator(status: TCachedUpdateStatus;
      aBufID: TRecordBuffer; var RecordSkipped: boolean);
    function GetSelectStmtIntf: IStatement;
    function GetUpdatesPending: Boolean;
    procedure SetCaseSensitiveParameterNames(AValue: boolean);
    procedure SetDefaultTZDate(AValue: TDateTime);
    procedure SetSQLFiltered(AValue: boolean);
    procedure SetSQLFilterParams(AValue: TStrings);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TIBDataSetUpdateObject);

    function CanEdit: Boolean;
    function CanInsert: Boolean;
    function CanDelete: Boolean;
    function CanRefresh: Boolean;
    procedure CheckEditState;
    procedure ClearIBLinks;
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    procedure DoAfterDatabaseDisconnect(Sender: TObject);
    procedure DoDatabaseFree(Sender: TObject);
    procedure DoBeforeTransactionEnd(Sender: TObject; Action: TTransactionAction);
    procedure DoAfterTransactionEnd(Sender: TObject);
    procedure DoTransactionFree(Sender: TObject);
    procedure DoDeleteReturning(QryResults: IResults);
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
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
                            Options: TLocateOptions): Boolean; virtual;
    procedure InternalPostRecord(Qry: TIBSQL; Buff: TRecordBuffer); virtual;
    procedure RegisterIBLink(Sender: TIBControlLink);
    procedure UnRegisterIBLink(Sender: TIBControlLink);
    procedure SetBufferChunks(Value: Integer);
    procedure SetDatabase(Value: TIBDatabase);
    procedure SetDeleteSQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetRefreshSQL(Value: TStrings);
    procedure SetSelectSQL(Value: TStrings);
    procedure SetModifySQL(Value: TStrings);
    procedure SetTransaction(Value: TIBTransaction);
    procedure SetUpdateRecordTypes(Value: TIBUpdateRecordTypes);
    procedure SetUniDirectional(Value: Boolean);
    procedure RefreshParams;
    procedure HandleOnMonitorEvent(Sender: TObject; eventType: TIBMonitorEventType; stmt: IStatement);

  protected
    function GetMasterDetailDelay: integer; virtual;
    procedure SetMasterDetailDelay(AValue: integer); virtual;
    procedure ActivateConnection;
    function ActivateTransaction: Boolean;
    procedure DeactivateTransaction;
    procedure CheckDatasetClosed;
    procedure CheckDatasetOpen;
    function CreateParser: TSelectSQLParser; virtual;
    function GetActiveBuf: TRecordBuffer;
    procedure FieldDefsFromQuery(SourceQuery: TIBSQL);
    procedure InternalBatchInput(InputObject: TIBBatchInput); virtual;
    procedure InternalBatchOutput(OutputObject: TIBBatchOutput); virtual;
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecQuery; virtual;
    procedure InternalRefreshRow(Buff: TRecordBuffer); virtual;
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
    procedure DoOnNewRecord; override;
    procedure InitRecord(Buffer: TRecordBuffer); override;
    procedure Disconnect; virtual;
    function ConstraintsStored: Boolean;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterInsert; override;
    procedure DoBeforeClose; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetDBAliasName(FieldNo: integer): string;
    function GetFieldDefFromAlias(aliasName: string): TFieldDef;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
                       DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    function GetRecordSize: Word; override;
    procedure InternalAutoCommit;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalDeleteRecord(Qry: TIBSQL; aBuffID: TRecordBuffer);
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    procedure InternalInsert; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalSetFieldData(Field: TField; Buffer: Pointer); virtual;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure Loaded; override;
    procedure ReQuery;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetDataSource(Value: TDataSource);
    procedure SetGenerateParamNames(AValue: Boolean); virtual;
    procedure SetFieldData(Field : TField; Buffer : Pointer); override;
    procedure SetFieldData(Field : TField; Buffer : Pointer;
      NativeFormat : Boolean); overload; override;
    procedure SetRecNo(Value: Integer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;

  protected
    {Likely to be made public by descendant classes}
    property AutoCommit: TIBAutoCommit read FAutoCommit write FAutoCommit default acDisabled;
    property Cursor: IIBCursor read FCursor;
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
    procedure Undelete;
    procedure ResetParser; virtual;
    function HasParser: boolean;

    { TDataSet support methods }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetArray(Field: TIBArrayField): IArray;
    procedure SetArrayIntf(AnArray: IArray; Field: TIBArrayField);
    function GetCurrentRecord(Buffer: TRecordBuffer): Boolean; override;
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
    property DatabaseInfo: TIBDatabaseInfo read FDatabaseInfo;
    property UpdateObject: TIBDataSetUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read GetUpdatesPending;
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

  TIBFieldTypes = ftUnknown..ftWideMemo;

const
  DefaultFieldClasses: array[TIBFieldTypes] of TFieldClass = (
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
    );
(*var
  CreateProviderProc: function(DataSet: TIBCustomDataSet): IProvider = nil;*)

implementation

uses Variants, FmtBCD, LazUTF8, IBMessages, IBQuery, DateUtils, dbconst, IBSQLMonitor;

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
  Buffer: Pointer;
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
  Buffer: Pointer;
  s: RawByteString;
begin
  Buffer := nil;
  IBAlloc(Buffer, 0, DataSize + 1); {allow for trailing #0}
  try
    s := Value;
    if StringCodePage(s) <> CodePage then
      SetCodePage(s,CodePage,CodePage<>CP_NONE);
    StrLCopy(Buffer, PAnsiChar(s), DataSize);
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
  FUniDirectional := False;
  FBufferChunks := BufferCacheSize;
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
  FComputedFieldNames := TStringList.Create;
  FComputedFieldNames.Duplicates := dupError;
  FComputedFieldNames.CaseSensitive := true;
end;

destructor TIBCustomDataSet.Destroy;
begin
  if Active then Active := false;
  if assigned(FGeneratorField) then FGeneratorField.Free;
  FDataLink.Free;
  FBase.Free;
  ClearIBLinks;
  FIBLinks.Free;
  if assigned(FBaseSQLSelect) then FBaseSQLSelect.Free;
  if assigned(FParser) then FParser.Free;
  if assigned(FSQLFilterParams) then FSQLFilterParams.Free;
  if assigned(FComputedFieldNames) then FComputedFieldNames.Free;
  inherited Destroy;
end;

procedure TIBCustomDataSet.ApplyUpdatesIterator(status: TCachedUpdateStatus; aBufID: TRecordBuffer;
                                                        var RecordSkipped: boolean);

var UpdateAction: TIBUpdateAction;
    UpdateKind: TUpdateKind;

  procedure GetUpdateKind;
  begin
    case status of
    cusModified:
      UpdateKind := ukModify;
    cusInserted:
      UpdateKind := ukInsert;
    else
      UpdateKind := ukDelete;
    end;
  end;

  procedure UpdateUsingOnUpdateRecord;
  begin
    FCachedUpdatesBuffer := aBufID;
    try
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
    finally
      FCachedUpdatesBuffer := nil;
    end;
  end;

  procedure UpdateUsingUpdateObject;
  begin
    try
      FUpdateObject.Apply(UpdateKind,aBufID);
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
      case status of
        cusModified:
          InternalPostRecord(FQModify, aBufID);
        cusInserted:
          InternalPostRecord(FQInsert, aBufID);
        cusDeleted:
          InternalDeleteRecord(FQDelete, aBufID);
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
  GetUpdateKind;
  repeat
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
      uaApplied: ;
      uaSkip:
        RecordSkipped := True;
      uaRetry:  ;
    end;
  until UpdateAction <> uaRetry;
end;

procedure TIBCustomDataSet.ApplyUpdates;
var
  CurUpdateTypes: TIBUpdateRecordTypes;
begin
  if State in [dsEdit, dsInsert] then
    Post;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  DisableControls;
  CurUpdateTypes := FUpdateRecordTypes;
  FUpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
  try
    FCursor.ApplyUpdates(ApplyUpdatesIterator);
  finally
    FUpdateRecordTypes := CurUpdateTypes;
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
begin
  if State in [dsEdit, dsInsert] then
    Post;
  if FCachedUpdates and UpdatesPending then
  begin
    DisableControls;
    try
      FCursor.CancelUpdates;
      Resync([]);
    finally
      First;
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

function TIBCustomDataSet.GetActiveBuf: TRecordBuffer;
begin
  if FCachedUpdatesBuffer <> nil then
    Result := FCachedUpdatesBuffer
  else
  case State of
  dsBrowse:
    if IsEmpty then
      Result := nil
    else
      Result := ActiveBuffer;
  dsFilter:
    Result := FFilterBuffer;
  dsCalcFields:
    Result := CalcBuffer;
  else
    if not FOpen then
      Result := nil
    else
      Result := ActiveBuffer;
  end;
end;

procedure TIBCustomDataSet.CheckNotUniDirectional;
begin
  if UniDirectional then
    IBError(ibxeDataSetUniDirectional, [nil]);
end;

function TIBCustomDataSet.CanEdit: Boolean;
begin
  Result := (Trim(FQModify.SQL.Text) <> '') or
    (Assigned(FUpdateObject) and (Trim(FUpdateObject.GetSQL(ukModify).Text) <> '')) or
    (FCachedUpdates and (FCursor.GetCachedUpdateStatus(GetActiveBuf) = cusInserted));
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

function TIBCustomDataSet.GetMasterDetailDelay: integer;
begin
  Result := FDataLink.DelayTimerValue;
end;

function TIBCustomDataSet.CachedUpdateStatus: TCachedUpdateStatus;
begin
  if Active then
    result := FCursor.GetCachedUpdateStatus(GetActiveBuf)
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

procedure TIBCustomDataSet.InternalDeleteRecord(Qry: TIBSQL;
  aBuffID: TRecordBuffer);
begin
  begin
    if (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
      FUpdateObject.Apply(ukDelete,aBuffID)
    else
    begin
      FCursor.SetSQLParams(aBuffID,Qry.Params);
      Qry.ExecQuery;
      if (Qry.FieldCount > 0)  then
        DoDeleteReturning(Qry.Current);
    end;
    FCursor.Delete(ActiveBuffer);
  end;
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

procedure TIBCustomDataSet.InternalPostRecord(Qry: TIBSQL; Buff: TRecordBuffer);
begin
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
  else
  begin
    FCursor.SetSQLParams(Buff,Qry.Params);
    Qry.ExecQuery;
    Qry.Statement.GetRowsAffected(FSelectCount, FInsertCount, FUpdateCount, FDeleteCount);
//    write('before ');ShowRefreshState;
    if Qry.FieldCount > 0 then {Has RETURNING Clause}
      FCursor.UpdateRecordFromQuery(Buff,Qry.Current);
//    write('after ');ShowRefreshState;
  end;
  SetModified(False);
  if (FForcedRefresh or FCursor.NeedRefresh(Buff)) and CanRefresh then
    InternalRefreshRow(Buff);
end;

procedure TIBCustomDataSet.InternalRefreshRow(Buff: TRecordBuffer);
var
  Qry: TIBSQL;
begin
  FBase.SetCursor;
  try
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
        FCursor.SetSQLParams(Buff,Qry.Params);
        Qry.ExecQuery;
        try
          if (Qry.SQLStatementType = SQLExecProcedure) or Qry.Next then
            FCursor.UpdateRecordFromQuery(Buff,Qry.Current);
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

procedure TIBCustomDataSet.HandleOnMonitorEvent(Sender: TObject;
  eventType: TIBMonitorEventType; stmt: IStatement);
begin
  if not (csDesigning in ComponentState) then
    case eventType of
      mtFetch: MonitorHook.SQLFetch(FQSelect);
      mtExecute:;
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
begin
  CheckActive;
  FCursor.UnDelete(GetActiveBuf);
end;

procedure TIBCustomDataSet.UnRegisterIBLink(Sender: TIBControlLink);
begin
  FIBLinks.Remove(Sender);
end;

function TIBCustomDataSet.UpdateStatus: TUpdateStatus;
begin
  if Active then
    Result := FCursor.GetUpdateStatus(GetActiveBuf)
  else
    Result := usUnmodified;
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

function TIBCustomDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := FCursor.AllocRecordBuffer;
end;

function TIBCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  Result := FCursor.CreateBlobStream(GetActiveBuf,Field,Mode);
end;

function TIBCustomDataSet.GetArray(Field: TIBArrayField): IArray;
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  Result := FCursor.GetArray(GetActiveBuf,Field);
end;

procedure TIBCustomDataSet.SetArrayIntf(AnArray: IArray; Field: TIBArrayField);
begin
  if (Field = nil) or (Field.DataSet <> self) then
    IBError(ibxFieldNotinDataSet,[Field.Name,Name]);
  FCursor.SetArrayIntf(GetActiveBuf,AnArray,Field);
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
    if PIBRecordNumber(Bookmark1)^ < PIBRecordNumber(Bookmark2)^ then
      Result := CMPLess
    else
    if PIBRecordNumber(Bookmark1)^ > PIBRecordNumber(Bookmark2)^ then
      Result := CMPGtr
    else
      Result := CMPEql;
  end;
end;

procedure TIBCustomDataSet.DoBeforeDelete;
begin
  if not CanDelete then
    IBError(ibxeCannotDelete, [nil]);
  inherited DoBeforeDelete;
end;

procedure TIBCustomDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  FBase.DoAfterDelete(self);
  InternalAutoCommit;
end;

procedure TIBCustomDataSet.DoBeforeEdit;
begin
  if not(CanEdit or (FQModify.SQL.Count <> 0) or
    (FCachedUpdates and Assigned(FOnUpdateRecord))) then
    IBError(ibxeCannotUpdate, [nil]);
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
  if FCachedUpdates and FCursor.UpdatesPending and (DataSetCloseAction = dcSaveChanges) then
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

procedure TIBCustomDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FCursor.FreeRecordBuffer(Buffer);
end;

procedure TIBCustomDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  FCursor.GetBookmarkData(Buffer,Data);
end;

function TIBCustomDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
 Result := FCursor.GetBookmarkFlag(Buffer);
end;

function TIBCustomDataSet.GetCanModify: Boolean;
begin
  result := (FQInsert.SQL.Text <> '') or
    (FQModify.SQL.Text <> '') or
    (FQDelete.SQL.Text <> '') or
    (Assigned(FUpdateObject));
end;

function TIBCustomDataSet.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
begin
  Result := not IsEmpty and (FCursor.GetRecord(Buffer,gmCurrent, false) = grOK);
  if Result then
    UpdateCursorPos;
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
  if FieldType in [ftUnknown..ftWideMemo] then
    Result := DefaultFieldClasses[FieldType]
  else
    Result := nil;
end;

function TIBCustomDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  result := GetFieldData(FieldByNumber(FieldNo), buffer);
end;

{ GetRecNo and SetRecNo both operate off of 1-based indexes as
 opposed to 0-based indexes.
 This is because we want LastRecordNumber/RecordCount = 1 }

function TIBCustomDataSet.GetRecNo: Integer;
begin
  if GetActiveBuf = nil then
    result := 0
  else
    result := FCursor.GetRecNo(GetActiveBuf);
end;

function TIBCustomDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
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
      Result := FCursor.GetRecord(FFilterBuffer, GetMode, DoCheck);
      if Result <> grOK then
        break;
      try
        Accept := True;
        OnFilterRecord(Self, Accept);
        if not Accept and (GetMode = gmCurrent) then
          GetMode := gmPrior;
      except
//        FBase.HandleException(Self);
      end;
    end;
    FCursor.SwapDataBuffer(FFilterBuffer,Buffer);
    RestoreState(SaveState);
  end
  else
    Result := FCursor.GetRecord(Buffer, GetMode, DoCheck);
  if Result = grOK then
    GetCalcFields(Buffer);
end;

function TIBCustomDataSet.GetRecordCount: Integer;
begin
  result := FCursor.GetRecordCount;
end;

function TIBCustomDataSet.GetRecordSize: Word;
begin
  result := FCursor.GetRecordSize;
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

{InternalAddRecord does not appear to be used by TDataset. Hence, is this worth having?}

procedure TIBCustomDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  CheckEditState;
  if Append then
    FCursor.Append(Buffer)
  else
    FCursor.InsertBefore(Buffer);

  if not CachedUpdates then
    InternalPostRecord(FQInsert, Buffer);
  InternalSetToRecord(Buffer);
end;

procedure TIBCustomDataSet.InternalCancel;
begin
  FCursor.CancelChanges(ActiveBuffer);
end;


procedure TIBCustomDataSet.InternalClose;
begin
  if FDidActivate then
    DeactivateTransaction;
  if (FCursor <> nil) and (FFilterBuffer <> nil) then
    FCursor.FreeRecordBuffer(FFilterBuffer);
  FCursor := nil;
  FQSelect.Close;
  FOpen := False;
  BindFields(False);
  ResetParser;
  if DefaultFields then DestroyFields;
end;

procedure TIBCustomDataSet.InternalDelete;
begin
  if State = dsEdit then Cancel;
  FBase.SetCursor;
  try
    if CanDelete then
    begin
      if not FCursor.GetCachedUpdatesEnabled then
        InternalDeleteRecord(FQDelete,ActiveBuffer)
      else
        FCursor.Delete(ActiveBuffer);
    end
    else
      IBError(ibxeCannotDelete, [nil]);
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalEdit;
begin
  FCursor.EditBuffer(ActiveBuffer);
end;

procedure TIBCustomDataSet.InternalFirst;
begin
  FCursor.GotoFirst;
end;

procedure TIBCustomDataSet.InternalInsert;
begin
  CursorPosChanged;
end;

procedure TIBCustomDataSet.InternalGotoBookmark(Bookmark: Pointer);
var RecNo: TIBRecordNumber;
begin
  Move(Bookmark^,RecNo,BookmarkSize);
  FCursor.GotoRecordNumber(RecNo);
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
  i, FieldPrecision: Integer;
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
  if not Database.InternalTransaction.InTransaction then
    Database.InternalTransaction.StartTransaction;
  Query := TIBSQL.Create(self);
  try
    Query.Database := DataBase;
    Query.Transaction := Database.InternalTransaction;
    FieldDefs.BeginUpdate;
    FieldDefs.Clear;
    FieldIndex := 0;
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
        if (FieldType <> ftUnknown) and (FieldAliasName <> sDBkeyAlias) then {do not localize}
        begin
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
                FComputedFieldNames.Add(DBAliasName);
              end
              else
              begin
                if Has_DEFAULT_VALUE(RelationName, FieldName) then
                begin
                  if not FieldNullable then
                    Attributes := [faRequired];
                end
                else
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

procedure TIBCustomDataSet.InternalLast;
begin
  FCursor.GotoLast;
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
              cur_param.AsBlob := TIBDSBlobStream(s).BlobStream.Blob;
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
  FQSelect.ExecQuery;
  FCursor.SetCursor(FQSelect.CurrentCursor);
  First;
end;

procedure TIBCustomDataSet.InternalOpen;
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
      BindFields(True);
      FQSelect.ExecQuery;
      FOpen := FQSelect.Open;
      if UniDirectional then
        FCursor := TIBUniDirectionalCursor.create(Name + ': ' + SUniCursor,
                                                       FQSelect.CurrentCursor,Fields,
                                                       FComputedFieldNames, CalcFieldsSize,
                                                       FDefaultTZDate,HandleOnMonitorEvent, CachedUpdates)
      else
        FCursor := TIBBiDirectionalCursor.create(Name + ': ' + SBiDirCursor,
                                                       FQSelect.CurrentCursor,Fields,
                                                       FComputedFieldNames, CalcFieldsSize,
                                                       FDefaultTZDate, HandleOnMonitorEvent, CachedUpdates,
                                                       FBufferChunks,
                                                       FBufferChunks);


      FFilterBuffer := FCursor.AllocRecordBuffer;
      BookmarkSize := FCursor.GetBookmarkSize;
   end
    else
      FQSelect.ExecQuery;
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalPost;
begin
  FBase.SetCursor;
  try
    CheckEditState;
    if (State = dsInsert) then
    begin
       if (not CachedUpdates) then
         InternalPostRecord(FQInsert,ActiveBuffer);
      FCursor.EditingDone(ActiveBuffer,cusInserted);
    end
    else
    begin
      if (not CachedUpdates) then
        InternalPostRecord(FQModify,ActiveBuffer);
      FCursor.EditingDone(ActiveBuffer,cusModified);
    end;
  finally
    FBase.RestoreCursor;
  end;
end;

procedure TIBCustomDataSet.InternalRefresh;
begin
  inherited InternalRefresh;
  InternalRefreshRow(GetActiveBuf);
end;

procedure TIBCustomDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  FCursor.SetCurrentRecord(Buffer);
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

procedure TIBCustomDataSet.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  FCursor.SetBookmarkFlag(Buffer,Value);
end;

procedure TIBCustomDataSet.SetCachedUpdates(Value: Boolean);
begin
  if not Value and FCachedUpdates and assigned (FCursor) then
    FCursor.CancelUpdates;
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
begin
  CheckEditState;
  FCursor.SetFieldData(GetActiveBuf,Field,Buffer);
  SetModified(True);
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, PtrInt(Field));
end;

procedure TIBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value < 1) then
    Value := 1
  else if Value > RecordCount then
  begin
    InternalLast;
    Value := Min(RecordCount, Value);
  end;
  if (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCursor.GotoRecordNumber(Value);
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TIBCustomDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer
  );
begin
  FCursor.SetBookmarkData(Buffer,Data);
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

procedure TIBCustomDataSet.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FCursor.ClearCalcFields(Buffer);
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
    FComputedFieldNames.Clear;
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

function TIBCustomDataSet.GetUpdatesPending: Boolean;
begin
  Result := FCursor.UpdatesPending;
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

procedure TIBCustomDataSet.InitRecord(Buffer: TRecordBuffer);
begin
  inherited InitRecord(Buffer);
  FCursor.InitRecord(Buffer);
end;

procedure TIBCustomDataSet.DoOnNewRecord;
begin
  if FCursor.GetBookmarkFlag(ActiveBuffer) = bfEOF then
    FCursor.Append(ActiveBuffer)
  else
   FCursor.InsertBefore(ActiveBuffer); {Bookmark data determines insertion point}
  inherited DoOnNewRecord;
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
var RecNo: TIBRecordNumber;
begin
  Result := false;
  if not Assigned(Bookmark) then
    exit;
  Move(pointer(Bookmark)^,RecNo,BookmarkSize);
  Result := RecNo <= FCursor.GetRecordCount;
end;

function TIBCustomDataSet.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
begin
  CheckActive;
  Result := FCursor.GetFieldData(GetActiveBuf,Field, Buffer);
end;

function TIBCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  CheckActive;
  {These datatypes use IBX conventions and not TDataset conventions}
  if (Field.DataType in [ftBCD,ftDateTime,ftDate,ftTime]) and not NativeFormat then
    Result := FCursor.GetFieldData(GetActiveBuf,Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
  CheckActive;
  InternalSetFieldData(Field, Buffer);
end;

procedure TIBCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
begin
  CheckActive;
  {These datatypes use IBX conventions and not TDataset conventions}
  if (not NativeFormat) and (Field.DataType in [ftBCD,ftDateTime,ftDate,ftTime]) then
    InternalSetFieldData(Field, Buffer)
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
  buff: TRecordBuffer);
begin
  if not Assigned(DataSet) then Exit;
  DataSet.FCursor.SetSQLParams(buff,Params);
end;

procedure TIBDataSetUpdateObject.InternalSetParams(Query: TIBSQL; buff: TRecordBuffer);
begin
  InternalSetParams(Query.Params,buff);
end;

procedure TIBDataSetUpdateObject.UpdateRecordFromQuery(UpdateKind: TUpdateKind;
  QryResults: IResults; Buffer: TRecordBuffer);
begin
  if not Assigned(DataSet) then Exit;
  case UpdateKind of
  ukModify, ukInsert:
    DataSet.FCursor.UpdateRecordFromQuery(Buffer,QryResults);
  ukDelete:
    DataSet.DoDeleteReturning(QryResults);
  end;
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
