{************************************************************************}
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{                                                                        }
{    The Original Code was created by Jeff Overcash.                     } 
{    Portions based upon code by Inprise Corporation are Copyright (C)   }
{       Inprise Corporation. All Rights Reserved.                        }
{                                                                        }
{    IBX Version 4.2 or higher required                                  }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2018                                               }
{                                                                        }
{************************************************************************}

{ Syntax Enhancements Supported (by Firebird Version no.):

Multi-action triggers (1.5)
CREATE SEQUENCE (2.0)
Database Triggers (2.1)
Global Temporary Tables (2.1)
Boolean Type (3.0)
Identity Column Type (3.0)
DDL Triggers (3.0)
Internal Functions (3.0)
TIME_TZ, TIMESTAMP_TZ, DECFLOAT types (4.0)
Identity Column increment (4.0)
Default Role (4.0)
SQL Security clause for Database, Tables, Procedures and Triggers (4.0)
}

unit IBExtract;

{$Mode Delphi}
{$codepage UTF8}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, IBDatabase, IBDatabaseInfo,
  IBSQL, IBUtils, IB;

type
  TExtractObjectTypes =
    (eoDatabase, eoDomain, eoTable, eoView, eoProcedure, eoFunction, eoPackage,
     eoGenerator, eoException, eoBLOBFilter, eoRole, eoTrigger, eoForeign,
     eoIndexes, eoChecks, eoComments, eoData);

  TExtractType =
    (etDomain, etTable, etRole, etTrigger, etForeign,
     etIndex, etData, etGrant, etCheck, etGrantsToUser,
     etDatabaseTriggers, etDDLTriggers);

  TExtractTypes = Set of TExtractType;

  TProcDDLType = (pdCreateProc,pdCreateStub,pdAlterProc);

  TDomainDDLType = (dtCreateDomain,dtCreateNoCheckConstraint, dtAddCheckConstraint);

  TPackageDDLType = (paHeader,paBody,paBoth);

  TCommentType = (ctDatabase, ctCharacterSet,ctCollation,ctDomain,ctException,
                  ctExternalFunction, ctFilter, ctGenerator, ctIndex, ctPackage,
                  ctProcedure, ctFunction, ctRole, ctSequence, ctTable, ctTrigger,
                  ctView, ctColumn,ctParameter, ctArgument);
  TCommentTypes = set of TCommentType;

  TOnExtractLines = procedure(Sender: TObject; start, count: integer) of object;

  { TIBExtract }

  TIBExtract = class(TComponent)
  private
    FAlwaysQuoteIdentifiers: boolean;
    FCaseSensitiveObjectNames: boolean;
    FDatabase : TIBDatabase;
    FIncludeMetaDataComments: boolean;
    FOnExtractLines: TOnExtractLines;
    FTransaction : TIBTransaction;
    FMetaData: TStrings;
    FDatabaseInfo: TIBDatabaseInfo;
    FShowSystem: Boolean;
    FDefaultCharSetName: string;
    { Private declarations }
    function AddSQLSecurity(SQLSecurity: ISQLData): string;
    procedure Add2MetaData(const Msg: string; IsError: boolean=true);
    procedure AddComment(Query: TIBSQL; cType: TCommentType; OutStrings: TStrings;
        CommentFieldName: string = 'RDB$DESCRIPTION'); overload;
    procedure AddComment(Query: TIBSQL; cType: TCommentType;
        CommentFieldName: string = 'RDB$DESCRIPTION'); overload;
    procedure ExtractOut(DDLLine: string); overload;
    procedure ExtractOut(DDLStrings: TStrings); overload;
    function GetCharacterSetName(CharSetID: integer; ForceCharSet: boolean=false): string;
    function GetCollationName(CharacterSetID, CollationID: integer): string;
    function GetDatabase: TIBDatabase;
    function GetIndexSegments ( indexname : String) : String;
    function GetTransaction: TIBTransaction;
    function GetTriggerType(TypeID: Int64): string;
    function LookupDDLObject(cType: TCommentType): integer;
    procedure SetDatabase(const Value: TIBDatabase);
    procedure SetTransaction(const Value: TIBTransaction);
    function PrintValidation(ToValidate : String;	flag : Boolean) : String;
    procedure ShowGrants(MetaObject: String; Terminator : String; NoUserGrants: boolean=false);
    procedure ShowGrantsTo(MetaObject: String; ObjectType: integer;
      Terminator: String);
    procedure ShowGrantRoles(Terminator : String);
    procedure GetProcedureArgs(Proc : String);
  protected
    { Protected declarations }
    function ExtractDDL(Flag: Boolean; TableName: String; ExtractTypes: TExtractTypes =
      []): Boolean;
    function ExtractListTable(RelationName, NewName: String; DomainFlag: Boolean): Boolean;
    procedure ExtractListView (ViewName : String);
    procedure ListData(ObjectName : String);
    procedure ListRoles(ObjectName : String = ''; IncludeGrants:boolean=false);
    procedure ListGrants(ExtractTypes : TExtractTypes = []);
    procedure ListPackages(PackageDDLType: TPackageDDLType; PackageName: string = ''; IncludeGrants:boolean = false);
    procedure ListProcs(ProcDDLType: TProcDDLType = pdCreateProc; ProcedureName : String = '';
      IncludeGrants:boolean=false);
    procedure ListAllTables(flag : Boolean);
    procedure ListTriggers(ObjectName: String=''; ExtractTypes: TExtractTypes = [etTrigger]);
    procedure ListCheck(ObjectName : String = ''; ExtractType : TExtractType = etCheck);
    procedure ListComments(CommentTypes: TCommentTypes = []);
    procedure ListCreateDb(TargetDb : String = '');
    procedure ListDomains(DomainDDLType: TDomainDDLType; ObjectName : String = ''; ExtractType : TExtractType = etDomain);
    procedure ListException(ExceptionName : String = '');
    procedure ListFilters(FilterName : String = '');
    procedure ListForeign(ObjectName : String = ''; ExtractType : TExtractType = etForeign);
    procedure ListExternalFunctions(FunctionName : String = '');
    procedure ListFunctions(ProcDDLType: TProcDDLType = pdCreateProc; FunctionName : String = ''; IncludeGrants:boolean=false);
    procedure ListGenerators(GeneratorName : String = ''; ExtractTypes: TExtractTypes=[]);
    procedure ListIndex(ObjectName : String = ''; ExtractType : TExtractType = etIndex);
    procedure ListViews(ViewName : String = '');
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PrintSet(var Used : Boolean) : String;
    function QuoteIdentifier(Value: String): String;
    function GetFieldType(qry: TIBSQL; ForceCharSet: boolean=false): string; overload;

  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetArrayField(FieldName : String) : String;
    function GetFieldType(FieldType, FieldSubType, FieldScale, FieldPrecision,
      FieldLength: integer;
      HasCharacterSet: boolean=false;
      ForceCharSet: boolean=false;
      FieldCharacterSetID: integer=0;
      FieldCharacterLength: integer=0;
      FieldSegmentLen: integer=0;
      IsArray: boolean=false;
      FieldSourceName: string=''): String; overload;
    function GetCharacterSets(CharSetId, Collation : integer;	CollateOnly : Boolean) : String;
    procedure ExtractObject(ObjectType : TExtractObjectTypes; ObjectName : String = '';
      ExtractTypes : TExtractTypes = []);
    procedure ListObjectNames(ObjectType: integer; Names: TStrings);
    property DatabaseInfo : TIBDatabaseInfo read FDatabaseInfo;
    property Items : TStrings read FMetaData;

  published
    { Published declarations }
    property Database : TIBDatabase read GetDatabase write SetDatabase;
    property Transaction : TIBTransaction read GetTransaction write SetTransaction;
    property ShowSystem: Boolean read FShowSystem write FShowSystem;
    property AlwaysQuoteIdentifiers: boolean read FAlwaysQuoteIdentifiers write FAlwaysQuoteIdentifiers;
    property IncludeMetaDataComments: boolean read FIncludeMetaDataComments write FIncludeMetaDataComments default true;
    property CaseSensitiveObjectNames: boolean read FCaseSensitiveObjectNames write FCaseSensitiveObjectNames;
    property OnExtractLines: TOnExtractLines read FOnExtractLines write FOnExtractLines;
  end;

  TSQLType = record
    SqlType : Integer;
    TypeName : String;
  end;

  TPrivTypes = record
    PrivFlag : Integer;
    PrivString : String;
  end;

  TSQLTypes = Array[0..19] of TSQLType;

  TUserPrivilege = record
    Privilege: string;
    mask: cardinal;
  end;

const
  MAX_INTSUBTYPES = 2;
  MAXSUBTYPES = 8;     { Top of subtypes array }

  priv_UNKNOWN = 1;
  priv_SELECT = 2;
  priv_INSERT = 4;
  priv_UPDATE = 8;
  priv_DELETE = 16;
  priv_EXECUTE = 32;
  priv_REFERENCES = 64;
  priv_USAGE = 128;

 PrivTypes : Array[0..6] of TPrivTypes = (
  (PrivFlag : priv_DELETE; PrivString : 'DELETE' ),
  (PrivFlag : priv_EXECUTE; PrivString : 'EXECUTE' ),
  (PrivFlag : priv_INSERT; PrivString : 'INSERT' ),
  (PrivFlag : priv_SELECT; PrivString : 'SELECT' ),
  (PrivFlag : priv_UPDATE; PrivString : 'UPDATE' ),
  (PrivFlag : priv_REFERENCES; PrivString : 'REFERENCES'),
  (PrivFlag : priv_USAGE; PrivString : 'USAGE' ));

  ColumnTypes : TSQLTypes = (
    (SqlType : blr_short; TypeName :	'SMALLINT'),		{ NTX: keyword }
    (SqlType : blr_long; TypeName : 'INTEGER'),		{ NTX: keyword }
    (SqlType : blr_quad; TypeName : 'QUAD'),		{ NTX: keyword }
    (SqlType : blr_float; TypeName : 'FLOAT'),		{ NTX: keyword }
    (SqlType : blr_text; TypeName : 'CHAR'),		{ NTX: keyword }
    (SqlType : blr_double; TypeName : 'DOUBLE PRECISION'),	{ NTX: keyword }
    (SqlType : blr_varying; TypeName : 'VARCHAR'),		{ NTX: keyword }
    (SqlType : blr_cstring; TypeName : 'CSTRING'),		{ NTX: keyword }
    (SqlType : blr_blob_id; TypeName : 'BLOB_ID'),		{ NTX: keyword }
    (SqlType : blr_blob; TypeName : 'BLOB'),		{ NTX: keyword }
    (SqlType : blr_sql_time; TypeName : 'TIME'),		{ NTX: keyword }
    (SqlType : blr_sql_date; TypeName : 'DATE'),		{ NTX: keyword }
    (SqlType : blr_timestamp; TypeName : 'TIMESTAMP'),		{ NTX: keyword }
    (SqlType : blr_int64; TypeName : 'BIGINT'),
    (SqlType : blr_bool; TypeName : 'BOOLEAN'),
    (SqlType : blr_dec64; TypeName : 'DECFLOAT'),
    (SqlType : blr_dec128; TypeName : 'DECFLOAT'),
    (SqlType : blr_int128; TypeName : 'NUMERIC'),
    (SqlType : blr_sql_time_tz; TypeName : 'TIME WITH TIME ZONE'),
    (SqlType : blr_timestamp_tz; TypeName : 'TIMESTAMP WITH TIME ZONE')
    );

  SubTypes : Array[0..MAXSUBTYPES] of String = (
    'UNKNOWN',			{ NTX: keyword }
    'TEXT',				{ NTX: keyword }
    'BLR',				{ NTX: keyword }
    'ACL',				{ NTX: keyword }
    'RANGES',			{ NTX: keyword }
    'SUMMARY',			{ NTX: keyword }
    'FORMAT',			{ NTX: keyword }
    'TRANSACTION_DESCRIPTION',	{ NTX: keyword }
    'EXTERNAL_FILE_DESCRIPTION');	{ NTX: keyword }

  IntegralSubtypes : Array[0..MAX_INTSUBTYPES] of String = (
    'UNKNOWN',			{ Defined type, NTX: keyword }
    'NUMERIC',			{ NUMERIC, NTX: keyword }
    'DECIMAL'); 			{ DECIMAL, NTX: keyword }

  ODS_VERSION6 = 6;	{ on-disk structure as of v3.0 }
  ODS_VERSION7 = 7;	{ new on disk structure for fixing index bug }
  ODS_VERSION8 =	8;	{ new btree structure to support pc semantics }
  ODS_VERSION9 =	9;	{ btree leaf pages are always propogated up }
  ODS_VERSION10 = 10; { V6.0 features. SQL delimited idetifier,
                                        SQLDATE, and 64-bit exact numeric
                                        type }
  ODS_VERSION12 = 12; {Firebird 3}
  ODS_VERSION13 = 13; {Firebird 4}

  { flags for RDB$FILE_FLAGS }
  FILE_shadow = 1;
  FILE_inactive = 2;
  FILE_manual = 4;
  FILE_cache = 8;
  FILE_conditional = 16;

  { flags for RDB$LOG_FILES }
  LOG_serial = 1;
  LOG_default = 2;
  LOG_raw = 4;
  LOG_overflow = 8;




{ Object types used in RDB$DEPENDENCIES and RDB$USER_PRIVILEGES }

  obj_relation = 0;
  obj_view = 1;
  obj_trigger = 2;
  obj_computed = 3;
  obj_validation = 4;
  obj_procedure = 5;
  obj_expression_index = 6;
  obj_exception = 7;
  obj_user = 8;
  obj_field = 9;
  obj_index = 10;
  obj_character_set = 11;
  obj_user_group = 12;
  obj_sql_role = 13;
  obj_generator = 14;
  obj_udf = 15;
  obj_blob_filter = 16;
  obj_collation = 17;
  obj_package = 18;
  obj_package_body = 19;

  obj_function = 100;
  obj_domain = 101;

  {User Privileges}
  UserPrivileges: array [0..23] of TUserPrivilege = (
  (Privilege:'USER_MANAGEMENT'; Mask: $02000000),	        {Manage users}
  (Privilege:'READ_RAW_PAGES'; Mask: $04000000),	        {Read pages in raw format using Attachment::getInfo()}
  (Privilege:'CREATE_USER_TYPES'; Mask: $08000000),	        {Add/change/delete non-system records in RDB$TYPES}
  (Privilege:'USE_NBACKUP_UTILITY'; Mask: $10000000),	        {Use nbackup to create database copies}
  (Privilege:'CHANGE_SHUTDOWN_MODE'; Mask: $20000000),	        {Shut down database and bring online}
  (Privilege:'TRACE_ANY_ATTACHMENT'; Mask: $40000000),	        {Trace other users' attachments}
  (Privilege:'MONITOR_ANY_ATTACHMENT'; Mask: $80000000),	{Monitor (tables MON$) other users' attachments}
  (Privilege:'ACCESS_SHUTDOWN_DATABASE'; Mask: $00010000),	{Access database when it is shut down}
  (Privilege:'CREATE_DATABASE'; Mask: $00020000),	        {Create new databases (given in security.db)}
  (Privilege:'DROP_DATABASE'; Mask: $00040000),	                {Drop this database}
  (Privilege:'USE_GBAK_UTILITY'; Mask: $00080000),	        {Use appropriate utility}
  (Privilege:'USE_GSTAT_UTILITY'; Mask: $00100000),	        {...}
  (Privilege:'USE_GFIX_UTILITY'; Mask: $00200000),	        {       ...}
  (Privilege:'IGNORE_DB_TRIGGERS'; Mask: $00400000),	        {Instruct engine not to run DB-level triggers}
  (Privilege:'CHANGE_HEADER_SETTINGS'; Mask: $00800000),	{Modify parameters in DB header page}
  (Privilege:'SELECT_ANY_OBJECT_IN_DATABASE'; Mask: $00000100),	{Use SELECT for any selectable object}
  (Privilege:'ACCESS_ANY_OBJECT_IN_DATABASE'; Mask: $00000200),	{Access (in any possible way) any object}
  (Privilege:'MODIFY_ANY_OBJECT_IN_DATABASE'; Mask: $00000400),	{Modify (up to drop) any object}
  (Privilege:'CHANGE_MAPPING_RULES'; Mask: $00000800),	        {Change authentication mappings}
  (Privilege:'USE_GRANTED_BY_CLAUSE'; Mask: $00001000),	        {Use GRANTED BY in GRANT and REVOKE operators}
  (Privilege:'GRANT_REVOKE_ON_ANY_OBJECT'; Mask: $00002000),	{GRANT and REVOKE rights on any object in database}
  (Privilege:'GRANT_REVOKE_ANY_DDL_RIGHT'; Mask: $00004000),	{GRANT and REVOKE any DDL rights}
  (Privilege:'CREATE_PRIVILEGED_ROLES'; Mask: $00008000),	{Use SET SYSTEM PRIVILEGES in roles}
  (Privilege:'MODIFY_EXT_CONN_POOL'; Mask: $00000002)	        {Use command ALTER EXTERNAL CONNECTIONS POOL}
  );


implementation

uses IBMessages, IBDataOutput;

const
  TERM = ';';
  ProcTerm = '^';

  ArraySQL =
    'SELECT * FROM RDB$FIELD_DIMENSIONS FDIM ' +
    'WHERE ' +
    '  FDIM.RDB$FIELD_NAME = :FIELDNAME ' +
    'ORDER BY FDIM.RDB$DIMENSION';

type
  TTriggerPhase = (tpNone,tpCreate,tpAlter,tpDrop);

  TDDLTriggerMap = record
    ObjectName: string;
    Bits: integer;
    Bit1: TTriggerPhase;
    Bit2: TTriggerPhase;
    Bit3: TTriggerPhase;
  end;

const
  DDLTriggers : array [0..15] of TDDLTriggerMap = (
  (ObjectName: 'TABLE'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'PROCEDURE'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'FUNCTION'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'TRIGGER'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'Empty slot'; Bits: 3; Bit1: tpNone; Bit2: tpNone; Bit3: tpNone),
  (ObjectName: 'EXCEPTION'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'VIEW'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'DOMAIN'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'ROLE'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'INDEX'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'SEQUENCE'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'USER'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'COLLATION'; Bits: 2; Bit1: tpCreate; Bit2: tpDrop; Bit3: tpNone),
  (ObjectName: 'CHARACTER SET'; Bits: 1; Bit1: tpAlter; Bit2: tpNone; Bit3: tpNone),
  (ObjectName: 'PACKAGE'; Bits: 3; Bit1: tpCreate; Bit2: tpAlter; Bit3: tpDrop),
  (ObjectName: 'PACKAGE BODY'; Bits: 2; Bit1: tpCreate; Bit2: tpDrop; Bit3: tpNone)
);

type
  TDDLObjects = record
    ObjectName: string;
    ObjType: integer;
    SystemTableName: string;
    NameField: string;
    NameSpaceField: string;
    Condition: string;
    CommentType: TCommentType;
  end;

const
  DDLObjects: array [0..18] of TDDLObjects = (
  (ObjectName: 'CHARACTER SET';
        ObjType: obj_character_set;
        SystemTableName:  'RDB$CHARACTER_SETS';
        NameField: 'RDB$CHARACTER_SET_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctCharacterSet),
  (ObjectName: 'COLLATION';
        ObjType: obj_collation;
        SystemTableName: 'RDB$COLLATIONS';
        NameField: 'RDB$COLLATION_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctCollation),
  (ObjectName: 'DOMAIN';
        ObjType: obj_domain;
        SystemTableName: 'RDB$FIELDS';
        NameField: 'RDB$FIELD_NAME';
        NameSpaceField: '';
        Condition: 'RDB$SYSTEM_FLAG = 0';
        CommentType: ctDomain),
  (ObjectName: 'EXCEPTION';
        ObjType: obj_exception;
        SystemTableName: 'RDB$EXCEPTIONS';
        NameField: 'RDB$EXCEPTION_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctException),
  (ObjectName: 'EXTERNAL FUNCTION';
        ObjType: obj_function;
        SystemTableName: 'RDB$FUNCTIONS';
        NameField: 'RDB$FUNCTION_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctExternalFunction),
  (ObjectName: 'FILTER';
        ObjType: obj_blob_filter;
        SystemTableName: 'RDB$FILTERS';
        NameField: 'RDB$FUNCTION_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctFilter),
  (ObjectName: 'GENERATOR';
        ObjType: obj_generator;
        SystemTableName: 'RDB$GENERATORS';
        NameField: 'RDB$GENERATOR_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctGenerator),
  (ObjectName: 'INDEX';
        ObjType: obj_index;
        SystemTableName: 'RDB$INDICES';
        NameField: 'RDB$INDEX_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctIndex),
  (ObjectName: 'PACKAGE';
        ObjType: obj_package;
        SystemTableName: 'RDB$PACKAGES';
        NameField: 'RDB$PACKAGE_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctPackage),
  (ObjectName: 'PROCEDURE';
        ObjType: obj_procedure;
        SystemTableName: 'RDB$PROCEDURES';
        NameField: 'RDB$PROCEDURE_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctProcedure),
  (ObjectName: 'ROLE';
        ObjType: obj_sql_role;
        SystemTableName: 'RDB$ROLES';
        NameField: 'RDB$ROLE_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctRole),
  (ObjectName: 'SEQUENCE';
        ObjType: obj_generator;
        SystemTableName: 'RDB$GENERATORS';
        NameField: 'RDB$GENERATOR_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctSequence),
  (ObjectName: 'TABLE';
        ObjType: obj_relation;
        SystemTableName: 'RDB$RELATIONS';
        NameField: 'RDB$RELATION_NAME';
        NameSpaceField: '';
        Condition: 'RDB$RELATION_TYPE = 0';
        CommentType: ctTable),
  (ObjectName: 'TRIGGER';
        ObjType: obj_trigger;
        SystemTableName: 'RDB$TRIGGERS';
        NameField: 'RDB$TRIGGER_NAME';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctTrigger),
  (ObjectName: 'VIEW';
        ObjType: obj_view;
        SystemTableName: 'RDB$RELATIONS';
        NameField: 'RDB$RELATION_NAME';
        NameSpaceField: '';
        Condition: 'RDB$RELATION_TYPE = 1';
        CommentType: ctView),
  (ObjectName: 'COLUMN';
        ObjType: -1;
        SystemTableName: 'RDB$RELATION_FIELDS';
        NameField: 'RDB$FIELD_NAME';
        NameSpaceField: 'RDB$RELATION_NAME';
        Condition: '';
        CommentType: ctColumn),
  (ObjectName: 'PARAMETER';
        ObjType: -1;
        SystemTableName: 'RDB$PROCEDURE_PARAMETERS';
        NameField: 'RDB$PARAMETER_NAME';
        NameSpaceField: 'RDB$PROCEDURE_NAME';
        Condition: '';
        CommentType: ctParameter),
  (ObjectName: 'PARAMETER';
        ObjType: -1;
        SystemTableName: 'RDB$FUNCTION_ARGUMENTS';
        NameField: 'RDB$ARGUMENT_NAME';
        NameSpaceField: 'RDB$FUNCTION_NAME';
        Condition: '';
        CommentType: ctArgument),
  (ObjectName: 'DATABASE';
        ObjType: -1;
        SystemTableName: 'RDB$DATABASE';
        NameField: '';
        NameSpaceField: '';
        Condition: '';
        CommentType: ctDatabase)
);
{ TIBExtract }

{	                ArrayDimensions
   Functional description
   Retrieves the dimensions of arrays and prints them.

  	Parameters:  fieldname -- the actual name of the array field }

function TIBExtract.GetArrayField(FieldName: String): String;
var
  qryArray : TIBSQL;
begin
  qryArray := TIBSQL.Create(FDatabase);
  Result := '';
  qryArray.SQL.Add(ArraySQL);
  qryArray.Params.ByName('FieldName').AsString := FieldName;
  qryArray.ExecQuery;

    {  Format is [lower:upper, lower:upper,..]  }

  if not qryArray.Eof then
  begin
    Result := '[';
    while not qryArray.Eof do
    begin
      if (qryArray.FieldByName('RDB$DIMENSION').AsInteger > 0) then
        Result := Result + ', ';
      Result := Result + qryArray.FieldByName('RDB$LOWER_BOUND').AsString + ':' +
             qryArray.FieldByName('RDB$UPPER_BOUND').AsString;
      qryArray.Next;
    end;
    Result := Result + ']';
  end;

  qryArray.Free;
  
end;

constructor TIBExtract.Create(AOwner: TComponent);
begin
  inherited;
  FMetaData := TStringList.Create;
  FDatabaseInfo := TIBDatabaseInfo.Create(nil);
  FDatabaseInfo.Database := FDatabase;
  if AOwner is TIBDatabase then
    Database := TIBDatabase(AOwner);
  if AOwner is TIBTransaction then
    Transaction := TIBTransaction(AOwner);
  FIncludeMetaDataComments := true;
end;

destructor TIBExtract.Destroy;
begin
  FMetaData.Free;
  FDatabasEInfo.Free;
  inherited;
end;

function TIBExtract.ExtractDDL(Flag: Boolean; TableName: String;
  ExtractTypes: TExtractTypes): Boolean;
var
	DidConnect : Boolean;
	DidStart : Boolean;
begin
  Result := true;
  DidConnect := false;
  DidStart := false;

  if not FDatabase.Connected then
  begin
    FDatabase.Connected := true;
    didConnect := true;
  end;

  ExtractOut(Format('SET SQL DIALECT %d;', [FDatabase.SQLDialect]));
  ExtractOut('SET AUTODDL ON;');
  ExtractOut('');

  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
    DidStart := true;
  end;

  if TableName <> '' then
  begin
    if not ExtractListTable(TableName, '', true) then
      Result := false;
  end
  else
  begin
    FDefaultCharSetName := '';
    ListCreateDb;
    ListFilters;
    ListDomains(dtCreateNoCheckConstraint);
    ListAllTables(flag);
    ListExternalFunctions;
    if etData in ExtractTypes then
      ListData('');
    ListIndex;
    ListForeign;
    if etData in ExtractTypes then
      ListGenerators('',[etData])
    else
      ListGenerators;
    ListViews;
    ListDomains(dtAddCheckConstraint);
    ListCheck;
    ListException;
    if DatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
      ListPackages(paHeader);
    ListProcs(pdCreateStub);
    ListFunctions(pdCreateStub);
    ListTriggers;
    if DatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
      ListPackages(paBody);
    ListProcs(pdAlterProc);
    ListFunctions(pdAlterProc);
    ExtractOut('');
    ExtractOut('/* Comments on System Objects */');
    ExtractOut('');
    ListComments([ctCollation,ctCharacterSet]);
    ListGrants(ExtractTypes);
    FDefaultCharSetName := '';
  end;

  if DidStart then
    FTransaction.Commit;

  if DidConnect then
    FDatabase.Connected := false;
end;

{                   ExtractListTable
  Functional description
  	Shows columns, types, info for a given table name
  	and text of views.
  	If a new_name is passed, substitute it for relation_name

  	relation_name -- Name of table to investigate
  	new_name -- Name of a new name for a replacement table
  	domain_flag -- extract needed domains before the table }

function TIBExtract.ExtractListTable(RelationName, NewName: String;
  DomainFlag: Boolean): Boolean;
const
  TableListSQL =
    'SELECT * FROM RDB$RELATIONS REL JOIN RDB$RELATION_FIELDS RFR ON ' + {Do Not Localize}
    '  RFR.RDB$RELATION_NAME = REL.RDB$RELATION_NAME JOIN RDB$FIELDS FLD ON ' +
    '  RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +
    'WHERE REL.RDB$RELATION_NAME = :RelationName ' +
    'ORDER BY RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';

  ConstraintSQL =
    'SELECT RCO.RDB$CONSTRAINT_NAME, RDB$CONSTRAINT_TYPE, RDB$RELATION_NAME, ' +
    'RDB$DEFERRABLE, RDB$INITIALLY_DEFERRED, RDB$INDEX_NAME, RDB$TRIGGER_NAME ' +
    'FROM RDB$RELATION_CONSTRAINTS RCO, RDB$CHECK_CONSTRAINTS CON ' +
    'WHERE ' +
    '  CON.RDB$TRIGGER_NAME = :FIELDNAME AND ' +
    '  CON.RDB$CONSTRAINT_NAME = RCO.RDB$CONSTRAINT_NAME AND ' +
    '  RCO.RDB$CONSTRAINT_TYPE = ''NOT NULL'' AND ' +
    '  RCO.RDB$RELATION_NAME = :RELATIONNAME';

  RelConstraintsSQL =
    'SELECT * FROM RDB$RELATION_CONSTRAINTS RELC ' +
    'WHERE ' +
    '  (RELC.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' OR ' +
    '  RELC.RDB$CONSTRAINT_TYPE = ''UNIQUE'') AND ' +
    '  RELC.RDB$RELATION_NAME = :RELATIONNAME ' +
    'ORDER BY RELC.RDB$CONSTRAINT_TYPE desc, RELC.RDB$CONSTRAINT_NAME';

  GetGeneratorSQL =
    'SELECT * FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = :GENERATOR';

var
  Column, Constraint, Trailer : String;
  qryTables, qryConstraints, qryRelConstraints, qryGenerators : TIBSQL;
  ValidRelation : Boolean;
  CreateTable: string;
  TableType: integer;
  Comments: TStrings;
  SQLSecurity: string;
  Columns: TStringList;
begin
  Result := true;
  ValidRelation := false;
  SQLSecurity := '';

  if DomainFlag then
    ListDomains(dtCreateDomain,RelationName);
  qryTables := TIBSQL.Create(FDatabase);
  qryConstraints := TIBSQL.Create(FDatabase);
  qryRelConstraints := TIBSQL.Create(FDatabase);
  qryGenerators := TIBSQL.Create(FDatabase);
  Comments := TStringList.Create;
  Columns := TStringList.Create;
  try
    qryTables.SQL.Add(TableListSQL);
    RelationName := trim(RelationName);
    qryTables.Params.ByName('RelationName').AsString := RelationName;
    qryTables.ExecQuery;
    qryConstraints.SQL.Add(ConstraintSQL);
    qryRelConstraints.SQL.Add(RelConstraintsSQL);
    qryGenerators.SQL.Add(GetGeneratorSQL);
    if qryTables.Eof then
      Exit; {table does not exist}

    {Format Table Declaration header}
    ValidRelation := true;
    TableType := qryTables.FieldByName('RDB$RELATION_TYPE').AsInteger;
    if (not qryTables.FieldByName('RDB$OWNER_NAME').IsNull) and
       (Trim(qryTables.FieldByName('RDB$OWNER_NAME').AsString) <> '') then
      ExtractOut(Format('%s/* Table: %s, Owner: %s */%s',
        [LineEnding, RelationName,
         TrimRight(qryTables.FieldByName('RDB$OWNER_NAME').AsString), LineEnding]));
    if TableType > 3 then
     CreateTable := 'CREATE GLOBAL TEMPORARY TABLE'
    else
      CreateTable := 'CREATE TABLE';

    if NewName <> '' then
      ExtractOut(Format('%s %s ', [CreateTable,QuoteIdentifier(NewName)]))
    else
      ExtractOut(Format('%s %s ', [CreateTable,QuoteIdentifier(RelationName)]));
    if not qryTables.FieldByName('RDB$EXTERNAL_FILE').IsNull then
      ExtractOut(Format('EXTERNAL FILE %s ',
        [QuotedStr(qryTables.FieldByName('RDB$EXTERNAL_FILE').AsString)]));
    ExtractOut('(');
    AddComment(qryTables,ctTable,Comments);

    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
      SQLSecurity := AddSQLSecurity(qryTables.FieldByName('RDB$SQL_SECURITY'));

    {add columns}
    while not qryTables.Eof do
    begin
       AddComment(qryTables,ctColumn,Comments,'RDB$DESCRIPTION1');
       Column := TAB + QuoteIdentifier(trim(qryTables.FieldByName('RDB$FIELD_NAME').AsString));

      if qryTables.FieldByName('rdb$computed_blr').IsNull then
         Column := Column + TAB + GetFieldType(qryTables);

      if not qryTables.FieldByName('rdb$computed_blr').IsNull then
      begin
        Column := Column + ' COMPUTED BY ';
       if not qryTables.FieldByName('RDB$COMPUTED_SOURCE').IsNull then
         Column := Column + PrintValidation(qryTables.FieldByName('RDB$COMPUTED_SOURCE').AsString, true);
      end
      else
      begin
        {Firebird 3 introduces IDENTITY columns. We need to check for them here}
        if qryTables.HasField('RDB$GENERATOR_NAME') and not qryTables.FieldByName('RDB$GENERATOR_NAME').IsNull then
        begin
          qryGenerators.ParamByName('GENERATOR').AsString :=  qryTables.FieldByName('RDB$GENERATOR_NAME').AsString;
          qryGenerators.ExecQuery;
          if not qryGenerators.Eof then
          begin
            Column := Column + ' GENERATED';
            if qryTables.HasField('RDB$IDENTITY_TYPE') and (qryTables.FieldByName('RDB$IDENTITY_TYPE').AsInteger = 1) then
              Column := Column + ' BY DEFAULT'
            else
              Column := Column + ' ALWAYS';
            Column := Column + ' AS IDENTITY' + Format(' (START WITH %d',
                     [qryGenerators.FieldByName('RDB$INITIAL_VALUE').AsInt64]);
            {INCREMENT BY Added in Firebird 4}
            if (FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13) and qryGenerators.HasField('RDB$GENERATOR_INCREMENT') then
            Column := Column + Format(' INCREMENT BY %d',
                     [qryGenerators.FieldByName('RDB$GENERATOR_INCREMENT').AsInt64]);
            Column := Column + ')';
          end;
          qryGenerators.Close;
        end;

        { Handle defaults for columns }
        { Originally This called PrintMetadataTextBlob,
            should no longer need }
        if not qryTables.FieldByName('RDB$DEFAULT_SOURCE').IsNull then
          Column := Column + ' ' + qryTables.FieldByName('RDB$DEFAULT_SOURCE').AsString;


        { The null flag is either 1 or null (for nullable) .  if there is
          a constraint name, print that too.  Domains cannot have named
          constraints.  The column name is in rdb$trigger_name in
          rdb$check_constraints.  We hope we get at most one row back. }

        if qryTables.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then
        begin
          qryConstraints.Params.ByName('FIELDNAME').AsString := qryTables.FieldByName('RDB$FIELD_NAME').AsString;
          qryConstraints.Params.ByName('RELATIONNAME').AsString := qryTables.FieldByName('RDB$RELATION_NAME').AsString;
          qryConstraints.ExecQuery;

          while not qryConstraints.Eof do
          begin
            if Pos('INTEG', qryConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString) <> 1 then
              Column := Column + Format(' CONSTRAINT %s',
                [ QuoteIdentifier(
                      qryConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString)]);
            qryConstraints.Next;
          end;
          qryConstraints.Close;
          Column := Column + ' NOT NULL';
        end;

        if not qryTables.FieldByName('RDB$CHARACTER_SET_ID').IsNull and
           not qryTables.FieldByName('RDB$COLLATION_ID').IsNull then
          Column := Column + GetCollationName(qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                                              qryTables.FieldByName('RDB$COLLATION_ID').AsInteger);

      end;
      qryTables.Next;
      if not qryTables.Eof then
        Column := Column + ',';
      Columns.Add(Column);
    end;

    { Do primary and unique keys only. references come later }

    qryRelConstraints.Params.ByName('relationname').AsString := RelationName;
    qryRelConstraints.ExecQuery;
    while not qryRelConstraints.Eof do
    begin
      Constraint := '';
      Columns.Strings[Columns.Count - 1] := Columns.Strings[Columns.Count - 1]  + ',';
      { If the name of the constraint is not INTEG..., print it }
      if Pos('INTEG', qryRelConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString) <> 1 then
        Constraint := Constraint + 'CONSTRAINT ' +
          QuoteIdentifier(
          qryRelConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString) + ' ';


      if Pos('PRIMARY', qryRelConstraints.FieldByName('RDB$CONSTRAINT_TYPE').AsString) = 1 then
      begin
        Columns.Add(Constraint + Format('PRIMARY KEY (%s)',
           [GetIndexSegments(qryRelConstraints.FieldByName('RDB$INDEX_NAME').AsString)]));
      end
      else
        if Pos('UNIQUE', qryRelConstraints.FieldByName('RDB$CONSTRAINT_TYPE').AsString) = 1 then
        begin
          Columns.Add(Constraint + Format('UNIQUE (%s)',
             [GetIndexSegments(qryRelConstraints.FieldByName('RDB$INDEX_NAME').AsString)]));
        end;
      qryRelConstraints.Next;
    end;
    Trailer := '';
    if ValidRelation then
    begin
      if TableType = 4 then
        Trailer := ' ) ON COMMIT PRESERVE ROWS '
      else
       Trailer := ')';
    end;
    {SQL Security added in Firebird 4}
    if SQLSecurity <> '' then
      Trailer := Trailer + SQLSecurity;

    Trailer := Trailer + TERM;

    Columns.Add(Trailer);
    ExtractOut(Columns);

    ExtractOut(Comments);
  finally
    Columns.Free;
    Comments.Free;
    qryTables.Free;
    qryConstraints.Free;
    qryRelConstraints.Free;
    qryGenerators.Free;
  end;
end;

{	           ExtractListView
  Functional description
   	Show text of the specified view.
   	Use a SQL query to get the info and print it.
 	  Note: This should also contain check option }

procedure TIBExtract.ExtractListView(ViewName: String);
const
  ViewsSQL = 'SELECT * FROM RDB$RELATIONS REL ' +
             ' WHERE ' +
             '  (REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
             '  NOT REL.RDB$VIEW_BLR IS NULL AND ' +
             '  REL.RDB$RELATION_NAME = :VIEWNAME AND ' +
             '  REL.RDB$FLAGS = 1 ' +
             'ORDER BY REL.RDB$RELATION_ID ';

  ColumnsSQL = 'SELECT * FROM RDB$RELATION_FIELDS RFR ' +
               'WHERE ' +
               '  RFR.RDB$RELATION_NAME = :RELATIONNAME ' +
               'ORDER BY RFR.RDB$FIELD_POSITION ';

var
  qryViews, qryColumns : TIBSQL;
  RelationName, ColList : String;
  Comments: TStrings;
begin
  ColList := '';
  qryViews := TIBSQL.Create(FDatabase);
  qryColumns := TIBSQL.Create(FDatabase);
  Comments := TStringList.Create;
  try
    qryViews.SQL.Add(ViewsSQL);
    qryViews.Params.ByName('viewname').AsString := ViewName;
    qryViews.ExecQuery;
    while not qryViews.Eof do
    begin
      ExtractOut('');
      AddComment(qryViews,ctView,Comments);
      RelationName := QuoteIdentifier(trim(
          qryViews.FieldByName('RDB$RELATION_NAME').AsString));
      ExtractOut(Format('%s/* View: %s, Owner: %s */%s', [
        RelationName,
        Trim(qryViews.FieldByName('RDB$OWNER_NAME').AsString)]));
      ExtractOut('');
      ExtractOut(Format('CREATE VIEW %s (', [RelationName]));

      { Get Column List}
      qryColumns.SQL.Add(ColumnsSQL);
      qryColumns.Params.ByName('relationname').AsString := RelationName;
      qryColumns.ExecQuery;
      while not qryColumns.Eof do
      begin
        AddComment(qryColumns,ctColumn,Comments);
        ColList := ColList + QuoteIdentifier(
              qryColumns.FieldByName('RDB$FIELD_NAME').AsString);
        qryColumns.Next;
        if not qryColumns.Eof then
          ColList := ColList + ', ';
      end;
      ExtractOut(ColList + ') AS');
      ExtractOut(qryViews.FieldByName('RDB$VIEW_SOURCE').AsString + Term);
      qryViews.Next;
    end;
  finally
    ExtractOut(Comments);
    Comments.Free;
    qryViews.Free;
    qryColumns.Free;
  end;
end;

function TIBExtract.GetCharacterSets(CharSetId, Collation: integer;
  CollateOnly: Boolean): String;
const
  CollationSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME, COL.RDB$COLLATION_NAME, CST.RDB$DEFAULT_COLLATE_NAME ' +
    'FROM RDB$COLLATIONS COL JOIN RDB$CHARACTER_SETS CST ON ' +
    '  COL.RDB$CHARACTER_SET_ID = CST.RDB$CHARACTER_SET_ID ' +
    'WHERE ' +
    '  COL.RDB$COLLATION_ID = :COLLATION AND ' +
    '  CST.RDB$CHARACTER_SET_ID = :CHAR_SET_ID ' +
    'ORDER BY COL.RDB$COLLATION_NAME, CST.RDB$CHARACTER_SET_NAME';

  NonCollationSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME ' +
    'FROM RDB$CHARACTER_SETS CST ' +
    'WHERE CST.RDB$CHARACTER_SET_ID = :CHARSETID ' +
    'ORDER BY CST.RDB$CHARACTER_SET_NAME';

var
  CharSetSQL : TIBSQL;
  DidActivate : Boolean;
begin
  Result := '';
  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
    DidActivate := true;
  end
  else
    DidActivate := false;
  CharSetSQL := TIBSQL.Create(FDatabase);
  try
    if Collation <> 0 then
    begin
      CharSetSQL.SQL.Add(CollationSQL);
      CharSetSQL.Params.ByName('Char_Set_Id').AsInteger := CharSetId;
      CharSetSQL.Params.ByName('Collation').AsInteger := Collation;
      CharSetSQL.ExecQuery;

      { Is specified collation the default collation for character set? }
      if (Trim(CharSetSQL.FieldByName('RDB$DEFAULT_COLLATE_NAME').AsString) =
         Trim(CharSetSQL.FieldByName('RDB$COLLATION_NAME').AsString)) then
      begin
        if not CollateOnly then
          Result := ' CHARACTER SET ' + Trim(CharSetSQL.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
      end
      else
        if CollateOnly then
          Result := ' COLLATE ' + Trim(CharSetSQL.FieldByName('RDB$COLLATION_NAME').AsString)
        else
          Result := ' CHARACTER SET ' +
            Trim(CharSetSQL.FieldByName('RDB$CHARACTER_SET_NAME').AsString) +
            ' COLLATE ' +
            Trim(CharSetSQL.FieldByName('RDB$COLLATION_NAME').AsString);
    end
    else
      if CharSetId <> 0 then
      begin
        CharSetSQL.SQL.Add(NonCollationSQL);
        CharSetSQL.Params.ByName('CharSetId').AsShort := CharSetId;
        CharSetSQL.ExecQuery;
        Result := ' CHARACTER SET ' + Trim(CharSetSQL.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
      end;
  finally
    CharSetSQL.Free;
  end;
  if DidActivate then
    FTransaction.Commit;
end;

function TIBExtract.AddSQLSecurity(SQLSecurity: ISQLData): string;
begin
  Result := '';
  if (FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13) and not SQLSecurity.IsNull then
  begin
    if SQLSecurity.AsBoolean then
      Result := ' SQL SECURITY DEFINER'
    else
    Result := ' SQL SECURITY INVOKER';
  end;
end;

procedure TIBExtract.Add2MetaData(const Msg: string; IsError: boolean);
begin
  ExtractOut(Msg);
end;

function TIBExtract.LookupDDLObject(cType: TCommentType): integer;
begin
  for Result := Low(DDLObjects) to High(DDLObjects) do
  begin
    if DDLObjects[Result].CommentType = cType then Exit;
  end;
  Result := -1;
end;


procedure TIBExtract.AddComment(Query: TIBSQL; cType: TCommentType;
  OutStrings: TStrings; CommentFieldName: string);
var cmt: string;
    index: integer;
begin
  if IncludeMetaDataComments and
      Query.HasField(CommentFieldName) and not Query.FieldByName(CommentFieldName).IsNull then
  begin
    index := LookupDDLObject(cType);
    if index = -1 then Exit;

    with DDLObjects[index] do
    begin
      cmt := 'COMMENT ON ' + ObjectName + ' ';
      if NameSpaceField <> '' then
        cmt += QuoteIdentifier(query.FieldByName(NameSpaceField).AsString) + '.';
      if NameField <> '' then
        cmt += QuoteIdentifier(query.FieldByName(NameField).AsString);
    end;

    cmt += ' IS ''' + SQLSafeString(Query.FieldByName(CommentFieldName).AsString) + '''' + TERM;
    OutStrings.Add(cmt);
  end;
end;

procedure TIBExtract.AddComment(Query: TIBSQL; cType: TCommentType;
  CommentFieldName: string);
begin
  AddComment(Query,cType,FMetaData,CommentFieldName);
end;

procedure TIBExtract.ExtractOut(DDLLine: string);
var index: integer;
begin
  index := FMetaData.Count;
  FMetaData.Add(DDLLine);
  if assigned(FOnExtractLines) then
    OnExtractLines(self,index,FMetaData.Count-index);
end;

procedure TIBExtract.ExtractOut(DDLStrings: TStrings);
var index: integer;
begin
  index := FMetaData.Count;
  FMetaData.AddStrings(DDLStrings);
  if assigned(FOnExtractLines) then
    OnExtractLines(self,index,FMetaData.Count-index);
end;

function TIBExtract.GetCollationName(CharacterSetID, CollationID: integer
  ): string;
const
  CollationSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME, COL.RDB$COLLATION_NAME, CST.RDB$DEFAULT_COLLATE_NAME ' +
    'FROM RDB$COLLATIONS COL JOIN RDB$CHARACTER_SETS CST ON ' +
    '  COL.RDB$CHARACTER_SET_ID = CST.RDB$CHARACTER_SET_ID ' +
    'WHERE ' +
    '  COL.RDB$COLLATION_ID = :COLLATION AND ' +
    '  CST.RDB$CHARACTER_SET_ID = :CHAR_SET_ID ' +
    'ORDER BY COL.RDB$COLLATION_NAME, CST.RDB$CHARACTER_SET_NAME';

var CharSetSQL : TIBSQL;
begin
  Result := '';
  CharSetSQL := TIBSQL.Create(FDatabase);
  try
   CharSetSQL.SQL.Add(CollationSQL);
   CharSetSQL.Params.ByName('Char_Set_Id').AsInteger := CharacterSetID;
   CharSetSQL.Params.ByName('Collation').AsInteger := CollationID;
   CharSetSQL.ExecQuery;

   { Is specified collation the default collation for character set? }
   if (Trim(CharSetSQL.FieldByName('RDB$DEFAULT_COLLATE_NAME').AsString) <>
                  Trim(CharSetSQL.FieldByName('RDB$COLLATION_NAME').AsString)) then
     Result := ' COLLATE ' + Trim(CharSetSQL.FieldByName('RDB$COLLATION_NAME').AsString)
  finally
    CharSetSQL.Free;
  end;
end;

function TIBExtract.GetDatabase: TIBDatabase;
begin
  result := FDatabase;
end;

 {	          GetIndexSegments
   Functional description
  	returns the list of columns in an index. }

function TIBExtract.GetIndexSegments(indexname: String): String;
const
  IndexNamesSQL =
    'SELECT * FROM RDB$INDEX_SEGMENTS SEG ' +
    'WHERE SEG.RDB$INDEX_NAME = :INDEXNAME ' +
    'ORDER BY SEG.RDB$FIELD_POSITION';

var
  qryColNames : TIBSQL;
begin
{ Query to get column names }
  Result := '';
  qryColNames := TIBSQL.Create(FDatabase);
  try
    qryColNames.SQL.Add(IndexNamesSQL);
    qryColNames.Params.ByName('IndexName').AsString := IndexName;
    qryColNames.ExecQuery;
    while not qryColNames.Eof do
    begin
      { Place a comma and a blank between each segment column name }

      Result := Result + QuoteIdentifier(
        qryColNames.FieldByName('RDB$FIELD_NAME').AsString);
      qryColNames.Next;
      if not qryColNames.Eof then
        Result := Result + ', ';
    end;
  finally
    qryColNames.Free;
  end;
end;

function TIBExtract.GetTransaction: TIBTransaction;
begin
  Result := FTransaction;
end;

function TIBExtract.GetTriggerType(TypeID: Int64): string;
const
  AllDDLTriggers = $7FFFFFFFFFFFDFFF shr 1;
var separator: string;
    i: integer;

  function GetDDLEvent(Phase: TTriggerPhase; ObjectName: string): string;
  begin
    Result := '';
    case Phase of
    tpCreate:
     Result := separator + 'CREATE ' + ObjectName;
    tpAlter:
     Result := separator + 'ALTER ' + ObjectName;
    tpDrop:
     Result := separator + 'Drop ' + ObjectName;
    end;
    if Result <> '' then
      separator := ' OR ';
  end;

begin
  if TypeID and $2000 <> 0 then
  {database trigger}
  begin
    Result := 'ON ';
    case TypeID of
    $2000:
      Result += 'CONNECT';
    $2001:
      Result += 'DISCONNECT';
    $2002:
      Result +='TRANSACTION START';
    $2003:
      Result += 'TRANSACTION COMMIT';
    $2004:
      Result += 'TRANSACTION ROLLBACK';
    end;
  end
  else
  if TypeID and $4000 <> 0 then
  {DDL Trigger}
  begin
    if TypeID and $01 <> 0 then
      Result := 'AFTER '
    else
      Result := 'BEFORE ';
    TypeID := TypeID shr 1;
    separator := '';
    i := 0;
    if TypeID = AllDDLTriggers then
      Result += 'ANY DDL STATEMENT'
    else
      repeat
        if (DDLTriggers[i].Bits > 0) and (TypeID and $01 <> 0) then
         Result += GetDDLEvent(DDLTriggers[i].Bit1,DDLTriggers[i].ObjectName);

        if (DDLTriggers[i].Bits > 1) and (TypeID and $02 <> 0) then
          Result += GetDDLEvent(DDLTriggers[i].Bit2,DDLTriggers[i].ObjectName);

        if (DDLTriggers[i].Bits > 2) and (TypeID and $04 <> 0) then
          Result += GetDDLEvent(DDLTriggers[i].Bit3,DDLTriggers[i].ObjectName);
        TypeID := TypeID shr DDLTriggers[i].Bits;
        Inc(i);
      until TypeID = 0;
  end
  else
  {Normal Trigger}
  begin
    Inc(TypeID);
    if TypeID and $01 <> 0 then
      Result := 'AFTER '
    else
      Result := 'BEFORE ';
    TypeID := TypeID shr 1;
    separator := '';
    repeat
      Result += separator;
      separator := ' or ';
      case TypeID and $03 of
      1:
        Result += 'INSERT';
      2:
        Result += 'UPDATE';
      3:
        Result += 'DELETE';
      end;
      TypeID := TypeID shr 2;
    until TypeID = 0
  end
end;

{	   ListAllGrants
  Functional description
 	 Print the permissions on all user tables.
 	 Get separate permissions on table/views and then procedures }

procedure TIBExtract.ListGrants(ExtractTypes: TExtractTypes);
const
  SecuritySQL = 'SELECT * FROM RDB$RELATIONS ' +
                'WHERE ' +
                '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
                '  RDB$SECURITY_CLASS STARTING WITH ''SQL$'' ' +
                'ORDER BY RDB$RELATION_NAME';

  DomainSQL  = 'select RDB$FIELD_NAME from RDB$FIELDS '+
    'where RDB$SYSTEM_FLAG <> 1 and RDB$FIELD_NAME not Similar to ''RDB$%|SEC$%|MON$%|SQL$%'' '+
    'order BY RDB$FIELD_NAME';

  CharacterSetSQL = 'Select * From RDB$CHARACTER_SETS  '+
                    'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                    'Order by RDB$CHARACTER_SET_NAME';

  CollationsSQL = 'Select * From RDB$COLLATIONS  '+
                    'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                    'Order by RDB$COLLATION_NAME';

  ProcedureSQL = 'select * from RDB$PROCEDURES '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$PROCEDURE_NAME';

  PackagesSQL = 'select * from RDB$PACKAGES '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$PACKAGE_NAME';

  ExceptionSQL = 'select * from RDB$EXCEPTIONS '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$EXCEPTION_NAME';

  GeneratorSQL = 'select * from RDB$GENERATORS '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$GENERATOR_NAME';

  MetadataGrantsSQL =
  'Select PR.RDB$USER, PR.RDB$USER_TYPE, '+
  'T.RDB$TYPE_NAME as USER_TYPE_NAME, '+
  'Case PR.RDB$PRIVILEGE '+
  '  When ''C'' then ''CREATE'' '+
  '  When ''O'' then ''DROP ANY'' '+
  '  When ''L'' then ''ALTER ANY''  End as Privilege, '+
  'Case PR.RDB$RELATION_NAME '+
  '  When ''SQL$COLLATIONS'' then ''COLLATION'' '+
  '  When ''SQL$CHARSETS'' then ''CHARACTER SET'' '+
  '  When ''SQL$DATABASE'' then ''DATABASE'' '+
  '  When ''SQL$DOMAINS'' then ''DOMAIN'' '+
  '  When ''SQL$EXCEPTIONS'' then ''EXCEPTION'' '+
  '  When ''SQL$FILTERS'' then ''FILTER'' '+
  '  When ''SQL$FUNCTIONS'' then ''FUNCTION'' '+
  '  When ''SQL$GENERATORS'' then ''GENERATOR'' '+
  '  When ''SQL$PACKAGES'' then ''PACKAGE'' '+
  '  When ''SQL$PROCEDURES'' then ''PROCEDURE'' '+
  '  When ''SQL$ROLES'' then ''ROLE'' '+
  '  When ''SQL$VIEWS'' then ''VIEW'' '+
  '  When ''SQL$TABLES'' then ''TABLE'' End as METAOBJECTNAME,'+

  'case when coalesce(RDB$GRANT_OPTION,0) <> 0 then '' WITH GRANT OPTION'' '+
  'ELSE '''' End as GRANTOPTION '+
  'FROM RDB$USER_PRIVILEGES PR '+
  'JOIN RDB$TYPES T On T.RDB$TYPE = PR.RDB$USER_TYPE and T.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE'' '+
  'Where PR.RDB$RELATION_NAME like ''SQL$%'' and PR.RDB$PRIVILEGE in (''L'',''C'',''O'')';

var
  qryRoles : TIBSQL;
  RelationName : String;
begin
  ListRoles;
  qryRoles := TIBSQL.Create(FDatabase);
  try
  { This version of cursor gets only sql tables identified by security class
     and misses views, getting only null view_source }

    ExtractOut('');
    ExtractOut('/* Grant permissions for this database */');
    ExtractOut('');

    try
      qryRoles.SQL.Text := SecuritySQL;
      qryRoles.ExecQuery;
      while not qryRoles.Eof do
      begin
        RelationName := Trim(qryRoles.FieldByName('rdb$relation_Name').AsString);
        ShowGrants(RelationName, Term, not (etGrantsToUser in ExtractTypes));
        qryRoles.Next;
      end;
    finally
     qryRoles.Close;
    end;

    if etGrantsToUser in ExtractTypes then
      ShowGrantRoles(Term);

    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
    begin
      qryRoles.SQL.Text := DomainSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$FIELD_NAME').AsString), Term,
                  not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;

      qryRoles.SQL.Text := CharacterSetSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$CHARACTER_SET_NAME').AsString), Term,
              not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;

      qryRoles.SQL.Text := CollationsSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$COLLATION_NAME').AsString), Term,
                 not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;

      qryRoles.SQL.Text := ExceptionSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$EXCEPTION_NAME').AsString), Term,
                     not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;

      qryRoles.SQL.Text := GeneratorSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$GENERATOR_NAME').AsString), Term,
                 not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;
    end;

    qryRoles.SQL.Text := ProcedureSQL;
    qryRoles.ExecQuery;
    try
      while not qryRoles.Eof do
      begin
        ShowGrants(Trim(qryRoles.FieldByName('RDB$PROCEDURE_NAME').AsString), Term,
                not (etGrantsToUser in ExtractTypes));
        qryRoles.Next;
      end;
    finally
      qryRoles.Close;
    end;

    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
    begin
      qryRoles.SQL.Text := PackagesSQL;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          ShowGrants(Trim(qryRoles.FieldByName('RDB$PACKAGE_NAME').AsString), Term,
                  not (etGrantsToUser in ExtractTypes));
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;
    end;

    {Metadata Grants}
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
    begin
      qryRoles.SQL.Text := MetadataGrantsSQL;
      qryRoles.ExecQuery;
      while not qryRoles.Eof do
      begin
        if (etGrantsToUser in ExtractTypes) or
           (qryRoles.FieldByName('RDB$USER_TYPE').AsInteger <> obj_user) or
           (qryRoles.FieldByName('RDB$USER').AsString = 'PUBLIC') then
        ExtractOut(Format('GRANT %s %s TO %s %s %s%s', [
                              TrimRight(qryRoles.FieldByName('Privilege').AsString),
                              TrimRight(qryRoles.FieldByName('METAOBJECTNAME').AsString),
                              TrimRight(qryRoles.FieldByName('USER_TYPE_NAME').AsString),
                              QuoteIdentifier(TrimRight(qryRoles.FieldByName('RDB$USER').AsString)),
                              TrimRight(qryRoles.FieldByName('GRANTOPTION').AsString),
                              Term]));
        qryRoles.Next;
      end;
      qryRoles.Close;
    end;
  finally
    qryRoles.Free;
  end;
end;

procedure TIBExtract.ListPackages(PackageDDLType: TPackageDDLType;
  PackageName: string; IncludeGrants: boolean);
const
  PackageSQL = 'Select * From RDB$PACKAGES Where RDB$SYSTEM_FLAG = 0 order by RDB$PACKAGE_NAME';
  PackageNameSQL = 'Select * From RDB$PACKAGES Where RDB$PACKAGE_NAME = :PackageName order by RDB$PACKAGE_NAME';
  PackageHeaderSQL = 'CREATE PACKAGE %s%s%sAS%s';
  PackageBodySQL = 'CREATE PACKAGE BODY %s%sAS%s';
var
  qryPackages : TIBSQL;
  Header : Boolean;
  SList : TStrings;
  aPackageName: string;
  Comments: TStrings;
  SQLSecurity: string;
begin
  Header := true;
  qryPackages := TIBSQL.Create(FDatabase);
  Comments := TStringList.Create;
  SList := TStringList.Create;
  try
    if PackageName = '' then
      qryPackages.SQL.Text := PackageSQL
    else
    begin
      qryPackages.SQL.Text := PackageNameSQL;
      qryPackages.ParamByName('PackageName').AsString := PackageName;
    end;

    qryPackages.ExecQuery;
    while not qryPackages.Eof do
    begin
      if Header then
      begin
        ExtractOut('COMMIT WORK;');
        ExtractOut('SET AUTODDL OFF;');
        ExtractOut(Format('SET TERM %s %s', [ProcTerm, Term]));
        ExtractOut(Format('%s/* Package Definitions */%s', [LineEnding, LineEnding]));
        Header := false;
      end;

      AddComment(qryPackages,ctPackage,Comments);
      aPackageName := qryPackages.FieldByName('RDB$PACKAGE_NAME').AsString;
      if PackageDDLType in [paHeader,paBoth] then
      begin
         {SQL Security added in Firebird 4}
        if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
          SQLSecurity := AddSQLSecurity(qryPackages.FieldByName('RDB$SQL_SECURITY'))
        else
          SQLSecurity := '';

       ExtractOut(Format(PackageHeaderSQL,[aPackageName,SQLSecurity,
                                               LineEnding,LineEnding]));
        SList.Text :=  qryPackages.FieldByName('RDB$PACKAGE_HEADER_SOURCE').AsString;
        SList.Add(Format(' %s%s', [ProcTerm, LineEnding]));
        ExtractOut(SList);
      end;

      if PackageDDLType in [paBody,paBoth] then
      begin
        ExtractOut(Format(PackageBodySQL,[aPackageName,
                                               LineEnding,LineEnding]));
        SList.Text :=  qryPackages.FieldByName('RDB$PACKAGE_BODY_SOURCE').AsString;
        SList.Add(Format(' %s%s', [ProcTerm, LineEnding]));
        ExtractOut(SList);
      end;

      if IncludeGrants then
        ShowGrantsTo(aPackageName,obj_package,ProcTerm);
      qryPackages.Next;
    end;
    qryPackages.Close;

    if not Header then
    begin
      ExtractOut(Format('SET TERM %s %s', [Term, ProcTerm]));
      ExtractOut('COMMIT WORK;');
      ExtractOut('SET AUTODDL ON;');
    end;
    ExtractOut(Comments);
  finally
    Comments.Free;
    SList.Free;
    qryPackages.Free;
  end;
end;

{	  ListAllProcs
  Functional description
  	Shows text of a stored procedure given a name.
  	or lists procedures if no argument.
 	 Since procedures may reference each other, we will create all
  	dummy procedures of the correct name, then alter these to their
  	correct form.
       Add the parameter names when these procedures are created.

 	 procname -- Name of procedure to investigate }

procedure TIBExtract.ListProcs(ProcDDLType: TProcDDLType;
  ProcedureName: String; IncludeGrants: boolean);
const
  CreateProcedureStr1 = 'CREATE PROCEDURE %s ';
  CreateProcedureStr2 = 'BEGIN EXIT; END';
  CreateProcedureStr3 = 'BEGIN SUSPEND; EXIT; END';
  ProcedureSQL =  {Order procedures by dependency order and then procedure name}
                  'with recursive Procs as ( ' +
                  'Select RDB$PROCEDURE_NAME, 1 as ProcLevel from RDB$PROCEDURES ' +
                  'UNION ALL ' +
                  'Select D.RDB$DEPENDED_ON_NAME, ProcLevel + 1 From RDB$DEPENDENCIES D ' +
                  'JOIN Procs on Procs.RDB$PROCEDURE_NAME = D.RDB$DEPENDENT_NAME ' +
                  '  and Procs.RDB$PROCEDURE_NAME <> D.RDB$DEPENDED_ON_NAME ' +
                  'JOIN RDB$PROCEDURES P On P.RDB$PROCEDURE_NAME = D.RDB$DEPENDED_ON_NAME ' +
                  '  ) ' +
                  'SELECT * FROM RDB$PROCEDURES P ' +
                  'JOIN ( ' +
                  'Select RDB$PROCEDURE_NAME, max(ProcLevel) as ProcLevel From Procs ' +
                  'Group By RDB$PROCEDURE_NAME) A On A.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME ' +
                  'Order by A.ProcLevel desc, P.RDB$PROCEDURE_NAME asc';

  ProcedureSQLODS12 =  {Order procedures by dependency order and then procedure name}
                  'with recursive Procs as ( ' +
                  'Select RDB$PROCEDURE_NAME, 1 as ProcLevel from RDB$PROCEDURES ' +
                  'UNION ALL ' +
                  'Select D.RDB$DEPENDED_ON_NAME, ProcLevel + 1 From RDB$DEPENDENCIES D ' +
                  'JOIN Procs on Procs.RDB$PROCEDURE_NAME = D.RDB$DEPENDENT_NAME ' +
                  '  and Procs.RDB$PROCEDURE_NAME <> D.RDB$DEPENDED_ON_NAME ' +
                  'JOIN RDB$PROCEDURES P On P.RDB$PROCEDURE_NAME = D.RDB$DEPENDED_ON_NAME ' +
                  '  ) ' +
                  'SELECT * FROM RDB$PROCEDURES P ' +
                  'JOIN ( ' +
                  'Select RDB$PROCEDURE_NAME, max(ProcLevel) as ProcLevel From Procs ' +
                  'Group By RDB$PROCEDURE_NAME) A On A.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME ' +
                  'Where P.RDB$PACKAGE_NAME is NULL '+
                  'Order by A.ProcLevel desc, P.RDB$PROCEDURE_NAME asc';
  ProcedureNameSQL =
    'SELECT * FROM RDB$PROCEDURES ' +
    'WHERE RDB$PROCEDURE_NAME = :ProcedureName ' +
    'ORDER BY RDB$PROCEDURE_NAME';

  ProcedureSecuritySQL = 'Select RDB$SQL_SECURITY From RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :ProcedureName';

var
  qryProcedures : TIBSQL;
  qryProcSecurity: TIBSQL;
  ProcName : String;
  SList : TStrings;
  Header : Boolean;
  Comments: TStrings;
begin
  Header := true;
  qryProcedures := TIBSQL.Create(FDatabase);
  qryProcSecurity := TIBSQL.Create(FDatabase);
  Comments := TStringList.Create;
  SList := TStringList.Create;
  try
    if ProcedureName = '' then
    begin
      if DatabaseInfo.ODSMajorVersion < ODS_VERSION12 then
        qryProcedures.SQL.Text := ProcedureSQL
      else
        qryProcedures.SQL.Text := ProcedureSQLODS12;
    end
    else
    begin
      qryProcedures.SQL.Text := ProcedureNameSQL;
      qryProcedures.Params.ByName('ProcedureName').AsString := ProcedureName;
    end;

    qryProcedures.ExecQuery;
    while not qryProcedures.Eof do
    begin
      if Header then
      begin
        ExtractOut('COMMIT WORK;');
        ExtractOut('SET AUTODDL OFF;');
        ExtractOut(Format('SET TERM %s %s', [ProcTerm, Term]));
        ExtractOut('');
        if ProcDDLType in [pdCreateStub,pdCreateProc] then
          ExtractOut('/* Stored procedures Definitions*/')
        else
          ExtractOut('/* Stored procedure Bodies */');
        ExtractOut('');
        Header := false;
      end;
      ProcName := Trim(qryProcedures.FieldByName('RDB$PROCEDURE_NAME').AsString);

      case ProcDDLType of
      pdCreateStub:
        begin
          AddComment(qryProcedures,ctProcedure,Comments);
          ExtractOut(Format(CreateProcedureStr1, [QuoteIdentifier(
             ProcName)]));
          GetProcedureArgs(ProcName);
          if qryProcedures.FieldByName('RDB$PROCEDURE_TYPE').AsInteger = 1 then
            ExtractOut(CreateProcedureStr3)
          else
            ExtractOut(CreateProcedureStr2);

          {SQL Security added in Firebird 4}
          if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
          begin
            qryProcSecurity.SQL.Text := ProcedureSecuritySQL;
            qryProcSecurity.Params.ByName('ProcedureName').AsString := ProcName;
            qryProcSecurity.ExecQuery;
            ExtractOut(AddSQLSecurity(qryProcSecurity.FieldByName('RDB$SQL_SECURITY')));
          end;
          ExtractOut(ProcTerm+ LineEnding);
        end;

      pdCreateProc:
      begin
        AddComment(qryProcedures,ctProcedure,Comments);
        ExtractOut(Format(CreateProcedureStr1, [QuoteIdentifier(
           ProcName)]));

        GetProcedureArgs(ProcName);

        if not qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').IsNull then
        begin
          SList.Text := qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').AsString;
          ExtractOut(SList);
        end
        else
          ExtractOut(CreateProcedureStr2);

        {SQL Security added in Firebird 4}
        if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
        begin
          qryProcSecurity.SQL.Text := ProcedureSecuritySQL;
          qryProcSecurity.Params.ByName('ProcedureName').AsString := ProcName;
          qryProcSecurity.ExecQuery;
          ExtractOut(AddSQLSecurity(qryProcSecurity.FieldByName('RDB$SQL_SECURITY')));
        end;

        ExtractOut(ProcTerm + LineEnding);

      end;

      pdAlterProc:
       begin
         ExtractOut(Format('%sALTER PROCEDURE %s ', [LineEnding,
            QuoteIdentifier( ProcName)]));
         GetProcedureArgs(ProcName);

         if not qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').IsNull then
         begin
           SList.Text := qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').AsString;
           SList.Add(Format(' %s%s', [ProcTerm, LineEnding]));
           ExtractOut(SList);
         end
         else
           ExtractOut(Format(CreateProcedureStr2, [ProcTerm, LineEnding]));
       end;
      end;
      if IncludeGrants then
        ShowGrantsTo(ProcName,obj_procedure,ProcTerm);
      qryProcedures.Next;
    end;
    qryProcedures.Close;

    if not Header then
    begin
      ExtractOut(Format('SET TERM %s %s', [Term, ProcTerm]));
      ExtractOut('COMMIT WORK;');
      ExtractOut('SET AUTODDL ON;');
    end;
    ExtractOut(Comments);
  finally
    qryProcedures.Free;
    qryProcSecurity.Free;
    SList.Free;
    Comments.Free;
  end;
end;

{            	  ListAllTables
  Functional description
  	Extract the names of all user tables from
 	 rdb$relations.  Filter SQL tables by
  	security class after we fetch them
  	Parameters:  flag -- 0, get all tables }

procedure TIBExtract.ListAllTables(flag: Boolean);
const
  TableSQL =
    'SELECT * FROM RDB$RELATIONS ' +
    'WHERE ' +
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  RDB$VIEW_BLR IS NULL ' +
    'ORDER BY RDB$RELATION_NAME';

var
  qryTables : TIBSQL;
begin
{ This version of cursor gets only sql tables identified by security class
   and misses views, getting only null view_source }

   qryTables := TIBSQL.Create(FDatabase);
   try
     qryTables.SQL.Text := TableSQL;
     qryTables.ExecQuery;
     while not qryTables.Eof do
     begin
       if ((qryTables.FieldByName('RDB$FLAGS').AsInteger <> 1) and
           (not Flag)) then
         continue;
       if flag or (Pos('SQL$', qryTables.FieldByName('RDB$SECURITY_CLASS').AsString) <> 1) then
	       ExtractListTable(qryTables.FieldByName('RDB$RELATION_NAME').AsString,
           '', false);

       qryTables.Next;
     end;
   finally
     qryTables.Free;
   end;
end;

{	 ListAllTriggers
  Functional description
  	Lists triggers in general on non-system
  	tables with sql source only. }

procedure TIBExtract.ListTriggers(ObjectName: String; ExtractTypes: TExtractTypes
  );
const
{ Query gets the trigger info for non-system triggers with
   source that are not part of an SQL constraint }

  TriggerSQL =
    'SELECT * FROM RDB$TRIGGERS TRG Left Outer JOIN RDB$RELATIONS REL ON ' +
    '  TRG.RDB$RELATION_NAME = REL.RDB$RELATION_NAME ' +
    'WHERE ' +
    ' (REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$CHECK_CONSTRAINTS CHK WHERE ' +
    '     TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME) ' +
    'ORDER BY TRG.RDB$RELATION_NAME, TRG.RDB$TRIGGER_TYPE, ' +
    '    TRG.RDB$TRIGGER_SEQUENCE, TRG.RDB$TRIGGER_NAME';

  TriggerNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$RELATIONS REL ON ' +
    '  TRG.RDB$RELATION_NAME = REL.RDB$RELATION_NAME ' +
    'WHERE ' +
    ' REL.RDB$RELATION_NAME = :TableName AND ' +
    ' (REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$CHECK_CONSTRAINTS CHK WHERE ' +
    '     TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME) ' +
    'ORDER BY TRG.RDB$RELATION_NAME, TRG.RDB$TRIGGER_TYPE, ' +
    '    TRG.RDB$TRIGGER_SEQUENCE, TRG.RDB$TRIGGER_NAME';

  TriggerByNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG Left Outer JOIN RDB$RELATIONS REL ON ' +
    '  TRG.RDB$RELATION_NAME = REL.RDB$RELATION_NAME ' +
    'WHERE ' +
    ' TRG.RDB$TRIGGER_NAME = :TriggerName AND ' +
    ' (REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$CHECK_CONSTRAINTS CHK WHERE ' +
    '     TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME) ' +
    'ORDER BY TRG.RDB$RELATION_NAME, TRG.RDB$TRIGGER_TYPE, ' +
    '    TRG.RDB$TRIGGER_SEQUENCE, TRG.RDB$TRIGGER_NAME';

  TriggerSecuritySQL = 'Select RDB$SQL_SECURITY From RDB$TRIGGERS Where RDB$TRIGGER_NAME = :TriggerName';

var
  Header : Boolean;
  TriggerName, RelationName, InActive: String;
  TriggerHeader: string;
  qryTriggers : TIBSQL;
  qryTriggerSec: TIBSQL;
  SList : TStrings;
  Comments: TStrings;
begin
  Header := true;
  if [etTable,etTrigger ] * ExtractTypes <> [] then
    ExtractTypes -= [etDatabaseTriggers,etDDLTriggers];
  SList := TStringList.Create;
  Comments := TStringList.Create;
  qryTriggers := TIBSQL.Create(FDatabase);
  qryTriggerSec := TIBSQL.Create(FDatabase);
  try
    if ObjectName = '' then
      qryTriggers.SQL.Text := TriggerSQL
    else
    begin
      if etTable in ExtractTypes  then
      begin
        qryTriggers.SQL.Text := TriggerNameSQL;
        qryTriggers.Params.ByName('TableName').AsString := ObjectName;
      end
      else
      begin
        qryTriggers.SQL.Text := TriggerByNameSQL;
        qryTriggers.Params.ByName('TriggerName').AsString := ObjectName;
      end;
    end;
    qryTriggers.ExecQuery;
    while not qryTriggers.Eof do
    begin
      AddComment(qryTriggers,ctTrigger,Comments);
      SList.Clear;
      if Header then
      begin
        ExtractOut(Format('SET TERM %s %s%s', [Procterm, Term, LineEnding]));
        ExtractOut(Format('%s/* Triggers only will work for SQL triggers */%s',
		       [LineEnding, LineEnding]));
        Header := false;
      end;
      TriggerName := qryTriggers.FieldByName('RDB$TRIGGER_NAME').AsString;
      RelationName := qryTriggers.FieldByName('RDB$RELATION_NAME').AsString;
      if qryTriggers.FieldByName('RDB$TRIGGER_INACTIVE').IsNull then
        InActive := 'INACTIVE'
      else
        if qryTriggers.FieldByName('RDB$TRIGGER_INACTIVE').AsInteger = 1 then
          InActive := 'INACTIVE'
        else
          InActive := 'ACTIVE';

      if (ExtractTypes * [etDatabaseTriggers,etDDLTriggers] = []) or
         ((etDatabaseTriggers in ExtractTypes) and (qryTriggers.FieldByName('RDB$TRIGGER_TYPE').AsInt64 and $2000 <> 0)) or
         ((etDDLTriggers in ExtractTypes) and (qryTriggers.FieldByName('RDB$TRIGGER_TYPE').AsInt64 and $4000 <> 0))
         then
      begin
        if qryTriggers.FieldByName('RDB$FLAGS').AsInteger <> 1 then
          SList.Add('/* ');

        {Database or Transaction trigger}
        if RelationName <> '' then
        SList.Add(Format('CREATE TRIGGER %s FOR %s%s%s %s POSITION %d',
                [QuoteIdentifier( TriggerName), QuoteIdentifier( RelationName),
                LineEnding, InActive,
                GetTriggerType(qryTriggers.FieldByName('RDB$TRIGGER_TYPE').AsInt64),
                qryTriggers.FieldByName('RDB$TRIGGER_SEQUENCE').AsInteger]))
        else
          SList.Add(Format('CREATE TRIGGER %s%s%s %s POSITION %d',
                  [QuoteIdentifier( TriggerName),
                  LineEnding, InActive,
                  GetTriggerType(qryTriggers.FieldByName('RDB$TRIGGER_TYPE').AsInt64),
                  qryTriggers.FieldByName('RDB$TRIGGER_SEQUENCE').AsInteger]));

        {SQL Security added in Firebird 4}
        if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
        begin
          qryTriggerSec.SQL.Text := TriggerSecuritySQL;
          qryTriggerSec.Params.ByName('TriggerName').AsString := TriggerName;
          qryTriggerSec.ExecQuery;
          if not qryTriggerSec.FieldByName('RDB$SQL_SECURITY').IsNull then
            SList.Add(AddSQLSecurity(qryTriggerSec.FieldByName('RDB$SQL_SECURITY')));
        end;

        if not qryTriggers.FieldByName('RDB$TRIGGER_SOURCE').IsNull then
          SList.Add(qryTriggers.FieldByName('RDB$TRIGGER_SOURCE').AsString)
        else
          SList.Add('AS BEGIN EXIT; END');
        SList.Add(' ' + ProcTerm);
        if qryTriggers.FieldByName('RDB$FLAGS').AsInteger <> 1 then
          SList.Add(' */');
        ExtractOut(SList);
        if etGrant in ExtractTypes then
          ShowGrantsTo(TriggerName,obj_trigger,ProcTerm);
      end;
      qryTriggers.Next;
    end;
    if not Header then
    begin
      ExtractOut('COMMIT WORK ' + ProcTerm);
      ExtractOut('SET TERM ' + Term + ProcTerm);
    end;
    ExtractOut(Comments);
  finally
    Comments.Free;
    qryTriggers.Free;
    qryTriggerSec.Free;
    SList.Free;
  end;
end;

{	               ListCheck
  Functional description
 	  List check constraints for all objects to allow forward references }

procedure TIBExtract.ListCheck(ObjectName : String; ExtractType : TExtractType);
const
{ Query gets the check clauses for triggers stored for check constraints }
  CheckSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +
    'WHERE ' +
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';

  CheckNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +
    'WHERE ' +
    '  TRG.RDB$RELATION_NAME = :TableName AND ' +
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';

  CheckByNameSQL =
    'SELECT * FROM RDB$TRIGGERS TRG JOIN RDB$CHECK_CONSTRAINTS CHK ON ' +
    '  TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME ' +
    'WHERE ' +
    '  TRG.RDB$TRIGGER_NAME = :TriggerName AND ' +
    '  TRG.RDB$TRIGGER_TYPE = 1 AND ' +
    '  EXISTS (SELECT RDB$CONSTRAINT_NAME FROM RDB$RELATION_CONSTRAINTS RELC WHERE ' +
    '    CHK.RDB$CONSTRAINT_NAME = RELC.RDB$CONSTRAINT_NAME) ' +
    'ORDER BY CHK.RDB$CONSTRAINT_NAME';

var
  qryChecks : TIBSQL;
  SList : TStrings;
  RelationName : String;
begin
  qryChecks := TIBSQL.Create(FDatabase);
  SList := TStringList.Create;
  try
    if ObjectName = '' then
      qryChecks.SQL.Text := CheckSQL
    else
      if ExtractType = etTable then
      begin
        qryChecks.SQL.Text := CheckNameSQL;
        qryChecks.Params.ByName('TableName').AsString := ObjectName;
      end
      else
      begin
        qryChecks.SQL.Text := CheckByNameSQL;
        qryChecks.Params.ByName('TriggerName').AsString := ObjectName;
      end;
    qryChecks.ExecQuery;
    while not qryChecks.Eof do
    begin
      SList.Clear;
      RelationName := qryChecks.FieldByName('RDB$RELATION_NAME').AsString;
      SList.Add(Format('ALTER TABLE %s ADD',
		    [QuoteIdentifier( RelationName)]));
      if Pos('INTEG', qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsString) <> 1 then
        SList.Add(Format('%sCONSTRAINT %s ', [TAB,
          QuoteIdentifier( qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsString)]));

      if not qryChecks.FieldByName('RDB$TRIGGER_SOURCE').IsNull then
        SList.Text := SList.Text + qryChecks.FieldByName('RDB$TRIGGER_SOURCE').AsString;

      SList.Strings[SList.Count - 1] := SList.Strings[SList.Count - 1] + (Term) + LineEnding;
      ExtractOut(SList);
      qryChecks.Next;
    end;
  finally
    qryChecks.Free;
    SList.Free;
  end;
end;

procedure TIBExtract.ListComments(CommentTypes: TCommentTypes);

  procedure DoListComments(cmt: TCommentType);
  var qryCmt: TIBSQL;
      sql: string;
      index: integer;
  begin
    index := LookupDDLObject(cmt);
    if index = -1 then Exit;

    qryCmt := TIBSQL.Create(FDatabase);
    try
      with DDLObjects[index] do
      begin
        sql := 'Select * From '+ SystemTableName;
        if not (cmt in [ctCharacterSet, ctCollation, ctDatabase]) then
        begin
          if not ShowSystem then
            sql += ' Where (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG = 0)';
          if Condition <> '' then
          begin
            if not ShowSystem then
              sql += ' AND ' + Condition
            else
              sql += ' Where ' + Condition;
          end;
        end;
        sql += ' Order by 1';
      end;
      qryCmt.SQL.Text := sql;
      qryCmt.ExecQuery;
      while not qryCmt.Eof do
      begin
        AddComment(qryCmt,cmt);
        qryCmt.Next;
      end;
    finally
      qryCmt.Free;
    end;
  end;

var cType: TCommentType;
begin
  if CommentTypes = [] then
  begin
    for cType := low(TCommentType) to High(TCommentType) do
      DoListComments(cType)
  end
  else
  for cType in CommentTypes do
    DoListComments(cType);
end;

{             ListCreateDb
  Functional description
    Print the create database command if requested.  At least put
    the page size in a comment with the extracted db name }

procedure TIBExtract.ListCreateDb(TargetDb : String);
const
  CharInfoSQL =
    'SELECT * FROM RDB$DATABASE DBP ' +
    'WHERE NOT DBP.RDB$CHARACTER_SET_NAME IS NULL ' +
    '  AND DBP.RDB$CHARACTER_SET_NAME <> '' ''';

  DBSecuritySQL = 'Select RDB$SQL_SECURITY FROM RDB$DATABASE';

  FilesSQL =
    'select * from RDB$FILES ' +
    'order BY RDB$SHADOW_NUMBER, RDB$FILE_SEQUENCE';

  LogsSQL =
    'SELECT * FROM RDB$LOG_FILES ' +
    'ORDER BY RDB$FILE_FLAGS, RDB$FILE_SEQUENCE';

  DBSQLSecurity_SQL = 'ALTER DATABASE SET DEFAULT%s;';

var
  NoDb, First, FirstFile, HasWal, SetUsed : Boolean;
  Buffer : String;
  qryDB : TIBSQL;
  FileFlags, FileLength, FileSequence, FileStart : Integer;
  Comments: TStrings;

  function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): LongInt;
  begin
    with Database.Attachment.GetDBInformation([DatabaseInfoCommand]) do
      if (Count > 0) and (Items[0].GetItemType = DatabaseInfoCommand) then
        Result := Items[0].AsInteger
      else
        IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
  end;

begin
  NoDb := FALSE;
  First := TRUE;
  FirstFile := TRUE;
  HasWal := FALSE;
  SetUsed := FALSE;
  Buffer := '';
  if TargetDb = '' then
  begin
    Buffer := '/* ';
    TargetDb := FDatabase.DatabaseName;
    NoDb := true;
  end;
  Buffer := Buffer + 'CREATE DATABASE ' + QuotedStr(TargetDb) + ' PAGE_SIZE ' +
    IntToStr(FDatabaseInfo.PageSize) + LineEnding;
  ExtractOut(Buffer);
  Buffer := '';

  Comments := TStringList.Create;
  qryDB := TIBSQL.Create(FDatabase);
  try
    qryDB.SQL.Text := CharInfoSQL;
    qryDB.ExecQuery;

    if not qryDB.EOF then
    begin
      FDefaultCharSetName := trim(qryDB.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
      Buffer := Format(' DEFAULT CHARACTER SET %s',
        [FDefaultCharSetName]);
    end;
    if NoDB then
      Buffer := Buffer + Term + ' */'
    else
      Buffer := Buffer + Term;
    ExtractOut(Buffer);
    AddComment(qryDB,ctDatabase,Comments);
    qryDB.Close;
    {List secondary files and shadows as
      alter db and create shadow in comment}
    qryDB.SQL.Text := FilesSQL;
    qryDB.ExecQuery;
    while not qryDB.Eof do
    begin
      if First then
      begin
        ExtractOut(LineEnding + '/* Add secondary files in comments ');
        First := false;
      end; //end_if

      if qryDB.FieldByName('RDB$FILE_FLAGS').IsNull then
        FileFlags := 0
      else
        FileFlags := qryDB.FieldByName('RDB$FILE_FLAGS').AsInteger;
      if qryDB.FieldByName('RDB$FILE_LENGTH').IsNull then
        FileLength := 0
      else
        FileLength := qryDB.FieldByName('RDB$FILE_LENGTH').AsInteger;
      if qryDB.FieldByName('RDB$FILE_SEQUENCE').IsNull then
        FileSequence := 0
      else
        FileSequence := qryDB.FieldByName('RDB$FILE_SEQUENCE').AsInteger;
      if qryDB.FieldByName('RDB$FILE_START').IsNull then
        FileStart := 0
      else
        FileStart := qryDB.FieldByName('RDB$FILE_START').AsInteger;

      { Pure secondary files }
      if FileFlags = 0 then
      begin
        Buffer := Format('%sALTER DATABASE ADD FILE ''%s''',
          [LineEnding, qryDB.FieldByName('RDB$FILE_NAME').AsString]);
        if FileStart <> 0 then
          Buffer := Buffer + Format(' STARTING %d', [FileStart]);
        if FileLength <> 0 then
          Buffer := Buffer + Format(' LENGTH %d', [FileLength]);
        ExtractOut(Buffer);
      end; //end_if
      if (FileFlags and FILE_cache) <> 0 then
        ExtractOut(Format('%sALTER DATABASE ADD CACHE ''%s'' LENGTH %d',
          [LineEnding, qryDB.FieldByName('RDB$FILE_NAME').AsString, FileLength]));

      Buffer := '';
      if (FileFlags and FILE_shadow) <> 0 then
      begin
        if FileSequence <> 0 then
          Buffer := Format('%sFILE ''%s''',
            [TAB, qryDB.FieldByName('RDB$FILE_NAME').AsString])
        else
        begin
          Buffer := Format('%sCREATE SHADOW %d ''%s'' ',
            [LineEnding, qryDB.FieldByName('RDB$SHADOW_NUMBER').AsInteger,
             qryDB.FieldByName('RDB$FILE_NAME').AsString]);
          if (FileFlags and FILE_inactive) <> 0 then
            Buffer := Buffer + 'INACTIVE ';
          if (FileFlags and FILE_manual) <> 0 then
            Buffer := Buffer + 'MANUAL '
          else
            Buffer := Buffer + 'AUTO ';
          if (FileFlags and FILE_conditional) <> 0 then
            Buffer := Buffer + 'CONDITIONAL ';
        end; //end_else
        if FileLength <> 0 then
          Buffer := Buffer + Format('LENGTH %d ', [FileLength]);
        if FileStart <> 0 then
          Buffer := Buffer + Format('STARTING %d ', [FileStart]);
        ExtractOut(Buffer);
      end; //end_if
      qryDB.Next;
    end;
    qryDB.Close;

    qryDB.SQL.Text := LogsSQL;
    qryDB.ExecQuery;
    while not qryDB.Eof do
    begin

      if qryDB.FieldByName('RDB$FILE_FLAGS').IsNull then
        FileFlags := 0
      else
        FileFlags := qryDB.FieldByName('RDB$FILE_FLAGS').AsInteger;
      if qryDB.FieldByName('RDB$FILE_LENGTH').IsNull then
        FileLength := 0
      else
        FileLength := qryDB.FieldByName('RDB$FILE_LENGTH').AsInteger;

      Buffer := '';
      HasWal := true;
      if First then
      begin
        if NoDB then
          Buffer := '/* ';
        Buffer := Buffer + LineEnding + 'ALTER DATABASE ADD ';
        First := false;
      end; //end_if
      if FirstFile then
        Buffer := Buffer + 'LOGFILE ';
      { Overflow files also have the serial bit set }
      if (FileFlags and LOG_default) = 0 then
      begin
        if (FileFlags and LOG_overflow) <> 0 then
          Buffer := Buffer + Format(')%s   OVERFLOW ''%s''',
            [LineEnding, qryDB.FieldByName('RDB$FILE_NAME').AsString])
        else
          if (FileFlags and LOG_serial) <> 0 then
            Buffer := Buffer + Format('%s  BASE_NAME ''%s''',
              [LineEnding, qryDB.FieldByName('RDB$FILE_NAME').AsString])
          { Since we are fetching order by FILE_FLAGS, the LOG_0verflow will
             be last.  It will only appear if there were named round robin,
             so we must close the parens first }

          { We have round robin and overflow file specifications }
          else
          begin
            if FirstFile then
              Buffer := Buffer + '('
            else
              Buffer := Buffer + Format(',%s  ', [LineEnding]);
            FirstFile := false;

            Buffer := Buffer + Format('''%s''', [qryDB.FieldByName('RDB$FILE_NAME').AsString]);
          end; //end_else
      end;
      { Any file can have a length }
      if FileLength <> 0 then
        Buffer := Buffer + Format(' SIZE %d ', [FileLength]);
      ExtractOut(Buffer);
      qryDB.Next;
    end;
    qryDB.Close;
    Buffer := '';
    if HasWal then
    begin
      Buffer := Buffer + PrintSet(SetUsed);
      Buffer := Buffer + Format('NUM_LOG_BUFFERS = %d',
          [GetLongDatabaseInfo(isc_info_num_wal_buffers)]);
      Buffer := Buffer + PrintSet(SetUsed);
      Buffer := Buffer + Format('LOG_BUFFER_SIZE = %d',
          [GetLongDatabaseInfo(isc_info_wal_buffer_size)]);
      Buffer := Buffer + PrintSet(SetUsed);
      Buffer := Buffer + Format('GROUP_COMMIT_WAIT_TIME = %d',
          [GetLongDatabaseInfo(isc_info_wal_grpc_wait_usecs)]);
      Buffer := Buffer + PrintSet(SetUsed);
      Buffer := Buffer + Format('CHECK_POINT_LENGTH = %d',
          [GetLongDatabaseInfo(isc_info_wal_ckpt_length)]);
      ExtractOut(Buffer);

    end;

    {SQL Security added in Firebird 4}
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
    begin
      qryDB.SQL.Text := DBSecuritySQL;
      qryDB.ExecQuery;
      if not qryDB.FieldByName('RDB$SQL_SECURITY').IsNull then
        ExtractOut(Format(DBSQLSecurity_SQL,[AddSQLSecurity(qryDB.FieldByName('RDB$SQL_SECURITY'))]));
    end;

    if not First then
    begin
      if NoDB then
        ExtractOut(Format('%s */%s', [LineEnding, LineEnding]))
      else
        ExtractOut(Format('%s%s%s', [Term, LineEnding, LineEnding]));
    end;

    ExtractOut(Comments);
  finally
    qryDB.Free;
    Comments.Free;
  end;

(*
*)
end;

{	             ListDomainTable
  Functional description
  	List domains as identified by fields with any constraints on them
  	for the named table

  	Parameters:  table_name == only extract domains for this table }

procedure TIBExtract.ListDomains(DomainDDLType: TDomainDDLType;
  ObjectName: String; ExtractType: TExtractType);
const
  DomainSQL =
    'SELECT distinct fld.* FROM RDB$FIELDS FLD JOIN RDB$RELATION_FIELDS RFR ON ' +
    '  RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +
    'WHERE RFR.RDB$RELATION_NAME = :TABLE_NAME ' +
    'ORDER BY FLD.RDB$FIELD_NAME';

  DomainByNameSQL =
    'SELECT * FROM RDB$FIELDS FLD ' +
    'WHERE FLD.RDB$FIELD_NAME = :DomainName ' +
    'ORDER BY FLD.RDB$FIELD_NAME';

  AllDomainSQL =
    'select * from RDB$FIELDS ' +
    'where RDB$SYSTEM_FLAG <> 1 ' +
    'order BY RDB$FIELD_NAME';

var
  First : Boolean;
  qryDomains : TIBSQL;
  FieldName, Line : String;
begin
  First := true;
  qryDomains := TIBSQL.Create(FDatabase);
  try
    if ObjectName <> '' then
    begin
      if ExtractType = etTable then
      begin
        qryDomains.SQL.Text := DomainSQL;
        qryDomains.Params.ByName('table_name').AsString := ObjectName;
      end
      else
      begin
        qryDomains.SQL.Text := DomainByNameSQL;
        qryDomains.Params.ByName('DomainName').AsString := ObjectName;
      end;
    end
    else
      qryDomains.SQL.Text := AllDomainSQL;

    qryDomains.ExecQuery;
    while not qryDomains.Eof do
    begin
      FieldName := trim(qryDomains.FieldByName('RDB$FIELD_NAME').AsString);
      { Skip over artifical domains }
      if (Pos('RDB$',FieldName) = 1) and
         (FieldName[5] in ['0'..'9']) and
         (qryDomains.FieldByName('RDB$SYSTEM_FLAG').AsInteger <> 1) then
      begin
        qryDomains.Next;
        continue;
      end;

      if First then
      begin
        ExtractOut('');
        if DomainDDLType in [dtCreateDomain,dtCreateNoCheckConstraint] then
          ExtractOut('/* Domain definitions */')
        else
          ExtractOut('/* Add Domain Check Constraints */');
        ExtractOut('');
        First := false;
      end;

      Line := '';
      if DomainDDLType in [dtCreateDomain,dtCreateNoCheckConstraint] then
      begin
        Line := Format('CREATE DOMAIN %s AS ', [FieldName]) +
           GetFieldType(qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger,
                             qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
                             qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger,
                             qryDomains.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                             qryDomains.FieldByName('RDB$FIELD_LENGTH').AsInteger,
                             qryDomains.FieldByName('RDB$CHARACTER_SET_ID').IsNull,
                             false,
                             qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                             qryDomains.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
                             qryDomains.FieldByName('RDB$SEGMENT_LENGTH').AsInteger,
                             not qryDomains.FieldByName('RDB$DIMENSIONS').IsNull and
                                (qryDomains.FieldByName('RDB$DIMENSIONS').AsInteger <> 0),
                             FieldName);

        if not qryDomains.FieldByName('RDB$DEFAULT_SOURCE').IsNull then
          Line := Line + Format('%s%s %s', [LineEnding, TAB,
             qryDomains.FieldByName('RDB$DEFAULT_SOURCE').AsString]);

        if qryDomains.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then
          Line := Line + ' NOT NULL';
      end;

      if  not qryDomains.FieldByName('RDB$VALIDATION_SOURCE').IsNull then
      begin
        if DomainDDLType = dtAddCheckConstraint then
            Line := Format('ALTER DOMAIN %s ADD CONSTRAINT',[FieldName]);

        if DomainDDLType in [dtCreateDomain,dtAddCheckConstraint]  then
        begin
          if Pos('CHECK', AnsiUpperCase(qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString)) = 1 then
            Line := Line + Format('%s%s %s', [LineEnding, TAB,
               trim(qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString)])
          else
            Line := Line + Format('%s%s /* %s */', [LineEnding, TAB,
               trim(qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString)]);
        end;
      end;

      { Show the collation order if one has been specified.  If the collation
         order is the default for the character set being used, then no collation
         order will be shown ( because it isn't needed ).

         If the collation id is 0, then the default for the character set is
         being used so there is no need to retrieve the collation information.}

      if (DomainDDLType in [dtCreateDomain,dtCreateNoCheckConstraint]) and
         (not qryDomains.FieldByName('RDB$COLLATION_ID').IsNull) and
         (qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger <> 0) then
         Line := Line + GetCollationName(qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                                         qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger);
      if Line <> '' then
        Line := Line + Term;
      ExtractOut(Line);
      if (DomainDDLType in [dtCreateDomain,dtCreateNoCheckConstraint]) then
        AddComment(qryDomains,ctDomain);
      qryDomains.Next;
    end;
  finally
    qryDomains.Free;
  end;
end;

{          ListException
 Functional description
   List all exceptions defined in the database

   Parameters:  none }

procedure TIBExtract.ListException(ExceptionName : String = '');
const
  ExceptionSQL =
    'select * from RDB$EXCEPTIONS ' +
    'ORDER BY RDB$EXCEPTION_NAME';

  ExceptionNameSQL =
    'select * from RDB$EXCEPTIONS ' +
    'WHERE RDB$EXCEPTION_NAME = :ExceptionName ' +
    'ORDER BY RDB$EXCEPTION_NAME';

var
  First : Boolean;
  qryException : TIBSQL;
begin
  First := true;
  qryException := TIBSQL.Create(FDatabase);
  try
    if ExceptionName = '' then
      qryException.SQL.Text := ExceptionSQL
    else
    begin
      qryException.SQL.Text := ExceptionNameSQL;
      qryException.Params.ByName('ExceptionName').AsString := ExceptionName;
    end;

    qryException.ExecQuery;
    while not qryException.Eof do
    begin
      if First then
      begin
        ExtractOut('');
        ExtractOut('/*  Exceptions */');
        ExtractOut('');
        First := false;
      end; //end_if
      
      ExtractOut(Format('CREATE EXCEPTION %s %s%s',
        [QuoteIdentifier( qryException.FieldByName('RDB$EXCEPTION_NAME').AsString),
        QuotedStr(qryException.FieldByName('RDB$MESSAGE').AsString), Term]));
      AddComment(qryException,ctException);
      qryException.Next;
    end;
  finally
    qryException.Free;
  end;
end;

{              ListFilters

 Functional description
  List all blob filters

  Parameters:  none
  Results in
  DECLARE FILTER <fname> INPUT_TYPE <blob_sub_type> OUTPUT_TYPE <blob_subtype>
      ENTRY_POINT <string> MODULE_NAME <string> }

procedure TIBExtract.ListFilters(FilterName : String = '');
const
  FiltersSQL =
    'SELECT * FROM RDB$FILTERS ' +
    'ORDER BY RDB$FUNCTION_NAME';
  FilterNameSQL =
    'SELECT * FROM RDB$FILTERS ' +
    'WHERE RDB$FUNCTION_NAME = :FunctionName ' +
    'ORDER BY RDB$FUNCTION_NAME';

var
  First : Boolean;
  qryFilters : TIBSQL;
begin
  First := true;
  qryFilters := TIBSQL.Create(FDatabase);
  try
    if FilterName = '' then
      qryFilters.SQL.Text := FiltersSQL
    else
    begin
      qryFilters.SQL.Text := FilterNameSQL;
      qryFilters.Params.ByName('FunctionName').AsString := FilterName;
    end;
    qryFilters.ExecQuery;
    while not qryFilters.Eof do
    begin
      if First then
      begin
        ExtractOut('');
        ExtractOut('/*  BLOB Filter declarations */');
        ExtractOut('');
        First := false;
      end; //end_if

      ExtractOut(Format('DECLARE FILTER %s INPUT_TYPE %d OUTPUT_TYPE %d',
        [qryFilters.FieldByName('RDB$FUNCTION_NAME').AsString,
         qryFilters.FieldByName('RDB$INPUT_SUB_TYPE').AsInteger,
         qryFilters.FieldByName('RDB$OUTPUT_SUB_TYPE').AsInteger]));
      ExtractOut(Format('%sENTRY_POINT ''%s'' MODULE_NAME ''%s''%s%',
        [TAB, qryFilters.FieldByName('RDB$ENTRYPOINT').AsString,
         qryFilters.FieldByName('RDB$MODULE_NAME').AsString, Term]));
      ExtractOut('');
      AddComment(qryFilters,ctFilter);
      qryFilters.Next;
    end;

  finally
    qryFilters.Free;
  end;
end;

{            ListForeign
  Functional description
   List all foreign key constraints and alter the tables }

procedure TIBExtract.ListForeign(ObjectName : String; ExtractType : TExtractType);
const
  { Static queries for obtaining foreign constraints, where RELC1 is the
    foreign key constraints, RELC2 is the primary key lookup and REFC
    is the join table }
  ForeignSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' +
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' +
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +
    '     RDB$RELATION_CONSTRAINTS RELC2 ' +
    'WHERE ' +
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' +
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';

  ForeignNameSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' +
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' +
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +
    '     RDB$RELATION_CONSTRAINTS RELC2 ' +
    'WHERE ' +
    '  RELC1.RDB$RELATION_NAME = :TableName AND ' +
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' +
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';

  ForeignByNameSQL =
    'SELECT REFC.RDB$UPDATE_RULE REFC_UPDATE_RULE, REFC.RDB$DELETE_RULE REFC_DELETE_RULE, ' +
    '  RELC1.RDB$RELATION_NAME RELC1_RELATION_NAME, RELC2.RDB$RELATION_NAME RELC2_RELATION_NAME, ' +
    '  RELC1.RDB$INDEX_NAME RELC1_INDEX_NAME, RELC1.RDB$CONSTRAINT_NAME RELC1_CONSTRAINT_NAME, ' +
    '  RELC2.RDB$INDEX_NAME RELC2_INDEX_NAME ' +
    'FROM RDB$REF_CONSTRAINTS REFC, RDB$RELATION_CONSTRAINTS RELC1, ' +
    '     RDB$RELATION_CONSTRAINTS RELC2 ' +
    'WHERE ' +
    '  RELC1.RDB$CONSTRAINT_NAME = :ConstraintName AND ' +
    '  RELC1.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ' +
    '  REFC.RDB$CONST_NAME_UQ = RELC2.RDB$CONSTRAINT_NAME AND ' +
    '  REFC.RDB$CONSTRAINT_NAME = RELC1.RDB$CONSTRAINT_NAME AND ' +
    '  (RELC2.RDB$CONSTRAINT_TYPE = ''UNIQUE'' OR ' +
    '    RELC2.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ' +
    'ORDER BY RELC1.RDB$RELATION_NAME, RELC1.RDB$CONSTRAINT_NAME';

var
  qryForeign : TIBSQL;
  Line : String;

begin
  qryForeign := TIBSQL.Create(FDatabase);
  try
    if ObjectName = '' then
      qryForeign.SQL.Text := ForeignSQL
    else
    begin
      if ExtractType = etTable then
      begin
        qryForeign.SQL.Text := ForeignNameSQL;
        qryForeign.Params.ByName('TableName').AsString := ObjectName;
      end
      else
      begin
        qryForeign.SQL.Text := ForeignByNameSQL;
        qryForeign.Params.ByName('ConstraintName').AsString := ObjectName;
      end;
    end;
    qryForeign.ExecQuery;
    while not qryForeign.Eof do
    begin
      Line := Format('ALTER TABLE %s ADD ', [QuoteIdentifier(
        qryForeign.FieldByName('RELC1_RELATION_NAME').AsString)]);

      { If the name of the constraint is not INTEG..., print it.
         INTEG... are internally generated names. }
      if (not qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').IsNull) and
         ( Pos('INTEG', qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').AsString) <> 1) then
        Line := Line + Format('CONSTRAINT %s ', [QuoteIdentifier(
          Trim(qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').AsString))]);

      Line := Line + Format('FOREIGN KEY (%s) REFERENCES %s ', [
        GetIndexSegments(qryForeign.FieldByName('RELC1_INDEX_NAME').AsString),
        Trim(qryForeign.FieldByName('RELC2_RELATION_NAME').AsString)]);

      Line := Line + Format('(%s)',
        [GetIndexSegments(qryForeign.FieldByName('RELC2_INDEX_NAME').AsString)]);

      { Add the referential actions, if any }
      if (not qryForeign.FieldByName('REFC_UPDATE_RULE').IsNull) and
         (Trim(qryForeign.FieldByName('REFC_UPDATE_RULE').AsString) <> 'RESTRICT') then
        Line := Line + Format(' ON UPDATE %s',
           [Trim(qryForeign.FieldByName('REFC_UPDATE_RULE').AsString)]);

      if (not qryForeign.FieldByName('REFC_DELETE_RULE').IsNull) and
         (Trim(qryForeign.FieldByName('REFC_DELETE_RULE').AsString) <> 'RESTRICT') then
        Line := Line + Format(' ON DELETE %s',
           [Trim(qryForeign.FieldByName('REFC_DELETE_RULE').AsString)]);

      Line := Line + Term;
      ExtractOut(Line);
      qryForeign.Next;
    end;
  finally
    qryForeign.Free;
  end;
end;

{    ListExternalFunctions

 Functional description
   List all external functions

   Parameters:  none
  Results in
  DECLARE EXTERNAL FUNCTION function_name
                CHAR [256] , INTEGER, ....
                RETURNS INTEGER BY VALUE
                ENTRY_POINT entrypoint MODULE_NAME module; }

procedure TIBExtract.ListExternalFunctions(FunctionName: String);
const
  FunctionSQL =
    'SELECT * FROM RDB$FUNCTIONS WHERE RDB$SYSTEM_FLAG = 0 ' +
    'ORDER BY RDB$FUNCTION_NAME';

  FunctionNameSQL =
    'SELECT * FROM RDB$FUNCTIONS ' +
    'WHERE RDB$FUNCTION_NAME = :FunctionName ' +
    'ORDER BY RDB$FUNCTION_NAME';

  FunctionArgsSQL =
    'SELECT * FROM RDB$FUNCTION_ARGUMENTS ' +
    'WHERE ' +
    '  :FUNCTION_NAME = RDB$FUNCTION_NAME ' +
    'ORDER BY RDB$ARGUMENT_POSITION';

  function GetArgumentType(qryFuncArgs: TIBSQL): string;
  var FieldType: integer;
  begin
    FieldType := qryFuncArgs.FieldByName('RDB$FIELD_TYPE').AsInteger;
    if (FieldType = blr_cstring) or (FieldType = blr_cstring2) then
      Result := Format('CSTRING(%d)',[qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger]) +
                         GetCharacterSetName(qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').AsInteger, true)
    else
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
      Result := GetFieldType(FieldType,
                             qryFuncArgs.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
                             qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger,
                             qryFuncArgs.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                             qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger,
                             not qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').IsNull,
                             true,
                             qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                             qryFuncArgs.FieldByName('RDB$CHARACTER_LENGTH').AsInteger)
    else
      Result := GetFieldType(FieldType,
                             qryFuncArgs.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
                             qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger,
                             0,
                             qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger,
                             not qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').IsNull,
                             true,
                             qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                             qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger);

      case qryFuncArgs.FieldByName('RDB$MECHANISM').AsInteger of
      0:
       if qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0 then {Return parameter}
         Result := Result + ' BY VALUE';

      2:
        Result := Result + ' BY DESCRIPTOR';

      5:
        Result := Result + ' NULL';

      -1:
       if qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0 then {Return parameter}
         Result := Result + ' FREE_IT';
      end;
end;

var
  qryFunctions, qryFuncArgs : TIBSQL;
  First, FirstArg: Boolean;
  ReturnBuffer, Line : String;
  Comments: TStrings;
begin
  First := true;
  Comments := TStringList.Create;
  qryFunctions := TIBSQL.Create(FDatabase);
  qryFuncArgs := TIBSQL.Create(FDatabase);
  try
    if FunctionName = '' then
      qryFunctions.SQL.Text := FunctionSQL
    else
    begin
      qryFunctions.SQL.Text := FunctionNameSQL;
      qryFunctions.Params.ByName('FunctionName').AsString := FunctionName;
    end;
    qryFuncArgs.SQL.Text := FunctionArgsSQL;
    qryFunctions.ExecQuery;
    while not qryFunctions.Eof do
    begin
      if qryFunctions.HasField('RDB$LEGACY_FLAG') and (qryFunctions.FieldByName('RDB$LEGACY_FLAG').IsNULL or
          (qryFunctions.FieldByName('RDB$LEGACY_FLAG').AsInteger = 0)) then
      begin
        qryFunctions.Next;
        continue; {Internal stored procedure}
      end;
      if First then
      begin
        ExtractOut(Format('%s/* External Function declarations */%s',
          [LineEnding, LineEnding]));
        First := false;
      end; //end_if
      { Start new function declaration }
      AddComment(qryFunctions,ctExternalFunction,Comments);
      ExtractOut(Format('DECLARE EXTERNAL FUNCTION %s',
          [TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString)]));
      Line := '';

      FirstArg := true;
      qryFuncArgs.Params.ByName('FUNCTION_NAME').AsString :=
         qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString;

      qryFuncArgs.ExecQuery;
      while not qryFuncArgs.Eof do
      begin
        AddComment(qryFuncArgs,ctParameter,Comments);
        { Find parameter type }
        if qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0 then {return argument}
          ReturnBuffer := GetArgumentType(qryFuncArgs)
        else
        if FirstArg then
        begin
          Line := Line + GetArgumentType(qryFuncArgs);
          FirstArg := false;
        end
        else
          Line := Line + ', ' + GetArgumentType(qryFuncArgs);
        qryFuncArgs.Next;
      end;
      qryFuncArgs.Close;
      ExtractOut(Line);

      if qryFunctions.FieldByName('RDB$RETURN_ARGUMENT').AsInteger = 0 then
        ExtractOut('RETURNS ' + ReturnBuffer)
      else
        ExtractOut(Format('RETURNS PARAMETER %d',[qryFunctions.FieldByName('RDB$RETURN_ARGUMENT').AsInteger]));

      ExtractOut(Format('ENTRY_POINT ''%s'' MODULE_NAME ''%s''',
          [TrimRight(qryFunctions.FieldByName('RDB$ENTRYPOINT').AsString),
           TrimRight(qryFunctions.FieldByName('RDB$MODULE_NAME').AsString)]));

       {SQL Security added in Firebird 4}
       if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
         ExtractOut(AddSQLSecurity(qryFunctions.FieldByName('RDB$SQL_SECURITY')));

      ExtractOut(TERM + LineEnding + LineEnding);

      qryFunctions.Next;
    end;
    ExtractOut(Comments);
  finally
    qryFunctions.Free;
    qryFuncArgs.Free;
    Comments.Free;
  end;
end;

procedure TIBExtract.ListFunctions(ProcDDLType: TProcDDLType;
  FunctionName: String; IncludeGrants: boolean);
const
  FunctionSQL =
    'SELECT * FROM RDB$FUNCTIONS WHERE RDB$SYSTEM_FLAG = 0 and RDB$LEGACY_FLAG is not NULL and RDB$LEGACY_FLAG = 0 ' +
    'ORDER BY RDB$FUNCTION_NAME';

  FunctionNameSQL =
    'SELECT * FROM RDB$FUNCTIONS ' +
    'WHERE RDB$FUNCTION_NAME = :FUNCTION_NAME ' +
    'ORDER BY RDB$FUNCTION_NAME';

  FunctionArgsSQL =
    'SELECT * FROM RDB$FUNCTION_ARGUMENTS RFA JOIN RDB$FIELDS FLD ' +
    'ON RFA.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME '+
    'WHERE RDB$FUNCTION_NAME = :FUNCTION_NAME ' +
    'ORDER BY RDB$ARGUMENT_POSITION';

 var
  qryFunctions, qryFuncArgs : TIBSQL;
  First, FirstArg: Boolean;
  ReturnBuffer, Params : String;
  Comments: TStrings;
begin
  if FDatabaseInfo.ODSMajorVersion < ODS_VERSION12 then {Nothing to do}
    Exit;

  First := true;
  Comments := TStringList.Create;
  qryFunctions := TIBSQL.Create(FDatabase);
  qryFuncArgs := TIBSQL.Create(FDatabase);
  try
    if FunctionName = '' then
      qryFunctions.SQL.Text := FunctionSQL
    else
    begin
      qryFunctions.SQL.Text := FunctionNameSQL;
      qryFunctions.Params.ByName('FUNCTION_NAME').AsString := FunctionName;
    end;
    qryFuncArgs.SQL.Text := FunctionArgsSQL;
    qryFunctions.ExecQuery;
    while not qryFunctions.Eof do
    begin
       if First then
       begin
         ExtractOut('COMMIT WORK;');
         ExtractOut('SET AUTODDL OFF;');
         ExtractOut(Format('SET TERM %s %s', [ProcTerm, Term]));
         ExtractOut('');
         if ProcDDLType in [pdCreateStub,pdCreateProc] then
           ExtractOut('/* Stored Function declarations */')
         else
           ExtractOut('/* Stored Function Body */');
         ExtractOut('');
         First := false;
       end;

       AddComment(qryFunctions,ctFunction,Comments);
       Params := '';

       FirstArg := true;
       qryFuncArgs.Params.ByName('FUNCTION_NAME').AsString :=
          trim(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString);

       qryFuncArgs.ExecQuery;
       while not qryFuncArgs.Eof do
       begin
         AddComment(qryFuncArgs,ctParameter,Comments);
         if qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0 then {return argument}
           ReturnBuffer := ' RETURNS ' + GetFieldType(qryFuncArgs.FieldByName('RDB$FIELD_TYPE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_SUB_TYPE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_SCALE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_PRECISION1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_LENGTH1').AsInteger,
                                                      not qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID1').IsNull,
                                                      true,
                                                      qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$CHARACTER_LENGTH1').AsInteger)
         else
         begin
           if not FirstArg then
           begin
             Params := Params + ', ';
             FirstArg := false;
           end;
           Params := Params + TrimRight(qryFuncArgs.FieldByName('RDB$ARGUMENT_NAME').AsString) + ' ' +
                                         GetFieldType(qryFuncArgs.FieldByName('RDB$FIELD_TYPE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_SUB_TYPE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_SCALE1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_PRECISION1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$FIELD_LENGTH1').AsInteger,
                                                      not qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID1').IsNull,
                                                      true,
                                                      qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID1').AsInteger,
                                                      qryFuncArgs.FieldByName('RDB$CHARACTER_LENGTH1').AsInteger);
         end;
         qryFuncArgs.Next;
       end; // qryFuncArgs Iteration
       qryFuncArgs.Close;

       case ProcDDLType of
       pdCreateStub:
         begin
           if Params <> '' then
             ExtractOut(Format('CREATE FUNCTION %s (%s)',
               [TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString), Params]))
           else
             ExtractOut(Format('CREATE FUNCTION %s',[TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString)]));
           ExtractOut(ReturnBuffer);
           ExtractOut(' AS BEGIN END');

           {SQL Security added in Firebird 4}
           if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
             ExtractOut(AddSQLSecurity(qryFunctions.FieldByName('RDB$SQL_SECURITY')));
        end;

       pdCreateProc:
         begin
           if Params <> '' then
             ExtractOut(Format('CREATE FUNCTION %s (%s)',
               [TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString), Params]))
           else
             ExtractOut(Format('CREATE FUNCTION %s',[TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString)]));
           ExtractOut(ReturnBuffer);
           if not qryFunctions.FieldByName('RDB$FUNCTION_SOURCE').IsNull then
             ExtractOut(' AS ' + LineEnding + qryFunctions.FieldByName('RDB$FUNCTION_SOURCE').AsString)
           else
             ExtractOut(' AS BEGIN END');

           {SQL Security added in Firebird 4}
           if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13 then
             ExtractOut(AddSQLSecurity(qryFunctions.FieldByName('RDB$SQL_SECURITY')));
         end;

       pdAlterProc:
         begin
           if Params <> '' then
             ExtractOut(Format('ALTER FUNCTION %s (%s)',
               [TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString),Params]))
           else
             ExtractOut(Format('ALTER FUNCTION %s',
               [TrimRight(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString)]));
           ExtractOut(ReturnBuffer);
           if not qryFunctions.FieldByName('RDB$FUNCTION_SOURCE').IsNull then
             ExtractOut(' AS ' + LineEnding + qryFunctions.FieldByName('RDB$FUNCTION_SOURCE').AsString)
           else
             ExtractOut(' AS BEGIN END');
         end;
       end;
       ExtractOut(ProcTerm + LineEnding + LineEnding);
       if IncludeGrants then
         ShowGrantsTo(qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString,obj_Function,ProcTerm);

       qryFunctions.Next;
     end; // qryFunctions Iteration

      if not First then
      begin
        ExtractOut(Format('SET TERM %s %s', [Term, ProcTerm]));
        ExtractOut('COMMIT WORK;');
        ExtractOut('SET AUTODDL ON;');
      end;
      ExtractOut(Comments);
    finally
      qryFunctions.Free;
      qryFuncArgs.Free;
      Comments.Free;
    end;
end;

{  ListGenerators
 Functional description
   Re create all non-system generators }

procedure TIBExtract.ListGenerators(GeneratorName: String;
  ExtractTypes: TExtractTypes);
const
  GeneratorSQL =
    'SELECT RDB$GENERATOR_NAME ' +
    'FROM RDB$GENERATORS ' +
    'WHERE ' +
    '  (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +
    'ORDER BY RDB$GENERATOR_NAME';

  GeneratorNameSQL =
    'SELECT RDB$GENERATOR_NAME ' +
    'FROM RDB$GENERATORS ' +
    'WHERE RDB$GENERATOR_NAME = :GeneratorName AND ' +
    '  (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG <> 1) ' +
    'ORDER BY RDB$GENERATOR_NAME';

  GeneratorValueSQL =
    'SELECT GEN_ID(%s,0) as GENERATORVALUE From RDB$Database';

var
  qryGenerator : TIBSQL;
  qryValue: TIBSQL;
  GenName : String;
begin
  qryGenerator := TIBSQL.Create(FDatabase);
  qryValue := TIBSQL.Create(FDatabase);
  try
    if GeneratorName = '' then
      qryGenerator.SQL.Text := GeneratorSQL
    else
    begin
      qryGenerator.SQL.Text := GeneratorNameSQL;
      qryGenerator.Params.ByName('GeneratorName').AsString := GeneratorName;
    end;
    qryGenerator.ExecQuery;
    ExtractOut('');
    while not qryGenerator.Eof do
    begin
      GenName := qryGenerator.FieldByName('RDB$GENERATOR_NAME').AsString;
      if ((Pos('RDB$',GenName) = 1) and
         (GenName[5] in ['0'..'9'])) or
         ((Pos('SQL$',GenName) = 1) and
         (GenName[5] in ['0'..'9'])) then
      begin
        qryGenerator.Next;
        continue;
      end;
      ExtractOut(Format('CREATE SEQUENCE %s%s',
        [QuoteIdentifier( GenName),
         Term]));
      if etData in ExtractTypes then
      begin
        qryValue.SQL.Text := Format(GeneratorValueSQL,[GenName]);
        qryValue.ExecQuery;
        try
          if not qryValue.EOF then
            ExtractOut(Format('ALTER SEQUENCE %s RESTART WITH %d;',
                 [QuoteIdentifier( GenName),
                  qryValue.FieldByName('GENERATORVALUE').AsInt64]));
        finally
          qryValue.Close;
        end;
      end;
      AddComment(qryGenerator,ctSequence);
      qryGenerator.Next;
    end;
  finally
    qryGenerator.Free;
    qryValue.Free;
  end;
end;

{       ListIndex
 Functional description
   Define all non-constraint indices
   Use a static SQL query to get the info and print it.

   Uses get_index_segment to provide a key list for each index }

procedure TIBExtract.ListIndex(ObjectName : String; ExtractType : TExtractType);
const
  IndexSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' +
    '       IDX.RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';

  IndexNameSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' +
    '       IDX.RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  RELC.RDB$RELATION_NAME = :RelationName AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';

  IndexByNameSQL =
    'SELECT IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, IDX.RDB$UNIQUE_FLAG, ' +
    '       IDX.RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES IDX JOIN RDB$RELATIONS RELC ON ' +
    '  IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME ' +
    'WHERE (RELC.RDB$SYSTEM_FLAG <> 1 OR RELC.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  IDX.RDB$INDEX_NAME = :IndexName AND ' +
    '  NOT EXISTS (SELECT * FROM RDB$RELATION_CONSTRAINTS RC ' +
    '    WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME) ' +
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME';

var
  qryIndex : TIBSQL;
  First : Boolean;
  Unique, IdxType, Line : String;
begin
  First := true;
  qryIndex := TIBSQL.Create(FDatabase);
  try
    if ObjectName = '' then
      qryIndex.SQL.Text := IndexSQL
    else
    begin
      if ExtractType = etTable then
      begin
        qryIndex.SQL.Text := IndexNameSQL;
        qryIndex.Params.ByName('RelationName').AsString := ObjectName;
      end
      else
      begin
        qryIndex.SQL.Text := IndexByNameSQL;
        qryIndex.Params.ByName('IndexName').AsString := ObjectName;
      end;
    end;
    qryIndex.ExecQuery;
    while not qryIndex.Eof do
    begin
      if First then
      begin
        if ObjectName = '' then
          ExtractOut(LineEnding + '/*  Index definitions for all user tables */' + LineEnding)
        else
          ExtractOut(LineEnding + '/*  Index definitions for ' + ObjectName + ' */' + LineEnding);
        First := false;
      end; //end_if

      if qryIndex.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1 then
        Unique := ' UNIQUE'
      else
        Unique := '';

      if qryIndex.FieldByName('RDB$INDEX_TYPE').AsInteger = 1 then
        IdxType := ' DESCENDING'
      else
        IdxType := '';

      Line := Format('CREATE%s%s INDEX %s ON %s(', [Unique, IdxType,
        QuoteIdentifier(
            qryIndex.FieldByName('RDB$INDEX_NAME').AsString),
        QuoteIdentifier(
            qryIndex.FieldByName('RDB$RELATION_NAME').AsString)]);

      Line := Line + GetIndexSegments(qryIndex.FieldByName('RDB$INDEX_NAME').AsString) +
          ')' + Term;

      ExtractOut(Line);
      AddComment(qryIndex,ctIndex);
      qryIndex.Next;
    end;
  finally
    qryIndex.Free;
  end;
end;

{    ListViews
 Functional description
   Show text of views.
   Use a SQL query to get the info and print it.
   Note: This should also contain check option }

procedure TIBExtract.ListViews(ViewName : String);
const
  ViewSQL =
    'with recursive Views as ( ' +
    '  Select RDB$RELATION_NAME, 1 as ViewLevel from RDB$RELATIONS ' +
    '    Where RDB$RELATION_TYPE = 1 and RDB$SYSTEM_FLAG = 0 '+
    '  UNION ALL ' +
    '  Select D.RDB$DEPENDED_ON_NAME, ViewLevel + 1 From RDB$DEPENDENCIES D ' +
    '  JOIN Views on Views.RDB$RELATION_NAME = D.RDB$DEPENDENT_NAME ' +
    '     and Views.RDB$RELATION_NAME <> D.RDB$DEPENDED_ON_NAME ' +
    '  JOIN RDB$RELATIONS R On R.RDB$RELATION_NAME = D.RDB$DEPENDED_ON_NAME ' +
    ')' +
    'SELECT R.RDB$RELATION_NAME, R.RDB$OWNER_NAME, R.RDB$VIEW_SOURCE FROM RDB$RELATIONS R ' +
    'JOIN ( ' +
    'Select RDB$RELATION_NAME, max(ViewLevel) as ViewLevel From Views ' +
    'Group By RDB$RELATION_NAME) A On A.RDB$RELATION_NAME = R.RDB$RELATION_NAME ' +
    'Where R.RDB$RELATION_TYPE = 1 and R.RDB$SYSTEM_FLAG = 0 '+
    'Order by A.ViewLevel desc, R.RDB$RELATION_NAME asc';

{
    'SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$VIEW_SOURCE ' +
    'FROM RDB$RELATIONS ' +
    'WHERE ' +
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT RDB$VIEW_BLR IS NULL AND ' +
    '  RDB$FLAGS = 1 ' +
    'ORDER BY RDB$RELATION_ID'; }

  ViewNameSQL =
    'SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$VIEW_SOURCE ' +
    'FROM RDB$RELATIONS ' +
    'WHERE ' +
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  NOT RDB$VIEW_BLR IS NULL AND ' +
    '  RDB$FLAGS = 1 AND ' +
    '  RDB$RELATION_NAME = :ViewName ' +
    'ORDER BY RDB$RELATION_ID';

  ColumnSQL =
    'SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS ' +
    'WHERE ' +
    '  RDB$RELATION_NAME = :RELATION_NAME ' +
    'ORDER BY RDB$FIELD_POSITION';

var
  qryView, qryColumns : TIBSQL;
  SList : TStrings;
begin
  qryView := TIBSQL.Create(FDatabase);
  qryColumns := TIBSQL.Create(FDatabase);
  SList := TStringList.Create;
  try
    if ViewName = '' then
      qryView.SQL.Text := ViewSQL
    else
    begin
      qryView.SQL.Text := ViewNameSQL;
      qryView.Params.ByName('ViewName').AsString := ViewName;
    end;
    qryColumns.SQL.Text := ColumnSQL;
    qryView.ExecQuery;
    while not qryView.Eof do
    begin
      SList.Add(Format('%s/* View: %s, Owner: %s */%s',
         [LineEnding, TrimRight(qryView.FieldByName('RDB$RELATION_NAME').AsString),
          TrimRight(qryView.FieldByName('RDB$OWNER_NAME').AsString), LineEnding]));

      SList.Add(Format('CREATE VIEW %s (', [QuoteIdentifier(
        qryView.FieldByName('RDB$RELATION_NAME').AsString)]));

      qryColumns.Params.ByName('RELATION_NAME').AsString :=
          qryView.FieldByName('RDB$RELATION_NAME').AsString;
      qryColumns.ExecQuery;
      while not qryColumns.Eof do
      begin
        SList.Add('  ' + QuoteIdentifier(
           qryColumns.FieldByName('RDB$FIELD_NAME').AsString));
        qryColumns.Next;
        if not qryColumns.Eof then
          SList.Strings[SList.Count - 1] := SList.Strings[SList.Count - 1] + ', ';
      end;
      qryColumns.Close;
      SList.Text := SList.Text + Format(') AS%s', [LineEnding]);
      if not qryView.FieldByName('RDB$VIEW_SOURCE').IsNull then
        SList.Text := SList.Text + qryView.FieldByName('RDB$VIEW_SOURCE').AsString;
      SList.Text := SList.Text + Format('%s%s', [Term, LineEnding]);
      ExtractOut(SList);
      SList.Clear;
      qryView.Next;
    end;
  finally
    qryView.Free;
    qryColumns.Free;
    SList.Free;
  end;
end;

{    PrintSet
  Functional description
     print (using ISQL_printf) the word "SET"
     if the first line of the ALTER DATABASE
     settings options. Also, add trailing
     comma for end of prior line if needed.

  uses Print_buffer, a global }

function TIBExtract.PrintSet(var Used: Boolean) : String;
begin
  if not Used then
  begin
    Result := '  SET ';
    Used := true;
  end
  else
    Result := Format(', %s      ', [LineEnding]);
end;

{
           PrintValidation
  Functional description
    This does some minor syntax adjustmet for extracting
    validation blobs and computed fields.
    if it does not start with the word CHECK
    if this is a computed field blob,look for () or insert them.
    if flag = false, this is a validation clause,
    if flag = true, this is a computed field }

function TIBExtract.PrintValidation(ToValidate: String;
  flag: Boolean): String;
var
  IsSQL : Boolean;
begin
  IsSql := false;

  Result := '';
  ToValidate := Trim(ToValidate);

  if flag then
  begin
    if ToValidate[1] = '(' then
      IsSQL := true;
  end
  else
    if (Pos(ToValidate, 'check') = 1) or (Pos(ToValidate, 'CHECK') = 1) then
      IsSQL := TRUE;

	if not IsSQL then
  begin
    if Flag then
      Result := Result + '/* ' + ToValidate + ' */'
    else
      Result := Result + '(' + ToValidate + ')';
  end
  else
    Result := ToValidate;
end;

procedure TIBExtract.SetDatabase(const Value: TIBDatabase);
begin
  if (csLoading in ComponentState) or (FDatabase <> Value) then
  begin
    FDatabase := Value;
    if (not Assigned(FTransaction)) and (FDatabase <> nil) then
      Transaction := FDatabase.DefaultTransaction;
    FDatabaseInfo.Database := FDatabase;
  end;
end;

procedure TIBExtract.SetTransaction(const Value: TIBTransaction);
begin
  if FTransaction <> Value then
  begin
    FTransaction := Value;
    if (not Assigned(FDatabase)) and (FTransaction <> nil) then
      Database := FTransaction.DefaultDatabase;
  end;
end;

procedure TIBExtract.ExtractObject(ObjectType : TExtractObjectTypes;
      ObjectName : String = ''; ExtractTypes : TExtractTypes = []);
var
  DidActivate : Boolean;
begin
  DidActivate := false;
  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
    DidActivate := true;
  end;
  FMetaData.Clear;
  if not CaseSensitiveObjectNames then
    ObjectName := ExtractIdentifier(FDatabaseInfo.DBSQLDialect,ObjectName);
  case ObjectType of
    eoDatabase : ExtractDDL(true, '', ExtractTypes);
    eoDomain :
      if etTable in ExtractTypes then
        ListDomains(dtCreateDomain,ObjectName, etTable)
      else
        ListDomains(dtCreateDomain,ObjectName);
    eoTable :
    begin
      if ObjectName <> '' then
      begin
        if etDomain in ExtractTypes then
          ListDomains(dtCreateDomain,ObjectName, etTable);
        ExtractListTable(ObjectName, '', false);
        if etIndex in ExtractTypes then
          ListIndex(ObjectName, etTable);
        if etForeign in ExtractTypes then
          ListForeign(ObjectName, etTable);
        if etCheck in ExtractTypes then
          ListCheck(ObjectName, etTable);
        if etTrigger in ExtractTypes then
        begin
          if etGrant in ExtractTypes then
            ListTriggers(ObjectName, [etTable,etGrant])
          else
            ListTriggers(ObjectName, [etTable]);
        end;
        if etGrant in ExtractTypes then
          ShowGrants(ObjectName, Term);
        if etData in ExtractTypes then
          ListData(ObjectName);
      end
      else
        ListAllTables(true);
    end;
    eoView :
     begin
       ListViews(ObjectName);
       if ObjectName <> '' then
       begin
         if etTrigger in ExtractTypes then
         begin
           if etGrant in ExtractTypes then
             ListTriggers(ObjectName, [etTable,etGrant])
           else
             ListTriggers(ObjectName, [etTable]);
         end;
         if etGrant in ExtractTypes then
           ShowGrants(ObjectName, Term);
       end;
     end;
    eoProcedure :
     begin
       ListProcs(pdCreateProc,ObjectName,etGrant in ExtractTypes);
       ListFunctions(pdCreateProc,ObjectName,etGrant in ExtractTypes);
       if (ObjectName <> '' ) and (etGrant in ExtractTypes) then
         ShowGrants(ObjectName, Term);
     end;
    eoPackage:
     begin
       if DatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
       begin
         ListPackages(paBoth,ObjectName, etGrant in ExtractTypes);
         if (ObjectName <> '' ) and (etGrant in ExtractTypes) then
           ShowGrants(ObjectName, Term);
       end
       else
         IBError(ibxeODSVersionRequired,['12.0']);
     end;
    eoFunction : ListExternalFunctions(ObjectName);
    eoGenerator : ListGenerators(ObjectName,ExtractTypes);
    eoException : ListException(ObjectName);
    eoBLOBFilter : ListFilters(ObjectName);
    eoRole : ListRoles(ObjectName,etGrant in ExtractTypes);
    eoTrigger : 
      if etTable in ExtractTypes then
        ListTriggers(ObjectName, ExtractTypes * [etTable,etGrant])
      else
      if ExtractTypes * [etDatabaseTriggers,etDDLTriggers] = [] then
        ListTriggers(ObjectName,[etTrigger] + (ExtractTypes * [etGrant]))
      else
        ListTriggers(ObjectName,ExtractTypes * [etDatabaseTriggers,etDDLTriggers, etGrant]);
    eoForeign :
      if etTable in ExtractTypes then
        ListForeign(ObjectName, etTable)
      else
        ListForeign(ObjectName);
    eoIndexes :
      if etTable in ExtractTypes then
        ListIndex(ObjectName, etTable)
      else
        ListIndex(ObjectName);
    eoChecks :
      if etTable in ExtractTypes then
        ListCheck(ObjectName, etTable)
      else
        ListCheck(ObjectName);
    eoComments:
       ListComments;
    eoData : ListData(ObjectName);
  end;
  if DidActivate then
    FTransaction.Commit;
end;

procedure TIBExtract.ListObjectNames(ObjectType: integer; Names: TStrings);
var qryObjects: TIBSQL;
    i, index: integer;
    sql: string;
begin
  index := -1;
  Names.Clear;
  for i := Low(DDLObjects) to High(DDLObjects) do
  begin
    if DDLObjects[i].ObjType = ObjectType then
    begin
      index := i;
      break;
    end;
  end;
  if index = -1 then Exit;

  qryObjects := TIBSQL.Create(FDatabase);
  try
    with DDLObjects[index] do
    begin
      sql := 'Select ' + NameField + ' From ' + SystemTableName;
      if not ShowSystem then
        sql += ' Where (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG = 0)';
      if Condition <> '' then
      begin
        if not ShowSystem then
          sql += ' AND ' + Condition
        else
          sql += ' Where ' + Condition;
      end;
      sql += ' Order by 1';
    end;

    qryObjects.SQL.Text := sql;
    qryObjects.ExecQuery;
    while not qryObjects.Eof do
    begin
      Names.Add(qryObjects.Fields[0].AsString);
      qryObjects.Next;
    end;
  finally
    qryObjects.Free;
  end;
end;

function TIBExtract.GetCharacterSetName(CharSetID: integer; ForceCharSet: boolean): string;
{If ForceCharSet is true then Character Set always added. Otherwise returns empty if charset is
 database default}
const
    GetCharacterSetSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME ' +
    'FROM RDB$CHARACTER_SETS CST ' +
    'WHERE CST.RDB$CHARACTER_SET_ID = :CHARSETID ' +
    'ORDER BY CST.RDB$CHARACTER_SET_NAME';

var CharSetSQL : TIBSQL;
    CharSetName: string;
begin
  Result := '';
  CharSetSQL := TIBSQL.Create(FDatabase);
  try
    CharSetSQL.SQL.Add(GetCharacterSetSQL);
    CharSetSQL.Params.ByName('CHARSETID').AsInteger := CharSetID;
    CharSetSQL.ExecQuery;
    if (CharSetSQL.RecordCount > 0) then
    begin
      CharSetName := Trim(CharSetSQL.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
      if ForceCharSet or (FDefaultCharSetName <> CharSetName) then
      Result := ' CHARACTER SET ' + CharSetName;
    end;
  finally
    CharSetSQL.Free;
  end;
end;


function TIBExtract.GetFieldType(FieldType, FieldSubType, FieldScale,
  FieldPrecision, FieldLength: integer; HasCharacterSet: boolean;
  ForceCharSet: boolean; FieldCharacterSetID: integer;
  FieldCharacterLength: integer; FieldSegmentLen: integer; IsArray: boolean;
  FieldSourceName: string): String;
var i: integer;
    TypeName: string;
    DidActivate : Boolean;
begin
  Result := '';
  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
    DidActivate := true;
  end
  else
    DidActivate := false;

  { Look through types array }
  for i := Low(Columntypes) to High(ColumnTypes) do
  begin
    if FieldType = ColumnTypes[i].SQLType then {Process this and forget the rest}
    begin
      TypeName := ColumnTypes[i].TypeName;

      case FieldType of
      blr_text,
      blr_varying: {CHAR and VARCHAR}
        begin
          if FieldCharacterLength = 0 then
             Result := TypeName + Format('(%d)', [FieldLength])
          else
            Result := TypeName + Format('(%d)', [FieldCharacterLength]);
          if HasCharacterSet then
            Result :=  Result + GetCharacterSetName(FieldCharacterSetID,ForceCharSet);
        end;

      blr_blob:
        begin
          if (FieldSubType > 0) and (FieldSubType <= MAXSUBTYPES) then
            Result := TypeName + ' SUB_TYPE ' + SubTypes[FieldSubType]
          else
            Result := TypeName + ' SUB_TYPE ' + IntToStr(FieldSubType);

          if FieldSegmentLen > 0 then
            Result := Result + Format(' SEGMENT SIZE %d',[FieldSegmentLen]);
          if HasCharacterSet then
            Result :=  Result + GetCharacterSetName(FieldCharacterSetID,ForceCharSet);
        end;

        blr_dec64: {DecFloat(16) }
          begin
            if FieldPrecision = 0 then
              Result := TypeName + '(16)'
            else
              Result := TypeName + Format('(%d)',[FieldPrecision]);
          end;

        blr_dec128: { DecFloat(34)}
          begin
            if FieldPrecision = 0 then
              Result := TypeName + '(34)'
            else
              Result := TypeName + Format('(%d)',[FieldPrecision]);
          end;

        blr_short,
        blr_long,
        blr_double,
        blr_int64,
        blr_int128: {numeric types}
          begin
            if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
            begin
              { We are ODS >= 10 and could be any Dialect }
              if FieldPrecision > 0 then
              begin
                { We are Dialect >=3 since FIELD_PRECISION is non-NULL }
                if (FieldSubType > 0) and (FieldSubType <= MAX_INTSUBTYPES) then
                begin
                  Result := Format('%s(%d, %d)',
                                         [IntegralSubtypes[FieldSubType],
                                          FieldPrecision,
                                          -FieldScale]);
                  break;
                end;
              end;
            end;

            { Take a stab at numerics and decimals }
            if (FieldType = blr_short) and (FieldScale < 0) then
              Result :=  Format('NUMERIC(4, %d)', [-FieldScale])
            else
            if (FieldType = blr_long) and (FieldScale < 0) then
                Result := Format('NUMERIC(9, %d)', [-FieldScale])
            else
            if (FieldType = blr_double) and (FieldScale < 0) then
               Result :=  Format('NUMERIC(15, %d)', [-FieldScale])
            else
            if (FieldType = blr_int64) and (FieldScale < 0) then
               Result :=  Format('NUMERIC(18, %d)', [-FieldScale])
            else
            if (FieldType = blr_int128) and (FieldScale < 0) then
               Result :=  Format('NUMERIC(38, %d)', [-FieldScale])
            else
                Result :=  TypeName;
          end;

        else
          Result := TypeName; {e.g. Timestamp}
      end;

      { Catch arrays after detrermining the type declaration }
      if IsArray then
        Result := Result + GetArrayField(FieldSourceName);
      Break;
    end;
  end;
  if DidActivate then
    FTransaction.Commit;
end;

{  S H O W _ g r a n t s
 Functional description
   Show grants for given object name
   This function is also called by extract for privileges.
     It must extract granted privileges on tables/views to users,
     - these may be compound, so put them on the same line.
   Grant execute privilege on procedures to users
   Grant various privileges to procedures.
   All privileges may have the with_grant option set. }

procedure TIBExtract.ShowGrants(MetaObject: String; Terminator: String;
  NoUserGrants: boolean);
const
  GrantsBaseSelect =
    'Select Trim(RDB$USER) as RDB$USER,List("Privileges") as Privileges,  '+
    'coalesce(RDB$GRANT_OPTION,0) as RDB$GRANT_OPTION,METAOBJECTNAME,  '+
    'RDB$USER_TYPE, RDB$OBJECT_TYPE,  '+
    'case T2.RDB$TYPE_NAME  '+
    '  When ''RELATION'' then ''TABLE''  '+
    '  When ''FIELD'' then ''DOMAIN''  '+
    'Else T2.RDB$TYPE_NAME End as OBJECT_TYPE_NAME,  '+
    'T1.RDB$TYPE_NAME as USER_TYPE_NAME,  '+
    'case  '+
    'When coalesce(RDB$GRANT_OPTION,0) <> 0 and RDB$USER_TYPE = 13 then '' WITH ADMIN OPTION''  '+
    'When coalesce(RDB$GRANT_OPTION,0) <> 0 and RDB$USER_TYPE <> 13 then '' WITH GRANT OPTION''  '+
    'ELSE '''' End as GRANTOPTION,  '+
    'case When RDB$OWNER_NAME = RDB$GRANTOR then NULL '+
    'else Trim(RDB$GRANTOR) End as GRANTEDBY  '+
    'From (   '+
    'Select PR.RDB$USER,PR.RDB$RELATION_NAME as METAOBJECTNAME, LIST(DISTINCT Trim(Case PR.RDB$PRIVILEGE   '+
    'When ''X'' then ''EXECUTE''   '+
    'When ''S'' then ''SELECT''   '+
    'When ''U'' then ''UPDATE''    '+
    'When ''D'' then ''DELETE''   '+
    'When ''R'' then ''REFERENCES''   '+
    'When ''G'' then ''USAGE''   '+
    'When ''I'' then ''INSERT''  '+
    'end )) as "Privileges",   '+
    'PR.RDB$GRANT_OPTION,  PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE,OW.RDB$OWNER_NAME,PR.RDB$GRANTOR   '+
    'FROM RDB$USER_PRIVILEGES PR   '+
    'JOIN ObjectOwners OW On OW.METAOBJECTNAME = PR.RDB$RELATION_NAME and OW.ObjectType = PR.RDB$OBJECT_TYPE  '+
    'Where PR.RDB$PRIVILEGE <> ''M'' and (PR.RDB$PRIVILEGE <> ''U'' or PR.RDB$FIELD_NAME is null)   '+
    'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE,OW.RDB$OWNER_NAME,PR.RDB$GRANTOR   '+
    'UNION   '+
    'Select PR.RDB$USER,PR.RDB$RELATION_NAME, ''Update('' || List(Trim(PR.RDB$FIELD_NAME)) || '')'',   '+
    'PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE, OW.RDB$OWNER_NAME,PR.RDB$GRANTOR    '+
    'FROM RDB$USER_PRIVILEGES PR   '+
    'JOIN ObjectOwners OW On OW.METAOBJECTNAME = PR.RDB$RELATION_NAME and OW.ObjectType = PR.RDB$OBJECT_TYPE  '+
    'Where PR.RDB$PRIVILEGE = ''U'' and PR.RDB$FIELD_NAME is not null    '+
    'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE, OW.RDB$OWNER_NAME,PR.RDB$GRANTOR) A '+
    'JOIN RDB$TYPES T1 On T1.RDB$TYPE = RDB$USER_TYPE and T1.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE''  '+
    'JOIN RDB$TYPES T2 On T2.RDB$TYPE = RDB$OBJECT_TYPE and T2.RDB$FIELD_NAME = ''RDB$OBJECT_TYPE''  '+
    'Where METAOBJECTNAME = :METAOBJECTNAME and RDB$USER <> RDB$OWNER_NAME   '+
    'Group By RDB$USER,RDB$GRANT_OPTION,  RDB$USER_TYPE, RDB$OBJECT_TYPE,METAOBJECTNAME,RDB$GRANTOR,T2.RDB$TYPE_NAME,T1.RDB$TYPE_NAME,RDB$OWNER_NAME  '+
    'ORDER BY RDB$USER, RDB$OBJECT_TYPE';

  GrantsSQL12 =
  'with ObjectOwners As (  '+
  '  Select RDB$RELATION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 0 as ObjectType  '+
  '  From RDB$RELATIONS  '+
  '  UNION  '+
  '  Select RDB$PROCEDURE_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 5 as ObjectType  '+
  '  From RDB$PROCEDURES  '+
  '  UNION  '+
  '  Select RDB$FUNCTION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 100 as ObjectType  '+
  '  From RDB$FUNCTIONS  '+
  '  UNION  '+
  '  Select RDB$EXCEPTION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 7 as ObjectType  '+
  '  From RDB$EXCEPTIONS  '+
  '  UNION  '+
  '  Select RDB$FIELD_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 9 as ObjectType  '+
  '  From RDB$FIELDS Where RDB$FIELD_NAME not Similar to ''RDB$%|SEC$%|MON$%|SQL$%'' '+
  '  UNION  '+
  '  Select RDB$GENERATOR_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 14 as ObjectType  '+
  '  From RDB$GENERATORS  '+
  '  UNION  '+
  '  Select RDB$CHARACTER_SET_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 11 as ObjectType  '+
  '  From RDB$CHARACTER_SETS  '+
  '  UNION  '+
  '  Select RDB$COLLATION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 17 as ObjectType  '+
  '  From RDB$COLLATIONS  '+
  '  UNION  '+
  '  Select RDB$PACKAGE_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 18 as ObjectType  '+
  '  From RDB$PACKAGES  '+
  ') '+ GrantsBaseSelect;

  GrantsSQL =
  'with ObjectOwners As ( '+
  'Select RDB$RELATION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 0 as ObjectType '+
  'From RDB$RELATIONS '+
  'UNION '+
  'Select RDB$PROCEDURE_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 5 as ObjectType '+
  'From RDB$PROCEDURES '+
  'UNION '+
  'Select RDB$EXCEPTION_NAME as METAOBJECTNAME, ''SYSDBA'', 7 as ObjectType '+
  'From RDB$EXCEPTIONS '+
  'UNION '+
  'Select RDB$GENERATOR_NAME as METAOBJECTNAME, ''SYSDBA'', 14 as ObjectType '+
  'From RDB$GENERATORS '+
  'UNION '+
  'Select RDB$CHARACTER_SET_NAME as METAOBJECTNAME, ''SYSDBA'', 11 as ObjectType '+
  'From RDB$CHARACTER_SETS '+
  ') '+ GrantsBaseSelect;

var qryOwnerPriv : TIBSQL;

begin
  if MetaObject = '' then
    exit;

  qryOwnerPriv := TIBSQL.Create(FDatabase);
  try
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION12 then
      qryOwnerPriv.SQL.Text := GrantsSQL12
    else
    qryOwnerPriv.SQL.Text := GrantsSQL;
    qryOwnerPriv.Params.ByName('METAOBJECTNAME').AsString := MetaObject;
    qryOwnerPriv.ExecQuery;
    while not qryOwnerPriv.Eof do
    begin
      if not NoUserGrants or (qryOwnerPriv.FieldByName('RDB$USER_TYPE').AsInteger <> obj_user)
          or (qryOwnerPriv.FieldByName('RDB$USER').AsString = 'PUBLIC') then
      begin
        if qryOwnerPriv.FieldByName('GRANTEDBY').IsNull then
        ExtractOut(Format('GRANT %s ON %s %s TO %s %s %s %s', [
                          TrimRight(qryOwnerPriv.FieldByName('Privileges').AsString),
                          TrimRight(qryOwnerPriv.FieldByName('OBJECT_TYPE_NAME').AsString),
                          QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('METAOBJECTNAME').AsString)),
                          Trim(qryOwnerPriv.FieldByName('USER_TYPE_NAME').AsString),
                          QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('RDB$USER').AsString)),
                          TrimRight(qryOwnerPriv.FieldByName('GRANTOPTION').AsString),
                          Terminator]))
        else
          ExtractOut(Format('GRANT %s ON %s %s TO %s %s %s GRANTED BY %s %s', [
                            TrimRight(qryOwnerPriv.FieldByName('Privileges').AsString),
                            TrimRight(qryOwnerPriv.FieldByName('OBJECT_TYPE_NAME').AsString),
                            QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('METAOBJECTNAME').AsString)),
                            TrimRight(qryOwnerPriv.FieldByName('USER_TYPE_NAME').AsString),
                            QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('RDB$USER').AsString)),
                            TrimRight(qryOwnerPriv.FieldByName('GRANTOPTION').AsString),
                            QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('GRANTEDBY').AsString)),
                            Terminator]));
      end;
      qryOwnerPriv.Next;
    end;
    qryOwnerPriv.Close;
  finally
    qryOwnerPriv.Free;
  end;
end;

procedure TIBExtract.ShowGrantsTo(MetaObject: String; ObjectType: integer; Terminator: String);
const
  GrantsSQL =
  'Select Trim(RDB$USER) as RDB$USER,List("Privileges") as Privileges, '+
  'coalesce(RDB$GRANT_OPTION,0) as RDB$GRANT_OPTION,METAOBJECTNAME, '+
  'RDB$USER_TYPE, RDB$OBJECT_TYPE, '+
  'case  RDB$OBJECT_TYPE '+
  'When 0 then ''TABLE'' '+
  'When 5 then ''PROCEDURE'' '+
  'When 7 then ''EXCEPTION'' '+
  'When 11 then ''CHARACTER SET'' '+
  'ELSE NULL END as OBJECT_TYPE_NAME, '+
  'case RDB$USER_TYPE '+
  'When 5 then ''PROCEDURE'' '+
  'When 2 then ''TRIGGER'' '+
  'When 8 then ''USER'' '+
  'When 13 then ''ROLE'' '+
  'ELSE NULL END as USER_TYPE_NAME, '+
  'case '+
  'When coalesce(RDB$GRANT_OPTION,0) <> 0 and RDB$USER_TYPE = 13 then '' WITH ADMIN OPTION'' '+
  'When coalesce(RDB$GRANT_OPTION,0) <> 0 and RDB$USER_TYPE <> 13 then '' WITH GRANT OPTION'' '+
  'ELSE '''' End as GRANTOPTION '+
  'From (  '+
  'Select PR.RDB$USER,PR.RDB$RELATION_NAME as METAOBJECTNAME, LIST(DISTINCT Trim(Case PR.RDB$PRIVILEGE  '+
  'When ''X'' then ''EXECUTE''  '+
  'When ''S'' then ''SELECT''  '+
  'When ''U'' then ''UPDATE''   '+
  'When ''D'' then ''DELETE''  '+
  'When ''R'' then ''REFERENCES''  '+
  'When ''G'' then ''USAGE''  '+
  'When ''I'' then ''INSERT'' end )) as "Privileges",  '+
  'PR.RDB$GRANT_OPTION,  PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE  '+
  'FROM RDB$USER_PRIVILEGES PR  '+
  'Where PR.RDB$PRIVILEGE <> ''M'' and (PR.RDB$PRIVILEGE <> ''U'' or PR.RDB$FIELD_NAME is null)  '+
  'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE  '+
  'UNION  '+
  'Select PR.RDB$USER,PR.RDB$RELATION_NAME, ''Update('' || List(Trim(PR.RDB$FIELD_NAME)) || '')'',  '+
  'PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE   '+
  'FROM RDB$USER_PRIVILEGES PR  '+
  'Where PR.RDB$PRIVILEGE = ''U'' and PR.RDB$FIELD_NAME is not null   '+
  'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE)  '+
  'Where RDB$USER = :METAOBJECTNAME and RDB$USER_TYPE = :USERTYPE '+
  'Group By RDB$USER,RDB$GRANT_OPTION,  RDB$USER_TYPE, RDB$OBJECT_TYPE, METAOBJECTNAME '+
  'ORDER BY METAOBJECTNAME';

var qryOwnerPriv : TIBSQL;

begin
  if MetaObject = '' then
    exit;

  qryOwnerPriv := TIBSQL.Create(FDatabase);
  try
    qryOwnerPriv.SQL.Text := GrantsSQL;
    qryOwnerPriv.Params.ByName('METAOBJECTNAME').AsString := MetaObject;
    qryOwnerPriv.Params.ByName('USERTYPE').AsInteger := ObjectType;
    qryOwnerPriv.ExecQuery;
    while not qryOwnerPriv.Eof do
    begin
      ExtractOut(Format('GRANT %s ON %s %s TO %s %s %s%s', [
                            TrimRight(qryOwnerPriv.FieldByName('Privileges').AsString),
                            TrimRight(qryOwnerPriv.FieldByName('OBJECT_TYPE_NAME').AsString),
                            QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('METAOBJECTNAME').AsString)),
                            TrimRight(qryOwnerPriv.FieldByName('USER_TYPE_NAME').AsString),
                            QuoteIdentifier(TrimRight(qryOwnerPriv.FieldByName('RDB$USER').AsString)),
                            TrimRight(qryOwnerPriv.FieldByName('GRANTOPTION').AsString),
                            Terminator]));
      qryOwnerPriv.Next;
    end;
    qryOwnerPriv.Close;
  finally
    qryOwnerPriv.Free;
  end;
  ExtractOut('');
end;

{	  ShowGrantRoles
  Functional description
   	Show grants for given role name
   	This function is also called by extract for privileges.
   	All membership privilege may have the with_admin option set. }

procedure TIBExtract.ShowGrantRoles(Terminator: String);
const
  RoleSQL =
    'SELECT * FROM RDB$USER_PRIVILEGES ' +
    'WHERE ' +
    '  RDB$OBJECT_TYPE = %d AND ' +
    '  RDB$USER_TYPE = %d AND ' +
    '  RDB$PRIVILEGE = ''M'' AND RDB$USER <> ''SYSDBA''' +
    'ORDER BY  RDB$RELATION_NAME, RDB$USER';

var
  IsDefault, WithOption, UserString : String;
  qryRole : TIBSQL;

begin
  qryRole := TIBSQL.Create(FDatabase);
  try
    qryRole.SQL.Text := Format(RoleSQL, [obj_sql_role, obj_user]);
    qryRole.ExecQuery;
    while not qryRole.Eof do
    begin
      UserString := Trim(qryRole.FieldByName('RDB$USER').AsString);

      if (not qryRole.FieldByName('RDB$GRANT_OPTION').IsNull) and
         (qryRole.FieldByName('RDB$GRANT_OPTION').AsInteger = 1) then
        WithOption := ' WITH ADMIN OPTION'
      else
        WithOption := '';
      {DEFAULT added in Firebird 4}
      if qryRole.HasField('RDB$FIELD_NAME') and (qryRole.FieldByName('RDB$FIELD_NAME').AsString = 'D') then
        IsDefault := 'DEFAULT '
      else
        IsDefault := '';
      ExtractOut(Format('GRANT %s%s TO %s%s%s%s',
        [ IsDefault, QuoteIdentifier(TrimRight(qryRole.FieldByName('RDB$RELATION_NAME').AsString)),
         UserString, WithOption, Terminator, LineEnding]));

      qryRole.Next;
    end;
  finally
    qryRole.Free;
  end;
end;

{	            GetProcedureArgs
  Functional description
 	 This function extract the procedure parameters and adds it to the
 	 extract file }

procedure TIBExtract.GetProcedureArgs(Proc: String);
const
{ query to retrieve the input parameters. }
  ProcHeaderSQL =
    'SELECT * ' +
    ' FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON ' +
    ' PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +
    'WHERE ' +
    '    PRM.RDB$PROCEDURE_NAME = :PROCNAME AND ' +
    '    PRM.RDB$PARAMETER_TYPE = :Input ' +
    'ORDER BY PRM.RDB$PARAMETER_NUMBER';

var
  FirstTime : Boolean;
  Line : String;
  qryHeader : TIBSQL;

  function FormatParamStr : String;
  begin
    Result := Format('  %s ', [TrimRight(qryHeader.FieldByName('RDB$PARAMETER_NAME').AsString)]) +
      GetFieldType(qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger,
                           qryHeader.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
                           qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger,
                           qryHeader.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                           qryHeader.FieldByName('RDB$FIELD_LENGTH').AsInteger,
                           not qryHeader.FieldByName('RDB$CHARACTER_SET_ID').IsNull,
                           true,
                           qryHeader.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                           qryHeader.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
                           qryHeader.FieldByName('RDB$SEGMENT_LENGTH').AsInteger);

      if qryHeader.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then
         Result := Result + ' NOT NULL';

      if not qryHeader.FieldByName('RDB$CHARACTER_SET_ID').IsNull and
         not qryHeader.FieldByName('RDB$COLLATION_ID').IsNull then
        Result := Result + GetCollationName(qryHeader.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                                            qryHeader.FieldByName('RDB$COLLATION_ID').AsInteger);

      if not qryHeader.FieldByName('RDB$DEFAULT_SOURCE').IsNull then
        Result := Result + ' = ' + qryHeader.FieldByName('RDB$DEFAULT_SOURCE').AsString;

  end;

begin
  FirstTime := true;
  qryHeader := TIBSQL.Create(FDatabase);
  try
    qryHeader.SQL.Text := ProcHeaderSQL;
    qryHeader.Params.ByName('procname').AsString := Proc;
    qryHeader.Params.ByName('Input').AsInteger := 0;
    qryHeader.ExecQuery;
    while not qryHeader.Eof do
    begin
      if FirstTime then
      begin
        FirstTime := false;
        ExtractOut('(');
      end;

      Line := FormatParamStr;

      qryHeader.Next;
      if not qryHeader.Eof then
        Line := Line + ',';
      ExtractOut(Line);
    end;

    { If there was at least one param, close parens }
    if not FirstTime then
    begin
      ExtractOut( ')');
    end;

    FirstTime := true;
    qryHeader.Close;
    qryHeader.Params.ByName('Input').AsInteger := 1;
    qryHeader.ExecQuery;

    while not qryHeader.Eof do
    begin
      if FirstTime then
      begin
        FirstTime := false;
        ExtractOut('RETURNS' + LineEnding + '(');
      end;

      Line := FormatParamStr;

      qryHeader.Next;
      if not qryHeader.Eof then
        Line := Line + ',';
      ExtractOut(Line);
    end;

    { If there was at least one param, close parens }
    if not FirstTime then
    begin
      ExtractOut( ')');
    end;

    ExtractOut('AS');
  finally
    qryHeader.Free;
  end;
end;

procedure TIBExtract.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDatabase) and (Operation = opRemove) then
    FDatabase := nil;
  if (AComponent = FTransaction) and (Operation = opRemove) then
    FTransaction := nil;
end;

function TIBExtract.QuoteIdentifier(Value: String): String;
begin
  if AlwaysQuoteIdentifiers then
    Result := IBUtils.QuoteIdentifier(FDatabase.DBSQLDialect,Value)
  else
    Result := IBUtils.QuoteIdentifierIfNeeded(FDatabase.DBSQLDialect,Value)
end;

function TIBExtract.GetFieldType(qry: TIBSQL; ForceCharSet: boolean
  ): string;
var DomainName: string;
begin
  DomainName := qry.FieldByName('RDB$FIELD_SOURCE').AsString;
  if not ((length(DomainName) > 4) and (system.Copy(DomainName, 1, 4) = 'RDB$') and (DomainName[5] in ['0'..'9'])) and
    (qry.FieldByName('RDB$SYSTEM_FLAG').AsInteger <> 1) then
  begin
    {Must be a domain name}
    Result := QuoteIdentifier(trim(DomainName));

    { International character sets }
    if (qry.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_text, blr_varying])
        and (not qry.FieldByName('RDB$COLLATION_ID').IsNull)
        and (qry.FieldByName('RDB$COLLATION_ID').AsInteger <> 0) then
      Result := Result + GetCharacterSets(qry.FieldByName('RDB$CHARACTER_SET_ID').AsShort,
                                          qry.FieldByName('RDB$COLLATION_ID').AsInteger, true);
  end
  else
    Result := GetFieldType(qry.FieldByName('RDB$FIELD_TYPE').AsInteger,
                           qry.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
                           qry.FieldByName('RDB$FIELD_SCALE').AsInteger,
                           qry.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                           qry.FieldByName('RDB$FIELD_LENGTH').AsInteger,
                           not qry.FieldByName('RDB$CHARACTER_SET_ID').IsNull,
                           ForceCharSet,
                           qry.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
                           qry.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
                           qry.FieldByName('RDB$SEGMENT_LENGTH').AsInteger,
                           not qry.FieldByName('RDB$DIMENSIONS').IsNull and (qry.FieldByName('RDB$DIMENSIONS').AsInteger <> 0),
                           DomainName);
end;

procedure TIBExtract.ListData(ObjectName: String);
const
  SelectFieldListSQL = 'Select List(RDB$FIELD_NAME) From ( '+
    'Select RF.RDB$FIELD_NAME From RDB$RELATION_FIELDS RF '+
    'JOIN RDB$FIELDS F On F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE '+
    'Where F.RDB$COMPUTED_BLR is NULL and RF.RDB$RELATION_NAME = Upper(:Relation) '+
    'Order by RF.RDB$FIELD_POSITION asc)';

  TableSQL =
    'SELECT * FROM RDB$RELATIONS ' +
    'WHERE ' +
    '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
    '  RDB$VIEW_BLR IS NULL ' +
    'ORDER BY RDB$RELATION_NAME';

var FieldList: string;

begin
  if ObjectName = '' then {List all}
  begin
    with TIBSQL.Create(self) do
    try
      Database := FDatabase;
      SQL.Text := TableSQL;
      ExecQuery;
      ExtractOut('/* Data Starts */');
      while not EOF do
      begin
        ListData(Trim(FieldByName('RDB$RELATION_NAME').AsString));
        Next;
      end;
      ExtractOut('/* Data Ends */');
    finally
      Free;
    end;
  end
  else
  begin
    FieldList := '*';
    with TIBSQL.Create(self) do
    try
      Database := FDatabase;
      SQL.Text := SelectFieldListSQL;
      Params[0].AsString := ObjectName;
      ExecQuery;
      try
        if not EOF then
          FieldList := TrimRight(Fields[0].AsString);
      finally
        Close;
      end;
    finally
      Free
    end;

    with TIBInsertStmtsOut.Create(self) do
    try
      Database := FDatabase;
      if DataOut(Format('Select %s From %s',[FieldList,QuoteIdentifier( TrimRight(ObjectName))]),
                Add2MetaData) then
        ExtractOut('COMMIT;');
    finally
      Free
    end;
  end;
end;

procedure TIBExtract.ListRoles(ObjectName: String; IncludeGrants: boolean);
const
  RolesSQL =
    'select * from RDB$ROLES WHERE RDB$SYSTEM_FLAG = 0 ' +
    'order by RDB$ROLE_NAME';

  RolesByNameSQL =
    'select * from RDB$ROLES ' +
    'WHERE RDB$ROLE_NAME = :RoleName ' +
    'order by RDB$ROLE_NAME';

var
  qryRoles : TIBSQL;
  PrevOwner, RoleName, OwnerName : String;
  Comments: TStrings;
  sPrivileges: string;
  Privileges: cardinal;
  i: integer;
begin
  {Process GRANT roles}
  Comments := TStringList.Create;
  qryRoles := TIBSQL.Create(FDatabase);
  try
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION9 then
    begin
      PrevOwner := '';
      ExtractOut('');
      ExtractOut('/* Grant Roles for this database */');
      ExtractOut('');

      if ObjectName = '' then
        qryRoles.SQL.Text := RolesSQL
      else
      begin
        qryRoles.SQL.Text := RolesByNameSQL;
        qryRoles.Params.ByName('RoleName').AsString := ObjectName;
      end;
      qryRoles.ExecQuery;
      try
        while not qryRoles.Eof do
        begin
          AddComment(qryRoles,ctRole,Comments);
          RoleName := QuoteIdentifier(
              qryRoles.FieldByName('rdb$Role_Name').AsString);
          OwnerName := Trim(qryRoles.FieldByName('rdb$Owner_Name').AsString);
          if PrevOwner <> OwnerName then
          begin
            ExtractOut('');
            ExtractOut(Format('/* Role: %s, Owner: %s */', [RoleName, OwnerName]));
            ExtractOut('');
            PrevOwner := OwnerName;
          end;
          sPrivileges := '';
          if (FDatabaseInfo.ODSMajorVersion >= ODS_VERSION13) and
                          qryRoles.HasField('RDB$SYSTEM_PRIVILEGES') then
          begin
            sPrivileges := qryRoles.FieldByName('RDB$SYSTEM_PRIVILEGES').AsString;
            Privileges := byte(sPrivileges[1]) shl 24 + byte(sPrivileges[2]) shl 16 +
                          byte(sPrivileges[3]) shl 8 +  byte(sPrivileges[4]);
            sPrivileges := '';
            for i := Low(UserPrivileges) to high(UserPrivileges) do
              if (UserPrivileges[i].Mask and Privileges) <> 0 then
                if sPrivileges = '' then
                  sPrivileges := UserPrivileges[i].Privilege
                else
                  sPrivileges := sPrivileges + ',' +  UserPrivileges[i].Privilege;
          end;

          if sPrivileges <> '' then
            ExtractOut('CREATE ROLE ' + RoleName + ' SET SYSTEM PRIVILEGES TO ' + sPrivileges + Term)
          else
            ExtractOut('CREATE ROLE ' + RoleName + Term);

          if IncludeGrants then
            ShowGrantsTo(qryRoles.FieldByName('rdb$Role_Name').AsString,obj_sql_role,Term);
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;
      ExtractOut(Comments);
    end;
  finally
    qryRoles.Free;
    Comments.Free;
  end;
end;

end.


