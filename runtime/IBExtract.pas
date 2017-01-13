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
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

{ Syntax Enhancements Supported (by Firebird Version no.):

Multi-action triggers (1.5)
CREATE SEQUENCE (2.0)
Database Triggers (2.1)
Global Temporary Tables (2.1)
Boolean Type (3.0)
Identity Column Type (3.0)
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
  IBSQL, IBUtils, IBHeader, IB;

type
  TExtractObjectTypes =
    (eoDatabase, eoDomain, eoTable, eoView, eoProcedure, eoFunction,
     eoGenerator, eoException, eoBLOBFilter, eoRole, eoTrigger, eoForeign,
     eoIndexes, eoChecks, eoData);

  TExtractType =
    (etDomain, etTable, etRole, etTrigger, etForeign,
     etIndex, etData, etGrant, etCheck);

  TExtractTypes = Set of TExtractType;

  TProcDDLType = (pdCreateProc,pdCreateStub,pdAlterProc);

  { TIBExtract }

  TIBExtract = class(TComponent)
  private
    FDatabase : TIBDatabase;
    FTransaction : TIBTransaction;
    FMetaData: TStrings;
    FDatabaseInfo: TIBDatabaseInfo;
    FShowSystem: Boolean;
    { Private declarations }
    procedure Add2MetaData(const Msg: string; IsError: boolean=true);
    function GetDatabase: TIBDatabase;
    function GetIndexSegments ( indexname : String) : String;
    function GetTransaction: TIBTransaction;
    function GetTriggerType(TypeID: integer): string;
    procedure SetDatabase(const Value: TIBDatabase);
    procedure SetTransaction(const Value: TIBTransaction);
    function PrintValidation(ToValidate : String;	flag : Boolean) : String;
    procedure ShowGrants(MetaObject: String; Terminator : String);
    procedure ShowGrantsTo(MetaObject: String; ObjectType: integer;
      Terminator: String);
    procedure ShowGrantRoles(Terminator : String);
    procedure GetProcedureArgs(Proc : String);
  protected
    function ExtractDDL(Flag: Boolean; TableName: String; IncludeData: boolean =
      false): Boolean;
    function ExtractListTable(RelationName, NewName: String; DomainFlag: Boolean): Boolean;
    procedure ExtractListView (ViewName : String);
    procedure ListData(ObjectName : String);
    procedure ListRoles(ObjectName : String = '');
    procedure ListGrants;
    procedure ListProcs(ProcDDLType: TProcDDLType = pdCreateProc; ProcedureName : String = '';
      IncludeGrants:boolean=false);
    procedure ListAllTables(flag : Boolean);
    procedure ListTriggers(ObjectName: String=''; ExtractTypes: TExtractTypes = [etTrigger]);
    procedure ListCheck(ObjectName : String = ''; ExtractType : TExtractType = etCheck);
    function PrintSet(var Used : Boolean) : String;
    procedure ListCreateDb(TargetDb : String = '');
    procedure ListDomains(ObjectName : String = ''; ExtractType : TExtractType = etDomain);
    procedure ListException(ExceptionName : String = '');
    procedure ListFilters(FilterName : String = '');
    procedure ListForeign(ObjectName : String = ''; ExtractType : TExtractType = etForeign);
    procedure ListFunctions(FunctionName : String = '');
    procedure ListGenerators(GeneratorName : String = ''; ExtractTypes: TExtractTypes=[]);
    procedure ListIndex(ObjectName : String = ''; ExtractType : TExtractType = etIndex);
    procedure ListViews(ViewName : String = '');
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetArrayField(FieldName : String) : String;
    function GetFieldType(FieldType, FieldSubType, FieldScale, FieldSize,
      FieldPrec, FieldLen : Integer) : String;
    function GetCharacterSets(CharSetId, Collation : integer;	CollateOnly : Boolean) : String;
    procedure ExtractObject(ObjectType : TExtractObjectTypes; ObjectName : String = '';
      ExtractTypes : TExtractTypes = []);
    property DatabaseInfo : TIBDatabaseInfo read FDatabaseInfo;
    property Items : TStrings read FMetaData;

  published
    { Published declarations }
    property Database : TIBDatabase read GetDatabase write SetDatabase;
    property Transaction : TIBTransaction read GetTransaction write SetTransaction;
    property ShowSystem: Boolean read FShowSystem write FShowSystem;
  end;

  TSQLType = record
    SqlType : Integer;
    TypeName : String;
  end;

  TPrivTypes = record
    PrivFlag : Integer;
    PrivString : String;
  end;

  TSQLTypes = Array[0..14] of TSQLType;

const

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
    (SqlType : blr_int64; TypeName : 'INT64'),
    (SqlType : blr_bool; TypeName : 'BOOLEAN'));

  SubTypes : Array[0..8] of String = (
    'UNKNOWN',			{ NTX: keyword }
    'TEXT',				{ NTX: keyword }
    'BLR',				{ NTX: keyword }
    'ACL',				{ NTX: keyword }
    'RANGES',			{ NTX: keyword }
    'SUMMARY',			{ NTX: keyword }
    'FORMAT',			{ NTX: keyword }
    'TRANSACTION_DESCRIPTION',	{ NTX: keyword }
    'EXTERNAL_FILE_DESCRIPTION');	{ NTX: keyword }

  IntegralSubtypes : Array[0..2] of String = (
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



  MAX_INTSUBTYPES = 2;
  MAXSUBTYPES = 8;     { Top of subtypes array }

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
  obj_count = 11;
  obj_user_group = 12;
  obj_sql_role = 13;

implementation

uses FBMessages, IBDataOutput;

const
  TERM = ';';
  ProcTerm = '^';

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

  PrecisionSQL =
    'SELECT * FROM RDB$FIELDS ' +
    'WHERE RDB$FIELD_NAME = :FIELDNAME';

  ArraySQL =
    'SELECT * FROM RDB$FIELD_DIMENSIONS FDIM ' +
    'WHERE ' +
    '  FDIM.RDB$FIELD_NAME = :FIELDNAME ' +
    'ORDER BY FDIM.RDB$DIMENSION';

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
    Result := Result + '] ';
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
end;

destructor TIBExtract.Destroy;
begin
  FMetaData.Free;
  FDatabasEInfo.Free;
  inherited;
end;

function TIBExtract.ExtractDDL(Flag: Boolean; TableName: String; IncludeData: boolean = false) : Boolean;
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

  FMetaData.Add(Format('SET SQL DIALECT %d;', [FDatabase.SQLDialect]));
  FMetaData.Add('SET AUTODDL ON;');
  FMetaData.Add('');

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
    ListCreateDb;
    ListFilters;
    ListFunctions;
    ListDomains;
    ListAllTables(flag);
    if IncludeData then
      ListData('');
    ListIndex;
    ListForeign;
    if IncludeData then
      ListGenerators('',[etData])
    else
      ListGenerators;
    ListViews;
    ListCheck;
    ListException;
    ListProcs(pdCreateStub);
    ListTriggers;
    ListProcs(pdAlterProc);
    ListGrants;
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
  Collation, CharSetId : integer;
	i : integer;
  ColList, Column, Constraint : String;
  SubType : integer;
  IntChar : integer;
  qryTables, qryPrecision, qryConstraints, qryRelConstraints, qryGenerators : TIBSQL;
  PrecisionKnown, ValidRelation : Boolean;
  FieldScale, FieldType : Integer;
  CreateTable: string;
  TableType: integer;
begin
  Result := true;
  ColList := '';
  IntChar := 0;
  ValidRelation := false;

  if DomainFlag then
    ListDomains(RelationName);
  qryTables := TIBSQL.Create(FDatabase);
  qryPrecision := TIBSQL.Create(FDatabase);
  qryConstraints := TIBSQL.Create(FDatabase);
  qryRelConstraints := TIBSQL.Create(FDatabase);
  qryGenerators := TIBSQL.Create(FDatabase);
  try
    qryTables.SQL.Add(TableListSQL);
    RelationName := trim(RelationName);
    qryTables.Params.ByName('RelationName').AsString := RelationName;
    qryTables.ExecQuery;
    qryPrecision.SQL.Add(PrecisionSQL);
    qryConstraints.SQL.Add(ConstraintSQL);
    qryRelConstraints.SQL.Add(RelConstraintsSQL);
    qryGenerators.SQL.Add(GetGeneratorSQL);
    if not qryTables.Eof then
    begin
      ValidRelation := true;
      TableType := qryTables.FieldByName('RDB$RELATION_TYPE').AsInteger;
      if (not qryTables.FieldByName('RDB$OWNER_NAME').IsNull) and
         (Trim(qryTables.FieldByName('RDB$OWNER_NAME').AsString) <> '') then
        FMetaData.Add(Format('%s/* Table: %s, Owner: %s */%s',
          [LineEnding, RelationName,
           qryTables.FieldByName('RDB$OWNER_NAME').AsString, LineEnding]));
      if TableType > 3 then
       CreateTable := 'CREATE GLOBAL TEMPORARY TABLE'
      else
        CreateTable := 'CREATE TABLE';
      if NewName <> '' then
        FMetaData.Add(Format('%s %s ', [CreateTable,QuoteIdentifier(FDatabase.SQLDialect,NewName)]))
      else
        FMetaData.Add(Format('%s %s ', [CreateTable,QuoteIdentifier(FDatabase.SQLDialect,RelationName)]));
      if not qryTables.FieldByName('RDB$EXTERNAL_FILE').IsNull then
        FMetaData.Add(Format('EXTERNAL FILE %s ',
          [QuotedStr(qryTables.FieldByName('RDB$EXTERNAL_FILE').AsString)]));
      FMetaData.Add('(');
    end;

    while not qryTables.Eof do
    begin
      Column := '  ' + QuoteIdentifier(FDatabase.SQLDialect, qryTables.FieldByName('RDB$FIELD_NAME').AsString) + TAB;

    {  Check first for computed fields, then domains.
       If this is a known domain, then just print the domain rather than type
       Domains won't have length, array, or blob definitions, but they
       may have not null, default and check overriding their definitions }

      if not qryTables.FieldByName('rdb$computed_blr').IsNull then
      begin
        Column := Column + ' COMPUTED BY ';
       if not qryTables.FieldByName('RDB$COMPUTED_SOURCE').IsNull then
         Column := Column + PrintValidation(qryTables.FieldByName('RDB$COMPUTED_SOURCE').AsString, true);
      end
      else
      begin
        FieldType := qryTables.FieldByName('RDB$FIELD_TYPE').AsInteger;
        FieldScale := qryTables.FieldByName('RDB$FIELD_SCALE').AsInteger;
        if not ((Copy(qryTables.FieldByName('RDB$FIELD_NAME1').AsString, 1, 4) = 'RDB$') and
          (qryTables.FieldByName('RDB$FIELD_NAME1').AsString[5] in ['0'..'9'])) and
          (qryTables.FieldByName('RDB$SYSTEM_FLAG').AsInteger <> 1) then
        begin
          Column := Column + QuoteIdentifier(FDatabase.SQLDialect, trim(qryTables.FieldByName('RDB$FIELD_NAME1').AsString));
          { International character sets }
          if (qryTables.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_text, blr_varying])
              and (not qryTables.FieldByName('RDB$COLLATION_ID').IsNull)
              and (qryTables.FieldByName('RDB$COLLATION_ID').AsInteger <> 0) then
          begin
            Collation := qryTables.FieldByName('RDB$COLLATION_ID').AsInteger;
            Column := Column + GetCharacterSets(qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsShort,
                             Collation, true);
          end;
        end
        else
        begin
  	      { Look through types array }
          for i := Low(Columntypes) to High(ColumnTypes) do
          begin
            PrecisionKnown := false;
            if qryTables.FieldByname('RDB$FIELD_TYPE').AsShort = ColumnTypes[i].SQLType then
            begin

              if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
              begin
                { Handle Integral subtypes NUMERIC and DECIMAL }
                if qryTables.FieldByName('RDB$FIELD_TYPE').AsInteger in
                        [blr_short, blr_long, blr_int64] then
                begin
                  qryPrecision.Params.ByName('FIELDNAME').AsString :=
                    qryTables.FieldByName('RDB$FIELD_NAME1').AsString;
                  qryPrecision.ExecQuery;

                  { We are ODS >= 10 and could be any Dialect }
                  if not qryPrecision.FieldByName('RDB$FIELD_PRECISION').IsNull then
                  begin
                  { We are Dialect >=3 since FIELD_PRECISION is non-NULL }
                    if (qryPrecision.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and
                       (qryPrecision.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then
                    begin
                      Column := column + Format('%s(%d, %d)',
                         [IntegralSubtypes[qryPrecision.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],
                         qryPrecision.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                        -qryPrecision.FieldByName('RDB$FIELD_SCALE').AsInteger]);
                      PrecisionKnown := TRUE;
                    end;
                  end;
                  qryPrecision.Close;
                end;
              end;

              if PrecisionKnown = FALSE then
              begin
                { Take a stab at numerics and decimals }
                if (FieldType = blr_short) and (FieldScale < 0) then
                  Column := Column + Format('NUMERIC(4, %d)', [-FieldScale])
                else
                  if (FieldType = blr_long) and (FieldScale < 0) then
                    Column := Column + Format('NUMERIC(9, %d)', [-FieldScale])
                  else
                    if (FieldType = blr_double) and (FieldScale < 0) then
                      Column := Column + Format('NUMERIC(15, %d)', [-FieldScale])
                    else
                      Column := Column + ColumnTypes[i].TypeName;
              end;
            end;
          end;
          if FieldType in [blr_text, blr_varying] then
            if qryTables.FieldByName('RDB$CHARACTER_LENGTH').IsNull then
              Column := Column + Format('(%d)', [qryTables.FieldByName('RDB$FIELD_LENGTH').AsInteger])
            else
              Column := Column + Format('(%d)', [qryTables.FieldByName('RDB$CHARACTER_LENGTH').AsInteger]);

          { Catch arrays after printing the type  }

          if not qryTables.FieldByName('RDB$DIMENSIONS').IsNull and (qryTables.FieldByName('RDB$DIMENSIONS').AsInteger > 0) then
            Column := column + GetArrayField(qryTables.FieldByName('RDB$FIELD_SOURCE').AsString);

          if FieldType = blr_blob then
          begin
            subtype := qryTables.FieldByName('RDB$FIELD_SUB_TYPE').AsShort;
            Column := Column + ' SUB_TYPE ';
            if (subtype > 0) and (subtype <= MAXSUBTYPES) then
              Column := Column + SubTypes[subtype]
            else
              Column := Column + IntToStr(subtype);
            column := Column + Format(' SEGMENT SIZE %d',
                [qryTables.FieldByName('RDB$SEGMENT_LENGTH').AsInteger]);
          end;

          { International character sets }
          if ((FieldType in [blr_text, blr_varying]) or
              (FieldType = blr_blob)) and
             (not qryTables.FieldByName('RDB$CHARACTER_SET_ID').IsNull) and
             (qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsInteger <> 0) then
          begin
            { Override rdb$fields id with relation_fields if present }

            CharSetId := 0;
            if not qryTables.FieldByName('RDB$CHARACTER_SET_ID').IsNull then
              CharSetId := qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsInteger;

            Column := Column + GetCharacterSets(CharSetId, 0, false);
            intchar := 1;
          end;
        end;

        {Firebird 3 introduces IDENTITY columns. We need to check for them here}
        if qryTables.HasField('RDB$GENERATOR_NAME') then
        begin
          qryGenerators.ParamByName('GENERATOR').AsString :=  qryTables.FieldByName('RDB$GENERATOR_NAME').AsString;
          qryGenerators.ExecQuery;
          if not qryGenerators.Eof then
          begin
            Column := Column + Format(' GENERATED BY DEFAULT AS IDENTITY START WITH %d',
                     [qryGenerators.FieldByName('RDB$INITIAL_VALUE').AsInteger]);
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
                [ QuoteIdentifier( FDatabase.SQLDialect,
                      qryConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString)]);
            qryConstraints.Next;
          end;
          qryConstraints.Close;
          Column := Column + ' NOT NULL';
        end;

        if ((FieldType in [blr_text, blr_varying]) or
            (FieldType = blr_blob)) and
           (not qryTables.FieldByName('RDB$CHARACTER_SET_ID').IsNull) and
           (qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsInteger <> 0) and
           (intchar <> 0) then
        begin
          Collation := 0;
          if not qryTables.FieldByName('RDB$COLLATION_ID1').IsNull then
            Collation := qryTables.FieldByName('RDB$COLLATION_ID1').AsInteger
          else
            if not qryTables.FieldByName('RDB$COLLATION_ID').IsNull then
              Collation := qryTables.FieldByName('RDB$COLLATION_ID').AsInteger;

          CharSetId := 0;
          if not qryTables.FieldByName('RDB$CHARACTER_SET_ID').IsNull then
            CharSetId := qryTables.FieldByName('RDB$CHARACTER_SET_ID').AsInteger;

          if Collation <> 0 then
            Column := Column + GetCharacterSets(CharSetId, Collation, true);
        end;
      end;
      qryTables.Next;
      if not qryTables.Eof then
        Column := Column + ',';
      FMetaData.Add(Column);
    end;

    { Do primary and unique keys only. references come later }

    qryRelConstraints.Params.ByName('relationname').AsString := RelationName;
    qryRelConstraints.ExecQuery;
    while not qryRelConstraints.Eof do
    begin
      Constraint := '';
      FMetaData.Strings[FMetaData.Count - 1] := FMetaData.Strings[FMetaData.Count - 1]  + ',';
      { If the name of the constraint is not INTEG..., print it }
      if Pos('INTEG', qryRelConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString) <> 1 then
        Constraint := Constraint + 'CONSTRAINT ' +
          QuoteIdentifier(FDatabase.SQLDialect,
          qryRelConstraints.FieldByName('RDB$CONSTRAINT_NAME').AsString);


      if Pos('PRIMARY', qryRelConstraints.FieldByName('RDB$CONSTRAINT_TYPE').AsString) = 1 then
      begin
        FMetaData.Add(Constraint + Format(' PRIMARY KEY (%s)',
           [GetIndexSegments(qryRelConstraints.FieldByName('RDB$INDEX_NAME').AsString)]));
      end
      else
        if Pos('UNIQUE', qryRelConstraints.FieldByName('RDB$CONSTRAINT_TYPE').AsString) = 1 then
        begin
          FMetaData.Add(Constraint + Format(' UNIQUE (%s)',
             [GetIndexSegments(qryRelConstraints.FieldByName('RDB$INDEX_NAME').AsString)]));
        end;
      qryRelConstraints.Next;
    end;
    if ValidRelation then
    begin
      if TableType = 4 then
        FMetaData.Add(' ) ON COMMIT PRESERVE ROWS ' + TERM)
      else
       FMetaData.Add(')' + TERM);
    end;
  finally
    qryTables.Free;
    qryPrecision.Free;
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
begin
  qryViews := TIBSQL.Create(FDatabase);
  qryColumns := TIBSQL.Create(FDatabase);
  try
    qryViews.SQL.Add(ViewsSQL);
    qryViews.Params.ByName('viewname').AsString := ViewName;
    qryViews.ExecQuery;
    while not qryViews.Eof do
    begin
      FMetaData.Add('');
      RelationName := QuoteIdentifier(FDatabase.SQLDialect,
          qryViews.FieldByName('RDB$RELATION_NAME').AsString);
      FMetaData.Add(Format('%s/* View: %s, Owner: %s */%s', [
        RelationName,
        Trim(qryViews.FieldByName('RDB$OWNER_NAME').AsString)]));
      FMetaData.Add('');
      FMetaData.Add(Format('CREATE VIEW %s (', [RelationName]));

      { Get Column List}
      qryColumns.SQL.Add(ColumnsSQL);
      qryColumns.Params.ByName('relationname').AsString := RelationName;
      qryColumns.ExecQuery;
      while not qryColumns.Eof do
      begin
        ColList := ColList + QuoteIdentifier(FDatabase.SQLDialect,
              qryColumns.FieldByName('RDB$FIELD_NAME').AsString);
        qryColumns.Next;
        if not qryColumns.Eof then
          ColList := ColList + ', ';
      end;
      FMetaData.Add(ColList + ') AS');
      FMetaData.Add(qryViews.FieldByName('RDB$VIEW_SOURCE').AsString + Term);
      qryViews.Next;
    end;
  finally
    qryViews.Free;
    qryColumns.Free;
  end;
end;

function TIBExtract.GetCharacterSets(CharSetId, Collation: integer;
  CollateOnly: Boolean): String;
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

procedure TIBExtract.Add2MetaData(const Msg: string; IsError: boolean);
begin
  FMetaData.Add(Msg);
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

      Result := Result + QuoteIdentifier(FDatabase.SQLDialect,
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

function TIBExtract.GetTriggerType(TypeID: integer): string;
var separator: string;
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
    until TypeID = 0;
  end;
end;

{	   ListAllGrants
  Functional description
 	 Print the permissions on all user tables.
 	 Get separate permissions on table/views and then procedures }

procedure TIBExtract.ListGrants;
const
  SecuritySQL = 'SELECT * FROM RDB$RELATIONS ' +
                'WHERE ' +
                '  (RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL) AND ' +
                '  RDB$SECURITY_CLASS STARTING WITH ''SQL$'' ' +
                'ORDER BY RDB$RELATION_NAME';

  ProcedureSQL = 'select * from RDB$PROCEDURES '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$PROCEDURE_NAME';

  ExceptionSQL = 'select * from RDB$EXCEPTIONS '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$EXCEPTION_NAME';

  GeneratorSQL = 'select * from RDB$GENERATORS '+
                 'Where RDB$SYSTEM_FLAG <> 1 OR RDB$SYSTEM_FLAG IS NULL ' +
                 'Order BY RDB$GENERATOR_NAME';

var
  qryRoles : TIBSQL;
  RelationName : String;
begin
  ListRoles;
  qryRoles := TIBSQL.Create(FDatabase);
  try
  { This version of cursor gets only sql tables identified by security class
     and misses views, getting only null view_source }

    FMetaData.Add('');
    FMetaData.Add('/* Grant permissions for this database */');
    FMetaData.Add('');

    try
      qryRoles.SQL.Text := SecuritySQL;
      qryRoles.ExecQuery;
      while not qryRoles.Eof do
      begin
        RelationName := Trim(qryRoles.FieldByName('rdb$relation_Name').AsString);
        ShowGrants(RelationName, Term);
        qryRoles.Next;
      end;
    finally
     qryRoles.Close;
    end;

    ShowGrantRoles(Term);

    qryRoles.SQL.Text := ExceptionSQL;
    qryRoles.ExecQuery;
    try
      while not qryRoles.Eof do
      begin
        ShowGrants(Trim(qryRoles.FieldByName('RDB$EXCEPTION_NAME').AsString), Term);
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
        ShowGrants(Trim(qryRoles.FieldByName('RDB$GENERATOR_NAME').AsString), Term);
        qryRoles.Next;
      end;
    finally
      qryRoles.Close;
    end;

    qryRoles.SQL.Text := ProcedureSQL;
    qryRoles.ExecQuery;
    try
      while not qryRoles.Eof do
      begin
        ShowGrants(Trim(qryRoles.FieldByName('RDB$PROCEDURE_NAME').AsString), Term);
        qryRoles.Next;
      end;
    finally
      qryRoles.Close;
    end;
  finally
    qryRoles.Free;
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
  CreateProcedureStr2 = 'BEGIN EXIT; END %s%s';
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

  ProcedureNameSQL =
    'SELECT * FROM RDB$PROCEDURES ' +
    'WHERE RDB$PROCEDURE_NAME = :ProcedureName ' +
    'ORDER BY RDB$PROCEDURE_NAME';

var
  qryProcedures : TIBSQL;
  ProcName : String;
  SList : TStrings;
  Header : Boolean;

begin

  Header := true;
  qryProcedures := TIBSQL.Create(FDatabase);
  SList := TStringList.Create;
  try
    if ProcedureName = '' then
      qryProcedures.SQL.Text := ProcedureSQL
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
        FMetaData.Add('COMMIT WORK;');
        FMetaData.Add('SET AUTODDL OFF;');
        FMetaData.Add(Format('SET TERM %s %s', [ProcTerm, Term]));
        FMetaData.Add(Format('%s/* Stored procedures */%s', [LineEnding, LineEnding]));
        Header := false;
      end;
      ProcName := Trim(qryProcedures.FieldByName('RDB$PROCEDURE_NAME').AsString);

      case ProcDDLType of
      pdCreateStub:
        begin
          FMetaData.Add(Format(CreateProcedureStr1, [QuoteIdentifier(FDatabase.SQLDialect,
             ProcName)]));
          GetProcedureArgs(ProcName);
          FMetaData.Add(Format(CreateProcedureStr2, [ProcTerm, LineEnding]));
        end;

      pdCreateProc:
      begin
        FMetaData.Add(Format(CreateProcedureStr1, [QuoteIdentifier(FDatabase.SQLDialect,
           ProcName)]));
        GetProcedureArgs(ProcName);
        if not qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').IsNull then
        begin
          SList.Text := qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').AsString;
          SList.Add(Format(' %s%s', [ProcTerm, LineEnding]));
          FMetaData.AddStrings(SList);
        end
        else
          FMetaData.Add(Format(CreateProcedureStr2, [ProcTerm, LineEnding]));
      end;

      pdAlterProc:
       begin
         FMetaData.Add(Format('%sALTER PROCEDURE %s ', [LineEnding,
            QuoteIdentifier(FDatabase.SQLDialect, ProcName)]));
         GetProcedureArgs(ProcName);

         if not qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').IsNull then
         begin
           SList.Text := qryProcedures.FieldByName('RDB$PROCEDURE_SOURCE').AsString;
           SList.Add(Format(' %s%s', [ProcTerm, LineEnding]));
           FMetaData.AddStrings(SList);
         end
         else
           FMetaData.Add(Format(CreateProcedureStr2, [ProcTerm, LineEnding]));
       end;
      end;
      if IncludeGrants then
        ShowGrantsTo(ProcName,obj_procedure,ProcTerm);
      qryProcedures.Next;
    end;
    qryProcedures.Close;

    if not Header then
    begin
      FMetaData.Add(Format('SET TERM %s %s', [Term, ProcTerm]));
      FMetaData.Add('COMMIT WORK;');
      FMetaData.Add('SET AUTODDL ON;');
    end;
  finally
    qryProcedures.Free;
    SList.Free;
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

var
  Header : Boolean;
  TriggerName, RelationName, InActive: String;
  qryTriggers : TIBSQL;
  SList : TStrings;
begin
  Header := true;
  SList := TStringList.Create;
  qryTriggers := TIBSQL.Create(FDatabase);
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
      SList.Clear;
      if Header then
      begin
        FMetaData.Add(Format('SET TERM %s %s%s', [Procterm, Term, LineEnding]));
        FMetaData.Add(Format('%s/* Triggers only will work for SQL triggers */%s',
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

      if qryTriggers.FieldByName('RDB$FLAGS').AsInteger <> 1 then
        SList.Add('/* ');

      {Database or Transaction trigger}
      SList.Add(Format('CREATE TRIGGER %s%s%s %s POSITION %d',
                [QuoteIdentifier(FDatabase.SQLDialect, TriggerName),
                LineEnding, InActive,
                GetTriggerType(qryTriggers.FieldByName('RDB$TRIGGER_TYPE').AsInteger),
                qryTriggers.FieldByName('RDB$TRIGGER_SEQUENCE').AsInteger]));

      if RelationName <> '' then
        SList.Add('ON ' + QuoteIdentifier(FDatabase.SQLDialect, RelationName));

      if not qryTriggers.FieldByName('RDB$TRIGGER_SOURCE').IsNull then
        SList.Add(qryTriggers.FieldByName('RDB$TRIGGER_SOURCE').AsString)
      else
        SList.Add('AS BEGIN EXIT; END');
      SList.Add(' ' + ProcTerm);
      if qryTriggers.FieldByName('RDB$FLAGS').AsInteger <> 1 then
        SList.Add(' */');
      FMetaData.AddStrings(SList);
      if etGrant in ExtractTypes then
        ShowGrantsTo(TriggerName,obj_trigger,ProcTerm);
      qryTriggers.Next;
    end;
    if not Header then
    begin
      FMetaData.Add('COMMIT WORK ' + ProcTerm);
      FMetaData.Add('SET TERM ' + Term + ProcTerm);
    end;
  finally
    qryTriggers.Free;
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
		    [QuoteIdentifier(FDatabase.SQLDialect, RelationName)]));
      if Pos('INTEG', qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsString) <> 1 then
        SList.Add(Format('%sCONSTRAINT %s ', [TAB,
          QuoteIdentifier(FDatabase.SQLDialect, qryChecks.FieldByName('RDB$CONSTRAINT_NAME').AsString)]));

      if not qryChecks.FieldByName('RDB$TRIGGER_SOURCE').IsNull then
        SList.Text := SList.Text + qryChecks.FieldByName('RDB$TRIGGER_SOURCE').AsString;

      SList.Strings[SList.Count - 1] := SList.Strings[SList.Count - 1] + (Term) + LineEnding;
      FMetaData.AddStrings(SList);
      qryChecks.Next;
    end;
  finally
    qryChecks.Free;
    SList.Free;
  end;
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

  FilesSQL =
    'select * from RDB$FILES ' +
    'order BY RDB$SHADOW_NUMBER, RDB$FILE_SEQUENCE';

  LogsSQL =
    'SELECT * FROM RDB$LOG_FILES ' +
    'ORDER BY RDB$FILE_FLAGS, RDB$FILE_SEQUENCE';

var
  NoDb, First, FirstFile, HasWal, SetUsed : Boolean;
  Buffer : String;
  qryDB : TIBSQL;
  FileFlags, FileLength, FileSequence, FileStart : Integer;

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
  FMetaData.Add(Buffer);
  Buffer := '';

  qryDB := TIBSQL.Create(FDatabase);
  try
    qryDB.SQL.Text := CharInfoSQL;
    qryDB.ExecQuery;

    if not qryDB.EOF then
      Buffer := Format(' DEFAULT CHARACTER SET %s',
        [trim(qryDB.FieldByName('RDB$CHARACTER_SET_NAME').AsString)]);
    if NoDB then
      Buffer := Buffer + Term + ' */'
    else
      Buffer := Buffer + Term;
    FMetaData.Add(Buffer);
    qryDB.Close;
    {List secondary files and shadows as
      alter db and create shadow in comment}
    qryDB.SQL.Text := FilesSQL;
    qryDB.ExecQuery;
    while not qryDB.Eof do
    begin
      if First then
      begin
        FMetaData.Add(LineEnding + '/* Add secondary files in comments ');
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
        FMetaData.Add(Buffer);
      end; //end_if
      if (FileFlags and FILE_cache) <> 0 then
        FMetaData.Add(Format('%sALTER DATABASE ADD CACHE ''%s'' LENGTH %d',
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
        FMetaData.Add(Buffer);
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
      FMetaData.Add(Buffer);
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
      FMetaData.Add(Buffer);

    end;
    if not First then
    begin
      if NoDB then
        FMetaData.Add(Format('%s */%s', [LineEnding, LineEnding]))
      else
        FMetaData.Add(Format('%s%s%s', [Term, LineEnding, LineEnding]));
    end;
  finally
    qryDB.Free;
  end;

(*
*)
end;

{	             ListDomainTable
  Functional description
  	List domains as identified by fields with any constraints on them
  	for the named table

  	Parameters:  table_name == only extract domains for this table }

procedure TIBExtract.ListDomains(ObjectName: String; ExtractType : TExtractType);
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

  function FormatDomainStr : String;
  var
    i, SubType : Integer;
    PrecisionKnown : Boolean;
  begin
    Result := '';
    for i := Low(ColumnTypes) to High(ColumnTypes) do
      if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = ColumnTypes[i].SQLType then
      begin
        PrecisionKnown := FALSE;
        if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
        begin
          if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_short, blr_long, blr_int64] then
          begin
            { We are ODS >= 10 and could be any Dialect }
            if (FDatabaseInfo.DBSQLDialect >= 3) and
               (not qryDomains.FieldByName('RDB$FIELD_PRECISION').IsNull) and
               (qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and
               (qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then
            begin
              Result := Result + Format('%s(%d, %d)', [
                IntegralSubtypes [qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],
                qryDomains.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                -1 * qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger]);
              PrecisionKnown := true;
            end;
          end;
        end;
        if PrecisionKnown = false then
        begin
          { Take a stab at numerics and decimals }
          if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_short) and
              (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
            Result := Result + Format('NUMERIC(4, %d)',
              [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] )
          else
            if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_long) and
                (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
              Result := Result + Format('NUMERIC(9, %d)',
                [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] )
            else
              if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_double) and
                  (qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger  < 0) then
                Result := Result + Format('NUMERIC(15, %d)',
                  [-qryDomains.FieldByName('RDB$FIELD_SCALE').AsInteger] )
              else
                Result := Result + ColumnTypes[i].TypeName;
        end;
        break;
      end;

    if qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_blob then
    begin
      subtype := qryDomains.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      Result := Result + ' SUB_TYPE ';
      if (subtype > 0) and (subtype <= MAXSUBTYPES) then
        Result := Result + SubTypes[subtype]
      else
        Result := Result + Format('%d', [subtype]);
      Result := Result + Format(' SEGMENT SIZE %d', [qryDomains.FieldByName('RDB$SEGMENT_LENGTH').AsInteger]);
    end //end_if
    else
    if (qryDomains.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_text, blr_varying]) then
    begin
       if not qryDomains.FieldByName('RDB$CHARACTER_LENGTH').IsNull then
         Result := Result + Format('(%d)', [qryDomains.FieldByName('RDB$CHARACTER_LENGTH').AsInteger])
       else
         Result := Result + Format('(%d)', [qryDomains.FieldByName('RDB$FIELD_LENGTH').AsInteger]);
    end;

    { since the character set is part of the field type, display that
     information now. }
    if not qryDomains.FieldByName('RDB$CHARACTER_SET_ID').IsNull then
      Result := Result + GetCharacterSets(qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
         0, FALSE);
    if not qryDomains.FieldByName('RDB$DIMENSIONS').IsNull then
      Result := GetArrayField(qryDomains.FieldByName('RDB$FIELD_SOURCE').AsString);

    if not qryDomains.FieldByName('RDB$DEFAULT_SOURCE').IsNull then
      Result := Result + Format('%s%s %s', [LineEnding, TAB,
         qryDomains.FieldByName('RDB$DEFAULT_SOURCE').AsString]);

    if not qryDomains.FieldByName('RDB$VALIDATION_SOURCE').IsNull then
      if Pos('CHECK', AnsiUpperCase(qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString)) = 1 then
        Result := Result + Format('%s%s %s', [LineEnding, TAB,
           qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString])
      else
        Result := Result + Format('%s%s /* %s */', [LineEnding, TAB,
           qryDomains.FieldByName('RDB$VALIDATION_SOURCE').AsString]);

    if qryDomains.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then
      Result := Result + ' NOT NULL';

    { Show the collation order if one has been specified.  If the collation
       order is the default for the character set being used, then no collation
       order will be shown ( because it isn't needed ).

       If the collation id is 0, then the default for the character set is
       being used so there is no need to retrieve the collation information.}

    if (not qryDomains.FieldByName('RDB$COLLATION_ID').IsNull) and
       (qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger <> 0) then
      Result := Result + GetCharacterSets(qryDomains.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
        qryDomains.FieldByName('RDB$COLLATION_ID').AsInteger, true);
  end;

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
      FieldName := qryDomains.FieldByName('RDB$FIELD_NAME').AsString;
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
        FMetaData.Add('/* Domain definitions */');
        First := false;
      end;

      Line := Format('CREATE DOMAIN %s AS ', [FieldName]);
      Line := Line + FormatDomainStr + Term;
      FMetaData.Add(Line);
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
        FMetaData.Add('');
        FMetaData.Add('/*  Exceptions */');
        FMetaData.Add('');
        First := false;
      end; //end_if
      
      FMetaData.Add(Format('CREATE EXCEPTION %s %s%s',
        [QuoteIdentifier(FDatabase.SQLDialect, qryException.FieldByName('RDB$EXCEPTION_NAME').AsString),
        QuotedStr(qryException.FieldByName('RDB$MESSAGE').AsString), Term]));
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
        FMetaData.Add('');
        FMetaData.Add('/*  BLOB Filter declarations */');
        FMetaData.Add('');
        First := false;
      end; //end_if

      FMetaData.Add(Format('DECLARE FILTER %s INPUT_TYPE %d OUTPUT_TYPE %d',
        [qryFilters.FieldByName('RDB$FUNCTION_NAME').AsString,
         qryFilters.FieldByName('RDB$INPUT_SUB_TYPE').AsInteger,
         qryFilters.FieldByName('RDB$OUTPUT_SUB_TYPE').AsInteger]));
      FMetaData.Add(Format('%sENTRY_POINT ''%s'' MODULE_NAME ''%s''%s%',
        [TAB, qryFilters.FieldByName('RDB$ENTRYPOINT').AsString,
         qryFilters.FieldByName('RDB$MODULE_NAME').AsString, Term]));
      FMetaData.Add('');

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
      Line := Format('ALTER TABLE %s ADD ', [QuoteIdentifier(FDatabase.SQLDialect,
        qryForeign.FieldByName('RELC1_RELATION_NAME').AsString)]);

      { If the name of the constraint is not INTEG..., print it.
         INTEG... are internally generated names. }
      if (not qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').IsNull) and
         ( Pos('INTEG', qryForeign.FieldByName('RELC1_CONSTRAINT_NAME').AsString) <> 1) then
        Line := Line + Format('CONSTRAINT %s ', [QuoteIdentifier(FDatabase.SQLDialect,
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
      FMetaData.Add(Line);
      qryForeign.Next;
    end;
  finally
    qryForeign.Free;
  end;
end;

{    ListFunctions

 Functional description
   List all external functions

   Parameters:  none
  Results in
  DECLARE EXTERNAL FUNCTION function_name
                CHAR [256] , INTEGER, ....
                RETURNS INTEGER BY VALUE
                ENTRY_POINT entrypoint MODULE_NAME module; }

procedure TIBExtract.ListFunctions(FunctionName : String = '');
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

  FuncArgsPosSQL =
    'SELECT * FROM RDB$FUNCTION_ARGUMENTS ' +
    'WHERE ' +
    '  RDB$FUNCTION_NAME = :RDB$FUNCTION_NAME AND ' +
    '  RDB$ARGUMENT_POSITION = :RDB$ARGUMENT_POSITION';

  CharSetSQL =
    'SELECT * FROM RDB$CHARACTER_SETS ' +
    'WHERE ' +
    '  RDB$CHARACTER_SET_ID = :CHARACTER_SET_ID';

var
  qryFunctions, qryFuncArgs, qryCharSets, qryFuncPos : TIBSQL;
  First, FirstArg, DidCharset, PrecisionKnown : Boolean;
  ReturnBuffer, TypeBuffer, Line : String;
  i, FieldType : Integer;
begin
  First := true;
  qryFunctions := TIBSQL.Create(FDatabase);
  qryFuncArgs := TIBSQL.Create(FDatabase);
  qryFuncPos := TIBSQL.Create(FDatabase);
  qryCharSets := TIBSQL.Create(FDatabase);
  try
    if FunctionName = '' then
      qryFunctions.SQL.Text := FunctionSQL
    else
    begin
      qryFunctions.SQL.Text := FunctionNameSQL;
      qryFunctions.Params.ByName('FunctionName').AsString := FunctionName;
    end;
    qryFuncArgs.SQL.Text := FunctionArgsSQL;
    qryFuncPos.SQL.Text := FuncArgsPosSQL;
    qryCharSets.SQL.Text := CharSetSQL;
    qryFunctions.ExecQuery;
    while not qryFunctions.Eof do
    begin
      if First then
      begin
        FMEtaData.Add(Format('%s/*  External Function declarations */%s',
          [LineEnding, LineEnding]));
        First := false;
      end; //end_if
      { Start new function declaration }
      FMetaData.Add(Format('DECLARE EXTERNAL FUNCTION %s',
        [qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString]));
      Line := '';

      FirstArg := true;
      qryFuncArgs.Params.ByName('FUNCTION_NAME').AsString :=
         qryFunctions.FieldByName('RDB$FUNCTION_NAME').AsString;

      qryFuncArgs.ExecQuery;
      while not qryFuncArgs.Eof do
      begin
        { Find parameter type }
        i := 0;
        FieldType := qryFuncArgs.FieldByName('RDB$FIELD_TYPE').AsInteger;
        while FieldType <> ColumnTypes[i].SQLType do
          Inc(i);

        { Print length where appropriate }
        if FieldType in [ blr_text, blr_varying, blr_cstring] then
        begin
          DidCharset := false;

          qryCharSets.Params.ByName('CHARACTER_SET_ID').AsString :=
             qryFuncArgs.FieldByName('RDB$CHARACTER_SET_ID').AsString;
          qryCharSets.ExecQuery;
          while not qryCharSets.Eof do
          begin
            DidCharset := true;
            TypeBuffer := Format('%s(%d) CHARACTER SET %s',
              [ColumnTypes[i].TypeName,
               qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger div
               Max(1,qryCharSets.FieldByName('RDB$BYTES_PER_CHARACTER').AsInteger),
               qryCharSets.FieldByName('RDB$CHARACTER_SET_NAME').AsString]);
            qryCharSets.Next;
          end;
          qryCharSets.Close;
          if not DidCharset then
            TypeBuffer := Format('%s(%d)', [ColumnTypes[i].TypeName,
              qryFuncArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger]);
        end //end_if
        else
        begin
          PrecisionKnown := false;
          if (FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10) and
              (FieldType in [blr_short, blr_long, blr_int64]) then
          begin
            qryFuncPos.Params.ByName('RDB$FUNCTION_NAME').AsString :=
              qryFuncArgs.FieldByName('RDB$FUNCTION_NAME').AsString;
            qryFuncPos.Params.ByName('RDB$ARGUMENT_POSITION').AsInteger :=
              qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger;

            qryFuncPos.ExecQuery;
            while not qryFuncPos.Eof do
            begin
              { We are ODS >= 10 and could be any Dialect }
              if not qryFuncPos.FieldByName('RDB$FIELD_PRECISION').IsNull then
              begin
                { We are Dialect >=3 since FIELD_PRECISION is non-NULL }
                if (qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and
                    (qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then
                begin
                  TypeBuffer := Format('%s(%d, %d)',
                    [IntegralSubtypes[qryFuncPos.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],
                     qryFuncPos.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                     -qryFuncPos.FieldByName('RDB$FIELD_SCALE').AsInteger] );
                  PrecisionKnown := true;
                end; //end_if
              end; { if field_precision is not null }
              qryFuncPos.Next;
            end;
            qryFuncPos.Close;
          end; { if major_ods >= ods_version10 && }
          if not PrecisionKnown then
          begin
            { Take a stab at numerics and decimals }
            if (FieldType = blr_short) and
                (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
              TypeBuffer := Format('NUMERIC(4, %d)',
                [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
            else
              if (FieldType = blr_long) and
                  (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
                TypeBuffer := Format('NUMERIC(9, %d)',
                  [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
              else
                if (FieldType = blr_double) and
                    (qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
                  TypeBuffer := Format('NUMERIC(15, %d)',
                      [-qryFuncArgs.FieldByName('RDB$FIELD_SCALE').AsInteger])
                else
                  TypeBuffer := ColumnTypes[i].TypeName;
          end; { if  not PrecisionKnown  }
        end; { if FCHAR or VARCHAR or CSTRING ... else }

        if qryFunctions.FieldByName('RDB$RETURN_ARGUMENT').AsInteger =
               qryFuncArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger then
        begin
          ReturnBuffer := 'RETURNS ' + TypeBuffer;
          if qryFuncArgs.FieldByName('RDB$MECHANISM').AsInteger = 0 then
            ReturnBuffer := ReturnBuffer + ' BY VALUE ';
          if qryFuncArgs.FieldByName('RDB$MECHANISM').AsInteger < 0 then
            ReturnBuffer := ReturnBuffer + ' FREE_IT';
        end
        else
        begin
          { First arg needs no comma }
          if FirstArg then
          begin
            Line := Line + TypeBuffer;
            FirstArg := false;
          end
          else
            Line := Line + ', ' + TypeBuffer;
        end; //end_else
        qryFuncArgs.Next;
      end;
      qryFuncArgs.Close;

      FMetaData.Add(Line);
      FMetaData.Add(ReturnBuffer);
      FMetaData.Add(Format('ENTRY_POINT ''%s'' MODULE_NAME ''%s''%s%s%s',
        [qryFunctions.FieldByName('RDB$ENTRYPOINT').AsString,
         qryFunctions.FieldByName('RDB$MODULE_NAME').AsString,
         Term, LineEnding, LineEnding]));

      qryFunctions.Next;
    end;
  finally
    qryFunctions.Free;
    qryFuncArgs.Free;
    qryCharSets.Free;
    qryFuncPos.Free;
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
    FMetaData.Add('');
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
      FMetaData.Add(Format('CREATE SEQUENCE %s%s',
        [QuoteIdentifier(FDatabase.SQLDialect, GenName),
         Term]));
      if etData in ExtractTypes then
      begin
        qryValue.SQL.Text := Format(GeneratorValueSQL,[GenName]);
        qryValue.ExecQuery;
        try
          if not qryValue.EOF then
            FMetaData.Add(Format('ALTER SEQUENCE %s RESTART WITH %d;',
                 [QuoteIdentifier(FDatabase.SQLDialect, GenName),
                  qryValue.FieldByName('GENERATORVALUE').AsInteger]));
        finally
          qryValue.Close;
        end;
      end;
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
          FMetaData.Add(LineEnding + '/*  Index definitions for all user tables */' + LineEnding)
        else
          FMetaData.Add(LineEnding + '/*  Index definitions for ' + ObjectName + ' */' + LineEnding);
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
        QuoteIdentifier(FDataBase.SQLDialect,
            qryIndex.FieldByName('RDB$INDEX_NAME').AsString),
        QuoteIdentifier(FDataBase.SQLDialect,
            qryIndex.FieldByName('RDB$RELATION_NAME').AsString)]);

      Line := Line + GetIndexSegments(qryIndex.FieldByName('RDB$INDEX_NAME').AsString) +
          ')' + Term;

      FMetaData.Add(Line);
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
         [LineEnding, qryView.FieldByName('RDB$RELATION_NAME').AsString,
          qryView.FieldByName('RDB$OWNER_NAME').AsString, LineEnding]));

      SList.Add(Format('CREATE VIEW %s (', [QuoteIdentifier(FDatabase.SQLDialect,
        qryView.FieldByName('RDB$RELATION_NAME').AsString)]));

      qryColumns.Params.ByName('RELATION_NAME').AsString :=
          qryView.FieldByName('RDB$RELATION_NAME').AsString;
      qryColumns.ExecQuery;
      while not qryColumns.Eof do
      begin
        SList.Add('  ' + QuoteIdentifier(FDatabase.SQLDialect,
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
      FMetaData.AddStrings(SList);
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
  if FDatabase <> Value then
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
  case ObjectType of
    eoDatabase : ExtractDDL(true, '', etData in ExtractTypes);
    eoDomain :
      if etTable in ExtractTypes then
        ListDomains(ObjectName, etTable)
      else
        ListDomains(ObjectName);
    eoTable :
    begin
      if ObjectName <> '' then
      begin
        if etDomain in ExtractTypes then
          ListDomains(ObjectName, etTable);
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
       if (ObjectName <> '' ) and (etGrant in ExtractTypes) then
         ShowGrants(ObjectName, Term);
     end;
    eoFunction : ListFunctions(ObjectName);
    eoGenerator : ListGenerators(ObjectName,ExtractTypes);
    eoException : ListException(ObjectName);
    eoBLOBFilter : ListFilters(ObjectName);
    eoRole : ListRoles(ObjectName);
    eoTrigger : 
      if etTable in ExtractTypes then
      begin
        if etGrant in ExtractTypes then
          ListTriggers(ObjectName, [etTable,etGrant])
        else
          ListTriggers(ObjectName, [etTable])
      end
      else
      if etGrant in ExtractTypes then
        ListTriggers(ObjectName,[etTrigger,etGrant])
      else
        ListTriggers(ObjectName);
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
    eoData : ListData(ObjectName);
  end;
  if DidActivate then
    FTransaction.Commit;
end;

function TIBExtract.GetFieldType(FieldType, FieldSubType, FieldScale,
  FieldSize, FieldPrec, FieldLen: Integer): String;
var
  i : Integer;
  PrecisionKnown : Boolean;
begin
  Result := '';
  for i := Low(ColumnTypes) to High(ColumnTypes) do
    if FieldType = ColumnTypes[i].SQLType then
    begin
      PrecisionKnown := FALSE;
      if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
      begin
        if FieldType in [blr_short, blr_long, blr_int64] then
        begin
          { We are ODS >= 10 and could be any Dialect }
          if (FDatabaseInfo.DBSQLDialect >= 3) and
             (FieldPrec <> 0) and
             (FieldSubType > 0) and
             (FieldSubType <= MAX_INTSUBTYPES) then
          begin
            Result := Result + Format('%s(%d, %d)', [
              IntegralSubtypes [FieldSubType],
              FieldPrec,
              -1 * FieldScale]);
            PrecisionKnown := true;
          end;
        end;
      end;
      if PrecisionKnown = false then
      begin
        { Take a stab at numerics and decimals }
        if (FieldType = blr_short) and
            (FieldScale < 0) then
          Result := Result + Format('NUMERIC(4, %d)',
            [-FieldScale] )
        else
          if (FieldType = blr_long) and
              (FieldScale < 0) then
            Result := Result + Format('NUMERIC(9, %d)',
              [-FieldScale] )
          else
            if (FieldType = blr_double) and
                (FieldScale  < 0) then
              Result := Result + Format('NUMERIC(15, %d)',
                [-FieldScale] )
            else
              Result := Result + ColumnTypes[i].TypeName;
      end;
      break;
    end;
  if (FieldType in [blr_text, blr_varying]) and
     (FieldSize <> 0) then
    Result := Result + Format('(%d)', [FieldSize]);
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

procedure TIBExtract.ShowGrants(MetaObject: String; Terminator: String);
const
  GrantsBaseSelect =
  'Select Trim(RDB$USER) as RDB$USER,List("Privileges") as Privileges, '+
  'coalesce(RDB$GRANT_OPTION,0) as RDB$GRANT_OPTION,METAOBJECTNAME, '+
  'RDB$USER_TYPE, RDB$OBJECT_TYPE, '+
  'case  RDB$OBJECT_TYPE '+
  'When 0 then ''TABLE'' '+
  'When 5 then ''PROCEDURE'' '+
  'When 7 then ''EXCEPTION'' '+
  'When 11 then ''CHARACTER SET'' '+
  'When 14 then ''GENERATOR'' '+
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
  'PR.RDB$GRANT_OPTION,  PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE,OW.RDB$OWNER_NAME  '+
  'FROM RDB$USER_PRIVILEGES PR  '+
  'JOIN ObjectOwners OW On OW.METAOBJECTNAME = PR.RDB$RELATION_NAME and OW.ObjectType = PR.RDB$OBJECT_TYPE '+
  'Where PR.RDB$PRIVILEGE <> ''M'' and (PR.RDB$PRIVILEGE <> ''U'' or PR.RDB$FIELD_NAME is null)  '+
  'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE,OW.RDB$OWNER_NAME  '+
  'UNION  '+
  'Select PR.RDB$USER,PR.RDB$RELATION_NAME, ''Update('' || List(Trim(PR.RDB$FIELD_NAME)) || '')'',  '+
  'PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE, OW.RDB$OWNER_NAME   '+
  'FROM RDB$USER_PRIVILEGES PR  '+
  'JOIN ObjectOwners OW On OW.METAOBJECTNAME = PR.RDB$RELATION_NAME and OW.ObjectType = PR.RDB$OBJECT_TYPE '+
  'Where PR.RDB$PRIVILEGE = ''U'' and PR.RDB$FIELD_NAME is not null   '+
  'Group By PR.RDB$USER,PR.RDB$RELATION_NAME,PR.RDB$GRANT_OPTION, PR.RDB$USER_TYPE, PR.RDB$OBJECT_TYPE, OW.RDB$OWNER_NAME)  '+
  'Where METAOBJECTNAME = :METAOBJECTNAME and RDB$USER <> RDB$OWNER_NAME  '+
  'Group By RDB$USER,RDB$GRANT_OPTION,  RDB$USER_TYPE, RDB$OBJECT_TYPE,METAOBJECTNAME '+
  'ORDER BY RDB$USER, RDB$OBJECT_TYPE';

  GrantsSQL12 =
  'with ObjectOwners As ( '+
  'Select RDB$RELATION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 0 as ObjectType '+
  'From RDB$RELATIONS '+
  'UNION '+
  'Select RDB$PROCEDURE_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 5 as ObjectType '+
  'From RDB$PROCEDURES '+
  'UNION '+
  'Select RDB$EXCEPTION_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 7 as ObjectType '+
  'From RDB$EXCEPTIONS '+
  'UNION '+
  'Select RDB$GENERATOR_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 14 as ObjectType '+
  'From RDB$GENERATORS '+
  'UNION '+
  'Select RDB$CHARACTER_SET_NAME as METAOBJECTNAME, RDB$OWNER_NAME, 11 as ObjectType '+
  'From RDB$CHARACTER_SETS '+
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
      FMetaData.Add(Format('GRANT %s ON %s "%s" TO %s "%s" %s%s', [
                            qryOwnerPriv.FieldByName('Privileges').AsString,
                            qryOwnerPriv.FieldByName('OBJECT_TYPE_NAME').AsString,
                            qryOwnerPriv.FieldByName('METAOBJECTNAME').AsString,
                            qryOwnerPriv.FieldByName('USER_TYPE_NAME').AsString,
                            qryOwnerPriv.FieldByName('RDB$USER').AsString,
                            qryOwnerPriv.FieldByName('GRANTOPTION').AsString,
                            Terminator]));
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
      FMetaData.Add(Format('GRANT %s ON %s "%s" TO %s "%s" %s%s', [
                            qryOwnerPriv.FieldByName('Privileges').AsString,
                            qryOwnerPriv.FieldByName('OBJECT_TYPE_NAME').AsString,
                            qryOwnerPriv.FieldByName('METAOBJECTNAME').AsString,
                            qryOwnerPriv.FieldByName('USER_TYPE_NAME').AsString,
                            qryOwnerPriv.FieldByName('RDB$USER').AsString,
                            qryOwnerPriv.FieldByName('GRANTOPTION').AsString,
                            Terminator]));
      qryOwnerPriv.Next;
    end;
    qryOwnerPriv.Close;
  finally
    qryOwnerPriv.Free;
  end;
  FMetaData.Add('');
end;

{	  ShowGrantRoles
  Functional description
   	Show grants for given role name
   	This function is also called by extract for privileges.
   	All membership privilege may have the with_admin option set. }

procedure TIBExtract.ShowGrantRoles(Terminator: String);
const
  RoleSQL =
    'SELECT RDB$USER, RDB$GRANT_OPTION, RDB$RELATION_NAME ' +
    'FROM RDB$USER_PRIVILEGES ' +
    'WHERE ' +
    '  RDB$OBJECT_TYPE = %d AND ' +
    '  RDB$USER_TYPE = %d AND ' +
    '  RDB$PRIVILEGE = ''M'' ' +
    'ORDER BY  RDB$RELATION_NAME, RDB$USER';

var
  WithOption, UserString : String;
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
      FMetaData.Add(Format('GRANT %s TO %s%s%s%s',
        [ QuoteIdentifier(FDatabase.SQLDialect, qryRole.FieldByName('RDB$RELATION_NAME').AsString),
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
  FirstTime, PrecisionKnown : Boolean;
  Line : String;
  qryHeader : TIBSQL;

  function FormatParamStr : String;
  var
    i, CollationID, CharSetID : Integer;
  begin
    Result := Format('  %s ', [qryHeader.FieldByName('RDB$PARAMETER_NAME').AsString]);
    for i := Low(ColumnTypes) to High(ColumnTypes) do
      if qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger = ColumnTypes[i].SQLType then
      begin
        PrecisionKnown := FALSE;
        if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION10 then
        begin
          if qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_short, blr_long, blr_int64] then
          begin
            { We are ODS >= 10 and could be any Dialect }
            if (FDatabaseInfo.DBSQLDialect >= 3) and
               (not qryHeader.FieldByName('RDB$FIELD_PRECISION').IsNull) and
               (qryHeader.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0) and
               (qryHeader.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger <= MAX_INTSUBTYPES) then
            begin
              Result := Result + Format('%s(%d, %d)', [
                IntegralSubtypes [qryHeader.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger],
                qryHeader.FieldByName('RDB$FIELD_PRECISION').AsInteger,
                -1 * qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger]);
              PrecisionKnown := true;
            end;
          end;
        end;
        if PrecisionKnown = false then
        begin
          { Take a stab at numerics and decimals }
          if (qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_short) and
              (qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
            Result := Result + Format('NUMERIC(4, %d)',
              [-qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger] )
          else
            if (qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_long) and
                (qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger < 0) then
              Result := Result + Format('NUMERIC(9, %d)',
                [-qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger] )
            else
              if (qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger = blr_double) and
                  (qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger  < 0) then
                Result := Result + Format('NUMERIC(15, %d)',
                  [-qryHeader.FieldByName('RDB$FIELD_SCALE').AsInteger] )
              else
                Result := Result + ColumnTypes[i].TypeName;
        end;
        break;
      end;
    if (qryHeader.FieldByName('RDB$FIELD_TYPE').AsInteger in [blr_text, blr_varying]) then
    begin
       if not qryHeader.FieldByName('RDB$CHARACTER_LENGTH').IsNull then
         Result := Result + Format('(%d)', [qryHeader.FieldByName('RDB$CHARACTER_LENGTH').AsInteger])
       else
         Result := Result + Format('(%d)', [qryHeader.FieldByName('RDB$FIELD_LENGTH').AsInteger]);
    end;

    { Show international character sets and collations }

    if (not qryHeader.FieldByName('RDB$COLLATION_ID').IsNull) or
       (not qryHeader.FieldByName('RDB$CHARACTER_SET_ID').IsNull) then
    begin
      if qryHeader.FieldByName('RDB$COLLATION_ID').IsNull then
        CollationId := 0
      else
        CollationId := qryHeader.FieldByName('RDB$COLLATION_ID').AsInteger;

      if qryHeader.FieldByName('RDB$CHARACTER_SET_ID').IsNull then
        CharSetId := 0
      else
        CharSetId := qryHeader.FieldByName('RDB$CHARACTER_SET_ID').AsInteger;

      Result := Result + GetCharacterSets(CharSetId, CollationId, false);
    end;
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
        FMetaData.Add('(');
      end;

      Line := FormatParamStr;

      qryHeader.Next;
      if not qryHeader.Eof then
        Line := Line + ',';
      FMetaData.Add(Line);
    end;

    { If there was at least one param, close parens }
    if not FirstTime then
    begin
      FMetaData.Add( ')');
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
        FMetaData.Add('RETURNS' + LineEnding + '(');
      end;

      Line := FormatParamStr;

      qryHeader.Next;
      if not qryHeader.Eof then
        Line := Line + ',';
      FMetaData.Add(Line);
    end;

    { If there was at least one param, close parens }
    if not FirstTime then
    begin
      FMetaData.Add( ')');
    end;

    FMetaData.Add('AS');
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
      FMetaData.Add('/* Data Starts */');
      while not EOF do
      begin
        ListData(Trim(FieldByName('RDB$RELATION_NAME').AsString));
        Next;
      end;
      FMetaData.Add('/* Data Ends */');
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
          FieldList := Fields[0].AsString;
      finally
        Close;
      end;
    finally
      Free
    end;

    with TIBInsertStmtsOut.Create(self) do
    try
      Database := FDatabase;
      if DataOut(Format('Select %s From %s',[FieldList,QuoteIdentifier(FDatabase.SQLDialect, ObjectName)]),
                Add2MetaData) then
        FMetaData.Add('COMMIT;');
    finally
      Free
    end;
  end;
end;

procedure TIBExtract.ListRoles(ObjectName: String);
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
begin
  {Process GRANT roles}
  qryRoles := TIBSQL.Create(FDatabase);
  try
    if FDatabaseInfo.ODSMajorVersion >= ODS_VERSION9 then
    begin
      PrevOwner := '';
      FMetaData.Add('');
      FMetaData.Add('/* Grant Roles for this database */');
      FMetaData.Add('');

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
          RoleName := QuoteIdentifier(FDatabase.SQLDialect,
              qryRoles.FieldByName('rdb$Role_Name').AsString);
          OwnerName := Trim(qryRoles.FieldByName('rdb$Owner_Name').AsString);
          if PrevOwner <> OwnerName then
          begin
            FMetaData.Add('');
            FMetaData.Add(Format('/* Role: %s, Owner: %s */', [RoleName, OwnerName]));
            FMetaData.Add('');
            PrevOwner := OwnerName;
          end;
          FMetaData.Add('CREATE ROLE ' + RoleName + Term);
          qryRoles.Next;
        end;
      finally
        qryRoles.Close;
      end;
    end;
  finally
    qryRoles.Free;
  end;
end;

end.


