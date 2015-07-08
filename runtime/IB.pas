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
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IB;

{$Mode Delphi}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, IBExternals, IBUtils, DB, IBXConst, CustApp;

type
  TIBGUIInterface = interface
    function ServerLoginDialog(const AServerName: string;
                               var AUserName, APassword: string): Boolean;
    function LoginDialogEx(const ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean;
    procedure SetCursor;
    procedure RestoreCursor;
  end;

  TTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc);
  TTraceFlags = set of TTraceFlag;

  EIBError                  = class(EDatabaseError)
  private
    FSQLCode: Long;
    FIBErrorCode: Long;
  public
    constructor Create(ASQLCode: Long; Msg: string); overload;
    constructor Create(ASQLCode: Long; AIBErrorCode: Long; Msg: string); overload;
    property SQLCode: Long read FSQLCode;
    property IBErrorCode: Long read FIBErrorCode;
  end;

  EIBInterBaseError         = class(EIBError);
  EIBClientError            = class(EIBError);

  TIBDataBaseErrorMessage    = (ShowSQLCode,
                                ShowIBMessage,
                                ShowSQLMessage);
  TIBDataBaseErrorMessages   = set of TIBDataBaseErrorMessage;
  TIBClientError            = (
    ibxeUnknownError,
    ibxeInterBaseMissing,
    ibxeInterBaseInstallMissing,
    ibxeIB60feature,
    ibxeNotSupported,
    ibxeNotPermitted,
    ibxeFileAccessError,
    ibxeConnectionTimeout,
    ibxeCannotSetDatabase,
    ibxeCannotSetTransaction,
    ibxeOperationCancelled,
    ibxeDPBConstantNotSupported,
    ibxeDPBConstantUnknown,
    ibxeTPBConstantNotSupported,
    ibxeTPBConstantUnknown,
    ibxeDatabaseClosed,
    ibxeDatabaseOpen,
    ibxeDatabaseNameMissing,
    ibxeNotInTransaction,
    ibxeInTransaction,
    ibxeTimeoutNegative,
    ibxeNoDatabasesInTransaction,
    ibxeUpdateWrongDB,
    ibxeUpdateWrongTR,
    ibxeDatabaseNotAssigned,
    ibxeTransactionNotAssigned,
    ibxeXSQLDAIndexOutOfRange,
    ibxeXSQLDANameDoesNotExist,
    ibxeEOF,
    ibxeBOF,
    ibxeInvalidStatementHandle,
    ibxeSQLOpen,
    ibxeSQLClosed,
    ibxeDatasetOpen,
    ibxeDatasetClosed,
    ibxeUnknownSQLDataType,
    ibxeInvalidColumnIndex,
    ibxeInvalidParamColumnIndex,
    ibxeInvalidDataConversion,
    ibxeColumnIsNotNullable,
    ibxeBlobCannotBeRead,
    ibxeBlobCannotBeWritten,
    ibxeEmptyQuery,
    ibxeCannotOpenNonSQLSelect,
    ibxeNoFieldAccess,
    ibxeFieldReadOnly,
    ibxeFieldNotFound,
    ibxeNotEditing,
    ibxeCannotInsert,
    ibxeCannotPost,
    ibxeCannotUpdate,
    ibxeCannotDelete,
    ibxeCannotRefresh,
    ibxeBufferNotSet,
    ibxeCircularReference,
    ibxeSQLParseError,
    ibxeUserAbort,
    ibxeDataSetUniDirectional,
    ibxeCannotCreateSharedResource,
    ibxeWindowsAPIError,
    ibxeColumnListsDontMatch,
    ibxeColumnTypesDontMatch,
    ibxeCantEndSharedTransaction,
    ibxeFieldUnsupportedType,
    ibxeCircularDataLink,
    ibxeEmptySQLStatement,
    ibxeIsASelectStatement,
    ibxeRequiredParamNotSet,
    ibxeNoStoredProcName,
    ibxeIsAExecuteProcedure,
    ibxeUpdateFailed,
    ibxeNotCachedUpdates,
    ibxeNotLiveRequest,
    ibxeNoProvider,
    ibxeNoRecordsAffected,
    ibxeNoTableName,
    ibxeCannotCreatePrimaryIndex,
    ibxeCannotDropSystemIndex,
    ibxeTableNameMismatch,
    ibxeIndexFieldMissing,
    ibxeInvalidCancellation,
    ibxeInvalidEvent,
    ibxeMaximumEvents,
    ibxeNoEventsRegistered,
    ibxeInvalidQueueing,
    ibxeInvalidRegistration,
    ibxeInvalidBatchMove,
    ibxeSQLDialectInvalid,
    ibxeSPBConstantNotSupported,
    ibxeSPBConstantUnknown,
    ibxeServiceActive,
    ibxeServiceInActive,
    ibxeServerNameMissing,
    ibxeQueryParamsError,
    ibxeStartParamsError,
    ibxeOutputParsingError,
    ibxeUseSpecificProcedures,
    ibxeSQLMonitorAlreadyPresent,
    ibxeCantPrintValue,
    ibxeEOFReached,
    ibxeEOFInComment,
    ibxeEOFInString,
    ibxeParamNameExpected,
    ibxeSuccess,
    ibxeDelphiException,
    ibxeNoOptionsSet,
    ibxeNoDestinationDirectory,
    ibxeNosourceDirectory,
    ibxeNoUninstallFile,
    ibxeOptionNeedsClient,
    ibxeOptionNeedsServer,
    ibxeInvalidOption,
    ibxeInvalidOnErrorResult,
    ibxeInvalidOnStatusResult,
    ibxeDPBConstantUnknownEx,
    ibxeTPBConstantUnknownEx,
    ibxeSV5APIError,
    ibxeThreadFailed,
    ibxeFieldSizeError,
    ibxeTransactionNotEnding,
    ibxeDscInfoTokenMissing,
    ibxeNoLoginDialog
    );

  TStatusVector              = array[0..19] of ISC_STATUS;
  PStatusVector              = ^TStatusVector;

  {TResultBuffer inspired by IBPP RB class - access a isc_dsql_sql_info result buffer}

  TResultBuffer = class
  private
    mBuffer: PChar;
    mSize: short;
    function FindToken(token: char): PChar; overload;
    function FindToken(token: char; subtoken: char): PChar; overload;
  public
    constructor Create(aSize: integer = 1024);
    destructor Destroy; override;
    function Size: short;
    procedure Reset;
    function GetValue(token: char): integer; overload;
    function GetValue(token: char; subtoken: char): integer; overload;
    function GetCountValue(token: char): integer;
    function GetBool(token: char): boolean;
    function GetString(token: char; var data: string): integer;
    function buffer: PChar;
  end;

const
  IBPalette1 = 'Firebird'; {do not localize}
  IBPalette2 = 'Firebird Admin'; {do not localize}
  IBPalette3 = 'Firebird Data Controls';   {do not localize}

  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

  IBErrorMessages: array[TIBClientError] of string = (
    SUnknownError,
    SInterBaseMissing,
    SInterBaseInstallMissing,
    SIB60feature,
    SNotSupported,
    SNotPermitted,
    SFileAccessError,
    SConnectionTimeout,
    SCannotSetDatabase,
    SCannotSetTransaction,
    SOperationCancelled,
    SDPBConstantNotSupported,
    SDPBConstantUnknown,
    STPBConstantNotSupported,
    STPBConstantUnknown,
    SDatabaseClosed,
    SDatabaseOpen,
    SDatabaseNameMissing,
    SNotInTransaction,
    SInTransaction,
    STimeoutNegative,
    SNoDatabasesInTransaction,
    SUpdateWrongDB,
    SUpdateWrongTR,
    SDatabaseNotAssigned,
    STransactionNotAssigned,
    SXSQLDAIndexOutOfRange,
    SXSQLDANameDoesNotExist,
    SEOF,
    SBOF,
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SDatasetClosed,
    SUnknownSQLDataType,
    SInvalidColumnIndex,
    SInvalidParamColumnIndex,
    SInvalidDataConversion,
    SColumnIsNotNullable,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SEmptyQuery,
    SCannotOpenNonSQLSelect,
    SNoFieldAccess,
    SFieldReadOnly,
    SFieldNotFound,
    SNotEditing,
    SCannotInsert,
    SCannotPost,
    SCannotUpdate,
    SCannotDelete,
    SCannotRefresh,
    SBufferNotSet,
    SCircularReference,
    SSQLParseError,
    SUserAbort,
    SDataSetUniDirectional,
    SCannotCreateSharedResource,
    SWindowsAPIError,
    SColumnListsDontMatch,
    SColumnTypesDontMatch,
    SCantEndSharedTransaction,
    SFieldUnsupportedType,
    SCircularDataLink,
    SEmptySQLStatement,
    SIsASelectStatement,
    SRequiredParamNotSet,
    SNoStoredProcName,
    SIsAExecuteProcedure,
    SUpdateFailed,
    SNotCachedUpdates,
    SNotLiveRequest,
    SNoProvider,
    SNoRecordsAffected,
    SNoTableName,
    SCannotCreatePrimaryIndex,
    SCannotDropSystemIndex,
    STableNameMismatch,
    SIndexFieldMissing,
    SInvalidCancellation,
    SInvalidEvent,
    SMaximumEvents,
    SNoEventsRegistered,
    SInvalidQueueing,
    SInvalidRegistration,
    SInvalidBatchMove,
    SSQLDialectInvalid,
    SSPBConstantNotSupported,
    SSPBConstantUnknown,
    SServiceActive,
    SServiceInActive,
    SServerNameMissing,
    SQueryParamsError,
    SStartParamsError,
    SOutputParsingError,
    SUseSpecificProcedures,
    SSQLMonitorAlreadyPresent,
    SCantPrintValue,
    SEOFReached,
    SEOFInComment,
    SEOFInString,
    SParamNameExpected,
    SSuccess,
    SDelphiException,
    SNoOptionsSet,
    SNoDestinationDirectory,
    SNosourceDirectory,
    SNoUninstallFile,
    SOptionNeedsClient,
    SOptionNeedsServer,
    SInvalidOption,
    SInvalidOnErrorResult,
    SInvalidOnStatusResult,
    SDPBConstantUnknownEx,
    STPBConstantUnknownEx,
    SSV5APIError,
    SThreadFailed,
    SFieldSizeError,
    STransactionNotEnding,
    SDscInfoTokenMissing,
    SNoLoginDialog
  );

const
  IBGUIInterface: TIBGUIInterface = nil;


var
  IBCS: TRTLCriticalSection;

procedure IBAlloc(var P; OldSize, NewSize: Integer);

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
procedure IBDataBaseError;

function StatusVector: PISC_STATUS;
function StatusVectorArray: PStatusVector;
function CheckStatusVector(ErrorCodes: array of ISC_STATUS): Boolean;
function StatusVectorAsText: string;

procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;

implementation

uses
  IBIntf, IBHeader;

var
  IBDataBaseErrorMessages: TIBDataBaseErrorMessages;
threadvar
  FStatusVector : TStatusVector;

procedure IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(IBErrorMessages[ErrMess], Args));
end;

procedure IBDataBaseError;
var
  sqlcode: Long;
  IBErrorCode: Long;
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of char;
  usr_msg: string;
  status_vector: PISC_STATUS;
  IBDataBaseErrorMessages: TIBDataBaseErrorMessages;
begin
  usr_msg := '';

  { Get a local reference to the status vector.
    Get a local copy of the IBDataBaseErrorMessages options.
    Get the SQL error code }
  status_vector := StatusVector;
  IBErrorCode := StatusVectorArray[1];
  IBDataBaseErrorMessages := GetIBDataBaseErrorMessages;
  sqlcode := isc_sqlcode(status_vector);

  if (ShowSQLCode in IBDataBaseErrorMessages) then
    usr_msg := usr_msg + 'SQLCODE: ' + IntToStr(sqlcode); {do not localize}
  Exclude(IBDataBaseErrorMessages, ShowSQLMessage);
  if (ShowSQLMessage in IBDataBaseErrorMessages) then
  begin
    isc_sql_interprete(sqlcode, local_buffer, IBLocalBufferLength);
    if (ShowSQLCode in IBDataBaseErrorMessages) then
      usr_msg := usr_msg + CRLF;
    usr_msg := usr_msg + strpas(local_buffer);
  end;

  if (ShowIBMessage in IBDataBaseErrorMessages) then
  begin
    if (ShowSQLCode in IBDataBaseErrorMessages) or
       (ShowSQLMessage in IBDataBaseErrorMessages) then
      usr_msg := usr_msg + CRLF;
    while (isc_interprete(local_buffer, @status_vector) > 0) do
    begin
      if (usr_msg <> '') and (usr_msg[Length(usr_msg)] <> LF) then
        usr_msg := usr_msg + CRLF;
      usr_msg := usr_msg + strpas(local_buffer);
    end;
  end;
  if (usr_msg <> '') and (usr_msg[Length(usr_msg)] = '.') then
    Delete(usr_msg, Length(usr_msg), 1);
  raise EIBInterBaseError.Create(sqlcode, IBErrorCode, usr_msg);
end;

{ Return the status vector for the current thread }
function StatusVector: PISC_STATUS;
begin
  result := @FStatusVector;
end;

function StatusVectorArray: PStatusVector;
begin
  result := @FStatusVector;
end;

function CheckStatusVector(ErrorCodes: array of ISC_STATUS): Boolean;
var
  p: PISC_STATUS;
  i: Integer;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  p := @FStatusVector;
  result := False;
  while (p^ <> 0) and (not result) do
    case p^ of
      3: NextP(3);
      1, 4:
      begin
        NextP(1);
        i := 0;
        while (i <= High(ErrorCodes)) and (not result) do
        begin
          result := p^ = ErrorCodes[i];
          Inc(i);
        end;
        NextP(1);
      end;
      else
        NextP(2);
    end;
end;

function StatusVectorAsText: string;
var
  p: PISC_STATUS;
  function NextP(i: Integer): PISC_STATUS;
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
    result := p;
  end;
begin
  p := @FStatusVector;
  result := '';
  while (p^ <> 0) do
    if (p^ = 3) then
    begin
      result := result + Format('%d %d %d', [p^, NextP(1)^, NextP(1)^]) + CRLF;
      NextP(1);
    end
    else begin
      result := result + Format('%d %d', [p^, NextP(1)^]) + CRLF;
      NextP(1);
    end;
end;

{ TResultBuffer }

constructor TResultBuffer.Create(aSize: integer);
begin
  inherited Create;
  mSize := aSize;
  GetMem(mBuffer,aSize);
  FillChar(mBuffer^,mSize,255);
end;

destructor TResultBuffer.Destroy;
begin
  if mBuffer <> nil then FreeMem(mBuffer);
  inherited;
end;

function TResultBuffer.buffer: PChar;
begin
  Result := mBuffer;
end;

function TResultBuffer.FindToken(token: char): PChar;
var p: PChar;
    len: integer;
begin
  Result := nil;
  p := mBuffer;

  while p^ <> char(isc_info_end) do
  begin
    if p^ = token then
    begin
      Result := p;
      Exit;
    end;
    len := isc_vax_integer(p+1,2);
    Inc(p,len+3);
  end;
end;

function TResultBuffer.FindToken(token: char; subtoken: char): PChar;
var p: PChar;
    len, inlen: integer;
begin
  Result := nil;
  p := mBuffer;

  while p^ <> char(isc_info_end) do
  begin
    if p^ = token then
    begin
      {Found token, now find subtoken}
      inlen := isc_vax_integer(p+1, 2);
      Inc(p,3);
      while inlen > 0 do
      begin
	if p^ = subtoken then
        begin
          Result := p;
          Exit;
        end;
  	len := isc_vax_integer(p+1, 2);
        Inc(p,len + 3);
        Dec(inlen,len + 3);
      end;
      Exit;
    end;
    len := isc_vax_integer(p+1, 2);
    inc(p,len+3);
  end;
end;

function TResultBuffer.GetBool(token: char): boolean;
var aValue: integer;
    p: PChar;
begin
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  aValue := isc_vax_integer(p+1, 4);
  Result := aValue <> 0;
end;

function TResultBuffer.GetCountValue(token: char): integer;
var len: integer;
    p: PChar;
begin
  {Specifically used on tokens like isc_info_insert_count and the like
   which return detailed counts per relation. We sum up the values.}

  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  {len is the number of bytes in the following array}

  len := isc_vax_integer(p+1, 2);
  Inc(p,3);
  Result := 0;
  while len > 0 do
  begin
    {Each array item is 6 bytes : 2 bytes for the relation_id which
     we skip, and 4 bytes for the count value which we sum up across
     all tables.}

     Inc(Result,isc_vax_integer(p+2, 4));
     Inc(p,6);
     Dec(len,6);
  end;
end;

function TResultBuffer.GetString(token: char; var data: string): integer;
var p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  Result := isc_vax_integer(p+1, 2);
  SetString(data,p+3,Result);
end;

function TResultBuffer.GetValue(token: char): integer;
var len: integer;
    p: PChar;
begin
  Result := 0;
  p := FindToken(token);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  len := isc_vax_integer(p+1, 2);
  if (len <> 0) then
    Result := isc_vax_integer(p+3, len);
end;

function TResultBuffer.GetValue(token: char; subtoken: char): integer;
var len: integer;
    p: PChar;
begin
  Result := 0;
  p := FindToken(token, subtoken);

  if p = nil then
    IBError(ibxeDscInfoTokenMissing,[token]);

  len := isc_vax_integer(p+1, 2);
  if (len <> 0) then
    Result := isc_vax_integer(p+3, len);
end;

function TResultBuffer.Size: short;
begin
  Result := mSize;
end;

procedure TResultBuffer.Reset;
begin
  if mBuffer <> nil then FreeMem(mBuffer);
  GetMem(mBuffer,mSize);
  FillChar(mBuffer^,mSize,255);
end;


{ EIBError }
constructor EIBError.Create(ASQLCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

constructor EIBError.Create(ASQLCode: Long; AIBErrorCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode :=  ASQLCode;
  FIBErrorCode := AIBErrorCode;
end;

procedure SetIBDataBaseErrorMessages(Value: TIBDataBaseErrorMessages);
begin
  EnterCriticalSection(IBCS);
  try
    IBDataBaseErrorMessages := Value;
  finally
    LeaveCriticalSection(IBCS);
  end;
end;

function GetIBDataBaseErrorMessages: TIBDataBaseErrorMessages;
begin
  EnterCriticalSection(IBCS);
  try
    result := IBDataBaseErrorMessages;
  finally
    LeaveCriticalSection(IBCS);
  end;
end;

initialization
 // IsMultiThread := True;
  InitCriticalSection(IBCS);
  IBDataBaseErrorMessages := [ShowSQLMessage, ShowIBMessage];

finalization
  DoneCriticalSection(IBCS);

end.
