(*
 *  Firebird Interface (fbintf). The fbintf components provide a set of
 *  Pascal language bindings for the Firebird API. Although predominantly
 *  a new development they include source code taken from IBX and may be
 *  considered a derived product. This software thus also includes the copyright
 *  notice and license conditions from IBX.
 *
 *  Except for those parts dervied from IBX, contents of this file are subject
 *  to the Initial Developer's Public License Version 1.0 (the "License"); you
 *  may not use this file except in compliance with the License. You may obtain a
 *  copy of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2016 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
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
{    Associates Ltd 2011 - 2015                                                }
{                                                                        }
{************************************************************************}
unit IBMessages;
{$IFDEF MSWINDOWS} 
{$DEFINE WINDOWS} 
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage UTF8}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TIBClientError            = (
      ibxeUnknownError,
      ibxeIB60feature,
      ibxeNotSupported,
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
      ibxeInvalidStatementHandle,
      ibxeSQLOpen,
      ibxeSQLClosed,
      ibxeDatasetOpen,
      ibxeDatasetClosed,
      ibxeInvalidColumnIndex,
      ibxeBlobCannotBeRead,
      ibxeBlobCannotBeWritten,
      ibxeEmptyQuery,
      ibxeNoFieldAccess,
      ibxeFieldNotFound,
      ibxeNotEditing,
      ibxeCannotInsert,
      ibxeCannotUpdate,
      ibxeCannotDelete,
      ibxeCannotRefresh,
      ibxeBufferNotSet,
      ibxeCircularReference,
      ibxeUserAbort,
      ibxeDataSetUniDirectional,
      ibxeCannotCreateSharedResource,
      ibxeFieldUnsupportedType,
      ibxeCircularDataLink,
      ibxeEmptySQLStatement,
      ibxeIsASelectStatement,
      ibxeRequiredParamNotSet,
      ibxeNoStoredProcName,
      ibxeIsAExecuteProcedure,
      ibxeUpdateFailed,
      ibxeNoRecordsAffected,
      ibxeNoTableName,
      ibxeTableNameMismatch,
      ibxeIndexFieldMissing,
      ibxeInvalidEvent,
      ibxeMaximumEvents,
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
      ibxeDPBConstantUnknownEx,
      ibxeTPBConstantUnknownEx,
      ibxeSV5APIError,
      ibxeThreadFailed,
      ibxeFieldSizeError,
      ibxeNoLoginDialog,
      ibxeInfoBufferIndexError,
      ibxServiceRequestIndexError,
      ibxServiceParamTypeError,
      ibxArrayBoundsCantIncrease ,
      ibxeUnexpectedDatabaseInfoResp,
      ibxStringTooLong,
      ibxFieldNotinDataSet,
      ibxeNotCurrentArray,
      ibxeServiceRunning,
      ibxeUniqueRelationReqd,
      ibxeNegativeGenerator,
      ibxeServiceUnavailable,
      ibxeBadConnectString,
      ibxeServiceNotStarted,
      ibxeNotRequiredDataSetSource,
      ibxeNoLimboTransactionInsert,
      ibxeDatabaseNotConnected,
      ibxeMultiThreadRequired,
      ibxeODSVersionRequired,
      ibxErrorParsing,
      ibxeParameterNameNotFound,
      ibxeListFieldNotFound,
      ibxeBadDateTimeTZString
      );

function GetErrorMessage(ErrMess: TIBClientError): AnsiString;

{IBError is used internally and by IBX to throw an EIBClientError}

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
 resourcestring
  { generic strings used in code }
  SEOFReached = 'SEOFReached';
  SCantPrintValue = 'Cannot print value';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SCommitTransaction = 'Transaction is currently Active. Rollback and continue?';
  sSQLErrorSeparator = ' When Executing: ';
  SDatabaseFilter = 'Database Files (*.fdb; *.gdb)|*.gdb; *.fdb|All files (*.*)|*.*';
  {$IFDEF WINDOWS}
  SLibraryNameFilter = 'DLLs (*.dll)|*.dll|All files (*.*)|*.*';
  {$ELSE}
  SLibraryNameFilter = 'All files (*.*)|*.*';
  {$ENDIF}
  STrue = 'true';
  SFalse = 'false';
  SArray = '(array)';
  SBlob = '(blob)';

implementation

uses IBUtils, IB;

resourcestring

{ strings used in error messages}
  SUnknownError = 'Unknown error';
  SIB60feature = '%s is an InterBase 6 function. Please upgrade to InterBase 6 to use this functonality';
  SNotSupported = 'Unsupported feature';
  SOperationCancelled = 'Operation cancelled at user''s request';
  SDPBConstantNotSupported = 'DPB Constant (isc_dpb_%s) is unsupported';
  SDPBConstantUnknown = 'DPB Constant (%d) is unknown';
  STPBConstantNotSupported = 'TPB Constant (isc_tpb_%s) is unsupported';
  STPBConstantUnknown = 'TPB Constant (%d) is unknown';
  SDatabaseClosed = 'Cannot perform operation -- DB is not open';
  SDatabaseOpen = 'Cannot perform operation -- DB is currently open';
  SDatabaseNameMissing = 'Database name is missing';
  SNotInTransaction = 'Transaction is not active';
  SInTransaction = 'Transaction is active';
  STimeoutNegative = 'Timeout values cannot be negative';
  SNoDatabasesInTransaction = 'No databases are listed in transaction component';
  SUpdateWrongDB = 'Updating wrong database';
  SUpdateWrongTR = 'Updating wrong transaction. Unique transaction expected in set';
  SDatabaseNotAssigned = 'Database not assigned';
  STransactionNotAssigned = 'Transaction not assigned';
  SInvalidStatementHandle = 'Invalid statement handle';
  SSQLOpen = 'IBSQL Open';
  SSQLClosed = 'IBSQL Closed';
  SDatasetOpen = 'Dataset open';
  SDatasetClosed = 'Dataset closed';
  SInvalidColumnIndex = 'Invalid column index (index exceeds permitted range)';
  SBlobCannotBeRead = 'Blob stream cannot be read';
  SBlobCannotBeWritten = 'Blob stream cannot be written';
  SEmptyQuery = 'Empty query';
  SNoFieldAccess = 'No access to field "%s"';
  SFieldNotFound = 'Field "%s" not found';
  SNotEditing = 'Not in edit mode';
  SCannotInsert = 'Cannot insert into dataset. (No insert query)';
  SCannotUpdate = 'Cannot update. (No update query)';
  SCannotDelete = 'Cannot delete from dataset. (No delete query)';
  SCannotRefresh = 'Cannot refresh row. (No refresh query)';
  SBufferNotSet = 'Buffer not set';
  SCircularReference = 'Circular references not permitted';
  SUserAbort = 'User abort';
  SDataSetUniDirectional = 'Data set is uni-directional';
  {$IFDEF UNIX}
  SCannotCreateSharedResource = 'Cannot create shared resource. %s';
  {$ELSE}
  SCannotCreateSharedResource = 'Cannot create shared resource. (Windows error %d)';
  {$ENDIF}
  SFieldUnsupportedType = 'Unsupported Field Type';
  SCircularDataLink = 'Circular DataLink Reference';
  SEmptySQLStatement = 'Empty SQL Statement';
  SIsASelectStatement = 'use Open for a Select Statement';
  SRequiredParamNotSet = 'Required parameter "%s" value not set';
  SNoStoredProcName = 'No Stored Procedure Name assigned';
  SIsAExecuteProcedure = 'use ExecProc for Procedure; use TQuery for Select procedures';
  SUpdateFailed = 'Update Failed';

  SNoRecordsAffected = 'No Records Affected';
  SNoTableName = 'No Table Name assigned';
  STableNameMismatch = 'Table Name Mismatch';
  SIndexFieldMissing = 'Index Field Missing';
  SInvalidEvent = 'Invalid Event';
  SMaximumEvents = 'Exceded Maximum Event limits';
  SInvalidBatchMove = 'Invalid Batch Move';
  SSQLDialectInvalid = 'SQL Dialect Invalid';
  SSPBConstantNotSupported = 'SPB Constant Not supported';
  SSPBConstantUnknown = 'SPB Constant Unknown';
  SServiceActive = 'Cannot perform operation -- service is not attached';
  SServiceInActive = 'Cannot perform operation -- service is attached';
  SServerNameMissing = 'Server Name Missing';
  SQueryParamsError = 'Query Parameters missing or incorrect';
  SStartParamsError = 'start Parameters missing or incorrect';
  SOutputParsingError = 'Unexpected Output buffer value (%d) - %s';
  SUseSpecificProcedures = 'Generic ServiceStart not applicable: Use Specific Procedures to set configuration params';

  SDPBConstantUnknownEx = 'DPB Constant (%s) is unknown';
  STPBConstantUnknownEx = 'TPB Constant (%s) is unknown';
  SSV5APIError = 'SV5 API API Error - %s';
  SThreadFailed = '%s Thread failed with Exception: %s';
  SFieldSizeError = 'Field %s is too small to receive the data';
  SNoLoginDialog = 'Default Login Dlalog not found. Have you included ibexpress ' +
                   'in your program uses list?';
  SInfoBufferIndexError = 'Info Buffer Index Out of Range (%d)';
  SServiceRequestIndexError = 'Service Request Index Out of Range (%d)';
  SServiceParamTypeError = 'Invalid Request for Service Param Type';
  SArrayBoundsCantIncrease = 'Array Bounds can only be narrowed';
  SUnexpectedDatabaseInfoResp = 'Unexpected Database Information Response';
  SStringTooLong = 'String "%s" is too long. Max %d characters';
  SFieldNotinDataSet = 'Field %s is not a member of DataSet %s';
  SNotCurrentArray = 'Cannot Edit an Array that is not part of the current record';
  SServiceRunning = 'Cannot start a new service while an existing service is running';
  SUniqueRelationReqd = 'All Output Fields must derived from the same table';
  SNegativeGenerator = 'A Generator Increment cannot be negative';
  SServiceUnavailable = 'Request Service is not available';
  SBadConnectString = 'Parse Error in Connect String';
  SServiceNotStarted = 'Cannot Query running service until the service has been started';
  SNotRequiredDataSetSource = 'Object of class %s is not a valid dataset source';
  SNoLimboTransactionInsert = 'You cannot add to a Limbo Transaction list';
  SDatabaseNotConnected = 'Cannot connect using an unattached database';
  SMultiThreadRequired = 'Multi-threading required for %s but not enabled. Please recompile with multi-threading support enabled. '+
                         'Hint: you probably need to add -dUseCThreads to the Custom Options.';
  SODSVersionRequired = 'This feature requires ODS Version %s or later';
  SErrorParsing = 'Error parsing SQL Statement at clause starting with %s';
  SParameterNameNotFound = 'Parameter Name (%s) not found';
  SListFieldNotFound = 'ListField Name is not a valid dataset column name (%s)';
  SBadDateTimeTZString = 'Unable to parse Date/Time Time Zone string "%s"';

const
  IBErrorMessages: array[TIBClientError] of string = (
    SUnknownError,
    SIB60feature,
    SNotSupported,
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
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SDatasetClosed,
    SInvalidColumnIndex,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SEmptyQuery,
    SNoFieldAccess,
    SFieldNotFound,
    SNotEditing,
    SCannotInsert,
    SCannotUpdate,
    SCannotDelete,
    SCannotRefresh,
    SBufferNotSet,
    SCircularReference,
    SUserAbort,
    SDataSetUniDirectional,
    SCannotCreateSharedResource,
    SFieldUnsupportedType,
    SCircularDataLink,
    SEmptySQLStatement,
    SIsASelectStatement,
    SRequiredParamNotSet,
    SNoStoredProcName,
    SIsAExecuteProcedure,
    SUpdateFailed,
    SNoRecordsAffected,
    SNoTableName,
    STableNameMismatch,
    SIndexFieldMissing,
    SInvalidEvent,
    SMaximumEvents,
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
    SDPBConstantUnknownEx,
    STPBConstantUnknownEx,
    SSV5APIError,
    SThreadFailed,
    SFieldSizeError,
    SNoLoginDialog,
    SInfoBufferIndexError,
    SServiceRequestIndexError,
    SServiceParamTypeError,
    SArrayBoundsCantIncrease,
    SUnexpectedDatabaseInfoResp,
    SStringTooLong,
    SFieldNotinDataSet,
    SNotCurrentArray,
    SServiceRunning,
    SUniqueRelationReqd,
    SNegativeGenerator,
    SServiceUnavailable,
    SBadConnectString,
    SServiceNotStarted,
    SNotRequiredDataSetSource,
    SNoLimboTransactionInsert,
    SDatabaseNotConnected,
    SMultiThreadRequired,
    SODSVersionRequired,
    SErrorParsing,
    SParameterNameNotFound,
    SListFieldNotFound,
    SBadDateTimeTZString
  );

function GetErrorMessage(ErrMess: TIBClientError): AnsiString;
begin
  Result := IBErrorMessages[ErrMess];
end;

procedure IBError(ErrMess: TIBClientError; const Args: array of const);
begin
  raise EIBClientError.Create(Ord(ErrMess),
                              Format(GetErrorMessage(ErrMess), Args));
end;

end.

