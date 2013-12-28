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

unit IBDatabaseInfo;

{$Mode Delphi}

interface

uses
  SysUtils, Classes, IBHeader, IBExternals, IB, IBDatabase;

type

  TIBDatabaseInfo = class(TComponent)
  protected
    FIBLoaded: Boolean;
    FDatabase: TIBDatabase;
    FUserNames   : TStringList;
    FBackoutCount: TStringList;
    FDeleteCount: TStringList;
    FExpungeCount: TStringList;
    FInsertCount: TStringList;
    FPurgeCount: TStringList;
    FReadIdxCount: TStringList;
    FReadSeqCount: TStringList;
    FUpdateCount: TStringList;
    function GetAllocation: Long;
    function GetBaseLevel: Long;
    function GetDBFileName: String;
    function GetDBSiteName: String;
    function GetDBImplementationNo: Long;
    function GetDBImplementationClass: Long;
    function GetNoReserve: Long;
    function GetODSMinorVersion: Long;
    function GetODSMajorVersion: Long;
    function GetPageSize: Long;
    function GetVersion: String;
    function GetCurrentMemory: Long;
    function GetForcedWrites: Long;
    function GetMaxMemory: Long;
    function GetNumBuffers: Long;
    function GetSweepInterval: Long;
    function GetUserNames: TStringList;
    function GetFetches: Long;
    function GetMarks: Long;
    function GetReads: Long;
    function GetWrites: Long;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetOperationCounts(DBInfoCommand: Integer; FOperation: TStringList): TStringList;
    function GetReadOnly: Long;
    function GetStringDatabaseInfo(DatabaseInfoCommand: Integer): String;
    function GetDBSQLDialect: Long;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
    property Allocation: Long read GetAllocation;
    property BaseLevel: Long read GetBaseLevel;
    property DBFileName: String read GetDBFileName;
    property DBSiteName: String read GetDBSiteName;
    property DBImplementationNo: Long read GetDBImplementationNo;
    property DBImplementationClass: Long read GetDBImplementationClass;
    property NoReserve: Long read GetNoReserve;
    property ODSMinorVersion: Long read GetODSMinorVersion;
    property ODSMajorVersion: Long read GetODSMajorVersion;
    property PageSize: Long read GetPageSize;
    property Version: String read GetVersion;
    property CurrentMemory: Long read GetCurrentMemory;
    property ForcedWrites: Long read GetForcedWrites;
    property MaxMemory: Long read GetMaxMemory;
    property NumBuffers: Long read GetNumBuffers;
    property SweepInterval: Long read GetSweepInterval;
    property UserNames: TStringList read GetUserNames;
    property Fetches: Long read GetFetches;
    property Marks: Long read GetMarks;
    property Reads: Long read GetReads;
    property Writes: Long read GetWrites;
    property BackoutCount: TStringList read GetBackoutCount;
    property DeleteCount: TStringList read GetDeleteCount;
    property ExpungeCount: TStringList read GetExpungeCount;
    property InsertCount: TStringList read GetInsertCount;
    property PurgeCount: TStringList read GetPurgeCount;
    property ReadIdxCount: TStringList read GetReadIdxCount;
    property ReadSeqCount: TStringList read GetReadSeqCount;
    property UpdateCount: TStringList read GetUpdateCount;
    property DBSQLDialect : Long read GetDBSQLDialect;
    property ReadOnly: Long read GetReadOnly;
  published
    property Database: TIBDatabase read FDatabase write FDatabase;
  end;

implementation

uses
  IBIntf;

{ TIBDatabaseInfo }

constructor TIBDatabaseInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIBLoaded := False;
  CheckIBLoaded;
  FIBLoaded := True;
  FUserNames := TStringList.Create;
  FBackoutCount                        := nil;
  FDeleteCount                         := nil;
  FExpungeCount                        := nil;
  FInsertCount                         := nil;
  FPurgeCount                          := nil;
  FReadIdxCount                        := nil;
  FReadSeqCount                        := nil;
  FUpdateCount                         := nil;
end;

destructor TIBDatabaseInfo.Destroy;
begin
  if FIBLoaded then
  begin
    FUserNames.Free;
    FBackoutCount.Free;
    FDeleteCount.Free;
    FExpungeCount.Free;
    FInsertCount.Free;
    FPurgeCount.Free;
    FReadIdxCount.Free;
    FReadSeqCount.Free;
    FUpdateCount.Free;
  end;
  inherited Destroy;
end;


function TIBDatabaseInfo.Call(ErrCode: ISC_STATUS;
  RaiseError: Boolean): ISC_STATUS;
begin
  result := ErrCode;
  if RaiseError and (ErrCode > 0) then
    IBDataBaseError;
end;
function TIBDatabaseInfo.GetAllocation: Long;
begin
  result := GetLongDatabaseInfo(isc_info_allocation);
end;

function TIBDatabaseInfo.GetBaseLevel: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_base_level);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[4], 1);
end;

function TIBDatabaseInfo.GetDBFileName: String;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TIBDatabaseInfo.GetDBSiteName: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  p: PChar;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_id);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
  p := @local_buffer[5 + Int(local_buffer[4])]; { DBSiteName Length }
  p := p + Int(p^) + 1;                         { End of DBSiteName }
  p^ := #0;                                     { Null it }
  result := String(PChar(@local_buffer[6 + Int(local_buffer[4])]));
end;

function TIBDatabaseInfo.GetDBImplementationNo: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        IBLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[3], 1);
end;

function TIBDatabaseInfo.GetDBImplementationClass: Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_implementation);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
  result := isc_vax_integer(@local_buffer[4], 1);
end;

function TIBDatabaseInfo.GetNoReserve: Long;
begin
  result := GetLongDatabaseInfo(isc_info_no_reserve);
end;

function TIBDatabaseInfo.GetODSMinorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_minor_version);
end;

function TIBDatabaseInfo.GetODSMajorVersion: Long;
begin
  result := GetLongDatabaseInfo(isc_info_ods_version);
end;

function TIBDatabaseInfo.GetPageSize: Long;
begin
  result := GetLongDatabaseInfo(isc_info_page_size);
end;

function TIBDatabaseInfo.GetVersion: String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_version);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        IBBigLocalBufferLength, local_buffer), True);
  local_buffer[5 + Int(local_buffer[4])] := #0;
  result := String(PChar(@local_buffer[5]));
end;

function TIBDatabaseInfo.GetCurrentMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_current_memory);
end;

function TIBDatabaseInfo.GetForcedWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_forced_writes);
end;

function TIBDatabaseInfo.GetMaxMemory: Long;
begin
  result := GetLongDatabaseInfo(isc_info_max_memory);
end;

function TIBDatabaseInfo.GetNumBuffers: Long;
begin
  result := GetLongDatabaseInfo(isc_info_num_buffers);
end;

function TIBDatabaseInfo.GetSweepInterval: Long; 
begin
  result := GetLongDatabaseInfo(isc_info_sweep_interval);
end;

function TIBDatabaseInfo.GetUserNames: TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  temp_buffer: array[0..IBLocalBufferLength - 2] of Char;
  DatabaseInfoCommand: Char;
  i, user_length: Integer;
begin
  result := FUserNames;
  DatabaseInfoCommand := Char(isc_info_user_names);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                        IBHugeLocalBufferLength, local_buffer), True);
  FUserNames.Clear;
  i := 0;
  while local_buffer[i] = Char(isc_info_user_names) do
  begin
    Inc(i, 3); { skip "isc_info_user_names byte" & two unknown bytes of structure (see below) }
    user_length := Long(local_buffer[i]);
    Inc(i,1);
    Move(local_buffer[i], temp_buffer[0], user_length);
    Inc(i, user_length);
    temp_buffer[user_length] := #0;
    FUserNames.Add(String(temp_buffer));
  end;
end;

function TIBDatabaseInfo.GetFetches: Long;
begin
  result := GetLongDatabaseInfo(isc_info_fetches);
end;

function TIBDatabaseInfo.GetMarks: Long;
begin
  result := GetLongDatabaseInfo(isc_info_marks);
end;

function TIBDatabaseInfo.GetReads: Long;
begin
  result := GetLongDatabaseInfo(isc_info_reads);
end;

function TIBDatabaseInfo.GetWrites: Long;
begin
  result := GetLongDatabaseInfo(isc_info_writes);
end;

function TIBDatabaseInfo.GetOperationCounts(DBInfoCommand: Integer; FOperation: TStringList): TStringList;
var
  local_buffer: array[0..IBHugeLocalBufferLength - 1] of Char;
  DatabaseInfoCommand: Char;
  i, qtd_tables, id_table, qtd_operations: Integer;
begin
  if FOperation = nil then FOperation := TStringList.Create;
  result := FOperation;
  DatabaseInfoCommand := Char(DBInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                         IBHugeLocalBufferLength, local_buffer), True);
  FOperation.Clear;
  { 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
    2. 2 bytes telling how many bytes compose the subsequent value pairs.
    3. A pair of values for each table in the database on wich the requested
      type of operation has occurred since the database was last attached.
    Each pair consists of:
    1. 2 bytes specifying the table ID.
    2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
  }
  qtd_tables := trunc(isc_vax_integer(@local_buffer[1],2)/6);
  for i := 0 to qtd_tables - 1 do
  begin
    id_table := isc_vax_integer(@local_buffer[3+(i*6)],2);
    qtd_operations := isc_vax_integer(@local_buffer[5+(i*6)],4);
    FOperation.Add(IntToStr(id_table)+'='+IntToStr(qtd_operations));
  end;
end;

function TIBDatabaseInfo.GetBackoutCount: TStringList;
begin
  result := GetOperationCounts(isc_info_backout_count,FBackoutCount);
end;

function TIBDatabaseInfo.GetDeleteCount: TStringList;
begin
  result := GetOperationCounts(isc_info_delete_count,FDeleteCount);
end;

function TIBDatabaseInfo.GetExpungeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_expunge_count,FExpungeCount);
end;

function TIBDatabaseInfo.GetInsertCount: TStringList;
begin
  result := GetOperationCounts(isc_info_insert_count,FInsertCount);
end;

function TIBDatabaseInfo.GetPurgeCount: TStringList;
begin
  result := GetOperationCounts(isc_info_purge_count,FPurgeCount);
end;

function TIBDatabaseInfo.GetReadIdxCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_idx_count,FReadIdxCount);
end;

function TIBDatabaseInfo.GetReadSeqCount: TStringList;
begin
  result := GetOperationCounts(isc_info_read_seq_count,FReadSeqCount);
end;

function TIBDatabaseInfo.GetUpdateCount: TStringList;
begin
  result := GetOperationCounts(isc_info_update_count,FUpdateCount);
end;

function TIBDatabaseInfo.GetReadOnly: Long;
begin
  result := GetLongDatabaseInfo(isc_info_db_read_only);
end;

function TIBDatabaseInfo.GetLongDatabaseInfo(DatabaseInfoCommand: Integer): Long;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @_DatabaseInfoCommand,
                         IBLocalBufferLength, local_buffer), True);
  length := isc_vax_integer(@local_buffer[1], 2);
  result := isc_vax_integer(@local_buffer[3], length);
end;

function TIBDatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: Integer): String;
var
  local_buffer: array[0..IBBigLocalBufferLength - 1] of Char;
  _DatabaseInfoCommand: Char;
begin
  _DatabaseInfoCommand := Char(DatabaseInfoCommand);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @_DatabaseInfoCommand,
                         IBBigLocalBufferLength, local_buffer), True);
  local_buffer[4 + Int(local_buffer[3])] := #0;
  result := String(PChar(@local_buffer[4]));
end;


function TIBDatabaseInfo.GetDBSQLDialect: Integer;
var
  local_buffer: array[0..IBLocalBufferLength - 1] of Char;
  length: Integer;
  DatabaseInfoCommand: Char;
begin
  DatabaseInfoCommand := Char(isc_info_db_SQL_Dialect);
  Call(isc_database_info(StatusVector, @FDatabase.Handle, 1, @DatabaseInfoCommand,
                       IBLocalBufferLength, local_buffer), True);
  if (local_buffer[0] <> Char(isc_info_db_SQL_dialect)) then
    result := 1
  else begin
    length := isc_vax_integer(@local_buffer[1], 2);
    result := isc_vax_integer(@local_buffer[3], length);
  end;
end;


end.
