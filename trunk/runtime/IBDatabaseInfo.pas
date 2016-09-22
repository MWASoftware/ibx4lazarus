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
  SysUtils, Classes, IB, IBExternals, IBDatabase;

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
  FBMessages;

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


function TIBDatabaseInfo.GetAllocation: Long;
begin
  result := GetLongDatabaseInfo(isc_info_allocation);
end;

function TIBDatabaseInfo.GetBaseLevel: Long;
var Response: TByteArray;
begin
  with Database.Attachment.GetDBInformation([isc_info_base_level]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_base_level) then
    begin
      Response := Items[0].GetAsBytes;
      Result := Response[1];
    end
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBFileName: String;
var
  ConnectionType: integer;
  SiteName: string;
begin
  with Database.Attachment.GetDBInformation([isc_info_db_id]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_db_id) then
      Items[0].DecodeIDCluster(ConnectionType,Result,SiteName)
    else
       IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBSiteName: String;
var
  ConnectionType: integer;
  FileName: string;
begin
  with Database.Attachment.GetDBInformation([isc_info_db_id]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_db_id) then
      Items[0].DecodeIDCluster(ConnectionType,FileName,Result)
    else
       IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBImplementationNo: Long;
var Response: TByteArray;
begin
  with Database.Attachment.GetDBInformation([isc_info_implementation]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_implementation) then
    begin
      Response := Items[0].GetAsBytes;
      Result := Response[1];
    end
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBImplementationClass: Long;
var Response: TByteArray;
begin
  with Database.Attachment.GetDBInformation([isc_info_implementation]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_implementation) then
    begin
      Response := Items[0].GetAsBytes;
      Result := Response[2];
    end
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
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
var Version: byte;
begin
  with Database.Attachment.GetDBInformation([isc_info_version]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_version) then
      Items[0].DecodeVersionString(Version,Result)
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
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
begin
  Result := FUserNames;
  FUserNames.Clear;
  with Database.Attachment.GetDBInformation([isc_info_user_names]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_user_names) then
      Items[0].DecodeUserNames(Result)
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
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
var opCounts: TDBOperationCounts;
    i: integer;
begin
  if FOperation = nil then FOperation := TStringList.Create;
  result := FOperation;
  with Database.Attachment.GetDBInformation([DBInfoCommand]) do
    if (Count > 0) and (Items[0].GetItemType = DBInfoCommand) then
      opCounts := Items[0].getOperationCounts
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
  for i := 0 to Length(opCounts) - 1 do
    FOperation.Add(IntToStr(opCounts[i].TableID) +'='+IntToStr(opCounts[i].Count));
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
begin
  with Database.Attachment.GetDBInformation([DatabaseInfoCommand]) do
    if (Count > 0) and (Items[0].GetItemType = DatabaseInfoCommand) then
      Result := Items[0].AsInteger
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: Integer): String;
begin
  with Database.Attachment.GetDBInformation([DatabaseInfoCommand]) do
    if (Count > 0) and (Items[0].GetItemType = DatabaseInfoCommand) then
      Result := Items[0].AsString
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;


function TIBDatabaseInfo.GetDBSQLDialect: Integer;
begin
  with Database.Attachment.GetDBInformation([isc_info_db_SQL_Dialect]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_db_SQL_Dialect) then
      Result := Items[0].AsInteger
    else
      Result := 1;
end;


end.
