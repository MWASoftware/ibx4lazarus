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
{    Associates Ltd 2011 - 2018                                               }
{                                                                        }
{************************************************************************}

unit IBDatabaseInfo;

{$Mode Delphi}

interface

uses
  SysUtils, Classes, IB, IBExternals, IBDatabase;

type

  { TIBDatabaseInfo }

  TIBDatabaseInfo = class(TComponent)
  private
    function GetDateDBCreated: TDateTime;
    function GetEncrypted: boolean;
    function GetEncryptionKeyName: string;
    function GetFirebirdVersion: String;
    function GetPagesFree: int64;
    function GetPagesUsed: int64;
    function GetTransactionCount: int64;
  protected
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
    procedure CheckDatabase;
    function GetAllocation: int64;
    function GetBaseLevel: byte;
    function GetDBFileName: String;
    function GetDBSiteName: String;
    function GetDBImplementationNo: byte;
    function GetDBImplementationClass: byte;
    function GetNoReserve: int64;
    function GetODSMinorVersion: integer;
    function GetODSMajorVersion: integer;
    function GetPageSize: int64;
    function GetVersion: String;
    function GetCurrentMemory: Int64;
    function GetForcedWrites: Int64;
    function GetMaxMemory: Int64;
    function GetNumBuffers: Int64;
    function GetSweepInterval: Int64;
    function GetUserNames: TStringList;
    function GetFetches: Int64;
    function GetMarks: Int64;
    function GetReads: Int64;
    function GetWrites: Int64;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetOperationCounts(DBInfoCommand: Integer; var FOperation: TStringList): TStringList;
    function GetReadOnly: Int64;
    function GetStringDatabaseInfo(DatabaseInfoCommand: Integer): String;
    function GetDBSQLDialect: Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetLongDatabaseInfo(DatabaseInfoCommand: Integer): long;  deprecated 'Use GetIntDatabaseInfo instead';
    function GetIntDatabaseInfo(DatabaseInfoCommand: Integer): Int64;
    function GetDatabasePage(PageNo: integer): string;
    property Allocation: Int64 read GetAllocation;
    property BaseLevel: byte read GetBaseLevel;
    property DateDBCreated: TDateTime read GetDateDBCreated;
    property DBFileName: String read GetDBFileName;
    property DBSiteName: String read GetDBSiteName;
    property DBImplementationNo: byte read GetDBImplementationNo;
    property DBImplementationClass: byte read GetDBImplementationClass;
    property Encrypted: boolean read GetEncrypted;
    property EncryptionKeyName: string read GetEncryptionKeyName;
    property NoReserve: Int64 read GetNoReserve;
    property ODSMinorVersion: integer read GetODSMinorVersion;
    property ODSMajorVersion: integer read GetODSMajorVersion;
    property PageSize: Int64 read GetPageSize;
    property FirebirdVersion: String read GetFirebirdVersion;
    property Version: String read GetVersion;
    property CurrentMemory: Int64 read GetCurrentMemory;
    property ForcedWrites: Int64 read GetForcedWrites;
    property MaxMemory: Int64 read GetMaxMemory;
    property NumBuffers: Int64 read GetNumBuffers;
    property SweepInterval: Int64 read GetSweepInterval;
    property UserNames: TStringList read GetUserNames;
    property Fetches: Int64 read GetFetches;
    property Marks: Int64 read GetMarks;
    property Reads: Int64 read GetReads;
    property Writes: Int64 read GetWrites;
    property TransactionCount: Int64 read GetTransactionCount;
    property BackoutCount: TStringList read GetBackoutCount;
    property DeleteCount: TStringList read GetDeleteCount;
    property ExpungeCount: TStringList read GetExpungeCount;
    property InsertCount: TStringList read GetInsertCount;
    property PurgeCount: TStringList read GetPurgeCount;
    property ReadIdxCount: TStringList read GetReadIdxCount;
    property ReadSeqCount: TStringList read GetReadSeqCount;
    property UpdateCount: TStringList read GetUpdateCount;
    property DBSQLDialect : Int64 read GetDBSQLDialect;
    property PagesUsed: Int64 read GetPagesUsed;
    property PagesFree: Int64 read GetPagesFree;
    property ReadOnly: Int64 read GetReadOnly;
  published
    property Database: TIBDatabase read FDatabase write FDatabase;
  end;

implementation

uses
  IBMessages;

{ TIBDatabaseInfo }

constructor TIBDatabaseInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  if assigned(FUserNames) then FUserNames.Free;
  if assigned(FBackoutCount) then FBackoutCount.Free;
  if assigned(FDeleteCount) then FDeleteCount.Free;
  if assigned(FExpungeCount) then FExpungeCount.Free;
  if assigned(FInsertCount) then FInsertCount.Free;
  if assigned(FPurgeCount) then FPurgeCount.Free;
  if assigned(FReadIdxCount) then FReadIdxCount.Free;
  if assigned(FReadSeqCount) then FReadSeqCount.Free;
  if assigned(FUpdateCount) then FUpdateCount.Free;
  inherited Destroy;
end;

function TIBDatabaseInfo.GetDateDBCreated: TDateTime;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_creation_date]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_creation_date) then
      Result := Items[0].GetAsDateTime
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetEncrypted: boolean;
var ConnFlags: Int64;
begin
  Result := ODSMajorVersion >= 12;
  if Result then
  try
    ConnFlags := GetIntDatabaseInfo(fb_info_conn_flags);
    Result := (ConnFlags and fb_info_crypt_encrypted) <> 0;
  except
    Result := false; {Introduced in Firebird 3.0.3}
  end;
end;

function TIBDatabaseInfo.GetEncryptionKeyName: string;
begin
  CheckDatabase;
  {Introduced in Firebird 3.0.3}
  with Database.Attachment.GetDBInformation([fb_info_crypt_key]) do
    if (Count > 0) and (Items[0].GetItemType = fb_info_crypt_key) then
      Result := Items[0].AsString
    else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetFirebirdVersion: String;
var Version: byte;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_firebird_version]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_firebird_version) then
      Items[0].DecodeVersionString(Version,Result)
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetPagesFree: int64;
begin
  result := GetIntDatabaseInfo(fb_info_pages_used);
end;

function TIBDatabaseInfo.GetPagesUsed: int64;
begin
  result := GetIntDatabaseInfo(fb_info_pages_free);
end;

function TIBDatabaseInfo.GetTransactionCount: int64;
begin
  result := GetIntDatabaseInfo(isc_info_active_tran_count);
end;

procedure TIBDatabaseInfo.CheckDatabase;
begin
  if Database = nil then
    IBError(ibxeDatabaseNotAssigned,[]);
  if Database.Attachment = nil then
    IBError(ibxeDatabaseClosed,[]);
end;

function TIBDatabaseInfo.GetAllocation: int64;
begin
  result := GetIntDatabaseInfo(isc_info_allocation);
end;

function TIBDatabaseInfo.GetBaseLevel: byte;
var Response: TByteArray;
begin
  CheckDatabase;
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
  CheckDatabase;
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
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_db_id]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_db_id) then
      Items[0].DecodeIDCluster(ConnectionType,FileName,Result)
    else
       IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBImplementationNo: byte;
var Response: TByteArray;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_implementation]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_implementation) then
    begin
      Response := Items[0].GetAsBytes;
      Result := Response[1];
    end
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDBImplementationClass: byte;
var Response: TByteArray;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_implementation]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_implementation) then
    begin
      Response := Items[0].GetAsBytes;
      Result := Response[2];
    end
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetNoReserve: int64;
begin
  result := GetIntDatabaseInfo(isc_info_no_reserve);
end;

function TIBDatabaseInfo.GetODSMinorVersion: integer;
begin
  CheckDatabase;
  Result := Database.Attachment.GetODSMinorVersion;
end;

function TIBDatabaseInfo.GetODSMajorVersion: integer;
begin
  CheckDatabase;
  Result := Database.Attachment.GetODSMajorVersion;
end;

function TIBDatabaseInfo.GetPageSize: int64;
begin
  result := GetIntDatabaseInfo(isc_info_page_size);
end;

function TIBDatabaseInfo.GetVersion: String;
var Version: byte;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_version]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_version) then
      Items[0].DecodeVersionString(Version,Result)
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetCurrentMemory: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_current_memory);
end;

function TIBDatabaseInfo.GetForcedWrites: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_forced_writes);
end;

function TIBDatabaseInfo.GetMaxMemory: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_max_memory);
end;

function TIBDatabaseInfo.GetNumBuffers: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_num_buffers);
end;

function TIBDatabaseInfo.GetSweepInterval: Int64; 
begin
  result := GetIntDatabaseInfo(isc_info_sweep_interval);
end;

function TIBDatabaseInfo.GetUserNames: TStringList;
begin
  CheckDatabase;
  Result := FUserNames;
  FUserNames.Clear;
  with Database.Attachment.GetDBInformation([isc_info_user_names]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_user_names) then
      Items[0].DecodeUserNames(Result)
  else
     IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetFetches: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_fetches);
end;

function TIBDatabaseInfo.GetMarks: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_marks);
end;

function TIBDatabaseInfo.GetReads: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_reads);
end;

function TIBDatabaseInfo.GetWrites: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_writes);
end;

function TIBDatabaseInfo.GetOperationCounts(DBInfoCommand: Integer;
  var FOperation: TStringList): TStringList;
var opCounts: TDBOperationCounts;
    i: integer;
begin
  CheckDatabase;
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

function TIBDatabaseInfo.GetReadOnly: Int64;
begin
  result := GetIntDatabaseInfo(isc_info_db_read_only);
end;

function TIBDatabaseInfo.GetLongDatabaseInfo(DatabaseInfoCommand: Integer
  ): long;
begin
  Result := GetIntDatabaseInfo(DatabaseInfoCommand);
end;

function TIBDatabaseInfo.GetIntDatabaseInfo(DatabaseInfoCommand: Integer
  ): Int64;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([DatabaseInfoCommand]) do
    if (Count > 0) and (Items[0].GetItemType = DatabaseInfoCommand) then
      Result := Items[0].AsInteger
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetDatabasePage(PageNo: integer): string;
var DBRequest: IDIRB;
begin
  DBRequest := Database.Attachment.AllocateDIRB;
  DBRequest.Add(fb_info_page_contents).AsInteger := PageNo;
  with Database.Attachment.GetDBInformation(DBRequest) do
    if (Count > 0) and (Items[0].GetItemType = fb_info_page_contents) then
      Result := Items[0].AsString
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;

function TIBDatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: Integer): String;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([DatabaseInfoCommand]) do
    if (Count > 0) and (Items[0].GetItemType = DatabaseInfoCommand) then
      Result := Items[0].AsString
    else
      IBError(ibxeUnexpectedDatabaseInfoResp,[nil]);
end;


function TIBDatabaseInfo.GetDBSQLDialect: Int64;
begin
  CheckDatabase;
  with Database.Attachment.GetDBInformation([isc_info_db_SQL_Dialect]) do
    if (Count > 0) and (Items[0].GetItemType = isc_info_db_SQL_Dialect) then
      Result := Items[0].AsInteger
    else
      Result := 1;
end;


end.
