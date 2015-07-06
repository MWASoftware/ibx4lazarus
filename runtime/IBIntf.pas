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

unit IBIntf;

{$Mode Delphi}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  IBHeader,IBExternals;

var
  BLOB_get: TBLOB_get;
  BLOB_put: TBLOB_put;
  isc_sqlcode: Tisc_sqlcode;
  isc_sql_interprete: Tisc_sql_interprete;
  isc_interprete: Tisc_interprete;
  isc_vax_integer: Tisc_vax_integer;
  isc_portable_integer: Tisc_portable_integer;
  isc_blob_info: Tisc_blob_info;
  isc_open_blob2: Tisc_open_blob2;
  isc_close_blob: Tisc_close_blob;
  isc_get_segment: Tisc_get_segment;
  isc_put_segment: Tisc_put_segment;
  isc_create_blob2: Tisc_create_blob2;
  isc_service_attach: Tisc_service_attach;
  isc_service_detach: Tisc_service_detach;
  isc_service_query: Tisc_service_query;
  isc_service_start: Tisc_service_start;
  isc_decode_date: Tisc_decode_date;
  isc_decode_sql_date: Tisc_decode_sql_date;
  isc_decode_sql_time: Tisc_decode_sql_time;
  isc_decode_timestamp: Tisc_decode_timestamp;
  isc_encode_date: Tisc_encode_date;
  isc_encode_sql_date: Tisc_encode_sql_date;
  isc_encode_sql_time: Tisc_encode_sql_time;
  isc_encode_timestamp: Tisc_encode_timestamp;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_execute2: Tisc_dsql_execute2;
  isc_dsql_execute: Tisc_dsql_execute;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_fetch: Tisc_dsql_fetch;
  isc_dsql_sql_info: Tisc_dsql_sql_info;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_prepare: Tisc_dsql_prepare;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_describe: Tisc_dsql_describe;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_drop_database: Tisc_drop_database;
  isc_detach_database: Tisc_detach_database;
  isc_attach_database: Tisc_attach_database;
  isc_database_info: Tisc_database_info;
  isc_start_multiple: Tisc_start_multiple;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_cancel_events: Tisc_cancel_events;
  isc_que_events: Tisc_que_events;
  isc_event_counts: Tisc_event_counts;
  isc_event_block: Tisc_event_block;
  isc_free: Tisc_free;
  isc_add_user   : Tisc_add_user;
  isc_delete_user: Tisc_delete_user;
  isc_modify_user: Tisc_modify_user;


{ Library Initialization }
procedure LoadIBLibrary;
procedure FreeIBLibrary;
function TryIBLoad: Boolean;
procedure CheckIBLoaded;

{ Stubs for 6.0 only functions }
function isc_rollback_retaining_stub(status_vector   : PISC_STATUS;
              tran_handle     : PISC_TR_HANDLE):
                                     ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function isc_service_attach_stub(status_vector      : PISC_STATUS;
                                 isc_arg2           : UShort;
                                 isc_arg3           : PChar;
                                 service_handle     : PISC_SVC_HANDLE;
                                 isc_arg5           : UShort;
                                 isc_arg6           : PChar):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function isc_service_detach_stub(status_vector      : PISC_STATUS;
                                 service_handle     : PISC_SVC_HANDLE):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function isc_service_query_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar;
                                isc_arg6             : UShort;
                                isc_arg7             : PChar;
                                isc_arg8             : UShort;
                                isc_arg9             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function isc_service_start_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_encode_sql_date_stub(tm_date           : PCTimeStructure;
                 ib_date           : PISC_DATE);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_encode_sql_time_stub(tm_date           : PCTimeStructure;
                   ib_time           : PISC_TIME);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_encode_timestamp_stub(tm_date          : PCTimeStructure;
                  ib_timestamp     : PISC_TIMESTAMP);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_decode_sql_date_stub(ib_date           : PISC_DATE;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_decode_sql_time_stub(ib_time           : PISC_TIME;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

procedure isc_decode_timestamp_stub(ib_timestamp     : PISC_TIMESTAMP;
                                    tm_date          : PCTimeStructure);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


var  IBServiceAPIPresent: boolean;

type
  TOnGetLibraryName = procedure(var libname: string);

const
  OnGetLibraryName: TOnGetLibraryName = nil;


implementation

uses Sysutils, IB, Dynlibs, Classes
{$IFDEF WINDOWS}
, Registry
{$ENDIF}
;

var
  IBLibrary: TLibHandle;

procedure LoadIBLibrary;

  function GetProcAddr(ProcName: PChar): Pointer;
  begin
    Result := GetProcAddress(IBLibrary, ProcName);
    if not Assigned(Result) then
      raise Exception.Create('Unable to load Firebird Client Library');
  end;
{$IFDEF UNIX }
  function FindLibrary(LibNameList: string): TLibHandle;
  var LibNames: TStringList;
      i: integer;
  begin
    Result := NilHandle;
    LibNames := TStringList.Create;
    try
      LibNames.Delimiter := ':';
      LibNames.StrictDelimiter := true;
      LibNames.DelimitedText := LibNameList; {Split list on semi-colon}
      for i := 0 to LibNames.Count - 1 do
      begin
        Result := LoadLibrary(LibNames[i]);
        if Result <> NilHandle then Exit;
      end;
    finally
      LibNames.Free;
    end;
  end;

  function InternalLoadLibrary: TLibHandle;
  var LibName: string;
  begin
    //Use default unless FBLIB overrides
    LibName := GetEnvironmentVariable('FBLIB');
    if LibName = '' then
    begin
      if assigned(OnGetLibraryName) then
        OnGetLibraryName(LibName)
      else
        LibName := FIREBIRD_SO2;
    end;
    Result := FindLibrary(LibName);
    {$IFDEF DARWIN}
    if Result = NilHandle then
    begin
      {See http://paulbeachsblog.blogspot.co.uk/2008/03/where-is-libfbclientdylib-on-macosx.html
       Try loading direct from Firebird Framework}

      LibName := '/Library/Frameworks/Firebird.framework/Firebird';
      Result := LoadLibrary(LibName);
    end
    {$ENDIF}
  end;
{$ENDIF}
{$IFDEF WINDOWS}
  function InternalLoadLibrary: TLibHandle;
  var InstallDir: string;
      dllPathName: string;
  begin
    if assigned(OnGetLibraryName) then
    begin
      OnGetLibraryName(dllPathName);
      Result := LoadLibrary(dllPathName);
      Exit
    end;

    //First look for Firebird Embedded Server in installation dir
    InstallDir := ExtractFilePath(Paramstr(0));  {Using ParamStr(0) assumes windows conventions}
    if FileExists(InstallDir + FIREBIRD_EMBEDDED) then
    begin
         dllPathName := InstallDir + FIREBIRD_EMBEDDED;
         Result := LoadLibrary(dllPathName)
    end
    else
    //Otherwise look for Firebird Client in installation dir
    if FileExists(InstallDir + FIREBIRD_CLIENT) then
    begin
      //assume firebird.conf and firebird.msg in same dir
      SetEnvironmentVariable('FIREBIRD',PChar(InstallDir));
      dllPathName := InstallDir +FIREBIRD_CLIENT;
      Result := LoadLibrary(dllPathName)
    end
    else
    //Use Registry key if it exists to locate library
    begin
      with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('SOFTWARE\Firebird Project\Firebird Server\Instances',false) then
        begin
          if ValueExists('DefaultInstance') then
          begin
            dllPathName := ReadString('DefaultInstance')  + 'bin' + DirectorySeparator + FIREBIRD_CLIENT;
            if FileExists(dllPathName) then
            begin
              Result := LoadLibrary(dllPathName);
              Exit
            end
          end
        end
      finally
        Free
      end;

      //Otherwise see if Firebird client is in path
      //and rely on registry for location of firebird.conf and firebird.msg
      Result := LoadLibrary(FIREBIRD_CLIENT);
      if Result <= HINSTANCE_ERROR then
         //well maybe InterBase is present...
         Result := LoadLibrary(IBASE_DLL);
    end
  end;
{$ENDIF}

begin
  IBLibrary := InternalLoadLibrary;
  if (IBLibrary <> NilHandle) then
  begin
    BLOB_get := GetProcAddr('BLOB_get'); {do not localize}
    BLOB_put := GetProcAddr('BLOB_put'); {do not localize}
    isc_sqlcode := GetProcAddr('isc_sqlcode'); {do not localize}
    isc_sql_interprete := GetProcAddr('isc_sql_interprete'); {do not localize}
    isc_interprete := GetProcAddr('isc_interprete'); {do not localize}
    isc_vax_integer := GetProcAddr('isc_vax_integer'); {do not localize}
    isc_portable_integer := GetProcAddr('isc_portable_integer'); {do not localize}
    isc_blob_info := GetProcAddr('isc_blob_info'); {do not localize}
    isc_open_blob2 := GetProcAddr('isc_open_blob2'); {do not localize}
    isc_close_blob := GetProcAddr('isc_close_blob'); {do not localize}
    isc_get_segment := GetProcAddr('isc_get_segment'); {do not localize}
    isc_put_segment := GetProcAddr('isc_put_segment'); {do not localize}
    isc_create_blob2 := GetProcAddr('isc_create_blob2'); {do not localize}
    isc_decode_date := GetProcAddr('isc_decode_date'); {do not localize}
    isc_encode_date := GetProcAddr('isc_encode_date'); {do not localize}
    isc_dsql_free_statement := GetProcAddr('isc_dsql_free_statement'); {do not localize}
    isc_dsql_execute2 := GetProcAddr('isc_dsql_execute2'); {do not localize}
    isc_dsql_execute := GetProcAddr('isc_dsql_execute'); {do not localize}
    isc_dsql_set_cursor_name := GetProcAddr('isc_dsql_set_cursor_name'); {do not localize}
    isc_dsql_fetch := GetProcAddr('isc_dsql_fetch'); {do not localize}
    isc_dsql_sql_info := GetProcAddr('isc_dsql_sql_info'); {do not localize}
    isc_dsql_alloc_statement2 := GetProcAddr('isc_dsql_alloc_statement2'); {do not localize}
    isc_dsql_prepare := GetProcAddr('isc_dsql_prepare'); {do not localize}
    isc_dsql_describe_bind := GetProcAddr('isc_dsql_describe_bind'); {do not localize}
    isc_dsql_describe := GetProcAddr('isc_dsql_describe'); {do not localize}
    isc_dsql_execute_immediate := GetProcAddr('isc_dsql_execute_immediate'); {do not localize}
    isc_drop_database := GetProcAddr('isc_drop_database'); {do not localize}
    isc_detach_database := GetProcAddr('isc_detach_database'); {do not localize}
    isc_attach_database := GetProcAddr('isc_attach_database'); {do not localize}
    isc_database_info := GetProcAddr('isc_database_info'); {do not localize}
    isc_start_multiple := GetProcAddr('isc_start_multiple'); {do not localize}
    isc_commit_transaction := GetProcAddr('isc_commit_transaction'); {do not localize}
    isc_commit_retaining := GetProcAddr('isc_commit_retaining'); {do not localize}
    isc_rollback_transaction := GetProcAddr('isc_rollback_transaction'); {do not localize}
    isc_cancel_events := GetProcAddr('isc_cancel_events'); {do not localize}
    isc_que_events := GetProcAddr('isc_que_events'); {do not localize}
    isc_event_counts := GetProcAddr('isc_event_counts'); {do not localize}
    isc_event_block := GetProcAddr('isc_event_block'); {do not localize}
    isc_free := GetProcAddr('isc_free'); {do not localize}
    isc_add_user := GetProcAddr('isc_add_user'); {do not localize}
    isc_delete_user := GetProcAddr('isc_delete_user'); {do not localize}
    isc_modify_user := GetProcAddr('isc_modify_user'); {do not localize}

    IBServiceAPIPresent := true;
    isc_rollback_retaining := GetProcAddress(IBLibrary, 'isc_rollback_retaining'); {do not localize}
    if Assigned(isc_rollback_retaining) then
    begin
      isc_service_attach := GetProcAddr('isc_service_attach'); {do not localize}
      isc_service_detach := GetProcAddr('isc_service_detach'); {do not localize}
      isc_service_query := GetProcAddr('isc_service_query'); {do not localize}
      isc_service_start := GetProcAddr('isc_service_start'); {do not localize}
      isc_decode_sql_date := GetProcAddr('isc_decode_sql_date'); {do not localize}
      isc_decode_sql_time := GetProcAddr('isc_decode_sql_time'); {do not localize}
      isc_decode_timestamp := GetProcAddr('isc_decode_timestamp'); {do not localize}
      isc_encode_sql_date := GetProcAddr('isc_encode_sql_date'); {do not localize}
      isc_encode_sql_time := GetProcAddr('isc_encode_sql_time'); {do not localize}
      isc_encode_timestamp := GetProcAddr('isc_encode_timestamp'); {do not localize}
    end else
    begin
      IBServiceAPIPresent := false;
      isc_rollback_retaining := isc_rollback_retaining_stub;
      isc_service_attach := isc_service_attach_stub;
      isc_service_detach := isc_service_detach_stub;
      isc_service_query := isc_service_query_stub;
      isc_service_start := isc_service_start_stub;
      isc_decode_sql_date := isc_decode_sql_date_stub;
      isc_decode_sql_time := isc_decode_sql_time_stub;
      isc_decode_timestamp := isc_decode_timestamp_stub;
      isc_encode_sql_date := isc_encode_sql_date_stub;
      isc_encode_sql_time := isc_encode_sql_time_stub;
      isc_encode_timestamp := isc_encode_timestamp_stub;
    end;
  end;
end;

procedure FreeIBLibrary;
begin
  if IBLibrary <> NilHandle then
  begin
    FreeLibrary(IBLibrary);
    IBLibrary := 0;
  end;
end;

function TryIBLoad: Boolean;
begin
  if (IBLibrary = NilHandle) then
    LoadIBLibrary;
  if (IBLibrary = NilHandle) then
    result := False
  else
    result := True;
end;

procedure CheckIBLoaded;
begin
  if not TryIBLoad then
    IBError(ibxeInterBaseMissing, [nil]);
end;


function isc_rollback_retaining_stub(status_vector   : PISC_STATUS;
              tran_handle     : PISC_TR_HANDLE):
                                     ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_rollback_retaining']); {do not localize}
end;

function isc_service_attach_stub(status_vector      : PISC_STATUS;
                                 isc_arg2           : UShort;
                                 isc_arg3           : PChar;
                                 service_handle     : PISC_SVC_HANDLE;
                                 isc_arg5           : UShort;
                                 isc_arg6           : PChar):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_attach']); {do not localize}
end;

function isc_service_detach_stub(status_vector      : PISC_STATUS;
                                 service_handle     : PISC_SVC_HANDLE):
                                 ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_detach']); {do not localize}
end;

function isc_service_query_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar;
                                isc_arg6             : UShort;
                                isc_arg7             : PChar;
                                isc_arg8             : UShort;
                                isc_arg9             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_query']); {do not localize}
end;

function isc_service_start_stub(status_vector        : PISC_STATUS;
                                service_handle       : PISC_SVC_HANDLE;
                                recv_handle          : PISC_SVC_HANDLE;
                                isc_arg4             : UShort;
                                isc_arg5             : PChar):
                                ISC_STATUS; {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Result := 0;
  IBError(ibxeIB60feature, ['isc_service_start']); {do not localize}
end;

procedure isc_encode_sql_date_stub(tm_date           : PCTimeStructure;
                 ib_date           : PISC_DATE);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_date']); {do not localize}
end;

procedure isc_encode_sql_time_stub(tm_date           : PCTimeStructure;
                   ib_time           : PISC_TIME);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_time']); {do not localize}
end;

procedure isc_encode_timestamp_stub(tm_date          : PCTimeStructure;
                  ib_timestamp     : PISC_TIMESTAMP);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_encode_sql_timestamp']); {do not localize}
end;

procedure isc_decode_sql_date_stub(ib_date           : PISC_DATE;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_sql_date']); {do not localize}
end;

procedure isc_decode_sql_time_stub(ib_time           : PISC_TIME;
                                   tm_date           : PCTimeStructure);
                                   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_sql_time']); {do not localize}
end;

procedure isc_decode_timestamp_stub(ib_timestamp     : PISC_TIMESTAMP;
                                    tm_date          : PCTimeStructure);
                                    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  IBError(ibxeIB60feature, ['isc_decode_timestamp']); {do not localize}
end;
initialization

finalization
  FreeIBLibrary;
end.
