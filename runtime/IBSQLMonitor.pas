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
{************************************************************************}

unit IBSQLMonitor;

{$Mode Delphi}

interface

uses
{$IFDEF LINUX }
  unix,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, LMessages, Messages, LCLIntf, LCLType, LCLProc, Classes, Forms, Controls, Dialogs, StdCtrls,
  IB, IBUtils, IBSQL, IBCustomDataSet, IBDatabase, IBServices, IBXConst;

const
  WM_MIN_IBSQL_MONITOR = WM_USER;
  WM_MAX_IBSQL_MONITOR = WM_USER + 512;
  WM_IBSQL_SQL_EVENT = WM_MIN_IBSQL_MONITOR + 1;

type
  TIBCustomSQLMonitor = class;

  { TIBSQLMonitor }
  TSQLEvent = procedure(EventText: String; EventTime : TDateTime) of object;

  TIBCustomSQLMonitor = class(TComponent)
  private
    FHWnd: HWND;
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
    procedure MonitorWndProc(var Message : TMessage);
    procedure SetEnabled(const Value: Boolean);
  protected
    property OnSQL: TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property Enabled : Boolean read FEnabled write SetEnabled default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release;
    property Handle : HWND read FHWnd;
  end;

  TIBSQLMonitor = class(TIBCustomSQLMonitor)
  published
    property OnSQL;
    property TraceFlags;
    property Enabled;
  end;

  IIBSQLMonitorHook = interface
    ['{CF65434C-9B75-4298-BA7E-E6B85B3C769D}']
    procedure RegisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
    procedure SQLPrepare(qry: TIBSQL); 
    procedure SQLExecute(qry: TIBSQL);
    procedure SQLFetch(qry: TIBSQL); 
    procedure DBConnect(db: TIBDatabase);
    procedure DBDisconnect(db: TIBDatabase);
    procedure TRStart(tr: TIBTransaction); 
    procedure TRCommit(tr: TIBTransaction);
    procedure TRCommitRetaining(tr: TIBTransaction); 
    procedure TRRollback(tr: TIBTransaction);
    procedure TRRollbackRetaining(tr: TIBTransaction);
    procedure ServiceAttach(service: TIBCustomService); 
    procedure ServiceDetach(service: TIBCustomService);
    procedure ServiceQuery(service: TIBCustomService);
    procedure ServiceStart(service: TIBCustomService);
    procedure SendMisc(Msg : String);
    function GetTraceFlags : TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetTraceFlags(const Value : TTraceFlags);
    function GetEnabled : boolean;
    procedure SetEnabled(const Value : Boolean);
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;


function MonitorHook: IIBSQLMonitorHook;
procedure EnableMonitoring;
procedure DisableMonitoring;
function MonitoringEnabled: Boolean;

implementation

uses
  contnrs {$IFDEF LINUX}, ipc {$ENDIF};

const
  MonitorHookNames: array[0..5] of String = (
    'FB.SQL.MONITOR.Mutex1_0',
    'FB.SQL.MONITOR.SharedMem1_0',
    'FB.SQL.MONITOR.WriteEvent1_0',
    'FB.SQL.MONITOR.WriteFinishedEvent1_0',
    'FB.SQL.MONITOR.ReadEvent1_0',
    'FB.SQL.MONITOR.ReadFinishedEvent1_0'
  );
  cMonitorHookSize = 1024;
  cMaxBufferSize = cMonitorHookSize - (4 * SizeOf(Integer)) - SizeOf(TDateTime);
  cDefaultTimeout = 500; { 1 seconds }


type

  { TIBSQLMonitorHook }
  TIBSQLMonitorHook = class(TInterfacedObject, IIBSQLMonitorHook)
  private
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create;
    procedure RegisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure UnregisterMonitor(SQLMonitor : TIBCustomSQLMonitor);
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
    procedure SQLPrepare(qry: TIBSQL); virtual;
    procedure SQLExecute(qry: TIBSQL); virtual;
    procedure SQLFetch(qry: TIBSQL); virtual;
    procedure DBConnect(db: TIBDatabase); virtual;
    procedure DBDisconnect(db: TIBDatabase); virtual;
    procedure TRStart(tr: TIBTransaction); virtual;
    procedure TRCommit(tr: TIBTransaction); virtual;
    procedure TRCommitRetaining(tr: TIBTransaction); virtual;
    procedure TRRollback(tr: TIBTransaction); virtual;
    procedure TRRollbackRetaining(tr: TIBTransaction); virtual;
    procedure ServiceAttach(service: TIBCustomService); virtual;
    procedure ServiceDetach(service: TIBCustomService); virtual;
    procedure ServiceQuery(service: TIBCustomService); virtual;
    procedure ServiceStart(service: TIBCustomService); virtual;
    procedure SendMisc(Msg : String);
    function GetEnabled: Boolean;
    function GetTraceFlags: TTraceFlags;
    function GetMonitorCount : Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetTraceFlags(const Value: TTraceFlags);
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
  end;

  { There are two possible objects.  One is a trace message object.
    This object holds the flag of the trace type plus the message.
    The second object is a Release object.  It holds the handle that
    the CM_RELEASE message is to be queued to. }

  { TTraceObject }

  TTraceObject = Class(TObject)
    FDataType : TTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
  public
    constructor Create(Msg : String; DataType : TTraceFlag); overload;
    constructor Create(obj : TTraceObject); overload;
    constructor Create(obj : TTraceObject; MsgOffset, MsgLen: integer); overload;
  end;

  TReleaseObject = Class(TObject)
    FHandle : THandle;
  public
    constructor Create(Handle : THandle);
  end;

  TWriterThread = class(TThread)
  private
    { Private declarations }
    FMsgs : TObjectList;
    procedure RemoveFromList;
  protected
    procedure Lock;
    Procedure Unlock;
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure WriteToBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteSQLData(Msg : String; DataType : TTraceFlag);
    procedure ReleaseMonitor(HWnd : THandle);
  end;

  TReaderThread = class(TThread)
  private
    st : TTraceObject;
    FMonitors : TObjectList;
    { Private declarations }
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure ReadSQLData;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMonitor(Arg : TIBCustomSQLMonitor);
    procedure RemoveMonitor(Arg : TIBCustomSQLMonitor);
  end;

  {Interprocess Communication Objects. All platform dependent IPC is abstracted
   into this set of objects }

  { TCommonSecurityAttributes }

  TCommonSecurityAttributes = class
  protected
    {$IFDEF LINUX}
    {$ELSE}
    Sa : TSecurityAttributes;
    {$ENDIF}
  public
    constructor Create;
  end;

  { TSyncEvent }

  TSyncGate = class(TCommonSecurityAttributes)
  private
    {$IFDEF LINUX}
    {$ELSE}
    FEvent: THandle;
    {$ENDIF}
  public
    constructor Create(EventName: string; InitialState: boolean);
    constructor Open(EventName: string);
    destructor Destroy; override;
    function WaitFor(Timeout: DWORD): DWORD;
    procedure OpenGate;
    procedure CloseGate;
  end;

  {TMutex}

  TMutex = class(TCommonSecurityAttributes)
  private
    {$IFDEF LINUX}
    {$ELSE}
    FMutex: THandle;
    {$ENDIF}
  public
    constructor Create(MutexName: string);
    constructor Open(MutexName: string);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  { TGlobalInterface }

  TGlobalInterface = class(TCommonSecurityAttributes)
  private
    {$IFDEF LINUX}
    {$ELSE}
    FSharedBuffer: THandle;
    {$ENDIF}
    FWriteLock: TMutex;
    FBuffer: PChar;
    FMonitorCount,
    FReaderCount,
    FTraceDataType,
    FBufferSize: PInteger;
    FTimeStamp: PDateTime;
    FReadEvent: TSyncGate;
    FReadFinishedEvent: TSyncGate;
    FWriteEvent: TSyncGate;
    FWriteFinishedEvent: TSyncGate;
    function GetMonitorCount: integer;
    function GetReaderCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IncMonitorCount: integer;
    function DecMonitorCount: integer;
    function IncReaderCount: integer;
    function DecReaderCount: integer;
    procedure SendTrace(TraceObject: TTraceObject);
    procedure ReceiveTrace(TraceObject: TTraceObject);
    property MontorCount: integer read GetMonitorCount;
    property ReaderCount: integer read GetReaderCount;
    property WriteEvent: TSyncGate read FWriteEvent;
    property WriteFinishedEvent: TSyncGate read FWriteFinishedEvent;
    property ReadEvent: TSyncGate read FReadEvent;
    property ReadFinishedEvent: TSyncGate read FReadFinishedEvent;
    property WriteLock: TMutex read FMutex;
  end;

{ TCommonSecurityAttributes }

constructor TCommonSecurityAttributes.Create;
{$IFDEF LINUX}
begin
{$ELSE}
var Sd : TSecurityDescriptor;
begin
  { Setup Security so anyone can connect to the MMF/Mutex/Event.  This is
    needed when IBX is used in a Service. }

  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
{$ENDIF}
end;

  { TMutex }

constructor TMutex.Create(MutexName: string);
{$IFDEF LINUX}
begin
{$ELSE}
begin
  inherited Create;
  FMutex := CreateMutex(@sa, False, PChar(MutexName))
{$ENDIF}
end;

constructor TMutex.Open(MutexName: string);
{$IFDEF LINUX}
begin
{$ELSE}
begin
  inherited Create;
  FMutex := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName))
{$ENDIF}
end;

destructor TMutex.Destroy;
begin
{$IFDEF LINUX}
{$ELSE}
  CloseHandle(FMutex);
{$ENDIF}
  inherited Destroy;
end;

{ Obtain ownership of the Mutex and prevent other threads from accessing protected resource }

procedure TMutex.Lock;
begin
{$IFDEF LINUX}
{$ELSE}
  WaitForSingleObject(FMutex, INFINITE);
{$ENDIF}
end;

{Give up ownership of the Mutex and allow other threads access }

procedure TMutex.Unlock;
begin
{$IFDEF LINUX}
{$ELSE}
  ReleaseMutex(FMutex);
{$ENDIF}
end;

{ TSyncGate }

constructor TSyncGate.Create(EventName: string; InitialState: boolean);
{$IFDEF LINUX}
begin
{$ELSE}
begin
  inherited Create;
  FEvent := CreateEvent(@sa, true, InitialState, PChar(EventName))

  if FEvent = 0 then
    IBError(ibxeCannotCreateSharedResource, [GetLastError])
{$ENDIF}
end;

constructor TSyncGate.Open(EventName: string);
{$IFDEF LINUX}
begin
{$ELSE}
begin
  inherited Create;
  FEvent := OpenEvent(EVENT_ALL_ACCESS, true, PChar(EventName));

  if FEvent = 0 then
    IBError(ibxeCannotCreateSharedResource, [GetLastError])
{$ENDIF}
end;

destructor TSyncGate.Destroy;
begin
{$IFDEF LINUX}
{$ELSE}
  CloseHandle(FEvent);
{$ENDIF}
  inherited Destroy;
end;

function TSyncGate.WaitFor(Timeout: DWORD): DWORD;
begin
  Result := WaitForSingleObject(FEvent,Timeout)  //Returns when Event is "signaled"
end;

procedure TSyncGate.OpenGate;
begin
{$IFDEF LINUX}
{$ELSE}
  SetEvent(FEvent) //Event State set to "signaled"
{$ENDIF}
end;

procedure TSyncGate.CloseGate;
begin
{$IFDEF LINUX}
{$ELSE}
  ResetEvent(FEvent) //Event State set to "unsignaled"
{$ENDIF}
end;


{ TGlobalInterface }

function TGlobalInterface.GetMonitorCount: integer;
begin
  Result := FMonitorCount^
end;

function TGlobalInterface.GetReaderCount: integer;
begin
  Result := FReaderCount^
end;

constructor TGlobalInterface.Create;
{$IFDEF LINUX}
begin
{$ELSE}
begin
  inherited Create;

  FSharedBuffer := CreateFileMapping($FFFFFFFF, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1]));

  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    FSharedBuffer := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1]));
    if (FSharedBuffer = 0) then
      IBError(ibxeCannotCreateSharedResource, [GetLastError]);

    FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);

    if FBuffer = nil then
      IBError(ibxeCannotCreateSharedResource, [GetLastError]);

    FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
    FReaderCount := PInteger(PChar(FMonitorCount) - SizeOf(Integer));
    FTraceDataType := PInteger(PChar(FReaderCount) - SizeOf(Integer));
    FTimeStamp := PDateTime(PChar(FTraceDataType) - SizeOf(TDateTime));
    FBufferSize := PInteger(PChar(FTimeStamp) - SizeOf(Integer));
    FWriteLock := TMutex.Create(PChar(MonitorHookNames[0]));
    FWriteEvent := TSyncGate.Create(MonitorHookNames[2],true);
    FWriteFinishedEvent := TSyncGate.Create(MonitorHookNames[3],true);
    FReadEvent := TSyncGate.Create(MonitorHookNames[4],true);
    FReadFinishedEvent := TSyncGate.Create(MonitorHookNames[5],true);
  end
  else
  begin
    FWriteLock := TMutex.Open(PChar(MonitorHookNames[0]));
    FWriteEvent := TSyncGate.Open(MonitorHookNames[2], False);
    FWriteFinishedEvent := TSyncGate.Open(MonitorHookNames[3], True);
    FReadEvent := TSyncGate.Open(MonitorHookNames[4], False);
    FReadFinishedEvent := TSyncGate.Open(MonitorHookNames[5], False);

    FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
    FReaderCount := PInteger(PChar(FMonitorCount) - SizeOf(Integer));
    FTraceDataType := PInteger(PChar(FReaderCount) - SizeOf(Integer));
    FTimeStamp := PDateTime(PChar(FTraceDataType) - SizeOf(TDateTime));
    FBufferSize := PInteger(PChar(FTimeStamp) - SizeOf(Integer));
    FMonitorCount^ := 0;
    FReaderCount^ := 0;
    FBufferSize^ := 0;
  end;

  { This should never evaluate to true, if it does
  there has been a hiccup somewhere. }

  if FMonitorCount^ < 0 then
    FMonitorCount^ := 0;
  if FReaderCount^ < 0 then
    FReaderCount^ := 0;
{$ENDIF}
end;

destructor TGlobalInterface.Destroy;
begin
{$IFDEF LINUX}
{$ELSE}
    UnmapViewOfFile(FBuffer);
    CloseHandle(FSharedBuffer);
{$ENDIF}
    if assigned(FWriteLock) then FWriteLock.Free;
    if assigned(FWriteEvent) then FWriteEvent.Free;
    if assigned(FWriteFinishedEvent) then FWriteFinishedEvent.Free;
    if assigned(FReadEvent) then FReadEvent.Free;
    if assigned(FReadFinishedEvent) then FReadFinishedEvent.Free;
  inherited Destroy;
end;

function TGlobalInterface.IncMonitorCount: integer;
begin
{$IFDEF LINUX}
{$ELSE}
  InterlockedIncrement(FMonitorCount^);
{$ENDIF}
  Result := FMonitorCount^
end;

function TGlobalInterface.DecMonitorCount: integer;
begin
{$IFDEF LINUX}
{$ELSE}
  InterlockedDecrement(FMonitorCount^);
{$ENDIF}
  Result := FMonitorCount^
end;

function TGlobalInterface.IncReaderCount: integer;
begin
  InterlockedIncrement(FReaderCount^);
  Result := FReaderCount^
end;

function TGlobalInterface.DecReaderCount: integer;
begin
  InterlockedDecrement(FReaderCount^);
  Result := FReaderCount^
end;

procedure TGlobalInterface.SendTrace(TraceObject: TTraceObject);
begin
  FTraceDataType^ := Integer(TraceObject.FDataType);
  FTimeStamp^ := TraceObject.FTimeStamp;
  FBufferSize^ := Min(Lrngth(TraceObject.FMsg), cMaxBufferSize);
  Move(TraceObject.FMsg[1], FBuffer[0], FBufferSize^);
end;

procedure TGlobalInterface.ReceiveTrace(TraceObject: TTraceObject);
begin
  SetString(TraceObject.FMsg, FBuffer, FBufferSize^);
  TraceObject.FDataType := TTraceFlag(FTraceDataType^);
  TraceObject.FTimeStamp := TDateTime(FTimeStamp^);
end;


var
  FGlobalInterface: TGlobalInterface;
  FWriterThread : TWriterThread;
  FReaderThread : TReaderThread;
  _MonitorHook: TIBSQLMonitorHook;
  bDone: Boolean;
  CS : TRTLCriticalSection;
  
{ TIBCustomSQLMonitor }

constructor TIBCustomSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTraceFlags := [tfqPrepare .. tfMisc];
  FEnabled := true;
  if not (csDesigning in ComponentState) then
  begin
    FHWnd := AllocateHWnd(MonitorWndProc);
    MonitorHook.RegisterMonitor(self);
  end;
end;

destructor TIBCustomSQLMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled then
      MonitorHook.UnregisterMonitor(self);
    DeallocateHwnd(FHWnd);
  end;
  inherited Destroy;
end;

procedure TIBCustomSQLMonitor.MonitorWndProc(var Message: TMessage);
var
  st : TTraceObject;
begin
  case Message.Msg of
    WM_IBSQL_SQL_EVENT:
    begin
      st := TTraceObject(Message.LParam);
      if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
      st.Free;
    end;
    CM_RELEASE :
      Free;
    else
      Dispatch(Message)
//      DefWindowProc(FHWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

procedure TIBCustomSQLMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TIBCustomSQLMonitor.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if not (csDesigning in ComponentState) then
      if FEnabled then
        Monitorhook.RegisterMonitor(self)
      else
        MonitorHook.UnregisterMonitor(self);
  end;
end;

{ TIBSQLMonitorHook }

constructor TIBSQLMonitorHook.Create;
begin
  inherited Create;
  FTraceFlags := [tfQPrepare..tfMisc];
  FEnabled := true;
end;

procedure TIBSQLMonitorHook.DBConnect(db: TIBDatabase);
var
  st : String;
begin
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Connect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

procedure TIBSQLMonitorHook.DBDisconnect(db: TIBDatabase);
var
  st: String;
begin
  if (Self = nil) then exit;
  if FEnabled then
  begin
    if not (tfConnect in FTraceFlags * db.TraceFlags) then
      Exit;
    st := db.Name + ': [Disconnect]'; {do not localize}
    WriteSQLData(st, tfConnect);
  end;
end;

function TIBSQLMonitorHook.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TIBSQLMonitorHook.GetMonitorCount: Integer;
begin
  Result := FGlobalInterface.MonitorCount
end;

function TIBSQLMonitorHook.GetTraceFlags: TTraceFlags;
begin
  Result := FTraceFlags;
end;

procedure TIBSQLMonitorHook.RegisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
begin
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
  if not Assigned(FReaderThread) then
    FReaderThread := TReaderThread.Create;
  FReaderThread.AddMonitor(SQLMonitor);
end;

procedure TIBSQLMonitorHook.ReleaseMonitor(Arg: TIBCustomSQLMonitor);
begin
  FWriterThread.ReleaseMonitor(Arg.FHWnd);
end;

procedure TIBSQLMonitorHook.SendMisc(Msg: String);
begin
  if FEnabled then
  begin
    WriteSQLData(Msg, tfMisc);
  end;
end;

procedure TIBSQLMonitorHook.ServiceAttach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Attach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceDetach(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Detach]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceQuery(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Query]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.ServiceStart(service: TIBCustomService);
var
  st: String;
begin
  if FEnabled then
  begin
    if not (tfService in (FTraceFlags * service.TraceFlags)) then
      Exit;
    st := service.Name + ': [Start]'; {do not localize}
    WriteSQLData(st, tfService);
  end;
end;

procedure TIBSQLMonitorHook.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
  if (not FEnabled) and (Assigned(FWriterThread)) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

procedure TIBSQLMonitorHook.SetTraceFlags(const Value: TTraceFlags);
begin
  FTraceFlags := Value
end;

procedure TIBSQLMonitorHook.SQLExecute(qry: TIBSQL);
var
  st: String;
  i: Integer;
begin
  if FEnabled then
  begin
    if not ((tfQExecute in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags)) ) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Execute] ' + qry.SQL.Text; {do not localize}
    if qry.Params.Count > 0 then begin
      for i := 0 to qry.Params.Count - 1 do begin
        st := st + CRLF + '  ' + qry.Params[i].Name + ' = '; 
        try
          if qry.Params[i].IsNull then
            st := st + '<NULL>'; {do not localize}
          st := st + qry.Params[i].AsString;
        except
          st := st + '<' + SCantPrintValue + '>';
        end;
      end;
    end;
    WriteSQLData(st, tfQExecute);
  end;
end;

procedure TIBSQLMonitorHook.SQLFetch(qry: TIBSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQFetch in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Fetch] ' + qry.SQL.Text; {do not localize}
    if (qry.EOF) then
      st := st + CRLF + '  ' + SEOFReached;
    WriteSQLData(st, tfQFetch);
  end;
end;

procedure TIBSQLMonitorHook.SQLPrepare(qry: TIBSQL);
var
  st: String;
begin
  if FEnabled then
  begin
    if not ((tfQPrepare in (FTraceFlags * qry.Database.TraceFlags)) or
            (tfStmt in (FTraceFlags * qry.Database.TraceFlags))) then
      Exit;
    if qry.Owner is TIBCustomDataSet then
      st := TIBCustomDataSet(qry.Owner).Name
    else
      st := qry.Name;
    st := st + ': [Prepare] ' + qry.SQL.Text + CRLF; {do not localize}
    st := st + '  Plan: ' + qry.Plan; {do not localize}
    WriteSQLData(st, tfQPrepare);
  end;
end;

procedure TIBSQLMonitorHook.TRCommit(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit (Hard commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRCommitRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Commit retaining (Soft commit)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRRollback(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRRollbackRetaining(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Rollback retaining (Soft rollback)]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.TRStart(tr: TIBTransaction);
var
  st: String;
begin
  if FEnabled then
  begin
    if Assigned(tr.DefaultDatabase) and
       (not (tfTransact in (FTraceFlags * tr.DefaultDatabase.TraceFlags))) then
      Exit;
    st := tr.Name + ': [Start transaction]'; {do not localize}
    WriteSQLData(st, tfTransact);
  end;
end;

procedure TIBSQLMonitorHook.UnregisterMonitor(SQLMonitor: TIBCustomSQLMonitor);
var
  Created : Boolean;
begin
  FReaderThread.RemoveMonitor(SQLMonitor);
  if FReaderThread.FMonitors.Count = 0 then
  begin
    FReaderThread.Terminate;

    { There is a possibility of a reader thread, but no writer one.
      When in that situation, the reader needs to be released after
      the terminate is set.  To do that, create a Writer thread, send
      the release code (a string of ' ' and type tfMisc) and then free
      it up. }
      
    Created := false;
    if not Assigned(FWriterThread) then
    begin
      FWriterThread := TWriterThread.Create;
      Created := true;
    end;
    FWriterThread.WriteSQLData(' ', tfMisc);
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
    if Created then
    begin
      FWriterThread.Terminate;
      FWriterThread.WaitFor;
      FreeAndNil(FWriterThread);
    end;
  end;
end;

procedure TIBSQLMonitorHook.WriteSQLData(Text: String;
  DataType: TTraceFlag);
begin
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
  Text := CRLF + '[Application: ' + Application.Title + ']' + CRLF + Text; {do not localize}
  if not Assigned(FWriterThread) then
    FWriterThread := TWriterThread.Create;
  FWriterThread.WriteSQLData(Text, DataType);
end;

{ TWriterThread }

constructor TWriterThread.Create;

begin
  inherited Create(true);
  FMsgs := TObjectList.Create(true);
  Resume;
end;

destructor TWriterThread.Destroy;
begin
  FMsgs.Free;
  inherited Destroy;
end;

procedure TWriterThread.Execute;
begin
  { Place thread code here }
  while ((not Terminated) and (not bDone)) or
        (FMsgs.Count <> 0) do
  begin
    { Any one listening? }
    if FGlobalInterface.MonitorCount = 0 then
    begin
      if FMsgs.Count <> 0 then
        Synchronize(RemoveFromList);
      Sleep(50);
    end
    else
      { Anything to process? }
      if FMsgs.Count <> 0 then
      begin
       { If the current queued message is a release release the object }
        if FMsgs.Items[0] is TReleaseObject then
          PostMessage(TReleaseObject(FMsgs.Items[0]).FHandle, CM_RELEASE, 0, 0)
        else
        { Otherwise write the TraceObject to the buffer }
        begin
          WriteToBuffer;
        end;
      end
      else
        Sleep(50);
  end;
end;

procedure TWriterThread.Lock;
begin
  FGlobalInterface.WriteLock.Lock;
end;

procedure TWriterThread.Unlock;
begin
  FGlobalInterface.WriteLock.Unlock;
end;

procedure TWriterThread.WriteSQLData(Msg : String; DataType: TTraceFlag);
begin
  FMsgs.Add(TTraceObject.Create(Msg, DataType));
end;

procedure TWriterThread.BeginWrite;
begin
  Lock;
end;

procedure TWriterThread.EndWrite;
begin
  {
   * 1. Wait to end the write until all registered readers have
   *    started to wait for a write event
   * 2. Block all of those waiting for the write to finish.
   * 3. Block all of those waiting for all readers to finish.
   * 4. Unblock all readers waiting for a write event.
   * 5. Wait until all readers have finished reading.
   * 6. Now, block all those waiting for a write event.
   * 7. Unblock all readers waiting for a write to be finished.
   * 8. Unlock the mutex.
   }
  with FGlobalInterface do
  begin
    while ReadEvent.WaitFor(cDefaultTimeout)= WAIT_TIMEOUT do
    { If we have timed out then we have lost a monitor }
    begin
      if MonitorCount > 0 then
        DecMonitorCount;
      if (ReaderCount = MonitorCount - 1) or (MonitorCount = 0) then
        ReadEvent.OpenGate
    end;
    WriteFinishedEvent.CloseGate;
    ReadFinishedEvent.CloseGate;
    WriteEvent.OpenGate; { Let all readers pass through. }
    while ReadFinishedEvent.WaitFor(cDefaultTimeout) = WAIT_TIMEOUT do
    { If we have timed out then we have lost a monitor }
      if (ReaderCount = 0) or (DecReaderCount = 0) then
        ReadFinishedEvent.OpenGate;
    WriteEvent.CloseGate;
    WriteFinishedEvent.OpenGate;
  end;
  Unlock;
end;

procedure TWriterThread.WriteToBuffer;
var I, len: integer;
    Temp: TTraceObject;
begin
  Lock;
  try
    { If there are no monitors throw out the message
      The alternative is to have messages queue up until a
      monitor is ready.}

    if FGlobalInterface.MonitorCount = 0 then
      Synchronize(RemoveFromList)
    else
    begin
      i := 1;
      len := Length(TTraceObject(FMsgs[0]).FMsg);
      if len <= cMaxBufferSize then
        FGlobalInterface.SendTrace(TTraceObject(FMsgs[0]))
      else
      while len > 0 do
      begin
        Temp := TTraceObject.Create(TTraceObject(FMsgs[0]),i,Min(len,cMaxBufferSize));
        try
          BeginWrite;
          try
            FGlobalInterface.SendTrace(Temp);
            Inc(i,cMaxBufferSize);
            Dec(len,cMaxBufferSize);
          finally
          {Do this in the main thread so the main thread
          adds and deletes}
            Synchronize(RemoveFromList);
           EndWrite;
          end;
        finally
          Temp.Free
        end
      end
    end;
  finally
    Unlock;
  end;
end;

procedure TWriterThread.RemoveFromList;
begin
  FMsgs.Remove(FMsgs[0]); { Pop the written item }
end;

procedure TWriterThread.ReleaseMonitor(HWnd: THandle);
begin
  FMsgs.Add(TReleaseObject.Create(HWnd));
end;

{ TTraceObject }

constructor TTraceObject.Create(Msg : String; DataType: TTraceFlag);
begin
  FMsg := Msg;
  FDataType := DataType;
  FTimeStamp := Now;
end;

constructor TTraceObject.Create(obj: TTraceObject);
begin
  FMsg := obj.FMsg;
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
end;

constructor TTraceObject.Create(obj: TTraceObject; MsgOffset, MsgLen: integer);
begin
  FDataType := obj.FDataType;
  FTimeStamp := obj.FTimeStamp;
  FMsg := copy(obj.FMsg,MsgOffset,MsgLen)
end;

{ TReleaseObject }

constructor TReleaseObject.Create(Handle: THandle);
begin
  FHandle := Handle;
end;

{ ReaderThread }

procedure TReaderThread.AddMonitor(Arg: TIBCustomSQLMonitor);
begin
  EnterCriticalSection(CS);
  if FMonitors.IndexOf(Arg) < 0 then
    FMonitors.Add(Arg);
  LeaveCriticalSection(CS);
end;

procedure TReaderThread.BeginRead;
begin
  {
   * 1. Wait for the "previous" write event to complete.
   * 2. Increment the number of readers.
   * 3. if the reader count is the number of interested readers, then
   *    inform the system that all readers are ready.
   * 4. Finally, wait for the FWriteEvent to signal.
   }
  with FGlobalInterface do
  begin
    WriteFinishedEvent.WaitFor(INFINITE);
    IncReaderCount;
    if ReaderCount = MonitorCount then
      ReadEvent.OpenGate;
    WriteEvent.WaitFor(INFINITE);
  end;
end;

constructor TReaderThread.Create;
begin
  inherited Create(true);
  st := TTraceObject.Create('', tfMisc);
  FMonitors := TObjectList.Create(false);
  FGlobalInterface.IncMonitorCount;
  Resume;
end;

destructor TReaderThread.Destroy;
begin
  if FGlobalInterface.MonitorCount > 0 then
    FGlobalInterface.DecMonitorCount;
  FMonitors.Free;
  st.Free;
  inherited Destroy;
end;

procedure TReaderThread.EndRead;
begin
  {
    * 1. Decrement the number of interested readers
    * 2. Once all readers have completed, reset read gate
    * 3. Tell the writer that all readers have completed
  }

  if FGlobalInterface.DecReaderCount = 0 then
  begin
    FGlobalInterface.ReadEvent.CloseGate;
    FGlobalInterface.ReadFinishedEvent.OpenGate;
  end;
end;

procedure TReaderThread.Execute;
var
  i : Integer;
  FTemp : TTraceObject;
begin
  { Place thread code here }
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData;
    if (st.FMsg <> '') and
       not ((st.FMsg = ' ') and (st.FDataType = tfMisc)) then
    begin
      for i := 0 to FMonitors.Count - 1 do
      begin
        FTemp := TTraceObject.Create(st);
        PostMessage(TIBCustomSQLMonitor(FMonitors[i]).Handle,
                    WM_IBSQL_SQL_EVENT,
                    0,
                    LPARAM(FTemp));
      end;
    end;
  end;
end;

procedure TReaderThread.ReadSQLData;
begin
  st.FMsg := '';
  BeginRead;
  if not bDone then
  try
    FGlobalInterface.ReadTrace(st)
  finally
    EndRead;
  end;
end;

procedure TReaderThread.RemoveMonitor(Arg: TIBCustomSQLMonitor);
begin
  EnterCriticalSection(CS);
  FMonitors.Remove(Arg);
  LeaveCriticalSection(CS);
end;

{ Misc methods }

function MonitorHook: IIBSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
  begin
    EnterCriticalSection(CS);
    if (_MonitorHook = nil) and (not bDone) then
    begin
      _MonitorHook := TIBSQLMonitorHook.Create;
      _MonitorHook._AddRef;
    end;
    LeaveCriticalSection(CS);
  end;
  result := _MonitorHook;
end;

procedure EnableMonitoring;
begin
  MonitorHook.Enabled := True;
end;

procedure DisableMonitoring;
begin
  MonitorHook.Enabled := False;
end;

function MonitoringEnabled: Boolean;
begin
  result := MonitorHook.Enabled;
end;

procedure CloseThreads;
begin
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Terminate;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end;
  if Assigned(FWriterThread) then
  begin
    FWriterThread.Terminate;
    FWriterThread.WaitFor;
    FreeAndNil(FWriterThread);
  end;
end;

initialization
  InitializeCriticalSection(CS);
  FGlobalInterface := nil;
  _MonitorHook := nil;
  FWriterThread := nil;
  FReaderThread := nil;
  bDone := False;

finalization
  try
    { Write an empty string to force the reader to unlock during termination }
    bDone := True;
    if Assigned(FReaderThread) then
    begin
      if not Assigned(FWriterThread) then
        FWriterThread := TWriterThread.Create;
      FWriterThread.WriteSQLData(' ', tfMisc);
    end;
    CloseThreads;
    if Assigned(_MonitorHook) then
      _MonitorHook._Release;
    if assigned(FGlobalInterface) then FreeAndNil(FGlobalInterface);
  finally
    _MonitorHook := nil;
    DeleteCriticalSection(CS);
  end;
end.
