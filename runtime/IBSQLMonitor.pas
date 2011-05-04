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

{
  This unit has been significantly revised for the Lazarus port. Specially,
  there was a need to re-organise the code to isolate the Windows specific
  IPC and to introduce SV5 IPC as an alternative for Linux and other platforms.
}

unit IBSQLMonitor;

{$Mode Delphi}

interface

uses
  LMessages, LCLIntf, LCLType, LCLProc,  Forms, Controls, Dialogs,
  IB, IBUtils, IBSQL, IBCustomDataSet, IBDatabase, IBServices, IBXConst,SysUtils,
  Classes,
{$IFDEF WINDOWS }
  Windows
{$ELSE}
  unix
{$ENDIF}
;

{Note that the original inter-thread communication between the Reader Thread and
 the ISQL Monitor used the Windows PostMessage interface. This is currently not
 useable under the FPC RTL as AllocateHWnd is not functional. It has been replaced
 by the use of the Synchronize method.}

{$IFDEF WINDOWS}
{$DEFINE USE_WINDOWS_IPC}
{$ENDIF}

{$IFDEF UNIX}
{$DEFINE USE_SV5_IPC}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE HAS_SEMTIMEDOP}
{$ENDIF}

const
  WM_MIN_IBSQL_MONITOR = WM_USER;
  WM_MAX_IBSQL_MONITOR = WM_USER + 512;
  WM_IBSQL_SQL_EVENT = WM_MIN_IBSQL_MONITOR + 1;

type
  TIBCustomSQLMonitor = class;

  { TIBSQLMonitor }
  TSQLEvent = procedure(EventText: String; EventTime : TDateTime) of object;

  { TIBCustomSQLMonitor }

  TIBCustomSQLMonitor = class(TComponent)
  private
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure ReleaseObject;  {Called from Writer Thread}
    procedure ReceiveMessage(Msg: TObject);    {Called from Reader Thread}
    property OnSQL: TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
    property Enabled : Boolean read FEnabled write SetEnabled default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release;
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
   contnrs, syncobjs
   {$IFDEF USE_SV5_IPC}
   ,ipc, Errors, baseunix
   {$IF FPC_FULLVERSION <= 20402 } , initc {$ENDIF}
   {$ENDIF};

   {$STATIC ON}

const
  cMonitorHookSize = 1024;
  cMsgWaitTime = 1000;
  cWriteMessageAvailable = 'WriterMsgQueue';

type
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

  { TReleaseObject }

  TReleaseObject = Class(TObject)
    FMonitor : TIBCustomSQLMonitor;
  public
    constructor Create(Monitor : TIBCustomSQLMonitor);
  end;

  {$IFDEF USE_SV5_IPC}
  {$I sv5ipc.inc}
  {$ENDIF}
  {$IFDEF USE_WINDOWS_IPC}
  {$I winipc.inc}
  {$ENDIF}

type

  { TIBSQLMonitorHook }

  TIBSQLMonitorHook = class(TInterfacedObject, IIBSQLMonitorHook)
  private
    FGlobalInterface: TGlobalInterface;
    FTraceFlags: TTraceFlags;
    FEnabled: Boolean;
  protected
    procedure WriteSQLData(Text: String; DataType: TTraceFlag);
  public
    constructor Create;
    destructor Destroy; override;
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
    procedure ForceRelease;
    property TraceFlags: TTraceFlags read GetTraceFlags write SetTraceFlags;
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
  end;

  { TWriterThread }

  TWriterThread = class(TThread)
  private
    { Private declarations }
    FGlobalInterface: TGlobalInterface;
    FMsgs : TObjectList;
    FCriticalSection: TCriticalSection;
    FMsgAvailable: TEventObject;
    procedure RemoveFromList;
    procedure PostRelease;
  public
    procedure ReleaseMonitor(Arg : TIBCustomSQLMonitor);
  protected
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure WriteToBuffer;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure WriteSQLData(Msg : String; DataType : TTraceFlag);
  end;

  { TReaderThread }

  TReaderThread = class(TThread)
  private
    { Private declarations }
    st : TTraceObject;
    FMonitors : TObjectList;
    FGlobalInterface: TGlobalInterface;
    FCriticalSection: TCriticalSection;
    procedure AlertMonitors;
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure ReadSQLData;
    procedure Execute; override;
  public
    constructor Create(GlobalInterface: TGlobalInterface);
    destructor Destroy; override;
    procedure AddMonitor(Arg : TIBCustomSQLMonitor);
    procedure RemoveMonitor(Arg : TIBCustomSQLMonitor);
  end;


var
  FWriterThread : TWriterThread;
  FReaderThread : TReaderThread;
  _MonitorHook: TIBSQLMonitorHook;
  bDone: Boolean;
  CS : TCriticalSection;
  
{ TIBCustomSQLMonitor }

constructor TIBCustomSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTraceFlags := [tfqPrepare .. tfMisc];
  if not (csDesigning in ComponentState)  then
  begin
    MonitorHook.RegisterMonitor(self);
  end;
  FEnabled := true;
end;

destructor TIBCustomSQLMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FEnabled and assigned(_MonitorHook) then
      MonitorHook.UnregisterMonitor(self);
  end;
  inherited Destroy;
end;

procedure TIBCustomSQLMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TIBCustomSQLMonitor.ReleaseObject;
begin
  Free
end;

procedure TIBCustomSQLMonitor.ReceiveMessage(Msg: TObject);
var
  st: TTraceObject;
begin
  st := (Msg as TTraceObject);
  if (Assigned(FOnSQLEvent)) and
         (st.FDataType in FTraceFlags) then
        FOnSQLEvent(st.FMsg, st.FTimeStamp);
  st.Free;
  {$IFDEF WINDOWS}
  Application.ProcessMessages
  {$ENDIF}
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
  FEnabled := false;
end;

destructor TIBSQLMonitorHook.Destroy;
begin
  if assigned(FGlobalInterface) then FGlobalInterface.Free;
  inherited Destroy;
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
   //writeln('Register Monitor');
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
 if not Assigned(FReaderThread) then
    FReaderThread := TReaderThread.Create(FGlobalInterface);
  FReaderThread.AddMonitor(SQLMonitor);
end;

procedure TIBSQLMonitorHook.ReleaseMonitor(Arg: TIBCustomSQLMonitor);
begin
  FWriterThread.ReleaseMonitor(Arg);
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

procedure TIBSQLMonitorHook.ForceRelease;
begin
    if Assigned(FReaderThread) then
    begin
      FReaderThread.Terminate;
      if not Assigned(FWriterThread) then
        FWriterThread := TWriterThread.Create(FGlobalInterface);
      FWriterThread.WriteSQLData(' ', tfMisc);
    end;
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
//writeln('Unregister Monitor');
  if assigned(FReaderThread) then
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
        FWriterThread := TWriterThread.Create(FGlobalInterface);
        Created := true;
      end;
      FWriterThread.WriteSQLData(' ', tfMisc);
      //writeln('Wait for read Terminate');
      FReaderThread.WaitFor;
      if assigned(FReaderThread.FatalException) then
        IBError(ibxeThreadFailed,['Reader',Exception(FReaderThread.FatalException).Message]);
      //writeln('Freeing Reader Thread');
      FreeAndNil(FReaderThread);
      //writeln('Reader Thread Freed');
      if Created then
      begin
        FWriterThread.Terminate;
        //writeln('Wait for write Terminate');
        FWriterThread.WaitFor;
        if assigned(FWriterThread.FatalException) then
          IBError(ibxeThreadFailed,['Writer',Exception(FWriterThread.FatalException).Message]);
        FreeAndNil(FWriterThread);
      end;
    end;
  end;
  //writeln('Unregister done')
end;

procedure TIBSQLMonitorHook.WriteSQLData(Text: String;
  DataType: TTraceFlag);
begin
 //writeln('Write SQL Data: '+Text);
  if not assigned(FGlobalInterface) then
    FGlobalInterface := TGlobalInterface.Create;
  Text := CRLF + '[Application: ' + Application.Title + ']' + CRLF + Text; {do not localize}
  if not Assigned(FWriterThread) then
    FWriterThread := TWriterThread.Create(FGLobalInterface);
  FWriterThread.WriteSQLData(Text, DataType);
end;

{ TWriterThread }

constructor TWriterThread.Create(GlobalInterface: TGlobalInterface);

begin
  inherited Create(true);
  //writeln('Write Object Created');
  FGlobalInterface := GlobalInterface;
  FMsgs := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
  FMsgAvailable := TEventObject.Create(FGlobalInterface.Sa,true,false,cWriteMessageAvailable);
  Resume;
end;

destructor TWriterThread.Destroy;
begin
  if assigned(FMsgs) then FMsgs.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  if assigned(FMsgAvailable) then FMsgAvailable.Free;
  inherited Destroy;
end;

procedure TWriterThread.Execute;
begin
//writeln('Write Thread starts');
 try
  { Place thread code here }
  while ((not Terminated) and (not bDone)) or
        (FMsgs.Count <> 0) do
  begin
    FMsgAvailable.WaitFor(cMsgWaitTime);
    { Any one listening? }
    if FGlobalInterface.MonitorCount = 0 then
    begin
      if FMsgs.Count <> 0 then
      begin
        //writeln('Write Thread Drop Message');
        RemoveFromList;
      end;
    end
    else
      { Anything to process? }
      if FMsgs.Count <> 0 then
      begin
       { If the current queued message is a release release the object }
        if FMsgs.Items[0] is TReleaseObject then
        begin
          //writeln('Post Release');
          Synchronize(PostRelease);
        end
        else
        { Otherwise write the TraceObject to the buffer }
        begin
          WriteToBuffer;
        end;
      end
      else
      begin
        FCriticalSection.Enter;
        try
          if FMsgs.Count = 0 then
          FMsgAvailable.ResetEvent
        finally
          FCriticalSection.Leave
        end;
      end;
  end;
 except on E: Exception do
   begin
     //writeln('Write Thread raised Exception: ' + E.Message);
     raise
   end
  end;
  //writeln('Write Thread Ends');
end;

procedure TWriterThread.WriteSQLData(Msg : String; DataType: TTraceFlag);
begin
  FCriticalSection.Enter;
  try
    FMsgs.Add(TTraceObject.Create(Msg, DataType));
  finally
    FCriticalSection.Leave;
  end;
  FMsgAvailable.SetEvent
end;

procedure TWriterThread.BeginWrite;
begin
//writeln('Begin Write');
  with FGlobalInterface do
  begin
    ReadReadyEvent.PassThroughGate;    {Wait for readers to become ready }
    WriterBusyEvent.Lock;     {Set Busy State}
  end;
//writeln('Begin Write Complete');
end;

procedure TWriterThread.EndWrite;
begin
  //writeln('End Write');
  with FGlobalInterface do
  begin
    DataAvailableEvent.Unlock;   { Signal Data Available. }
    ReadFinishedEvent.PassThroughGate; {Wait for readers to finish }
    DataAvailableEvent.Lock;  {reset Data Available }
    WriterBusyEvent.Unlock;      {Signal not Busy }
  end;
  //writeln('End Write Complete');
  end;

procedure TWriterThread.WriteToBuffer;
var I, len: integer;
    Temp: TTraceObject;
begin
  //writeln('Write to Buffer');
  FGlobalInterface.WriteLock.Lock;
  try
    { If there are no monitors throw out the message
      The alternative is to have messages queue up until a
      monitor is ready.}

    if FGlobalInterface.MonitorCount = 0 then
      RemoveFromList
    else
    begin
      i := 1;
      len := Length(TTraceObject(FMsgs[0]).FMsg);
      if len <= FGlobalInterface.MaxBufferSize then
      begin
        BeginWrite;
        try
          FGlobalInterface.SendTrace(TTraceObject(FMsgs[0]))
        finally
          RemoveFromList;
          EndWrite
        end;
      end
      else
      try
        while len > 0 do
        begin
          //writeln('Sending Partial Message, len = ',len);
          Temp := TTraceObject.Create(TTraceObject(FMsgs[0]),i,Min(len,FGlobalInterface.MaxBufferSize));
          try
            BeginWrite;
            FGlobalInterface.SendTrace(Temp);
            Inc(i,FGlobalInterface.MaxBufferSize);
            Dec(len,FGlobalInterface.MaxBufferSize);
          finally
            Temp.Free;
            EndWrite
          end
        end;
      finally
        RemoveFromList;
      end
    end;
  finally
    FGlobalInterface.WriteLock.Unlock;
  end;
  //writeln('Done Write');
end;

procedure TWriterThread.RemoveFromList;
begin
  //writeln('Write Thread: Remove object From List');
  FCriticalSection.Enter;
  try
    FMsgs.Remove(FMsgs[0]); { Pop the written item }
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TWriterThread.PostRelease;
var Monitor: TIBCustomSQLMonitor;
begin
  Monitor := TReleaseObject(FMsgs.Items[0]).FMonitor;
  Monitor.ReleaseObject
end;

procedure TWriterThread.ReleaseMonitor(Arg : TIBCustomSQLMonitor);
begin
  FMsgs.Add(TReleaseObject.Create(Arg));
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

constructor TReleaseObject.Create(Monitor : TIBCustomSQLMonitor);
begin
  FMonitor := Monitor;
end;

{ ReaderThread }

procedure TReaderThread.AddMonitor(Arg: TIBCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    if FMonitors.IndexOf(Arg) < 0 then
      FMonitors.Add(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

procedure TReaderThread.AlertMonitors;
var i : Integer;
    FTemp : TTraceObject;
    Monitor: TIBCustomSQLMonitor;
begin
  for i := 0 to FMonitors.Count - 1 do
  begin
     //writeln('Sending Message to Monitor ' +IntToStr(i));
     FTemp := TTraceObject.Create(st);
     Monitor := TIBCustomSQLMonitor(FMonitors[i]);
     Monitor.ReceiveMessage(FTemp);
  end;
end;

procedure TReaderThread.BeginRead;
begin
//writeln('Begin Read');
  with FGlobalInterface do
  begin
    WriterBusyEvent.PassthroughGate;     { Wait for Writer not busy}
    ReadFinishedEvent.Lock;          { Prepare Read Finished Gate}
    ReadReadyEvent.Unlock;                { Signal read ready  }
    //writeln('Read Ready Unlocked');
    DataAvailableEvent.PassthroughGate;  { Wait for a Data Available }
  end;
//writeln('Begin Read Complete');
end;

constructor TReaderThread.Create(GlobalInterface: TGlobalInterface);
begin
  inherited Create(true);
  FGlobalInterface := GlobalInterface;
  st := TTraceObject.Create('', tfMisc);
  FGlobalInterface.IncMonitorCount;
  FMonitors := TObjectList.Create(false);
  FCriticalSection := TCriticalSection.Create;
  //writeln('Reader Thread Created');
  FGlobalInterface.ReadReadyEvent.Lock;           { Initialise Read Ready}
  Resume;
end;

destructor TReaderThread.Destroy;
begin
//writeln('Reader Thread Destory');
  FGlobalInterface.ReadReadyEvent.UnLock;
  if assigned(FGlobalInterface) and (FGlobalInterface.MonitorCount > 0) then
     FGlobalInterface.DecMonitorCount;
  FMonitors.Free;
  if assigned(FCriticalSection) then FCriticalSection.Free;
  st.Free;
  inherited Destroy;
end;

procedure TReaderThread.EndRead;
begin
//writeln('End Read');
  FGlobalInterface.ReadReadyEvent.Lock;           { reset Read Ready}
  FGlobalInterface.ReadFinishedEvent.Unlock; {Signal Read completed }
  //writeln('End Read Complete');
end;

procedure TReaderThread.Execute;
begin
//writeln('Read Thread Starts');
  { Place thread code here }
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData;
    if (st.FMsg <> '') and
       not ((st.FMsg = ' ') and (st.FDataType = tfMisc)) then
    begin
      //writeln('Sending Message to Monitors');
      Synchronize(AlertMonitors);
    end;
  end;
  //writeln('Read Thread Ends');
end;

procedure TReaderThread.ReadSQLData;
begin
  st.FMsg := '';
  BeginRead;
  if not bDone then
  try
    FGlobalInterface.ReceiveTrace(st)
  finally
    EndRead;
  end;
end;

procedure TReaderThread.RemoveMonitor(Arg: TIBCustomSQLMonitor);
begin
  FCriticalSection.Enter;
  try
    FMonitors.Remove(Arg);
  finally
    FCriticalSection.Leave
  end;
end;

{ Misc methods }

function MonitorHook: IIBSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
  begin
    CS.Enter;
    if (_MonitorHook = nil) and (not bDone) then
    begin
      _MonitorHook := TIBSQLMonitorHook.Create;
      _MonitorHook._AddRef;
    end;
    CS.Leave;
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
//writeln('Closed Threads Called');
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
  CS := TCriticalSection.Create;
  _MonitorHook := nil;
  FWriterThread := nil;
  FReaderThread := nil;
  bDone := False;
{$IFDEF USE_SV5_IPC}
  if FpGetEnv('FBSQL_IPCFILENAME') <> nil then
    IPCFileName := strpas(FpGetEnv('FBSQL_IPCFILENAME'))
  else
    IPCFileName := '/tmp/' + IPCFileName + '.' + strpas(FpGetEnv('USER'));
{$ENDIF}

finalization
  //writeln('Entered Finalisation');
  try
    { Write an empty string to force the reader to unlock during termination }
    bDone := True;
    if Assigned(_MonitorHook) then
      _MonitorHook.ForceRelease;
    CloseThreads;
    if Assigned(_MonitorHook) then
      _MonitorHook._Release;

  finally
    _MonitorHook := nil;
    if assigned(CS) then CS.Free;
  end;
end.
