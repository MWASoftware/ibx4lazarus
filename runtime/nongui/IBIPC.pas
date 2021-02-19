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
{    Associates Ltd 2011 - 2018                                               }
{                                                                        }
{************************************************************************}

{Provides platform specific Interprocess Communication for use with IBX}

unit IBIPC;

{$Mode Delphi}
{$codepage UTF8}
{$interfaces COM}

{ $DEFINE DEBUG}

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

interface

uses
  Classes, SysUtils, syncobjs, IBInternals {$IFDEF WINDOWS}, Windows{$ENDIF};

const
  cMonitorHookSize = 4096;
  cMsgWaitTime = 1000;

type
  { There are two possible objects exchanged by OPC.  One is a trace message object.
    This object holds the flag of the trace type plus the message.
    The second object is a Release object.  It holds the handle that
    the CM_RELEASE message is to be queued to. }

  { TTraceObject }

  TTraceObject = Class(TObject)
    FDataType : TTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
    FMsgNumber: integer;
  public
    constructor Create(Msg : String; DataType : TTraceFlag; MsgNo: integer=0); overload;
    constructor Create(obj : TTraceObject); overload;
    constructor Create(obj : TTraceObject; MsgOffset, MsgLen: integer); overload;
  end;

  { TReleaseObject }

  TReleaseObject = Class(TObject)
    FMonitor : TObject;
  public
    constructor Create(Monitor : TObject);
  end;

  {
  A single lock gate is either open or closed. When open, any thread can pass
  through it while, when closed, all threads are blocked as they try to pass
  through the gate. When the gate is opened, all blocked threads are resumed.

  There is an implementation assumption that only one writer thread at
  a time (i.e. the thread which locks or unlocks the gate) can have access to
  it at any one time. I.e. an external Mutex prevents race conditions.
  }

  ISingleLockGate = interface
  ['{a123d688-cd01-4049-94aa-15d9457dbd01}']
  procedure PassthroughGate;
  procedure Unlock;
  procedure Lock;
  end;

  {
    A Multilock Gate is used where several reader threads must pass
    through the gate before it can be opened for a writer thread.

    The reader threads register their interest by each locking the gate.
    The writer thread then waits on the locked gate until all the reader
    threads have separately unlocked the gate.

    There is an underlying assumption of a single writer. A Mutex must
    be used to control access to the gate from the writer side if this
    assumption is invalid.
  }

  IMultiLockGate = interface
  ['{784a162f-1e53-4350-b8c4-ffe43ae7fb51}']
  procedure Lock;
  procedure Unlock;
  procedure PassthroughGate;
  function GetLockCount: integer;
  function GetOnGateTimeout: TNotifyEvent;
  procedure SetOnGateTimeout(aValue: TNotifyEvent);
  property LockCount: integer read GetLockCount;
  property OnGateTimeout: TNotifyEvent read GetOnGateTimeout write SetOnGateTimeout;
  end;

  IMutex = interface
  ['{a3badaf3-4840-4293-ba82-40890df7fae5}']
  procedure Lock;
  procedure Unlock;
  end;

  ISharedMemory = interface
  ['{db77bdd4-233a-4c9c-9212-dd7945e2e57c}']
  function Allocate(Size: integer): PByte;
  function GetLastAllocationSize: integer;
  property LastAllocationSize: integer read GetLastAllocationSize;
  end;

  IIPCInterface = interface
  ['{84760945-c014-454c-b69b-e4c5da88d9d6}']
  procedure IncMonitorCount;
  procedure DecMonitorCount;
  procedure SendTrace(TraceObject: TTraceObject);
  procedure ReceiveTrace(TraceObject: TTraceObject);
  function GetDataAvailableEvent: ISingleLockGate;
  function GetWriterBusyEvent: ISingleLockGate;
  function GetReadReadyEvent: IMultiLockGate;
  function GetReadFinishedEvent: IMultiLockGate;
  function GetWriteLock: IMutex;
  function GetMonitorCount: integer;
  function GetSharedMemory: ISharedMemory;
  function GetMaxBufferSize: integer;
  function GetSa: PSecurityAttributes;
  property DataAvailableEvent: ISingleLockGate read GetDataAvailableEvent;
  property WriterBusyEvent: ISingleLockGate read GetWriterBusyEvent;
  property ReadReadyEvent: IMultiLockGate read GetReadReadyEvent;
  property ReadFinishedEvent: IMultiLockGate read GetReadFinishedEvent;
  property WriteLock: IMutex read GetWriteLock;
  property MonitorCount: integer read GetMonitorCount;
  property SharedMemory: ISharedMemory read GetSharedMemory;
  property MaxBufferSize: integer read GetMaxBufferSize;
  property Sa : PSecurityAttributes read GetSa;
  end;

  function CreateIPCInterface: IIPCInterface;


implementation

   {$IFDEF USE_SV5_IPC}
   {$I sv5ipc.inc}
   {$ENDIF}

   {$IFDEF USE_WINDOWS_IPC}
   {$I winipc.inc}
   {$ENDIF}


function CreateIPCInterface: IIPCInterface;
begin
  Result := TIPCInterface.Create;
end;

{ TTraceObject }

constructor TTraceObject.Create(Msg: String; DataType: TTraceFlag;
  MsgNo: integer);
begin
  FMsg := Msg;
  FDataType := DataType;
  FTimeStamp := Now;
  FMsgNumber := MsgNo;
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

constructor TReleaseObject.Create(Monitor : TObject);
begin
  FMonitor := Monitor;
end;


end.

