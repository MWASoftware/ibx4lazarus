(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
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
 *  The Original Code is (C) 2015-2020 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBInternals;

{$mode objfpc}{$H+}

{Interfaces used internally and not normally made visible to users}

interface

uses
  Classes, SysUtils, DB;

type
  TTraceControlFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc, tfDisabled);
  TTraceFlag = tfQPrepare..tfMisc;
  TTraceFlags = set of TTraceFlag;

  { TIBXMonitoredComponent }

  TIBXMonitoredComponent = class(TComponent)
  private
    FTraceFlags: TTraceFlags;
  public
    constructor Create(aOwner: TComponent); override;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
  end;

  TIBMonitoredService = class(TIBXMonitoredComponent);
  TIBXMonitoredService = class(TIBXMonitoredComponent);

  { TIBXMonitoredConnection }

  TIBXMonitoredConnection = class(TCustomConnection)
  private
    FTraceFlags: TTraceFlags;
  public
    constructor Create(aOwner: TComponent); override;
    property TraceFlags: TTraceFlags read FTraceFlags write FTraceFlags;
  end;


  IIBTimerInf = interface
    function GetEnabled: boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);
    function GetOnStartTimer: TNotifyEvent;
    procedure SetOnStartTimer(Value: TNotifyEvent);
    function GetOnStopTimer: TNotifyEvent;
    procedure SetOnStopTimer(Value: TNotifyEvent);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property OnStartTimer: TNotifyEvent read GetOnStartTimer write SetOnStartTimer;
    property OnStopTimer: TNotifyEvent read GetOnStopTimer write SetOnStopTimer;
  end;

  IIBGUIInterface = interface
    function ServerLoginDialog(var AServerName: string;
                               var AUserName, APassword: string): Boolean;
    function LoginDialogEx(var ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean;
    procedure SetCursor;
    procedure RestoreCursor;
    function CreateTimer: IIBTimerInf;
  end;

const  IBGUIInterface : IIBGUIInterface = nil;

implementation

{ TIBXMonitoredComponent }

constructor TIBXMonitoredComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTraceFlags := [];
end;

{ TIBXMonitoredConnection }

constructor TIBXMonitoredConnection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTraceFlags := [];
end;

end.

