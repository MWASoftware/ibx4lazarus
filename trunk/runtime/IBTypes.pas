unit IBTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIBGUIInterface = interface
    function ServerLoginDialog(var AServerName: string;
                               var AUserName, APassword: string): Boolean;
    function LoginDialogEx(var ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean;
    procedure SetCursor;
    procedure RestoreCursor;
  end;

  TTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
     tfTransact, tfBlob, tfService, tfMisc);
  TTraceFlags = set of TTraceFlag;

const  IBGUIInterface : TIBGUIInterface = nil;

implementation

end.

