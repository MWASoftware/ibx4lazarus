unit ShutdownDatabaseDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBServices, IB;

type

  { TShutdownDatabaseDlg }

  TShutdownDatabaseDlg = class(TForm)
    Bevel1: TBevel;
    CloseBtn: TButton;
    IBConfigService: TIBConfigService;
    ProgressBar1: TProgressBar;
    StatusMsg: TLabel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FShutDownmode: TShutdownMode;
    FDelay: integer;
    FAborting: boolean;
    FSecContextError: boolean;
    FShutdownWaitThread: TThread;
    procedure OnWaitCompleted(Sender: TObject);
  public
     procedure Shutdown(aService: TIBConfigService; aShutDownmode: TShutdownMode;
       aDelay: integer);
     property Aborting: boolean read FAborting;
  end;

var
  ShutdownDatabaseDlg: TShutdownDatabaseDlg;

implementation

{$R *.lfm}

uses IBErrorCodes;

resourcestring
  sWaitStatusMsg = 'Waiting for %s to shutdown';
  sDatabaseShutdown  = 'Database has been successfully shutdown';
  sOnCompleted = 'Shutdown of %s completed with response: %s';

type
    { TShutdownWaitThread }

  TShutdownWaitThread = class(TThread)
  private
    FErrorMessage: string;
    FIBConfigService: TIBConfigService;
    FOptions: TShutdownMode;
    FSecContextError: boolean;
    FSuccess: boolean;
    FWait: integer;
    FOnCompleted: TNotifyEvent;
    procedure DoCallback;
  protected
    procedure Execute; override;
  public
    constructor Create(aService: TIBConfigService; Options: TShutdownMode;
      Wait: Integer; OnCompleted: TNotifyEvent);
    destructor Destroy; override;
    procedure Abort;
    property Success: boolean read FSuccess;
    property SecContextError: boolean read FSecContextError;
    property ErrorMessage: string read FErrorMessage;
  end;



{ TShutdownWaitThread }

procedure TShutdownWaitThread.DoCallback;
begin
  if assigned(FOnCompleted) then
    FOnCompleted(self);
end;

procedure TShutdownWaitThread.Execute;
begin
  FSuccess := false;
  FIBConfigService.Active := true;
  try
    try
      FIBConfigService.ShutDownDatabase(FOptions,FWait);
      FErrorMessage := 'Completed without error';
      FSuccess := true;
    except
      on E: EIBInterBaseError do
        if E.IBErrorCode = isc_sec_context then
          FSecContextError := true
        else
          FErrorMessage := E.Message;

      on E: Exception do
        FErrorMessage := E.Message;
    end;
  finally
    if not FSecContextError then
      while FIBConfigService.IsServiceRunning do;
    if Terminated and FSuccess then
      FIBConfigService.BringDatabaseOnline;
    FIBConfigService.Active := false;
  end;
  Synchronize(@DoCallback);
end;

constructor TShutdownWaitThread.Create(aService: TIBConfigService;
  Options: TShutdownMode; Wait: Integer; OnCompleted: TNotifyEvent);
var Password: string;
begin
  inherited Create(false);
  FOptions := Options;
  FWait := Wait;
  FOnCompleted := OnCompleted;
  FreeOnTerminate := true;
  FIBConfigService := TIBConfigService.Create(nil);
  FIBConfigService.Assign(aService);
  FIBConfigService.DatabaseName := aService.DatabaseNAme;
end;

destructor TShutdownWaitThread.Destroy;
begin
  if FIBConfigService <> nil then FIBConfigService.Free;
  inherited Destroy;
end;

procedure TShutdownWaitThread.Abort;
begin
  Terminate;
end;

{ TShutdownDatabaseDlg }

procedure TShutdownDatabaseDlg.FormShow(Sender: TObject);
begin
  FAborting := false;
  StatusMsg.Caption := Format(sWaitStatusMsg,[IBConfigService.DatabaseName]);
  FShutdownWaitThread := TShutdownWaitThread.Create(IBConfigService,FShutDownMode,FDelay,@OnWaitCompleted);
end;

procedure TShutdownDatabaseDlg.CloseBtnClick(Sender: TObject);
begin
  FAborting := true;
  FShutdownWaitThread.Terminate;
  Close;
end;

procedure TShutdownDatabaseDlg.OnWaitCompleted(Sender: TObject);
begin
  with TShutdownWaitThread(Sender) do
    if not Success and SecContextError then
      self.FSecContextError := true
    else
    if not FAborting then
      MessageDlg(Format(sOnCompleted,[IBConfigService.DatabaseName,ErrorMessage]),
               mtInformation,[mbOK],0);
  FAborting := false;
  Close;
end;

procedure TShutdownDatabaseDlg.Shutdown(aService: TIBConfigService;
  aShutDownmode: TShutdownMode; aDelay: integer);
begin
  IBConfigService.Assign(aService);
  IBConfigService.DatabaseName := aService.DatabaseName;
  FShutDownmode := aShutDownmode;
  FDelay := aDelay;
  FSecContextError := false;
  if aDelay <= 0 then
  begin
    IBConfigService.Active := true;
    try
      IBConfigService.ShutDownDatabase(aShutDownmode,0);
      while IBConfigService.IsServiceRunning do;
      if aDelay = 0 then
        MessageDlg(sDatabaseShutdown,mtInformation,[mbOK],0);
    finally
      IBConfigService.Active := false;
    end
  end
  else
  begin
    ShowModal;
    if FSecContextError  then
      raise EIBInterBaseError.Create(FirebirdAPI.getStatus); {re-raise the error}
  end;
end;

end.

