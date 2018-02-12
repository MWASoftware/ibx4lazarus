unit BringOnlineDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBServices;

type

  { TBringOnlineDlg }

  TBringOnlineDlg = class(TForm)
    Bevel1: TBevel;
    CloseBtn: TButton;
    IBConfigService: TIBConfigService;
    ProgressBar1: TProgressBar;
    StatusMsg: TLabel;
    WaitTimer: TTimer;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure WaitTimerTimer(Sender: TObject);
  private
    FAbort: boolean;
  public
    function ShowModal: TModalResult;
  end;

var
  BringOnlineDlg: TBringOnlineDlg;

implementation

{$R *.lfm}

uses MainFormUnit;

resourcestring
  sWaitStatusMsg = 'Waiting for %s to come online';

{ TBringOnlineDlg }

procedure TBringOnlineDlg.WaitTimerTimer(Sender: TObject);
begin
  if MainForm.IsDatabaseOnline then
  begin
    WaitTimer.Enabled := false;
    MessageDlg('Database is back online',mtInformation,[mbOK],0);
    ModalResult := mrOK;
  end;
end;

function TBringOnlineDlg.ShowModal: TModalResult;
begin
  IBConfigService.Active := true;
  FAbort := false;
  StatusMsg.Caption := Format(sWaitStatusMsg,[IBConfigService.DatabaseName]);
  try
    IBConfigService.BringDatabaseOnline;
  except
    while IBConfigService.IsServiceRunning do;
    raise;
  end;
  WaitTimer.Enabled := true;
  Result := inherited ShowModal;
end;

procedure TBringOnlineDlg.CloseBtnClick(Sender: TObject);
begin
  FAbort := true;
end;

procedure TBringOnlineDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  while IBConfigService.IsServiceRunning do;
  IBConfigService.Active := false;;
end;

end.

