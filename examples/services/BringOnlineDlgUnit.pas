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
    procedure FormShow(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);
  private
    FAbort: boolean;
  public

  end;

var
  BringOnlineDlg: TBringOnlineDlg;

implementation

{$R *.lfm}

uses MainFormUnit;

resourcestring
  sWaitStatusMsg = 'Waiting for %s to come online';

{ TBringOnlineDlg }

procedure TBringOnlineDlg.FormShow(Sender: TObject);
begin
  IBConfigService.Active := true;
  Cursor := crHourGlass;
  FAbort := false;
  StatusMsg.Caption := Format(sWaitStatusMsg,[IBConfigService.DatabaseName]);
  try
    IBConfigService.BringDatabaseOnline;
    while IBConfigService.IsServiceRunning do;
    IBConfigService.Active := false;
    WaitTimer.Enabled := true;
  except On E: Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TBringOnlineDlg.WaitTimerTimer(Sender: TObject);
begin
  if MainForm.IsDatabaseOnline then
  begin
    WaitTimer.Enabled := false;
    MessageDlg('Database is back online',mtInformation,[mbOK],0);
    ModalResult := mrOK;
  end;
end;

procedure TBringOnlineDlg.CloseBtnClick(Sender: TObject);
begin
  FAbort := true;
end;

end.

