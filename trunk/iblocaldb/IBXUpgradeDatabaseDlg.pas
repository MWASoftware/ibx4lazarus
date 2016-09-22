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
 *  The Original Code is (C) 2014 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBXUpgradeDatabaseDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,  Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBDatabase, ibxscript;

type
  { TUpgradeDatabaseDlg }

  TUpgradeDatabaseDlg = class(TForm)
    UpdateTransaction: TIBTransaction;
    IBXScript: TIBXScript;
    Label1: TLabel;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    FDatabase: TIBDatabase;
    Status: TLabel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure HandleCompletionEvent(Sender: TObject);
    procedure IBXScriptLogProc(Sender: TObject; Msg: string);
    procedure IBXScriptProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
  private
    FOnDoUpgrade: TNotifyEvent;
    { private declarations }
    FUpgradeLog: TStrings;
    procedure DoUpdate(Data: PtrInt);
  public
    { public declarations }
    SuccessfulCompletion: boolean;
    constructor Create(theOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add2Log(Msg: string);
    property OnDoUpgrade: TNotifyEvent read FOnDoUpgrade write FOnDoUpgrade;
  end; 



var
  UpgradeDatabaseDlg: TUpgradeDatabaseDlg;


implementation

{$R *.lfm}

uses  IBXViewLogDig;

{ TUpgradeDatabaseDlg }

procedure TUpgradeDatabaseDlg.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Status.Caption := '';
  FUpgradeLog.Clear;
  Application.QueueAsyncCall(@DoUpdate,0);
end;

procedure TUpgradeDatabaseDlg.HandleCompletionEvent(Sender: TObject);
begin
  Timer1.Enabled := false;
  if not SuccessfulCompletion then
  begin
    ShowViewLogDlg(FUpgradeLog);
    ModalResult := mrCancel
  end
  else
    ModalResult := mrOK;
end;

procedure TUpgradeDatabaseDlg.IBXScriptLogProc(Sender: TObject; Msg: string);
begin
  Add2Log(Msg);
end;

procedure TUpgradeDatabaseDlg.IBXScriptProgressEvent(Sender: TObject;
  Reset: boolean; value: integer);
begin
  if Reset then
  begin
    with ProgressBar1 do
    begin
      Position := 0;
      Max := value;
    end;
  end;
  ProgressBar1.StepIt;
  Application.ProcessMessages;
end;

procedure TUpgradeDatabaseDlg.Add2Log(Msg: string);
begin
  FUpgradeLog.Add(Msg);
end;

procedure TUpgradeDatabaseDlg.DoUpdate(Data: PtrInt);

begin
  SuccessfulCompletion := true;
  try
    if assigned(OnDoUpgrade) then
      OnDoUpgrade(self);
  except on E:Exception do
   begin
    SuccessfulCompletion := false;
    Add2Log(E.Message);
   end;
  end;
  Timer1.Enabled := true;
end;


constructor TUpgradeDatabaseDlg.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  FUpgradeLog := TStringList.Create;
end;

destructor TUpgradeDatabaseDlg.Destroy;
begin
  if assigned(FUpgradeLog) then
    FUpgradeLog.Free;
  inherited Destroy;
end;

end.

