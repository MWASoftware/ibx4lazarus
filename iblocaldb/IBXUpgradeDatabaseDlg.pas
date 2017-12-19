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
  ComCtrls, ExtCtrls, IBDatabase, ibxscript, IBXUpgradeConfFile;

type
  TGetDatabaseVersionNo = procedure (Sender: TObject; var VersionNo: integer) of object;

  { TUpgradeDatabaseDlg }

  TUpgradeDatabaseDlg = class(TForm)
    UpdateTransaction: TIBTransaction;
    IBXScript: TIBXScript;
    Label1: TLabel;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Status: TLabel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure HandleCompletionEvent(Sender: TObject);
    procedure IBXScriptLogProc(Sender: TObject; Msg: string);
    procedure IBXScriptProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
  private
    FOnGetDatabaseVersionNo: TGetDatabaseVersionNo;
    FOnUpgradeStepCompleted: TNotifyEvent;
    { private declarations }
    FUpgradeLog: TStrings;
    FTargetVersionNo: integer;
    FUpgradeConf: TUpgradeConfFile;
    FArchiveStub: string;
    procedure DoUpdate(Data: PtrInt);
    function CurrentDBVersionNo: integer;
  public
    { public declarations }
    SuccessfulCompletion: boolean;
    constructor Create(theOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add2Log(Msg: string);
    property OnGetDatabaseVersionNo: TGetDatabaseVersionNo read FOnGetDatabaseVersionNo
             write FOnGetDatabaseVersionNo;
    property OnUpgradeStepCompleted: TNotifyEvent read FOnUpgradeStepCompleted write FOnUpgradeStepCompleted;
  end; 


var
  UpgradeDatabaseDlg: TUpgradeDatabaseDlg;

function RunUpgradeDatabase(aDatabase: TIBDatabase;
                            UpgradeConf: TUpgradeConfFile;
                            ArchiveStub: string;
                            TargetVersionNo: integer;
                            aOnGetDatabaseVersionNo: TGetDatabaseVersionNo;
                            aOnUpgradeStepCompleted: TNotifyEvent): boolean;


implementation

{$R *.lfm}

uses  IBXViewLogDig, IBXSaveDatabaseDlg;

resourcestring
  sNoUpgradeConf = 'An Upgrade Conf file must be provided';
  sUpdateMsg =       'Applying Update from %s';
  sUpdateStarted =   '%s Update Started';
  sUpdateEnded =     '%s Update Completed';
  sUpdateFailed    = 'Update Failed - %s';

function RunUpgradeDatabase(aDatabase: TIBDatabase;
  UpgradeConf: TUpgradeConfFile; ArchiveStub: string; TargetVersionNo: integer;
  aOnGetDatabaseVersionNo: TGetDatabaseVersionNo;
  aOnUpgradeStepCompleted: TNotifyEvent): boolean;
begin
  if not assigned(UpgradeConf) then
    raise Exception.Create(sNoUpgradeConf);

  with TUpgradeDatabaseDlg.Create(Application) do
  try
    FTargetVersionNo := TargetVersionNo;
    FUpgradeConf := UpgradeConf;
    FArchiveStub := ArchiveStub;
    IBXScript.Database := aDatabase;
    UpdateTransaction.DefaultDatabase := aDatabase;
    IBXScript.GetParamValue := @UpgradeConf.GetParamValue;
    OnGetDatabaseVersionNo := aOnGetDatabaseVersionNo;
    OnUpgradeStepCompleted := aOnUpgradeStepCompleted;
    Result := ShowModal = mrOK;
  finally
    Free
  end;
end;

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
var UpdateAvailable: boolean;
    UpgradeInfo: TUpgradeInfo;
    DBArchive: string;
    LastVersionNo: integer;
    CurVersionNo: integer;
begin
  SuccessfulCompletion := true;
  try
    CurVersionNo := CurrentDBVersionNo;
    repeat
      if CurVersionNo >= FTargetVersionNo then break;
      LastVersionNo := CurVersionNo;
      UpdateAvailable := FUpgradeConf.GetUpgradeInfo(CurVersionNo+1,UpgradeInfo);
      if UpdateAvailable then
      begin
        if UpgradeInfo.BackupDB then
        begin
          CreateDir(ExtractFileDir(FArchiveStub));
          DBArchive := FArchiveStub + '.' + IntToStr(CurrentDBVersionNo) + '.gbk';
          with IBXScript.Database do
            SaveDatabaseToArchive(DatabaseName,Params,DBArchive);
        end;
        Add2Log(UpgradeInfo.UserMessage);
        Status.Caption := UpgradeInfo.UserMessage;
        Application.ProcessMessages;
        Add2Log(Format(sUpdateMsg,[UpgradeInfo.UpdateSQLFile]));
        Add2Log(Format(sUpdateStarted,[DateTimeToStr(Now)]));
        if not IBXScript.PerformUpdate(UpgradeInfo.UpdateSQLFile,true) then
        begin
         Add2Log(Format(sUpdateFailed,[DateTimeToStr(Now)]));
         SuccessfulCompletion := false;
         UpdateTransaction.Rollback;
         break;
        end;
        UpdateTransaction.Commit;
        Add2Log(Format(sUpdateEnded,[DateTimeToStr(Now)]));
        if assigned(FOnUpgradeStepCompleted) then
          OnUpgradeStepCompleted(self);
      end;
      CurVersionNo := CurrentDBVersionNo;
    until not UpdateAvailable or (LastVersionNo = CurVersionNo);
  except on E:Exception do
   begin
    SuccessfulCompletion := false;
    Add2Log(E.Message);
   end;
  end;
  Timer1.Enabled := true;
end;

function TUpgradeDatabaseDlg.CurrentDBVersionNo: integer;
begin
  if assigned(FOnGetDatabaseVersionNo) then
    OnGetDatabaseVersionNo(self,Result)
  else
    Result := 0;
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

