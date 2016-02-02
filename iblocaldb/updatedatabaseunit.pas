unit UpdateDatabaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,  Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBDatabase, ibxscript,  IBHeader;

type

  TUpdateDatabaseDlg = class;

  { TDBUpdateThread }

  TDBUpdateThread = class(TThread)
  private
   FParamFile: string;
   FLogMsg: string;
   FVersionFound: integer;
   FVersionWanted: integer;
   FOnCompletion: TNotifyEvent;
   FOwner: TUpdateDatabaseDlg;
   procedure Add2Log(Sender: TObject; Msg: string);
   procedure HandleGetParamValue(Sender: TObject; ParamName: string; var BlobID: TISC_QUAD);
   procedure OutputLog;
   function RunUpdate(FileName: string): integer;
   procedure ReportCompletion;
  protected
    FProgressBar: TThreadProgressBar;
    FCurrentVersion: string;
    FPatchDir: string;
    function Check4Update(VersionNo: integer; var FileName, DynaUpdate,
      Message: string): boolean;
    function GetSourceFile(Name: string; var FileName: string;
                                 var Compressed: boolean): boolean;
    function DoUpdate: integer;
    function ParseFileInfo(Info:string; var FileName: string;
                                 var Compressed: boolean): boolean;
    procedure Execute; override;
  public
    constructor Create(aOwner: TUpdateDatabaseDlg;
                        PatchDir, ParamFile: string;
                        ProgressBar: TProgressBar;
                        VersionFound,VersionWanted: integer;
                        OnCompletion: TNotifyEvent);
    destructor Destroy; override;
    function SuccessfulCompletion: boolean;
    property ParamFile: string read FParamFile;
  end;

  { TUpdateDatabaseDlg }

  TUpdateDatabaseDlg = class(TForm)
    UpdateDatabase: TIBDatabase;
    UpdateTransaction: TIBTransaction;
    IBXScript: TIBXScript;
    Label1: TLabel;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    FDatabase: TIBDatabase;
    Status: TLabel;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HandleCompletionEvent(Sender: TObject);
    procedure IBXScriptGetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure IBXScriptLogProc(Sender: TObject; Msg: string);
  private
    { private declarations }
    FUpdater: TDBUpdateThread;
    FDBParams: TStrings;
    FParamFile: string;
    FPatchDir: string;
    FUpgradeLog: TStrings;
    FVersionFound: integer;
    FVersionWanted: integer;
    procedure DoUpdate(Data: PtrInt);
    procedure HandleCompletion(Sender: TObject);
    procedure HandleLogEvent(Sender:TObject; LogMessage: string);
  public
    { public declarations }
    constructor Create(theOwner: TComponent); override;
    destructor Destroy; override;
    function ShowModal(PatchDir, ParamFile: string; VersionFound,VersionWanted: integer): TModalResult;
  end; 

  function GetCurrentVersion(ParamFile: string): integer;

var
  UpdateDatabaseDlg: TUpdateDatabaseDlg;

function RunUpgradeDatabase(Database: TIBDatabase; DBParams: TStrings;
  PatchDir, ParamFile: string; VersionFound,VersionWanted: integer): boolean;

implementation

{$R *.lfm}

uses  ViewLogDlgUnit, DB, IBBlob, ZStream, Process, IniFiles;

resourcestring
  sNoInfo      = 'Database Version is %d - Unknown Version - No Information Found';
  sRanProgram  = 'Ran Dynamic Update Program "%s" - Exit Code = %d';
  sNotFound    = 'Dynamic Update Program "%s" not found';

const
  sSectionheader      = 'Version.%.3d';

function GetCurrentVersion(ParamFile: string): integer;
begin
  with TIniFile.Create(ParamFile) do
  try
    try
      Result := StrToInt(ReadString('Status','Current','0'))
    except
      Result := -1
    end
  finally
    Free
  end
end;


function RunUpgradeDatabase(Database: TIBDatabase; DBParams: TStrings;
  PatchDir, ParamFile: string; VersionFound, VersionWanted: integer): boolean;
begin
  with TUpdateDatabaseDlg.Create(Application) do
  try
    UpdateDatabase.DatabaseName := Database.DatabaseName;
    UpdateDatabase.Params.Assign(DBParams);
    Result := ShowModal(PatchDir,ParamFile,VersionFound,VersionWanted) = mrOK;
  finally
    Free
  end;
end;

{ TUpdateDatabaseDlg }

procedure TUpdateDatabaseDlg.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Status.Caption := '';
  FUpgradeLog.Clear;
  FUpdater := TDBUpdateThread.Create(self,FPatchDir,FParamFile,ProgressBar1,
                                FVersionFound,FVersionWanted,@HandleCompletion) ;
  Application.QueueAsyncCall(@DoUpdate,0);
end;

procedure TUpdateDatabaseDlg.HandleCompletionEvent(Sender: TObject);
begin
  Timer1.Enabled := false;
  if not FUpdater.SuccessfulCompletion then
  begin
    ShowViewLogDlg(FUpgradeLog);
    ModalResult := mrCancel
  end
  else
    ModalResult := mrOK;
end;

procedure TUpdateDatabaseDlg.IBXScriptGetParamValue(Sender: TObject;
  ParamName: string; var BlobID: TISC_QUAD);
begin
  {Warning: called by separate thread to main window thread}
  if assigned(FUpdater) then
    FUpdater.HandleGetParamValue(Sender,ParamName,BlobID);
end;

procedure TUpdateDatabaseDlg.IBXScriptLogProc(Sender: TObject; Msg: string);
begin
  {Warning: called by separate thread to main window thread}
  if assigned(FUpdater) then
    FUpdater.Add2Log(Sender,Msg);
end;

procedure TUpdateDatabaseDlg.DoUpdate(Data: PtrInt);
begin
  if assigned(FUpdater) then
    FUpdater.Start
end;

procedure TUpdateDatabaseDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if assigned(FUpdater) then
    FreeAndNil(FUpdater)
end;

procedure TUpdateDatabaseDlg.HandleCompletion(Sender: TObject);
begin
  Timer1.Enabled := true;
  UpdateDatabase.Connected := false;
end;

procedure TUpdateDatabaseDlg.HandleLogEvent(Sender: TObject; LogMessage: string
  );
begin
  Status.Caption := LogMessage;
  Application.ProcessMessages
end;

constructor TUpdateDatabaseDlg.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  FUpgradeLog := TStringList.Create;
  FDBParams := TStringList.Create;
end;

destructor TUpdateDatabaseDlg.Destroy;
begin
  if assigned(FUpgradeLog) then
    FUpgradeLog.Free;
  if assigned(FDBParams) then
    FDBParams.Free;
  inherited Destroy;
end;

function TUpdateDatabaseDlg.ShowModal(PatchDir, ParamFile: string;
  VersionFound, VersionWanted: integer): TModalResult;
begin
  FPatchDir := PatchDir;
  FVersionFound := VersionFound;
  FVersionWanted := VersionWanted;
  if not FileExists(ParamFile) then
    raise Exception.CreateFmt('Database need to be upgraded but Upgrade Parameter File %s - not found',[ParamFile]);
  FParamFile := ParamFile;
  Result := inherited ShowModal
end;

constructor TDBUpdateThread.Create(aOwner: TUpdateDatabaseDlg; PatchDir,
  ParamFile: string; ProgressBar: TProgressBar; VersionFound,
  VersionWanted: integer; OnCompletion: TNotifyEvent);
begin
  inherited Create(true);
  FOwner := aOwner;
  FProgressBar := TThreadProgressBar.Create(self,ProgressBar);
  FOnCompletion := OnCompletion;
  FPatchDir := PatchDir;
  FParamFile := ParamFile;
  FVersionFound := VersionFound;
  FVersionWanted := VersionWanted;
end;

procedure TDBUpdateThread.Execute;
begin
 try
    ReturnValue := DoUpdate
  except on E:Exception do
    begin
      Add2Log(self,E.Message);
      ReturnValue := 1
    end
  end;
  Synchronize(@ReportCompletion)
end;

procedure TDBUpdateThread.HandleGetParamValue(Sender: TObject;
  ParamName: string; var BlobID: TISC_QUAD);
var Blob: TIBBlobStream;
    Source: TStream;
    FileName: string;
    Compressed: boolean;
    Z: Tcustomzlibstream;
begin
  Blob := TIBBlobStream.Create;
  try
    Blob.Database := (Sender as TIBXScript).Database;
    Blob.Mode := bmWrite;
    if not GetSourceFile(ParamName,FileName,Compressed) then Exit;
    Source := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
    try
      if Compressed then
      begin
        Z := Tcustomzlibstream.Create(Blob);
        try
          Z.CopyFrom(Source,0)
        finally
          Z.Free
        end;
      end
      else
        Blob.CopyFrom(Source,0)
    finally
      Source.Free
    end;
    Blob.Finalize;
    BlobID := Blob.BlobID
  finally
    Blob.Free
  end
end;

function TDBUpdateThread.RunUpdate(FileName: string): integer;
var
    Process: TProcess;
begin
  if not FileExists(FileName) then
  begin
    Add2Log(self,Format(sNotFound,[FileName]));
    Result := 1;
    Exit
  end;

  Process := TProcess.Create(nil);
  try
    Process.CommandLine := Format('"%s" "%s" "%s" "%s" "%s"',[
                FileName, //Program Path
                FOwner.UpdateDatabase.DatabaseName,                //Database Path
                FOwner.UpdateDatabase.Params.Values['user_name'],  //Login as
                FOwner.UpdateDatabase.Params.Values['password']    //use Password
                ]);
    Process.Options := [poWaitOnExit];
    Process.Execute;
    Add2Log(self,Format(sRanProgram,[FileName,Process.ExitStatus]));
    Result := Process.ExitStatus
  finally
    Process.Free
  end

end;

destructor TDBUpdateThread.Destroy;
begin
  if FProgressBar <> nil then FProgressBar.Free;
  inherited;
end;

function TDBUpdateThread.SuccessfulCompletion: boolean;
begin
  Result := (WaitFor = 0) and (Returnvalue = 0)
end;

procedure TDBUpdateThread.Add2Log(Sender: TObject; Msg: string);
begin
  FLogMsg := Msg;
  {$IFDEF DEBUG}
  writeln('Log: ' + Msg);
  {$ENDIF}
  Synchronize(@OutputLog)
end;

procedure TDBUpdateThread.OutputLog;
begin
  if assigned(FOwner) then
    FOwner.FUpgradeLog.Add(FLogMsg);
end;

function TDBUpdateThread.ParseFileInfo(Info: string; var FileName: string;
  var Compressed: boolean): boolean;
var index: integer;
begin
  Result := info <> '';
  if Result then
  begin
    index := Pos(',',Info);
    if index = 0 then
      FileName := Info
    else
    begin
      FileName := copy(Info,1,Index-1);
      Compressed := copy(Info,Index+1,Length(Info)-Index) = 'compressed'
    end;
  end;
end;

procedure TDBUpdateThread.ReportCompletion;
begin
  if assigned(FOnCompletion) then FOnCompletion(self)
end;

function TDBUpdateThread.Check4Update(VersionNo: integer; var FileName, DynaUpdate,
  Message: string): boolean;
var IniFile: TIniFile;
begin
  Result := false;
  IniFile := TIniFile.Create(ParamFile);
  try
   FCurrentVersion := Format(sSectionheader,[VersionNo]);
   Message := IniFile.ReadString(FCurrentVersion,'Msg',
                                Format(sNoInfo,[VersionNo]));
   FileName := IniFile.ReadString(FCurrentVersion,'Upgrade','');
   DynaUpdate := IniFile.ReadString(FCurrentVersion,'Exe','');
   Result := (FileName <> '') or (DynaUpdate <> '');
  finally
   IniFile.Free
  end
end;

function TDBUpdateThread.DoUpdate: integer;
var UpdateAvailable: boolean;
    UpdateSQLFile,
    UserMessage: string;
    DynaUpdate: string;
begin
  Result := 0;
  repeat
    if FVersionFound >= FVersionWanted then break;

     UpdateAvailable := Check4Update(FVersionFound+1,UpdateSQLFile, DynaUpdate, UserMessage);
     Add2Log(self,UserMessage);
     if UpdateAvailable then
     begin
       if not FOwner.IBXScript.PerformUpdate(FPatchDir + DirectorySeparator + UpdateSQLFile,FProgressBar,true) then
         begin
           Result := 1;
           break
         end
         else
           Result := 0;

       try
         if DynaUpdate <> '' then
           ReturnValue := RunUpdate(FPatchDir + DirectorySeparator + DynaUpdate);
       except on E: Exception do
         begin
           Add2Log(self,E.Message);
           Result := 1;
           break
         end
       end;
       Inc(FVersionFound);
     end;
   until not UpdateAvailable;
end;

function TDBUpdateThread.GetSourceFile(Name: string; var FileName: string;
                                 var Compressed: boolean): boolean;
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FParamFile);
  try
   Result := ParseFileInfo(IniFile.ReadString(FCurrentVersion,Name,''),FileName,Compressed);
   FileName := FPatchDir + DirectorySeparator + FileName
  finally
   IniFile.Free
  end
end;

end.

