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
unit UpdateDatabaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,  Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBDatabase, ibxscript,  IBHeader, IBLocalDBSupport;

type

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
    procedure FormShow(Sender: TObject);
    procedure HandleCompletionEvent(Sender: TObject);
    procedure IBXScriptGetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure IBXScriptLogProc(Sender: TObject; Msg: string);
    procedure IBXScriptProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
  private
    { private declarations }
    FIBLocalSupport: TIBLocalDBSupport;
    FDBParams: TStrings;
    FParamFile: string;
    FPatchDir: string;
    FUpgradeLog: TStrings;
    FVersionFound: integer;
    FVersionWanted: integer;
    FSuccessfulCompletion: boolean;
    FCurrentVersion: string;
    procedure Add2Log(Sender: TObject; Msg: string);
    function Check4Update(VersionNo: integer; var FileName, DynaUpdate,
      Message: string; var BackupDB: boolean): boolean;
    procedure DoBackup;
    procedure DoUpdate(Data: PtrInt);
    function GetSourceFile(aName: string; var FileName: string;
                                 var Compressed: boolean): boolean;
    function ParseFileInfo(Info:string; var FileName: string;
                                 var Compressed: boolean): boolean;
    function RunUpdate(FileName: string): integer;
  public
    { public declarations }
    constructor Create(theOwner: TComponent); override;
    destructor Destroy; override;
    function ShowModal(PatchDir, ParamFile: string; VersionFound,VersionWanted: integer): TModalResult;
  end; 

  function GetCurrentVersion(ParamFile: string): integer;

var
  UpdateDatabaseDlg: TUpdateDatabaseDlg;

function RunUpgradeDatabase(Sender: TIBLocalDBSupport; Database: TIBDatabase; DBParams: TStrings;
  PatchDir, ParamFile: string; VersionFound,VersionWanted: integer): boolean;

implementation

{$R *.lfm}

uses  ViewLogDlgUnit, DB, IBBlob, ZStream, Process, IniFiles;

resourcestring
  sNoInfo      = 'Database Version is %d - Unknown Version - No Information Found';
  sRanProgram  = 'Ran Dynamic Update Program "%s" - Exit Code = %d';
  sNotFound    = 'Dynamic Update Program "%s" not found';
  sNoParamFile = 'Database needs to be upgraded but Upgrade Parameter File %s - not found';

const
  sSectionheader      = 'Version.%.3d';

function GetCurrentVersion(ParamFile: string): integer;
begin
  if not FileExists(ParamFile) then
    raise Exception.CreateFmt(sNoParamFile,[ParamFile]);
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


function RunUpgradeDatabase(Sender: TIBLocalDBSupport; Database: TIBDatabase;
  DBParams: TStrings; PatchDir, ParamFile: string; VersionFound,
  VersionWanted: integer): boolean;
begin
  with TUpdateDatabaseDlg.Create(Application) do
  try
    UpdateDatabase.DatabaseName := Database.DatabaseName;
    UpdateDatabase.Params.Assign(DBParams);
    FIBLocalSupport := Sender;
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
  Application.QueueAsyncCall(@DoUpdate,0);
end;

procedure TUpdateDatabaseDlg.HandleCompletionEvent(Sender: TObject);
begin
  Timer1.Enabled := false;
  if not FSuccessfulCompletion then
  begin
    ShowViewLogDlg(FUpgradeLog);
    ModalResult := mrCancel
  end
  else
    ModalResult := mrOK;
end;

procedure TUpdateDatabaseDlg.IBXScriptGetParamValue(Sender: TObject;
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

procedure TUpdateDatabaseDlg.IBXScriptLogProc(Sender: TObject; Msg: string);
begin
  Add2Log(Sender,Msg);
end;

procedure TUpdateDatabaseDlg.IBXScriptProgressEvent(Sender: TObject;
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

procedure TUpdateDatabaseDlg.Add2Log(Sender: TObject; Msg: string);
begin
  FUpgradeLog.Add(Msg);
end;

function TUpdateDatabaseDlg.Check4Update(VersionNo: integer; var FileName,
  DynaUpdate, Message: string; var BackupDB: boolean): boolean;
var IniFile: TIniFile;
begin
  Result := false;
  IniFile := TIniFile.Create(FParamFile);
  try
   FCurrentVersion := Format(sSectionheader,[VersionNo]);
   Message := IniFile.ReadString(FCurrentVersion,'Msg',
                                Format(sNoInfo,[VersionNo]));
   FileName := IniFile.ReadString(FCurrentVersion,'Upgrade','');
   DynaUpdate := IniFile.ReadString(FCurrentVersion,'Exe','');
   BackupDB := CompareText(IniFile.ReadString(FCurrentVersion,'BackupDatabase','no'),'yes') = 0;
   Result := (FileName <> '') or (DynaUpdate <> '');
  finally
   IniFile.Free
  end
end;

procedure TUpdateDatabaseDlg.DoBackup;
var DBArchive: string;
begin
  DBArchive := ChangeFileExt(FIBLocalSupport.ActiveDatabasePathName,'');
  DBArchive := DBArchive + '.' + IntToStr(FVersionFound) + '.gbk';
  FIBLocalSupport.SaveDatabase(DBArchive);
end;

procedure TUpdateDatabaseDlg.DoUpdate(Data: PtrInt);

 function GetMessage(msg1,msg2: string): string;
 begin
   if msg1 <> '' then
     Result := msg1
   else
     Result := msg2;
 end;

var UpdateAvailable: boolean;
    UpdateSQLFile,
    UserMessage: string;
    DynaUpdate: string;
    BackupDB: boolean;
begin
  FSuccessfulCompletion := true;
  try
   repeat
    if FVersionFound >= FVersionWanted then break;

     UpdateAvailable := Check4Update(FVersionFound+1,UpdateSQLFile, DynaUpdate, UserMessage, BackupDB);
     Add2Log(self,UserMessage);
     if UpdateAvailable then
     begin
       if BackupDB then
         DoBackup;
       Status.Caption := GetMessage(UserMessage,'Applying Update from ' + UpdateSQLFile);
       Application.ProcessMessages;
       if not IBXScript.PerformUpdate(FPatchDir + DirectorySeparator + UpdateSQLFile,true) then
       begin
         FSuccessfulCompletion := false;
         break;
       end;

       try
         if DynaUpdate <> '' then
         begin
           Status.Caption := GetMessage(UserMessage,'Running ' + DynaUpdate);
           Application.ProcessMessages;
           if  RunUpdate(FPatchDir + DirectorySeparator + DynaUpdate) <> 0 then
           begin
             FSuccessfulCompletion := false;
             break;
           end;
         end;
       except on E: Exception do
         begin
           Add2Log(self,E.Message);
           FSuccessfulCompletion := false;
           break
         end
       end;
       Inc(FVersionFound);
     end;
   until not UpdateAvailable;
  except on E:Exception do
   begin
    FSuccessfulCompletion := false;
    Add2Log(self,E.Message);
   end;
  end;
  Timer1.Enabled := true;
end;

function TUpdateDatabaseDlg.GetSourceFile(aName: string; var FileName: string;
  var Compressed: boolean): boolean;
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FParamFile);
  try
   Result := ParseFileInfo(IniFile.ReadString(FCurrentVersion,aName,''),FileName,Compressed);
   FileName := FPatchDir + DirectorySeparator + FileName
  finally
   IniFile.Free
  end
end;

function TUpdateDatabaseDlg.ParseFileInfo(Info: string; var FileName: string;
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

function TUpdateDatabaseDlg.RunUpdate(FileName: string): integer;
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
                UpdateDatabase.DatabaseName,                //Database Path
                UpdateDatabase.Params.Values['user_name'],  //Login as
                UpdateDatabase.Params.Values['password']    //use Password
                ]);
    Process.Options := [poWaitOnExit];
    Process.Execute;
    Add2Log(self,Format(sRanProgram,[FileName,Process.ExitStatus]));
    Result := Process.ExitStatus
  finally
    Process.Free
  end
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
  Result := mrCancel;
  FPatchDir := PatchDir;
  FVersionFound := VersionFound;
  FVersionWanted := VersionWanted;
  if not FileExists(ParamFile) then
    raise Exception.CreateFmt('Database needs to be upgraded but Upgrade Parameter File %s - not found',[ParamFile]);
  FParamFile := ParamFile;
  Result := inherited ShowModal
end;

end.

