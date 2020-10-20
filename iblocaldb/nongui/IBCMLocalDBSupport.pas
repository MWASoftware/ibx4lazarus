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
unit IBCMLocalDBSupport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBXCustomIBLocalDBSupport, ibxscript, IBXServices;

type
  TOnLogMessage = procedure(Sender: TObject; Msg: string) of object;

  { TIBCMLocalDBSupport }

  TIBCMLocalDBSupport = class(TCustomIBLocalDBSupport)
  private
    FOnLogMessage: TOnLogMessage;
    FOnProgressEvent: TOnProgressEvent;
    procedure Add2Log(Sender: TObject; Msg: string);
    procedure DoUpgrade(IBXScript: TIBXScript; TargetVersionNo: integer);
    procedure WriteLog(Msg: string);
    procedure HandleOnGetNextLine(Sender: TObject; var Line: string);
    procedure IBXScriptCreateDatabase(Sender: TObject;
      var DatabaseFileName: string);
  protected
    procedure Downgrade(DBArchive: string); override;
    function InternalCreateNewDatabase(DBArchive: string): boolean; override;
    function RestoreDatabaseFromArchive(aFilename: string): boolean; override;
    function RunUpgradeDatabase(TargetVersionNo: integer): boolean; override;
    function SaveDatabaseToArchive( aFilename: string): boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    property OnLogMessage: TOnLogMessage read FOnLogMessage write FOnLogMessage;
    property OnProgressEvent: TOnProgressEvent read FOnProgressEvent write FOnProgressEvent; {Progress Bar Support}
  end;

implementation

uses IBXUpgradeConfFile, IBDatabase;

resourcestring
  sUpdateMsg         = 'Applying Update from %s';
  sCreatingDatabase  = 'Creating new Database';
  sBackupDone        = 'Database Archived to %s';

{ TIBCMLocalDBSupport }

procedure TIBCMLocalDBSupport.Add2Log(Sender: TObject; Msg: string);
begin
  WriteLog(Msg);
end;

procedure TIBCMLocalDBSupport.DoUpgrade(IBXScript: TIBXScript;
  TargetVersionNo: integer);
var UpdateAvailable: boolean;
    UpgradeInfo: TUpgradeInfo;
    DBArchive: string;
    LastVersionNo: integer;
begin
  repeat
    if CurrentDBVersionNo >= TargetVersionNo then break;
    LastVersionNo := CurrentDBVersionNo;
    UpdateAvailable := UpgradeConf.GetUpgradeInfo(CurrentDBVersionNo+1,UpgradeInfo);
    if UpdateAvailable then
    begin
      if UpgradeInfo.BackupDB then
      begin
       DBArchive := ChangeFileExt(ActiveDatabasePathName,'');
       DBArchive := DBArchive + '.' + IntToStr(CurrentDBVersionNo) + '.gbk';
       SaveDatabaseToArchive(DBArchive);
      end;
      Add2Log(self,UpgradeInfo.UserMessage);
      Add2Log(self,Format(sUpdateMsg,[UpgradeInfo.UpdateSQLFile]));
      if not IBXScript.RunScript(UpgradeInfo.UpdateSQLFile) then
       break;
      UpdateVersionNo;
    end;
  until not UpdateAvailable or (LastVersionNo = CurrentDBVersionNo);
end;

procedure TIBCMLocalDBSupport.WriteLog(Msg: string);
begin
  if assigned(OnLogMessage) then
    OnLogMessage(self,Msg);
end;

procedure TIBCMLocalDBSupport.HandleOnGetNextLine(Sender: TObject;
  var Line: string);
begin
  if assigned(OnLogMessage) then
    OnLogMessage(self,Line);
end;

procedure TIBCMLocalDBSupport.IBXScriptCreateDatabase(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := (Sender as TIBXScript).Database.Attachment.GetConnectString;
  (Sender as TIBXScript).Database.DropDatabase;
end;

procedure TIBCMLocalDBSupport.Downgrade(DBArchive: string);
begin
  RestoreDatabase(DBArchive);
end;

function TIBCMLocalDBSupport.InternalCreateNewDatabase(DBArchive: string
  ): boolean;
begin
  Result := true;
  if IsGbakFile(DBArchive) then
  begin
    with RestoreService do
    begin
      BackupFiles.Clear;
      BackupFiles.Add(DBArchive);
      Options := [Replace];
      WriteLog(sCreatingDatabase);
      Database.Attachment.Disconnect;
      try
        Execute(nil);
      finally
        Database.Attachment.Connect;
      end;
    end;
  end
  else
  with TIBXScript.Create(self) do
  try
    Database := self.Database;
    OnCreateDatabase := @IBXScriptCreateDatabase;
    OnProgressEvent := FOnProgressEvent;
    WriteLog(sCreatingDatabase);
    Result := RunScript(DBArchive);
    Add2Log(self,''); {ensure EOL sent}
  finally
    Free
  end
end;

function TIBCMLocalDBSupport.RestoreDatabaseFromArchive(
  aFilename: string): boolean;
begin
  Result := true;
  with RestoreService do
  begin
    BackupFiles.Clear;
    BackupFiles.Add(aFilename);
    Options := [Replace];
    Execute(nil);
  end;
end;

function TIBCMLocalDBSupport.RunUpgradeDatabase(TargetVersionNo: integer
  ): boolean;
var IBXScript: TIBXScript;
begin
  Result := true;
  IBXScript := TIBXScript.Create(self);
  try
    IBXScript.Database := Database;
    IBXScript.OnErrorLog := @Add2Log;
    IBXScript.OnOutputLog := @Add2Log;
    if assigned(UpgradeConf) then
      IBXScript.GetParamValue := @UpgradeConf.GetParamValue;
    DoUpgrade(IBXScript, TargetVersionNo);
  finally
    IBXScript.Free;
  end;

end;

function TIBCMLocalDBSupport.SaveDatabaseToArchive(aFilename: string): boolean;
begin
  Result := true;
  with BackupService do
  begin
    Execute(nil);
    WriteLog(Format(sBackupDone,[aFileName]));
  end;
end;

constructor TIBCMLocalDBSupport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  RestoreService.OnGetNextLine := @HandleOnGetNextLine;
  BackupService.OnGetNextLine := @HandleOnGetNextLine;
end;

end.

