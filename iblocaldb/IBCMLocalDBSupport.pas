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
  Classes, SysUtils, IBXCustomIBLocalDBSupport, ibxscript;

type
  TOnLogMessage = procedure(Sender: TObject; Msg: string) of object;

  { TIBCMLocalDBSupport }

  TIBCMLocalDBSupport = class(TCustomIBLocalDBSupport)
  private
    FOnLogMessage: TOnLogMessage;
    FDatabasePath: string;
    procedure Add2Log(Sender: TObject; Msg: string);
    procedure DoUpgrade(IBXScript: TIBXScript; TargetVersionNo: integer);
    procedure WriteLog(Msg: string);
    procedure HandleCreateDatabase(Sender: TObject; var DatabaseFileName: string);
  protected
    function InternalCreateNewDatabase(DBName:string; DBParams: TStrings; DBArchive: string): boolean; override;
    function RestoreDatabaseFromArchive(DBName:string; DBParams: TStrings; aFilename: string): boolean; override;
    function RunUpgradeDatabase(TargetVersionNo: integer): boolean; override;
    function SaveDatabaseToArchive(DBName: string; DBParams:TStrings; aFilename: string): boolean; override;
  public
    property OnLogMessage: TOnLogMessage read FOnLogMessage write FOnLogMessage;
  end;

implementation

uses IBServices, IBXUpgradeConfFile, IBDatabase;

resourcestring
  sUpdateMsg =       'Applying Update from %s';
  sCreatingDatabase= 'Creating new Database';

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
       SaveDatabase(DBArchive);
      end;
      Add2Log(self,UpgradeInfo.UserMessage);
      Add2Log(self,Format(sUpdateMsg,[UpgradeInfo.UpdateSQLFile]));
      if not IBXScript.PerformUpdate(UpgradeInfo.UpdateSQLFile,true) then
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

procedure TIBCMLocalDBSupport.HandleCreateDatabase(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := FDatabasePath;
end;

function TIBCMLocalDBSupport.InternalCreateNewDatabase(DBName: string;
  DBParams: TStrings; DBArchive: string): boolean;
var Service: TIBRestoreService;
    Ext: string;
begin
  Result := true;
  CreateDir(ExtractFileDir(DBName));
  Ext := AnsiUpperCase(ExtractFileExt(DBArchive));
  if Ext = '.GBK' then
  begin
    Service := TIBRestoreService.Create(self);
    with Service do
    try
     SetDBParams(DBParams);
     LoginPrompt := false;
     BackupFile.Clear;
     DatabaseName.Clear;
     Options := [CreateNewDB];
     BackupFile.Add(DBArchive);
     DatabaseName.Add(DBName);
     Active := true;
     WriteLog(sCreatingDatabase);
     ServiceStart;
     try
       while not Eof do
         WriteLog(Trim(GetNextLine));
     finally
       Active := false
     end;
    finally
      Free
    end;
  end
  else
  if Ext = '.SQL' then
  with TIBXScript.Create(self) do
  try
    Database := self.Database;
    FDatabasePath := DBName;
    OnCreateDatabase := @HandleCreateDatabase;
    WriteLog(sCreatingDatabase);
    Result := RunScript(DBArchive);
  finally
    Free
  end
  else
    raise Exception.CreateFmt('Archive file (%s) has an unknown extension',[DBArchive]);
end;

function TIBCMLocalDBSupport.RestoreDatabaseFromArchive(DBName: string;
  DBParams: TStrings; aFilename: string): boolean;
var Service: TIBRestoreService;
begin
  Result := true;
  Service := TIBRestoreService.Create(self);
  with Service do
  try
    SetDBParams(DBParams);
    LoginPrompt := false;
    BackupFile.Clear;
    DatabaseName.Clear;
    Options := [replace];
    BackupFile.Add(aFilename);
    DatabaseName.Add(DBName);
    Active := true;
    ServiceStart;
    try
      while not Eof do
        WriteLog(Trim(GetNextLine));
    finally
      Active := false
    end;
  finally
    Free
  end;
end;

function TIBCMLocalDBSupport.RunUpgradeDatabase(TargetVersionNo: integer
  ): boolean;
var IBXScript: TIBXScript;
    IBTransaction: TIBTransaction;
begin
  Result := true;
  IBXScript := TIBXScript.Create(self);
  IBTransaction := TIBTransaction.Create(self);
  try
    IBXScript.Database := Database;
    IBXScript.Transaction := IBTransaction;
    IBXScript.OnErrorLog := @Add2Log;
    IBXScript.OnOutputLog := @Add2Log;
    IBTransaction.DefaultDatabase := Database;
    IBTransaction.Params.Clear;
    IBTransaction.Params.Add('concurrency');
    IBTransaction.Params.Add('wait');
    if assigned(UpgradeConf) then
      IBXScript.GetParamValue := @UpgradeConf.GetParamValue;
    DoUpgrade(IBXScript, TargetVersionNo);
  finally
    IBXScript.Free;
    IBTransaction.Free;
  end;

end;

function TIBCMLocalDBSupport.SaveDatabaseToArchive(DBName: string;
  DBParams: TStrings; aFilename: string): boolean;
var Service: TIBBackupService;
begin
  Result := true;
  Service := TIBBackupService.Create(self);
  with Service do
  try
   SetDBParams(DBParams);
   LoginPrompt := false;
   BackupFile.Clear;
   DatabaseName := DBName;
   BackupFile.Add(aFilename);
   Active := true;
   ServiceStart;
   try
     while not Eof do
       WriteLog(Trim(GetNextLine));
   finally
     Active := false
   end;
  finally
    Free
  end;
end;

end.

