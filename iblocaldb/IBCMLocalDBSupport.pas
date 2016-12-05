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
    procedure Add2Log(Sender: TObject; Msg: string);
    procedure DoUpgrade(IBXScript: TIBXScript; TargetVersionNo: integer);
    procedure WriteLog(Msg: string);
  protected
    function CreateNewDatabase(DBName:string; DBParams: TStrings; DBArchive: string): boolean; override;
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

function TIBCMLocalDBSupport.CreateNewDatabase(DBName: string;
  DBParams: TStrings; DBArchive: string): boolean;
var Service: TIBRestoreService;
begin
  CreateDir(ExtractFileDir(DBName));
  Service := TIBRestoreService.Create(self);
  with Service do
  try
   SetDBParams(Service,DBParams);
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

end;

function TIBCMLocalDBSupport.RestoreDatabaseFromArchive(DBName: string;
  DBParams: TStrings; aFilename: string): boolean;
var Service: TIBRestoreService;
begin
  Service := TIBRestoreService.Create(self);
  with Service do
  try
    SetDBParams(Service,DBParams);
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
    IBXScript.GetParamValue := @HandleGetParamValue;
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
  Service := TIBBackupService.Create(self);
  with Service do
  try
   SetDBParams(Service,DBParams);
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

