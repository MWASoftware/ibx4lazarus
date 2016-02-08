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
unit IBLocalDBSupport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls,  Dialogs, CustomIBLocalDBSupport;

type

  { TIBLocalDBSupport }

  TIBLocalDBSupport = class(TCustomIBLocalDBSupport)
  private
    procedure DoDowngrade(Data: PtrInt);
    procedure HandleSave2Backup(Sender: TObject; DBArchive: string);
  protected
    function AllowInitialisation: boolean; override;
    function AllowRestore: boolean; override;
    function CreateNewDatabase(DBName:string; DBParams: TStrings; DBArchive: string): boolean; override;
    procedure Downgrade(DBArchive: string); override;
    function RestoreDatabaseFromArchive(DBName:string; DBParams: TStrings; aFilename: string): boolean; override;
    function RunUpgradeDatabase(DBParams: TStrings; PatchDir, ParamFile: string;
                              VersionFound,VersionWanted: integer): boolean; override;
    function SaveDatabaseToArchive(DBName: string; DBParams:TStrings; aFilename: string): boolean; override;
  published
    property Database;
    property DatabaseName;
    property Enabled;
    property EmptyDBArchive;
    property FirebirdDirectory;
    property Options;
    property PatchDirectory;
    property RequiredVersionNo;
    property UpgradeConfFile;
    property VendorName;
    property OnGetDatabaseName;
    property OnGetDBVersionNo;
    property OnNewDatabaseOpen;
  end;


implementation

uses Registry, UpdateDatabaseUnit, NewDatabaseUnit, SaveDatabaseUnit, IBServices;

resourcestring
  sDowngradePrompt = 'Database Version %d found but %d expected. If you have '+
                     'reinstalled this application after a failed upgrade then '+
                     'it may be possible to restore a saved archive of the database '+
                     'taken immediately before the upgrade. Do you want to do this?';

  sReplaceBackup = 'This action will replace the current database with the backup. '+
                              'All data in the current database will be lost!';
  sReplaceInitial = 'This action will replace the current database with an initial database. '+
                              'All data in the current database will be lost!';

{ TIBLocalDBSupport }

procedure TIBLocalDBSupport.DoDowngrade(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;
   RestoreDatabase(DownGradeArchive);
   DowngradeDone;
end;

procedure TIBLocalDBSupport.HandleSave2Backup(Sender: TObject; DBArchive: string
  );
begin
  SaveDatabase(DBArchive);
end;

function TIBLocalDBSupport.AllowInitialisation: boolean;
begin
  Result := (iblQuiet in Options) or
            (MessageDlg(sReplaceInitial, mtWarning,[mbOK,mbCancel],0) = mrOK);
end;

function TIBLocalDBSupport.AllowRestore: boolean;
begin
  Result := (iblQuiet in Options) or
            (MessageDlg(sReplaceBackup,mtWarning,[mbOK,mbCancel],0) = mrOK);
end;

function TIBLocalDBSupport.CreateNewDatabase(DBName: string;
  DBParams: TStrings; DBArchive: string): boolean;
begin
  CreateDir(ExtractFileDir(DBName));
  with TNewDatabase.Create(Application) do
  try
   SetDBParams(IBRestoreService1,DBParams);
   IBRestoreService1.BackupFile.Clear;
   IBRestoreService1.DatabaseName.Clear;
   IBRestoreService1.Options := [CreateNewDB];
   IBRestoreService1.BackupFile.Add(DBArchive);
   IBRestoreService1.DatabaseName.Add(DBName);
   ShowModal;
  finally
    Free
  end;
end;

procedure TIBLocalDBSupport.Downgrade(DBArchive: string);
begin
  if  (iblQuiet in Options) or
      (MessageDlg(Format(sDowngradePrompt, [CurrentDBVersionNo,RequiredVersionNo]),
                     mtConfirmation,[mbYes,mbNo],0) = mrYes) then
  begin
    inherited Downgrade(DBArchive);
    Application.QueueAsyncCall(@DoDowngrade,0);
  end;
end;

function TIBLocalDBSupport.RestoreDatabaseFromArchive(DBName: string;
  DBParams: TStrings; aFilename: string): boolean;
begin
  with TNewDatabase.Create(Application) do
  try
    if (aFilename = '') or not FileExists(aFileName) then
    begin
     OpenDialog1.InitialDir := GetUserDir;
     if OpenDialog1.Execute then
       aFilename := OpenDialog1.FileName
     else
       Exit;
    end;
    SetDBParams(IBRestoreService1,DBParams);
    IBRestoreService1.BackupFile.Clear;
    IBRestoreService1.DatabaseName.Clear;
    IBRestoreService1.Options := [replace];
    IBRestoreService1.BackupFile.Add(aFilename);
    IBRestoreService1.DatabaseName.Add(DBName);
    ShowModal;
  finally
    Free
  end;
end;

function TIBLocalDBSupport.RunUpgradeDatabase(DBParams: TStrings; PatchDir,
  ParamFile: string; VersionFound, VersionWanted: integer): boolean;
begin
  with TUpdateDatabaseDlg.Create(Application) do
  try
    UpdateDatabase.DatabaseName := Database.DatabaseName;
    UpdateDatabase.Params.Assign(DBParams);
    ActiveDatabasePathName := self.ActiveDatabasePathName;
    OnSave2Backup := @HandleSave2Backup;
    Result := ShowModal(PatchDir,ParamFile,VersionFound,VersionWanted) = mrOK;
  finally
    Free
  end;
end;

{$IFDEF WINDOWS}
const
  rgShellFolders      = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  rgPersonal          = 'Personal';
{$ENDIF}

function TIBLocalDBSupport.SaveDatabaseToArchive(DBName: string;
  DBParams: TStrings; aFilename: string): boolean;
begin
  with TSaveDatabase.Create(Application) do
  try
   if aFilename = ''  then
   begin
     SaveDialog1.InitialDir := GetUserDir;
     {$IFDEF WINDOWS}
     with TRegistry.Create do
     try
       if OpenKey(rgShellFolders,false) then
       begin
         SaveDialog1.InitialDir := ReadString(rgPersonal)
       end;
     finally
       Free
     end;
     {$ENDIF}
     if SaveDialog1.Execute then
       aFilename := SaveDialog1.FileName
     else
       Exit;
   end;
   SetDBParams(IBBackupService1,DBParams);
   IBBackupService1.BackupFile.Clear;
   IBBackupService1.DatabaseName := DBName;
   IBBackupService1.BackupFile.Add(aFilename);
   Result := ShowModal = mrOK
  finally
    Free
  end;
end;

end.

