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
  Classes, SysUtils, LResources, Forms, Controls,  Dialogs, IBXCustomIBLocalDBSupport,
  IBXServices;

type

  { TIBLocalDBSupport }

  TIBLocalDBSupport = class(TCustomIBLocalDBSupport)
  private
    procedure DoDowngrade(Data: PtrInt);
    procedure HandleGetDBVersionNo(Sender: TObject; var VersionNo: integer);
    procedure HandleUpgradeStepCompleted(Sender: TObject);
  protected
    function AllowInitialisation: boolean; override;
    function AllowRestore: boolean; override;
    function InternalCreateNewDatabase(DBArchive: string): boolean; override;
    procedure Downgrade(DBArchive: string); override;
    function RestoreDatabaseFromArchive(aFilename: string): boolean; override;
    function RunUpgradeDatabase(TargetVersionNo: integer): boolean; override;
    function SaveDatabaseToArchive(aFilename: string): boolean; override;
  published
    property Database;
    property DatabaseName;
    property Enabled;
    property EmptyDBArchive;
    property FirebirdDirectory;
    property Options;
    property RequiredVersionNo;
    property MinimumVersionNo;
    property UpgradeConfFile;
    property SectionHeaderTemplate;
    property VendorName;
    property OnGetDatabaseName;
    property OnGetDBVersionNo;
    property OnNewDatabaseOpen;
    property OnGetSharedDataDir;
  end;


implementation

uses IBXUpgradeDatabaseDlg, IBXCreateDatabaseDlg, IBXSaveDatabaseDlg,
  Registry, IBXCreateDatabaseFromSQLDlgUnit;

resourcestring
  sDowngradePrompt = 'Database Version %d found but Version %d expected. If you have '+
                     'reinstalled this application after a failed upgrade then '+
                     'it may be possible to restore a saved archive of the database '+
                     'taken immediately before the upgrade. Do you want to do this?';

  sReplaceBackup =   'This action will replace the current database with the backup. '+
                              'All data in the current database will be lost!';
  sReplaceInitial =   'This action will replace the current database with an initial database. '+
                              'All data in the current database will be lost!';

{ TIBLocalDBSupport }

procedure TIBLocalDBSupport.DoDowngrade(Data: PtrInt);
begin
  if AppDestroying in Application.Flags then Exit;
   RestoreDatabase(DownGradeArchive);
   DowngradeDone;
end;

procedure TIBLocalDBSupport.HandleGetDBVersionNo(Sender: TObject;
  var VersionNo: integer);
begin
  VersionNo := CurrentDBVersionNo;
end;

procedure TIBLocalDBSupport.HandleUpgradeStepCompleted(Sender: TObject);
begin
  UpdateVersionNo;
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

function TIBLocalDBSupport.InternalCreateNewDatabase(DBArchive: string
  ): boolean;
begin
  if IsGBakFile(DBArchive) then
  begin
    Database.Attachment.Disconnect;
    try
      Result := IBXCreateDatabaseDlg.RestoreDatabaseFromArchive(RestoreService,DBArchive)
    finally
      Database.Attachment.Connect;
    end;
  end
  else
    Result := IBXCreateDatabaseFromSQLDlgUnit.CreateNewDatabase(Database,DBArchive)
end;

procedure TIBLocalDBSupport.Downgrade(DBArchive: string);
begin
  if  (iblQuiet in Options) or
      (MessageDlg(Format(sDowngradePrompt, [CurrentDBVersionNo,RequiredVersionNo]),
                     mtWarning,[mbYes,mbNo],0) = mrYes) then
  begin
    inherited Downgrade(DBArchive);
    Application.QueueAsyncCall(@DoDowngrade,0);
  end;
end;

function TIBLocalDBSupport.RestoreDatabaseFromArchive(aFilename: string
  ): boolean;
begin
  Result := IBXCreateDatabaseDlg.RestoreDatabaseFromArchive(RestoreService,aFileName);
end;

function TIBLocalDBSupport.RunUpgradeDatabase(TargetVersionNo: integer
  ): boolean;
begin
  Result := IBXUpgradeDatabaseDlg.RunUpgradeDatabase(Database,BackupService,UpgradeConf,
                  SectionHeaderTemplate, ChangeFileExt(ActiveDatabasePathName,''),
                  TargetVersionNo,@HandleGetDBVersionNo, @HandleUpgradeStepCompleted);
end;

function TIBLocalDBSupport.SaveDatabaseToArchive(aFilename: string): boolean;
begin
  Result := IBXSaveDatabaseDlg.SaveDatabaseToArchive(BackupService,aFileName);
end;

end.

