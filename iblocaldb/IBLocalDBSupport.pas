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
  Classes, LResources, Forms, Controls,  Dialogs, IBDatabase, SysUtils;

type

  { TIBLocalDBSupport Properties

    * Database: reference to the TIBDatabase component for the local database
    * DatabaseName: filename (no path) to use for the Firebird Database file
    * EmptyDBArchive: filename (optional path) holding the database initialisation archive
    * Enabled: when false component does nothing
    * FirebirdDirectory: Path to directory holding firebird.conf. May either be
      absolute path or relative to application executeable. If empty, defaults
      to application executeable directory.
    * Name: Component Name
    * PatchDirectory: Path to directory holding SQL patch files. May either be
      absolute path or relative to application executeable.
    * Quiet: If true then no database overwrite warnings
    * UpgradeConfFile: Path to upgrade configuration file. May either be
      absolute path or relative to application executeable.
    * VendorName: Used to construct path to Database Directory.

    Note: the location of the local database file is platform specific.
    Windows: "User Application Directory"\Vendor Name\Database FileName
    Unix: "User Home Directory"/."Vendor Name"/Database FileName.

    Note that at design time paths may use '/' or '\' as directory separator. At
    run time, they must be specified using the appropriate Directory Separator
    for the current platform.

    If the VendorName property is left empty then Sysutils.VendorName is used. If
    this is empty then no VendorName component is present in the path.

    Note the use of a hidden directory under Unix.

    Events:
    * OnGetDatabaseName: allows modification of the generated database name
    * OnNewDatabaseOpen: called after the successful initialisation of an empty local
      database.
  }

  TOnGetDatabaseName = procedure(Sender: TObject; var DBName: string) of object;
  TOnGetDBVersionNo = procedure (Sender: TObject; var VersionNo: integer) of object;

  TIBLocalOption = (iblAutoUpgrade, iblAllowDowngrade, iblQuiet);
  TIBLocalOptions = set of TIBLocalOption;

  { TIBLocalDBSupport }

  TIBLocalDBSupport = class(TComponent)
  private
    { Private declarations }
    FActiveDatabasePathName: string;
    FCurrentDBVersionNo: integer;
    FEmptyDBArchive: string;
    FEnabled: boolean;
    FFirebirdDirectory: string;
    FIBBase: TIBBase;
    FDatabaseName: string;
    FOnGetDatabaseName: TOnGetDatabaseName;
    FOnGetDBVersionNo: TOnGetDBVersionNo;
    FOnNewDatabaseOpen: TNotifyEvent;
    FOptions: TIBLocalOptions;
    FPatchDirectory: string;
    FRequiredVersionNo: integer;
    FUpgradeConfFile: string;
    FNewDBCreated: boolean;
    FVendorName: string;
    FInUpgrade: boolean;
    FDownGradeArchive: string;
    FSharedDataDir: string;
    function IsAbsolutePath(aPath: string): boolean;
    procedure CheckEnabled;
    procedure CreateDatabase(DBName: string; DBParams: TStrings; Overwrite: boolean);
    procedure DoDowngrade(Data: PtrInt);
    function GetDatabase: TIBDatabase;
    procedure SetDatabase(AValue: TIBDatabase);
    function MapSharedDataDir(aDataDir: string): string;
    procedure OnBeforeDatabaseConnect(Sender: TObject; DBParams: TStrings;
                              var DBName: string);
    procedure OnDatabaseConnect(Sender: TObject);
    procedure OnAfterDatabaseDisconnect(Sender: TObject);
    procedure SetFirebirdDirectory(AValue: string);
    procedure SetPatchDirectory(AValue: string);
    procedure SetupFirebirdEnv;
    procedure UpgradeCheck;
  protected
    { Protected declarations }
    function GetDBNameAndPath: string;
    procedure InitDatabaseParameters(DBParams: TStrings;
                              var DBName: string);
    procedure Loaded; override;
    procedure PrepareDBParams(DBParams: TStrings);
 public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function DowngradePending: boolean;

    {NewDatabase: called to reinitialise the local database using the initialisation
     archive. Overwrites existing data so use carefully.}
    procedure NewDatabase;

    {ResolveDBVersionMismatch: request that the database schema is upgraded from
     version "VersionFound" to "VersionWanted"}
    function ResolveDBVersionMismatch(VersionFound, VersionWanted: integer): boolean;

    {RestoreDatabase: overwrites the existing local database with the specified
     gbak format archive. User is prompted to locate archive if filename empty.}
    procedure RestoreDatabase (filename: string = '');

    {SaveDatabase: Saves the current database into a gbak format archive. User
     is prompted for archive filename if filename empty.}
    procedure SaveDatabase(filename: string = '');

    {TryRestoreDBVersion: Called after a failed upgrade and on re-installation
     of an older software version. Locates if possible the archive saved at the
     time of upgrade and restores it.
    }

    procedure TryRestoreDBVersion(VersionNo: integer);

    property ActiveDatabasePathName: string read FActiveDatabasePathName;
    property CurrentDBVersionNo: integer read FCurrentDBVersionNo;

  published
    { Published declarations }
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property EmptyDBArchive: string read FEmptyDBArchive write FEmptyDBArchive;
    property FirebirdDirectory: string read FFirebirdDirectory write SetFirebirdDirectory;
    property Options: TIBLocalOptions read FOptions write FOptions;
    property PatchDirectory: string  read FPatchDirectory write SetPatchDirectory;
    property RequiredVersionNo: integer read FRequiredVersionNo write FRequiredVersionNo;
    property UpgradeConfFile: string read FUpgradeConfFile write FUpgradeConfFile;
    property VendorName: string read FVendorName write FVendorName;
    property OnGetDatabaseName: TOnGetDatabaseName read FOnGetDatabaseName write FOnGetDatabaseName;
    property OnGetDBVersionNo: TOnGetDBVersionNo read FOnGetDBVersionNo write FOnGetDBVersionNo;
    property OnNewDatabaseOpen: TNotifyEvent read FOnNewDatabaseOpen write FOnNewDatabaseOpen;
 end;

  EIBLocalFatalError = class(Exception);

implementation

uses UpdateDatabaseUnit, NewDatabaseUnit, SaveDatabaseUnit, IBIntf
  {$IFDEF Unix} ,initc, regexpr {$ENDIF}
  {$IFDEF WINDOWS} ,Windows {$ENDIF};

resourcestring
  sDowngradePrompt = 'Database Version %d found but %d expected. If you have '+
                     'reinstalled this application after a failed upgrade then '+
                     'it may be possible to restore a saved archive of the database '+
                     'taken immediately before the upgrade. Do you want to do this?';

  sSWUpgradeNeeded = 'Software Upgrade Required: Current DB Version No is %d. Version %d supported';

  sUpgradeRequired = 'Database Upgrade Required, but the Upgrade File is out of Date. '+
                              'Required Version = %d, Upgrade available for version %d';

  sReplaceBackup = 'This action will replace the current database with the backup. '+
                              'All data in the current database will be lost!';
  sReplaceInitial = 'This action will replace the current database with an initial database. '+
                              'All data in the current database will be lost!';

  sLocalDBDisabled = 'Local Database Access Disabled';

  sEmptyDBArchiveMissing = 'Unable to create database - no empty DB archive specified';

  sEmptyDBArchiveNotFound = 'Unable to create database - empty DB archive file not found';

{ TIBLocalDBSupport }

function TIBLocalDBSupport.IsAbsolutePath(aPath: string): boolean;
begin
  Result := false;
  {$IFDEF WINDOWS}
    Result := (ExtractFileDrive(aPath) <> '') or
      ((Length(aPath) > 0) and (aPath[1] = DirectorySeparator));
  {$ENDIF}
  {$IFDEF UNIX}
    Result := (Length(aPath) > 0) and (aPath[1] = DirectorySeparator);
  {$ENDIF}
end;

procedure TIBLocalDBSupport.CheckEnabled;
begin
  if not Enabled then
    raise Exception.Create(sLocalDBDisabled);
end;

procedure TIBLocalDBSupport.CreateDatabase(DBName: string; DBParams: TStrings;
  Overwrite: boolean);
var DBArchive: string;
begin
 CheckEnabled;
 if FileExists(DBName) then
 begin
   if not Overwrite then Exit;

   sysutils.DeleteFile(DBName);
 end;
 DBArchive := EmptyDBArchive;
 if DBArchive = '' then
   raise Exception.Create(sEmptyDBArchiveMissing);

 if not IsAbsolutePath(DBArchive) then
   DBArchive := FSharedDataDir + DBArchive;

 if not FileExists(DBArchive) then
   raise Exception.Create(sEmptyDBArchiveNotFound);

 SetupFirebirdEnv;
 CreateNewDatabase(DBName,DBParams,DBArchive);
 FNewDBCreated := true;
end;

procedure TIBLocalDBSupport.DoDowngrade(Data: PtrInt);
begin
 if AppDestroying in Application.Flags then Exit;
  RestoreDatabase(FDownGradeArchive);
  FDownGradeArchive := '';
 if assigned(OnGetDBVersionNo) then
   OnGetDBVersionNo(self,FCurrentDBVersionNo); {Update Version No}
end;

function TIBLocalDBSupport.GetDatabase: TIBDatabase;
begin
  Result := FIBBase.Database;
end;

procedure TIBLocalDBSupport.SetDatabase(AValue: TIBDatabase);
begin
  FIBBase.Database := AValue;
end;

function TIBLocalDBSupport.MapSharedDataDir(aDataDir: string): string;
{$IFDEF UNIX}
var  RegexObj: TRegExpr;
{$ENDIF}
begin
  Result := aDataDir;
{$IFDEF UNIX}

  {Under Unix transform application exe paths that are in installed locations
   e.g. /usr/local/bin to corresponding shared data locations ee.g. /usr/local/shared}
  RegexObj := TRegExpr.Create;
  try
    RegexObj.Expression := '^/usr(/local|)/(s|)bin/.*$';
    if RegexObj.Exec(aDataDir) then
      Result := '/usr' + RegexObj.Match[1] + '/shared/' + ApplicationName + '/';
  finally
    RegexObj.Free;
  end;
{$ENDIF}
end;

{$IFDEF Unix}
function setenv(name:Pchar; value:Pchar; replace:integer):integer;cdecl;external clib name 'setenv';
function unsetenv(name:Pchar):integer;cdecl;external clib name 'unsetenv';
function SetEnvironmentVariable(name:PChar; value:PChar):boolean;
// Set environment variable; if empty string given, remove it.
begin
  result:=false; //assume failure
  if value = '' then
  begin
    // Assume user wants to remove variable.
    if unsetenv(name)=0 then result:=true;
  end
  else
  begin
    // Non empty so set the variable
    if setenv(name, value, 1)=0 then result:=true;
  end;
end;
{$ENDIF}

procedure TIBLocalDBSupport.OnBeforeDatabaseConnect(Sender: TObject;
  DBParams: TStrings; var DBName: string);
begin
  if not Enabled then Exit;

  if not IsEmbeddedServer then
     raise EIBLocalFatalError.Create('Firebird Embedded Server is required but is not installed');

  InitDatabaseParameters(DBParams,DBName);
  FActiveDatabasePathName := DBName;
  SetupFirebirdEnv;
end;

procedure TIBLocalDBSupport.OnDatabaseConnect(Sender: TObject);
begin
  if not Enabled or
      FInUpgrade then Exit; {Avoids problem if RECONNECT used in script}

  UpgradeCheck;

  if FNewDBCreated and assigned(FOnNewDatabaseOpen) then
    OnNewDatabaseOpen(self);
  FNewDBCreated := false;

end;

procedure TIBLocalDBSupport.OnAfterDatabaseDisconnect(Sender: TObject);
begin
  FActiveDatabasePathName := '';
end;

procedure TIBLocalDBSupport.SetFirebirdDirectory(AValue: string);
begin
  if FFirebirdDirectory = AValue then Exit;
  FFirebirdDirectory := ExcludeTrailingPathDelimiter(AValue);
end;

procedure TIBLocalDBSupport.SetPatchDirectory(AValue: string);
begin
  if FPatchDirectory = AValue then Exit;
  FPatchDirectory := ExcludeTrailingPathDelimiter(AValue);
end;

procedure TIBLocalDBSupport.SetupFirebirdEnv;
var TmpDir: string;
begin
  TmpDir := GetTempDir +
      DirectorySeparator + 'firebird_' + sysutils.GetEnvironmentVariable('USER');
  if sysutils.GetEnvironmentVariable('FIREBIRD_TMP') = '' then
  begin
    if not DirectoryExists(tmpDir) then
      mkdir(tmpDir);
    SetEnvironmentVariable('FIREBIRD_TMP',PChar(TmpDir));
  end;
  if sysutils.GetEnvironmentVariable('FIREBIRD_LOCK') = '' then
  begin
    if not DirectoryExists(tmpDir) then
      mkdir(tmpDir);
    SetEnvironmentVariable('FIREBIRD_LOCK',PChar(TmpDir));
  end;
  if sysutils.GetEnvironmentVariable('FIREBIRD') = '' then
  begin
    if FirebirdDirectory <> '' then
    begin
      if not IsAbsolutePath(FirebirdDirectory) then
        FirebirdDirectory := FSharedDataDir + FirebirdDirectory;
      if FileExists(FirebirdDirectory + DirectorySeparator + 'firebird.conf') then
      begin
        SetEnvironmentVariable('FIREBIRD',PChar(FirebirdDirectory));
        Exit;
      end;
    end;
    if FileExists(FSharedDataDir + 'firebird.conf') then
      SetEnvironmentVariable('FIREBIRD',PChar(ExtractFileDir(Application.ExeName)));
  end;
end;

procedure TIBLocalDBSupport.UpgradeCheck;
var DBArchive: string;
begin
  if assigned(OnGetDBVersionNo) then
    OnGetDBVersionNo(self,FCurrentDBVersionNo)
  else
    Exit;

  if CurrentDBVersionNo > RequiredVersionNo then
  begin
    {Possible recovery after failed upgrade}
    DBArchive := ChangeFileExt(GetDBNameAndPath,'');
    DBArchive := DBArchive + '.' + IntToStr(RequiredVersionNo) + '.gbk';
    if FileExists(DBArchive) and (iblAllowDowngrade in FOptions) then
    begin
       if  (iblQuiet in FOptions) or
           (MessageDlg(Format(sDowngradePrompt, [CurrentDBVersionNo,RequiredVersionNo]),
                          mtConfirmation,[mbYes,mbNo],0) = mrYes) then
       begin
         FDownGradeArchive := DBArchive;
         Application.QueueAsyncCall(@DoDowngrade,0);
       end;
    end
    else
      raise EIBLocalFatalError.CreateFmt(sSWUpgradeNeeded,[CurrentDBVersionNo,RequiredVersionNo]);
  end
  else
  if (CurrentDBVersionNo < RequiredVersionNo) and (iblAutoUpgrade in FOptions) then
  begin
      FInUpgrade := true;
      try
        if ResolveDBVersionMismatch(CurrentDBVersionNo,RequiredVersionNo) and
           assigned(OnGetDBVersionNo)  then {Update Version No}
          OnGetDBVersionNo(self,FCurrentDBVersionNo);

      finally
        FInUpgrade := false;
      end;
  end;
end;

function TIBLocalDBSupport.GetDBNameAndPath: string;
begin
  Result := DatabaseName;
  if VendorName <> '' then
    Result := VendorName + DirectorySeparator + Result
  else
  if Sysutils.VendorName <> '' then
    Result := SysUtils.VendorName + DirectorySeparator + Result;
  {$IFDEF UNIX}
  Result := GetUserDir + '.' + Result;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := GetUserDir + 'Application Data' + DirectorySeparator + Result;
  {$ENDIF}
  if assigned(OnGetDatabaseName) then
    OnGetDatabaseName(self,Result);
end;

procedure TIBLocalDBSupport.PrepareDBParams(DBParams: TStrings);

  procedure Remove(s: string);
  var i: integer;
  begin
    i := DBParams.IndexOfName(s);
    if i <> -1 then
      DBParams.Delete(i);
  end;

begin
    Remove('user_name');
    Remove('password');
    {$IFDEF WINDOWS}
      DBParams.Values['user_name'] := 'SYSDBA';
      DBParams.Values['password'] := 'masterkey';
    {$ENDIF}
end;

constructor TIBLocalDBSupport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnabled := true;
  FIBBase := TIBBase.Create(self);
  FIBBase.AfterDatabaseConnect := @OnDatabaseConnect;
  FIBBase.BeforeDatabaseConnect := @OnBeforeDatabaseConnect;
  FIBBase.AfterDatabaseDisconnect := @OnAfterDatabaseDisconnect;
  FPatchDirectory := 'patches';
  FUpgradeConfFile := 'upgrade.conf';
  FOptions := [iblAutoUpgrade, iblAllowDowngrade];
  FSharedDataDir := MapSharedDataDir(Application.Location);
end;

destructor TIBLocalDBSupport.Destroy;
begin
  if assigned(FIBBase) then FreeAndNil(FIBBase);
  inherited Destroy;
end;

function TIBLocalDBSupport.DowngradePending: boolean;
begin
  Result := FDownGradeArchive <> '';
end;

procedure TIBLocalDBSupport.NewDatabase;
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not (iblQuiet in FOptions) then
  begin
    if (MessageDlg(sReplaceInitial, mtWarning,[mbOK,mbCancel],0) = mrCancel) then
      Exit;
  end;

  TempDBParams := TStringList.Create;
  try
   TempDBParams.Assign(Database.Params);
   Database.Connected := false;
   PrepareDBParams(TempDBParams);
   CreateDatabase(Database.DatabaseName,TempDBParams,true);
  finally
    TempDBParams.Free;
  end;
  Database.Connected := true;
end;

function TIBLocalDBSupport.ResolveDBVersionMismatch(VersionFound,
  VersionWanted: integer): boolean;
var confFile: string;
    patchDir: string;
    DBParams: TStringList;
    CurVersion: integer;
begin
  Result := false;
  if not Enabled then Exit;
  confFile := UpgradeConfFile;
  if (confFile = '') then Exit;
  if not IsAbsolutePath(confFile) then
    confFile := FSharedDataDir + confFile;

  CurVersion := GetCurrentVersion(confFile);
  if CurVersion < VersionWanted then
    raise Exception.CreateFmt(sUpgradeRequired, [VersionWanted,CurVersion]);

  patchDir := PatchDirectory;
  if patchDir = '' then Exit;
  if not IsAbsolutePath(patchDir) then
    patchDir := FSharedDataDir + patchDir;
  DBParams := TStringList.Create;
  try
    DBParams.Assign(Database.Params);
    PrepareDBParams(DBParams);
    Result := RunUpgradeDatabase(self,Database,DBParams,patchDir,confFile,VersionFound,VersionWanted);
  finally
    DBParams.Free;
  end;
end;

procedure TIBLocalDBSupport.InitDatabaseParameters(DBParams: TStrings;
  var DBName: string);
begin
    PrepareDBParams(DBParams);
    DBName := GetDBNameAndPath;
    if not FileExists(DBName) then
      CreateDatabase(DBName,DBParams,false);
end;

procedure TIBLocalDBSupport.Loaded;
begin
  inherited Loaded;
  if (Database <> nil) and (Database.DatabaseName = '') then
    Database.DatabaseName := ' '; {Avoid CheckDatabaseName Error}
  DoDirSeparators(FEmptyDBArchive);
  DoDirSeparators(FFirebirdDirectory);
  DoDirSeparators(FPatchDirectory);
  DoDirSeparators(FUpgradeConfFile);
end;

procedure TIBLocalDBSupport.RestoreDatabase(filename: string);
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not (iblQuiet in FOptions) then
  begin
    if(MessageDlg(sReplaceBackup,mtWarning,[mbOK,mbCancel],0) = mrCancel) then
    Exit;
  end;

  TempDBParams := TStringList.Create;
  try
   TempDBParams.Assign(Database.Params);
   PrepareDBParams(TempDBParams);
   Database.Connected := false;
   RestoreDatabaseFromArchive(Database.DatabaseName,TempDBParams,filename);
  finally
    TempDBParams.Free;
  end;
  Database.Connected := true;
end;

procedure TIBLocalDBSupport.SaveDatabase(filename: string);
var TempDBParams: TStringList;
begin
  CheckEnabled;
  TempDBParams := TStringList.Create;
  try
   TempDBParams.Assign(Database.Params);
   PrepareDBParams(TempDBParams);
   SaveDatabaseToArchive(Database.DatabaseName,TempDBParams,filename);
  finally
    TempDBParams.Free;
  end;
end;

procedure TIBLocalDBSupport.TryRestoreDBVersion(VersionNo: integer);
var DBArchive: string;
begin
  DBArchive := ChangeFileExt(GetDBNameAndPath,'');
  DBArchive := DBArchive + '.' + IntToStr(VersionNo) + '.gbk';
  RestoreDatabase(DBArchive);
end;

end.
