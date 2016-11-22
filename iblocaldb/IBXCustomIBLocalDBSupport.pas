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
unit IBXCustomIBLocalDBSupport;

{$mode objfpc}{$H+}

interface

uses
  Classes,  IBDatabase, SysUtils, IBServices, IBXUpgradeConfFile, ibxscript,
  IB;

type

  { TCustomIBLocalDBSupport Properties

    * Database: reference to the TIBDatabase component for the local database
    * DatabaseName: filename (no path) to use for the Firebird Database file.
    * EmptyDBArchive: filename (optional path) holding the database initialisation archive.
      May either be absolute path or relative to shared data directory.
    * Enabled: when false component does nothing
    * FirebirdDirectory: Full path to directory holding firebird.conf. May either be
      absolute path or relative to the shared data directory. If empty, defaults to  shared data directory.
    * Name: Component Name
    * Options:
              iblAutoUpgrade: Automatically apply upgrade when database schema
                              version is lower than required.
              IblAllowDowngrade: Automatically apply downgrade when available to
                                 schema version compatible with the required version.
              iblQuiet:  true then no database overwrite warnings
    * RequiredVersionNo: The schema version number required by the application.
      TIBLocalDBSupport will normally try to upgrade/downgrade the schema to satisfy
      this requirement.
    * UpgradeConfFile: Path to upgrade configuration file. May either be absolute
      path or relative to the shared data directory.
    * VendorName: Used to construct path to Database Directory.

    Note that at design time paths may use '/' or '\' as directory separator. At
    run time, they must be specified using the appropriate Directory Separator
    for the current platform.

    Events:

    * OnGetDatabaseName: The  database name is normally computed automatically.
      However, this event allows an application to inspect and override the result.
    * OnNewDatabaseOpen: called after the successful initialisation of an empty local  database.
    * OnGetDBVersionNo: called to get the current database schema version number.
      If this event is not handled then schema upgrade/downgrade is never performed.
    * OnGetSharedDataDir: The shared data directory is normally computed automatically.
      However, this event allows an application to inspect and override the result.
      }

  TOnGetDatabaseName = procedure(Sender: TObject; var DBName: string) of object;
  TOnGetDBVersionNo = procedure (Sender: TObject; var VersionNo: integer) of object;
  TOnGetSharedDataDir = procedure(Sender: TObject; var SharedDataDir: string) of object;

  TIBLocalOption = (iblAutoUpgrade, iblAllowDowngrade, iblQuiet);
  TIBLocalOptions = set of TIBLocalOption;


  { TCustomIBLocalDBSupport }

  TCustomIBLocalDBSupport = class(TComponent)
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
    FOnGetSharedDataDir: TOnGetSharedDataDir;
    FOnNewDatabaseOpen: TNotifyEvent;
    FOptions: TIBLocalOptions;
    FRequiredVersionNo: integer;
    FUpgradeConfFile: string;
    FNewDBCreated: boolean;
    FVendorName: string;
    FInUpgrade: boolean;
    FDownGradeArchive: string;
    FSharedDataDir: string;
    FUpgradeConf: TUpgradeConfFile;
    procedure CheckEnabled;
    procedure CreateDatabase(DBName: string; DBParams: TStrings; Overwrite: boolean);
    function GetDatabase: TIBDatabase;
    procedure SetDatabase(AValue: TIBDatabase);
    function GetDBNameAndPath: string;
    procedure InitDatabaseParameters(DBParams: TStrings;
                              var DBName: string);
    function MapSharedDataDir(aDataDir: string): string;
    procedure OnBeforeDatabaseConnect(Sender: TObject; DBParams: TStrings;
                              var DBName: string);
    procedure OnDatabaseConnect(Sender: TObject);
    procedure OnAfterDatabaseDisconnect(Sender: TObject);
    procedure PrepareDBParams(DBParams: TStrings);
    procedure SetDatabaseName(AValue: string);
    procedure SetFirebirdDirectory(AValue: string);
    procedure SetupFirebirdEnv;
    procedure UpgradeCheck;
  protected
    { Protected declarations }
    function AllowInitialisation: boolean; virtual;
    function AllowRestore: boolean; virtual;
    procedure CreateDir(DirName: string);
    function CreateNewDatabase(DBName:string; DBParams: TStrings; DBArchive: string): boolean; virtual; abstract;
    procedure HandleGetParamValue(Sender: TObject; ParamName: string; var BlobID: TISC_QUAD);
    procedure Downgrade(DBArchive: string); virtual;
    procedure DowngradeDone;
    procedure Loaded; override;
    function RestoreDatabaseFromArchive(DBName:string; DBParams: TStrings; aFilename: string): boolean; virtual; abstract;
    function RunUpgradeDatabase(TargetVersionNo: integer): boolean; virtual; abstract;
    function SaveDatabaseToArchive(DBName: string; DBParams:TStrings; aFilename: string): boolean; virtual; abstract;
    function UpdateVersionNo: boolean;
    property DownGradeArchive: string read FDownGradeArchive;
    property UpgradeConf: TUpgradeConfFile read FUpgradeConf;
 public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function DowngradePending: boolean;

    {NewDatabase: called to reinitialise the local database using the initialisation
     archive. Overwrites existing data so use carefully.}
    procedure NewDatabase;

    {Perform a downgrade to target version if backup archive available.
     Note: normally called from UpgradeCheck if iblAllowDowngrade in Options}
    procedure PerformDowngrade(TargetVersionNo: integer);

    {Perform schema upgrade to target version if upgrade scripts available.
     Note: normally called from UpgradeCheck if iblAllowUpgrade in Options}
    procedure PerformUpgrade(TargetVersionNo: integer);

    {RestoreDatabase: overwrites the existing local database with the specified
     gbak format archive. User is prompted to locate archive if filename empty.}
    procedure RestoreDatabase (filename: string = '');

    {SaveDatabase: Saves the current database into a gbak format archive. User
     is prompted for archive filename if filename empty.}
    procedure SaveDatabase(filename: string = '');

    {Copies database parameters as give in the DBParams to the Service
     omitting any parameters not appropriate for TIBService. Typically, the
     DBParams are TIBDatabase.Params}
    class procedure SetDBParams(aService: TIBCustomService; DBParams: TStrings);

    property ActiveDatabasePathName: string read FActiveDatabasePathName;
    property CurrentDBVersionNo: integer read FCurrentDBVersionNo;

    { Likely to be Published declarations }
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property EmptyDBArchive: string read FEmptyDBArchive write FEmptyDBArchive;
    property FirebirdDirectory: string read FFirebirdDirectory write SetFirebirdDirectory;
    property Options: TIBLocalOptions read FOptions write FOptions;
    property RequiredVersionNo: integer read FRequiredVersionNo write FRequiredVersionNo;
    property UpgradeConfFile: string read FUpgradeConfFile write FUpgradeConfFile;
    property VendorName: string read FVendorName write FVendorName;
    property OnGetDatabaseName: TOnGetDatabaseName read FOnGetDatabaseName write FOnGetDatabaseName;
    property OnGetDBVersionNo: TOnGetDBVersionNo read FOnGetDBVersionNo write FOnGetDBVersionNo;
    property OnNewDatabaseOpen: TNotifyEvent read FOnNewDatabaseOpen write FOnNewDatabaseOpen;
    property OnGetSharedDataDir: TOnGetSharedDataDir read FOnGetSharedDataDir write FOnGetSharedDataDir;
 end;

  EIBLocalFatalError = class(Exception);

implementation

uses  DB, IBBlob, ZStream
  {$IFDEF Unix} ,initc, regexpr {$ENDIF}
  {$IFDEF WINDOWS} ,Windows ,Windirs {$ENDIF};

resourcestring
  sNoDowngrade = 'Database Schema is %d. Unable to downgrade to version %d';
  sLocalDBDisabled = 'Local Database Access Disabled';
  sEmptyDBArchiveMissing = 'Unable to create database - no empty DB archive specified';
  sEmptyDBArchiveNotFound = 'Unable to create database - empty DB archive file not found';
  sNoEmbeddedServer = 'Firebird Embedded Server is required but is not installed';

{ TCustomIBLocalDBSupport }


procedure TCustomIBLocalDBSupport.HandleGetParamValue(Sender: TObject;
  ParamName: string; var BlobID: TISC_QUAD);
var Blob: TIBBlobStream;
    Source: TStream;
    FileName: string;
begin
  Blob := TIBBlobStream.Create;
  try
    Blob.Database := (Sender as TIBXScript).Database;
    Blob.Transaction := (Sender as TIBXScript).Transaction;
    Blob.Mode := bmWrite;
    if not assigned(UpgradeConf) or
       not UpgradeConf.GetSourceFile(ParamName,FileName) then Exit;

    if CompareText(ExtractFileExt(FileName),'.gz') = 0 then  {gzip compressed file}
      Source := TGZFileStream.Create(FileName,gzopenread)
    else
      Source := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
    try
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

procedure TCustomIBLocalDBSupport.CheckEnabled;
begin
  if not Enabled then
    raise Exception.Create(sLocalDBDisabled);
end;

procedure TCustomIBLocalDBSupport.CreateDatabase(DBName: string; DBParams: TStrings;
  Overwrite: boolean);
var DBArchive: string;
begin
 CheckEnabled;
 DBArchive := EmptyDBArchive;
 if DBArchive = '' then
   raise Exception.Create(sEmptyDBArchiveMissing);

 if not TUpgradeConfFile.IsAbsolutePath(DBArchive) then
   DBArchive := FSharedDataDir + DBArchive;

 if not FileExists(DBArchive) then
   raise Exception.Create(sEmptyDBArchiveNotFound);

 if FileExists(DBName) then
 begin
   if not Overwrite then Exit;

   sysutils.DeleteFile(DBName);
 end;

 SetupFirebirdEnv;
 CreateNewDatabase(DBName,DBParams,DBArchive);
 FNewDBCreated := true;
end;

function TCustomIBLocalDBSupport.GetDatabase: TIBDatabase;
begin
  Result := FIBBase.Database;
end;

procedure TCustomIBLocalDBSupport.SetDatabase(AValue: TIBDatabase);
begin
  FIBBase.Database := AValue;
end;

function TCustomIBLocalDBSupport.MapSharedDataDir(aDataDir: string): string;
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
      Result := '/usr' + RegexObj.Match[1] + '/share/' + ApplicationName + '/';
  finally
    RegexObj.Free;
  end;
{$ENDIF}
  if assigned (OnGetSharedDataDir) then
    OnGetSharedDataDir(self,Result);
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

procedure TCustomIBLocalDBSupport.OnBeforeDatabaseConnect(Sender: TObject;
  DBParams: TStrings; var DBName: string);
begin
  if not Enabled or (csDesigning in ComponentState) then Exit;

  if not FirebirdAPI.IsEmbeddedServer then
     raise EIBLocalFatalError.Create(sNoEmbeddedServer);

  InitDatabaseParameters(DBParams,DBName);
  FActiveDatabasePathName := DBName;
  SetupFirebirdEnv;
end;

procedure TCustomIBLocalDBSupport.OnDatabaseConnect(Sender: TObject);
begin
  if not Enabled or (csDesigning in ComponentState) or
      FInUpgrade then Exit; {Avoids problem if RECONNECT used in script}

  UpgradeCheck;

  if not DowngradePending and FNewDBCreated and assigned(FOnNewDatabaseOpen) then
    OnNewDatabaseOpen(self);
  FNewDBCreated := false;

end;

procedure TCustomIBLocalDBSupport.OnAfterDatabaseDisconnect(Sender: TObject);
begin
  FActiveDatabasePathName := '';
end;

procedure TCustomIBLocalDBSupport.SetFirebirdDirectory(AValue: string);
begin
  if FFirebirdDirectory = AValue then Exit;
  FFirebirdDirectory := ExcludeTrailingPathDelimiter(AValue);
end;

procedure TCustomIBLocalDBSupport.SetupFirebirdEnv;
begin
  if sysutils.GetEnvironmentVariable('FIREBIRD') = '' then
  begin
    if FirebirdDirectory <> '' then
    begin
      if not TUpgradeConfFile.IsAbsolutePath(FirebirdDirectory) then
        FirebirdDirectory := FSharedDataDir + FirebirdDirectory;
      if FileExists(FirebirdDirectory + DirectorySeparator + 'firebird.conf') then
      begin
        SetEnvironmentVariable('FIREBIRD',PChar(FirebirdDirectory));
        Exit;
      end;
    end;
    if FileExists(FSharedDataDir + 'firebird.conf') then
      SetEnvironmentVariable('FIREBIRD',PChar(FSharedDataDir));
  end;
end;

procedure TCustomIBLocalDBSupport.UpgradeCheck;
begin
  if not UpdateVersionNo then
    Exit;

  if (CurrentDBVersionNo > RequiredVersionNo) and (iblAllowDowngrade in FOptions)then
    {Possible recovery after failed upgrade}
    PerformDowngrade(RequiredVersionNo)
  else
  if (CurrentDBVersionNo < RequiredVersionNo) and (iblAutoUpgrade in FOptions) then
    PerformUpgrade(RequiredVersionNo);
end;

function TCustomIBLocalDBSupport.AllowInitialisation: boolean;
begin
  Result := true;
end;

function TCustomIBLocalDBSupport.AllowRestore: boolean;
begin
  Result := true;
end;

procedure TCustomIBLocalDBSupport.CreateDir(DirName: string);
var ParentDirName: string;
begin
  ParentDirName := ExtractFileDir(DirName);
  if not DirectoryExists(ParentDirName) and (ParentDirName <> DirName) then
    CreateDir(ParentDirName);
  if not DirectoryExists(DirName) then
    mkdir(DirName);
end;

procedure TCustomIBLocalDBSupport.Downgrade(DBArchive: string);
begin
  FDownGradeArchive := DBArchive;
end;

procedure TCustomIBLocalDBSupport.DowngradeDone;
begin
  FDownGradeArchive := '';
  UpdateVersionNo;
end;

function TCustomIBLocalDBSupport.GetDBNameAndPath: string;
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
  Result := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA) + Result;
  {$ENDIF}
  if assigned(OnGetDatabaseName) then
    OnGetDatabaseName(self,Result);
end;

procedure TCustomIBLocalDBSupport.PrepareDBParams(DBParams: TStrings);

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

procedure TCustomIBLocalDBSupport.SetDatabaseName(AValue: string);
begin
  if FDatabaseName = AValue then Exit;
  FDatabaseName := ExtractFileName(AValue);
end;

class procedure TCustomIBLocalDBSupport.SetDBParams(aService: TIBCustomService;
  DBParams: TStrings);
var i: integer;
    j: integer;
    k: integer;
    ParamName: string;
begin
  aService.Params.Clear;
  for i := 0 to DBParams.Count - 1 do
  begin
    ParamName := DBParams[i];
    k := Pos('=',ParamName);
    if k > 0 then system.Delete(ParamName,k,Length(ParamName)-k+1);
    for j := 1 to isc_spb_last_spb_constant do
      if ParamName = SPBConstantNames[j] then
      begin
        aService.Params.Add(DBParams[i]);
        break;
      end;
  end;
end;

function TCustomIBLocalDBSupport.UpdateVersionNo: boolean;
begin
  Result := assigned(OnGetDBVersionNo);
  if Result then
    OnGetDBVersionNo(self,FCurrentDBVersionNo); {Update Version No}
end;

constructor TCustomIBLocalDBSupport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnabled := true;
  FIBBase := TIBBase.Create(self);
  FIBBase.AfterDatabaseConnect := @OnDatabaseConnect;
  FIBBase.BeforeDatabaseConnect := @OnBeforeDatabaseConnect;
  FIBBase.AfterDatabaseDisconnect := @OnAfterDatabaseDisconnect;
  FUpgradeConfFile := 'upgrade.conf';
  FOptions := [iblAutoUpgrade, iblAllowDowngrade];
  FSharedDataDir := MapSharedDataDir(ExtractFilePath(ParamStr(0)));
end;

destructor TCustomIBLocalDBSupport.Destroy;
begin
  if assigned(FIBBase) then FreeAndNil(FIBBase);
  inherited Destroy;
end;

function TCustomIBLocalDBSupport.DowngradePending: boolean;
begin
  Result := FDownGradeArchive <> '';
end;

procedure TCustomIBLocalDBSupport.NewDatabase;
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not AllowInitialisation then Exit;

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

procedure TCustomIBLocalDBSupport.PerformDowngrade(TargetVersionNo: integer);
var DBArchive: string;
begin
  DBArchive := ChangeFileExt(ActiveDatabasePathName,'') +
                   '.' + IntToStr(TargetVersionNo) + '.gbk';
  if FileExists(DBArchive) then
    Downgrade(DBArchive)
  else
    raise EIBLocalFatalError.CreateFmt(sNoDowngrade,[CurrentDBVersionNo,TargetVersionNo]);
end;

procedure TCustomIBLocalDBSupport.PerformUpgrade(TargetVersionNo: integer);

  function GetUpgradeConfFile: string;
  begin
    Result := UpgradeConfFile;
    if Result = '' then Exit;

    if not TUpgradeConfFile.IsAbsolutePath(Result) then
      Result := FSharedDataDir + Result;
  end;

begin
  if FInUpgrade then Exit;

  FUpgradeConf := TUpgradeConfFile.Create(GetUpgradeConfFile);
  try
    FUpgradeConf.CheckUpgradeAvailable(TargetVersionNo);
    FInUpgrade := true;
    try
      RunUpgradeDatabase(TargetVersionNo);
    finally
      FInUpgrade := false;
    end;
  finally
    FreeAndNil(FUpgradeConf);
  end;
end;

procedure TCustomIBLocalDBSupport.InitDatabaseParameters(DBParams: TStrings;
  var DBName: string);
begin
    PrepareDBParams(DBParams);
    DBName := GetDBNameAndPath;
    if not FileExists(DBName) then
      CreateDatabase(DBName,DBParams,false);
end;

procedure TCustomIBLocalDBSupport.Loaded;
begin
  inherited Loaded;
  if (Database <> nil) and (Database.DatabaseName = '') then
    Database.DatabaseName := ' '; {Avoid CheckDatabaseName Error}
  DoDirSeparators(FEmptyDBArchive);
  DoDirSeparators(FFirebirdDirectory);
  DoDirSeparators(FUpgradeConfFile);
end;

procedure TCustomIBLocalDBSupport.RestoreDatabase(filename: string);
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not AllowRestore then Exit;

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

procedure TCustomIBLocalDBSupport.SaveDatabase(filename: string);
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

end.
