unit IBLocalDBSupport;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls,  Dialogs, IBDatabase,
  Registry;

type

  { TIBLocalDBSupport Properties

    * Database: reference to the TIBDatabase component for the local database
    * filename (no path) for the database file
    * EmptyDBArchive: filename (no path) holding the database initialisation archive
    * Enabled: when false component does nothing
    * FirebirdDir: Full path to directory holding firebird.conf. If empty, defaults
      to application executeable directory
    * Name: Component Name
    * PatchDirectory: Path to directory holding SQL patch files. May either be
      absolute path or relative to application executeable.
    * Quiet: If true then no database overwrite warnings
    * RegistryRootKey: Registry Hive used by this component to store path to database file
    * UpgradeConfFile: Path to upgrade configuration file. May either be
      absolute path or relative to application executeable.
    * VendorName: Used to construct path to Database Directory.

    Note: the location of the local database file is platform specific.
    Windows: "User Application Directory"\Vendor Name\Database FileName
    Unix: "User Home Directory"/."Vendor Name"/Database FileName

    Note the use of a hidden directory under Unix.

    Events:
    * OnNewDatabaseOpen: called after the successful initialisation of an empty local
      database.
  }

  { TIBLocalDBSupport }

  TIBLocalDBSupport = class(TComponent)
  private
    { Private declarations }
    FEmptyDBArchive: string;
    FEnabled: boolean;
    FFirebirdDir: string;
    FIBBase: TIBBase;
    FDatabaseName: string;
    FOnNewDatabaseOpen: TNotifyEvent;
    FPatchDirectory: string;
    FQuiet: boolean;
    FRegistry: TRegistry;
    FRegistryRootKey: string;
    FUpgradeConfFile: string;
    FNewDBCreated: boolean;
    FVendorName: string;
    procedure CheckEnabled;
    procedure CreateDatabase(DBName: string; DBParams: TStrings; Overwrite: boolean);
    function GetDatabase: TIBDatabase;
    function GetIntegerParameter(index: string): integer;
    function GetStringParameter(index: string): string;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetIntegerParameter(index: string; AValue: integer);
    procedure SetRegistryRootKey(AValue: string);
    procedure SetStringParameter(index: string; AValue: string);
    procedure OnBeforeDatabaseConnect(Sender: TObject; DBParams: TStrings;
                              var DBName: string);
    procedure OnDatabaseConnect(Sender: TObject);
    procedure NeedRegistry;
    procedure SetupFirebirdEnv;
  protected
    { Protected declarations }
    function GetDBNameAndPath: string;
    function HasValue(index: string): boolean;
    procedure InitDatabaseParameters(DBParams: TStrings;
                              var DBName: string);
    procedure Loaded; override;
    procedure PrepareDBParams(DBParams: TStrings);
    property StringParameter[index: string]: string read GetStringParameter
                                             write SetStringParameter;
    property IntegerParameter[index: string]: integer read GetIntegerParameter
                                                      write SetIntegerParameter;
 public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    {NewDatabase: called to reinitialise the local database using the initialisation
     archive. Overwrites existing data so use carefully.}
    procedure NewDatabase;

    {ResolveDBVersionMismatch: request that the database schema is upgraded from
     version "VersionFound" to "VersionWanted"}
    procedure ResolveDBVersionMismatch(VersionFound, VersionWanted: integer;
                                            var Upgraded: boolean);

    {RestoreDatabase: overwrites the existing local database with the specified
     gbak format archive. User is prompted to locate archive if filename empty.}
    procedure RestoreDatabase (filename: string = '');

    {SaveDatabase: Saves the current database into a gbak format archive. User
     is prompted for archive filename if filename empty.}
    procedure SaveDatabase(filename: string = '');

  published
    { Published declarations }
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property EmptyDBArchive: string read FEmptyDBArchive write FEmptyDBArchive;
    property FirebirdDir: string read FFirebirdDir write FFirebirdDir;
    property PatchDirectory: string  read FPatchDirectory write FPatchDirectory;
    property RegistryRootKey: string read FRegistryRootKey write SetRegistryRootKey;
    property Quiet: boolean read FQuiet write FQuiet;
    property UpgradeConfFile: string read FUpgradeConfFile write FUpgradeConfFile;
    property VendorName: string read FVendorName write FVendorName;
    property OnNewDatabaseOpen: TNotifyEvent read FOnNewDatabaseOpen write FOnNewDatabaseOpen;
 end;

implementation

uses UpdateDatabaseUnit, NewDatabaseUnit, SaveDatabaseUnit
  {$IFDEF Unix} ,initc {$ENDIF}
  {$IFDEF WINDOWS} ,Windows {$ENDIF}
  ,SysUtils;

const
  rgDatabasePath = 'DatabasePath';

{ TIBLocalDBSupport }

procedure TIBLocalDBSupport.CheckEnabled;
begin
  if not Enabled then
    raise Exception.Create('Local Database Access Disabled');
end;

procedure TIBLocalDBSupport.CreateDatabase(DBName: string; DBParams: TStrings;
  Overwrite: boolean);
var DBArchive: string;
begin
 CheckEnabled;
 if FileExists(DBName) then
 begin
   if not Overwrite then Exit;

   DeleteFile(DBName);
 end;
 DBArchive := EmptyDBArchive;
 if DBArchive = '' then
   raise Exception.Create('Unable to create database - no archive specified');

 {$IFDEF UNIX}
 if DBArchive[1] <> DirectorySeparator then
 {$ENDIF}
   DBArchive := Application.Location + DBArchive;

 SetupFirebirdEnv;
 CreateNewDatabase(DBName,DBParams,DBArchive);
 FNewDBCreated := true;
end;

function TIBLocalDBSupport.GetDatabase: TIBDatabase;
begin
  Result := FIBBase.Database;
end;

function TIBLocalDBSupport.GetIntegerParameter(index: string): integer;
begin
  Result := 0;
  NeedRegistry;
  if FRegistry.OpenKey(FRegistryRootKey,false) and FRegistry.ValueExists(index) then
     Result := FRegistry.ReadInteger(index);
  FRegistry.CloseKey;
end;

function TIBLocalDBSupport.GetStringParameter(index: string): string;
begin
  Result := '';
  NeedRegistry;
  if FRegistry.OpenKey(FRegistryRootKey,false) and FRegistry.ValueExists(index) then
     Result := FRegistry.ReadString(index);
  FRegistry.CloseKey;
end;

procedure TIBLocalDBSupport.SetDatabase(AValue: TIBDatabase);
begin
  FIBBase.Database := AValue;
end;

procedure TIBLocalDBSupport.SetIntegerParameter(index: string; AValue: integer);
begin
  NeedRegistry;
  if FRegistry.OpenKey(FRegistryRootKey,true) then
      FRegistry.WriteInteger(index,AValue);
  FRegistry.CloseKey;
end;

procedure TIBLocalDBSupport.SetRegistryRootKey(AValue: string);
begin
  if FRegistryRootKey = AValue then Exit;
  FRegistryRootKey := AValue;
  if assigned(FRegistry) then FreeAndNil(FRegistry);
end;

procedure TIBLocalDBSupport.SetStringParameter(index: string; AValue: string);
begin
  NeedRegistry;
  if FRegistry.OpenKey(FRegistryRootKey,true) then
      FRegistry.WriteString(index,AValue);
  FRegistry.CloseKey;
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
  InitDatabaseParameters(DBParams,DBName);
  SetupFirebirdEnv;
end;

procedure TIBLocalDBSupport.OnDatabaseConnect(Sender: TObject);
begin
  if not Enabled then Exit;
  if FNewDBCreated and assigned(FOnNewDatabaseOpen) then
    OnNewDatabaseOpen(self);
  FNewDBCreated := false;
end;

procedure TIBLocalDBSupport.NeedRegistry;
begin
  if not assigned(FRegistry) then
    FRegistry := TRegistry.Create;
end;

procedure TIBLocalDBSupport.SetupFirebirdEnv;
var TmpDir: string;
begin
  TmpDir := GetTempDir +
      DirectorySeparator + 'firebird_' + GetEnvironmentVariable('USER');
  if GetEnvironmentVariable('FIREBIRD_TMP') = '' then
  begin
    if not DirectoryExists(tmpDir) then
      mkdir(tmpDir);
    SetEnvironmentVariable('FIREBIRD_TMP',PChar(TmpDir));
  end;
  if GetEnvironmentVariable('FIREBIRD_LOCK') = '' then
  begin
    if not DirectoryExists(tmpDir) then
      mkdir(tmpDir);
    SetEnvironmentVariable('FIREBIRD_LOCK',PChar(TmpDir));
  end;
  if (FirebirdDir <> '') and FileExists(FirebirdDir + DirectorySeparator + 'firebird.conf') then
    SetEnvironmentVariable('FIREBIRD',PChar(FirebirdDir))
  else
  if FileExists(Application.Location + 'firebird.conf') then
    SetEnvironmentVariable('FIREBIRD',PChar(ExtractFileDir(Application.ExeName)));
end;

function TIBLocalDBSupport.GetDBNameAndPath: string;
begin
  if HasValue(rgDatabasePath) then
    Result := StringParameter[rgDatabasePath]
  else
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
    StringParameter[rgDatabasePath] := Result;
  end;
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

function TIBLocalDBSupport.HasValue(index: string): boolean;
begin
  NeedRegistry;
  Result := FRegistry.OpenKey(FRegistryRootKey,false) and FRegistry.ValueExists(index);
  FRegistry.CloseKey;
end;

constructor TIBLocalDBSupport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FEnabled := true;
  FIBBase := TIBBase.Create(self);
  FIBBase.AfterDatabaseConnect := @OnDatabaseConnect;
  FIBBase.BeforeDatabaseConnect := @OnBeforeDatabaseConnect;
  FRegistryRootKey := 'software\MWA Software\Database';
  FPatchDirectory := 'patches';
  FUpgradeConfFile := 'upgrade.conf';
end;

destructor TIBLocalDBSupport.Destroy;
begin
  if assigned(FIBBase) then FreeAndNil(FIBBase);
  if assigned(FRegistry) then FreeAndNil(FRegistry);
  inherited Destroy;
end;

procedure TIBLocalDBSupport.NewDatabase;
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not Quiet then
  begin
    if (MessageDlg('This action will replace the current database with an initial database. All data in the current database will be lost!',
        mtWarning,[mbOK,mbCancel],0) = mrCancel) then
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

procedure TIBLocalDBSupport.ResolveDBVersionMismatch(VersionFound,
  VersionWanted: integer; var Upgraded: boolean);
var confFile: string;
    patchDir: string;
    DBParams: TStringList;
begin
  Upgraded := false;
  if not Enabled then Exit;
  confFile := UpgradeConfFile;
  if (confFile = '') then Exit;
  if confFile[1] <> DirectorySeparator then
    confFile := Application.Location + confFile;
  patchDir := PatchDirectory;
  if patchDir = '' then Exit;
  if patchDir[1] <> DirectorySeparator then
    patchDir := Application.Location + patchDir;
  DBParams := TStringList.Create;
  try
    DBParams.Assign(Database.Params);
    PrepareDBParams(DBParams);
    Upgraded := RunUpgradeDatabase(Database,DBParams,patchDir,confFile,VersionFound,VersionWanted);
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
end;

procedure TIBLocalDBSupport.RestoreDatabase(filename: string);
var TempDBParams: TStringList;
begin
  CheckEnabled;
  if not Quiet then
  begin
    if(MessageDlg('This action will replace the current database with the backup. All data in the current database will be lost!',
        mtWarning,[mbOK,mbCancel],0) = mrCancel) then
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

end.
