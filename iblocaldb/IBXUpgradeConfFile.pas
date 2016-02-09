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
unit IBXUpgradeConfFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TUpgradeInfo = record
    UpdateSQLFile,
    UserMessage: string;
    BackupDB: boolean;
  end;

{ TUpgradeConfFile }

  TUpgradeConfFile = class
  private
    FConfFileName: string;
    FCurrentVersion: string;
    FPatchDir: string;
    FUpgradeInfo: TIniFile;
    function GetUpgradeAvailableToVersion: integer;
    function ParseFileInfo(Info:string; var FileName: string;
                                var Compressed: boolean): boolean;
  public
    constructor Create(aFileName: string; PatchDir: string);
    destructor Destroy; override;
    function CheckUpgradeAvailable(RequiredVersionNo: integer): boolean;
    function GetUpgradeInfo(VersionNo: integer; var UpgradeInfo: TUpgradeInfo): boolean;
    function GetSourceFile(aName: string; var FileName: string;
                                 var Compressed: boolean): boolean;
    property UpgradeAvailableToVersion: integer read GetUpgradeAvailableToVersion;
  end;

  EUpgradeConfFileError = class(Exception);


implementation

const
  sSectionheader      = 'Version.%.3d';

resourcestring
  sInvalidConfFile = 'Database Upgrade Required, but the Upgrade File (%s) is missing or not specified';
  sUpgradeRequired = 'Database Upgrade Required, but the Upgrade File is out of Date. '+
                              'Required Version = %d, Upgrade available for version %d';
  sNoInfo      = 'Upgrading Database Schema to Version %d';

{ TUpgradeConfFile }

function TUpgradeConfFile.GetUpgradeAvailableToVersion: integer;
begin
  Result := StrToInt(FUpgradeInfo.ReadString('Status','Current','0'))
end;

function TUpgradeConfFile.ParseFileInfo(Info: string; var FileName: string;
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

constructor TUpgradeConfFile.Create(aFileName: string; PatchDir: string);
begin
  inherited Create;
  FConfFileName := aFileName;
  FPatchDir := PatchDir;
  if (FConfFileName = '') or not FileExists(FConfFileName) then
     raise EUpgradeConfFileError.CreateFmt(sInvalidConfFile,[FConfFileName]);
  FUpgradeInfo := TIniFile.Create(FConfFileName);
end;

destructor TUpgradeConfFile.Destroy;
begin
  if assigned(FUpgradeInfo) then FUpgradeInfo.Free;
  inherited Destroy;
end;

function TUpgradeConfFile.CheckUpgradeAvailable(RequiredVersionNo: integer
  ): boolean;
var CurVersion: integer;
begin
  CurVersion := GetUpgradeAvailableToVersion;
  if CurVersion < RequiredVersionNo then
    raise EUpgradeConfFileError.CreateFmt(sUpgradeRequired, [RequiredVersionNo,CurVersion]);
end;

function TUpgradeConfFile.GetUpgradeInfo(VersionNo: integer;
  var UpgradeInfo: TUpgradeInfo): boolean;
var IniFile: TIniFile;
begin
   Result := false;
   FCurrentVersion := Format(sSectionheader,[VersionNo]);
   UpgradeInfo.UserMessage := FUpgradeInfo.ReadString(FCurrentVersion,'Msg',
                                Format(sNoInfo,[VersionNo]));
   UpgradeInfo.UpdateSQLFile := FPatchDir + DirectorySeparator + FUpgradeInfo.ReadString(FCurrentVersion,'Upgrade','');
   UpgradeInfo.BackupDB := CompareText(FUpgradeInfo.ReadString(FCurrentVersion,'BackupDatabase','no'),'yes') = 0;
   Result := (UpgradeInfo.UpdateSQLFile <> '');
end;

function TUpgradeConfFile.GetSourceFile(aName: string; var FileName: string;
  var Compressed: boolean): boolean;
begin
  Result := ParseFileInfo(FUpgradeInfo.ReadString(FCurrentVersion,aName,''),FileName,Compressed);
  FileName := FPatchDir + DirectorySeparator + FileName
end;

end.

