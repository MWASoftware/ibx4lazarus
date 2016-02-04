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
unit NewDatabaseUnit;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBServices, StdCtrls, ExtCtrls, IBDatabase, DB;

type

  { TNewDatabase }

  TNewDatabase = class(TForm)
    IBRestoreService1: TIBRestoreService;
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure DoRestore(Data: PtrInt);
    procedure SetDBParams(DBParams: TStrings);
  public
    { Public declarations }
  end;

var
  NewDatabase: TNewDatabase;

function CreateNewDatabase(DBName:string; DBParams: TStrings; DBArchive: string): boolean;
function RestoreDatabaseFromArchive(DBName:string; DBParams: TStrings; aFilename: string): boolean;

implementation

uses IBHeader;

procedure CreateDir(DirName: string);
var ParentDirName: string;
begin
  ParentDirName := ExtractFileDir(DirName);
  if not DirectoryExists(ParentDirName) and (ParentDirName <> DirName) then
    CreateDir(ParentDirName);
  if not DirectoryExists(DirName) then
    mkdir(DirName);
end;

function CreateNewDatabase(DBName: string;  DBParams: TStrings; DBArchive: string): boolean;
begin
  Result := false;
  CreateDir(ExtractFileDir(DBName));
  with TNewDatabase.Create(Application) do
  try
   SetDBParams(DBParams);
   IBRestoreService1.BackupFile.Clear;
   IBRestoreService1.DatabaseName.Clear;
   IBRestoreService1.Options := [CreateNewDB];
   IBRestoreService1.BackupFile.Add(DBArchive);
   IBRestoreService1.DatabaseName.Add(DBName);
   Result := ShowModal = mrOK;
  finally
    Free
  end;
end;

function RestoreDatabaseFromArchive(DBName: string; DBParams: TStrings;
  aFilename: string): boolean;
begin
 Result := false;

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
   SetDBParams(DBParams);
   IBRestoreService1.BackupFile.Clear;
   IBRestoreService1.DatabaseName.Clear;
   IBRestoreService1.Options := [replace];
   IBRestoreService1.BackupFile.Add(aFilename);
   IBRestoreService1.DatabaseName.Add(DBName);
   Result := ShowModal = mrOK;
 finally
   Free
 end;
end;

{$R *.lfm}

procedure TNewDatabase.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoRestore,0)
end;

procedure TNewDatabase.SetDBParams(DBParams: TStrings);
var i: integer;
    j: integer;
begin
  IBRestoreService1.Params.Clear;
  for i := 0 to DBParams.Count - 1 do
  begin
    for j := 1 to isc_spb_last_spb_constant do
      if DBParams[i] = SPBConstantNames[j] then
      begin
        IBRestoreService1.Params.Add(DBParams[i]);
        break;
      end;
  end;
end;

procedure TNewDatabase.DoRestore(Data: PtrInt);
begin
 try
  IBRestoreService1.Active := true;
  IBRestoreService1.ServiceStart;
  try
    while not IBRestoreService1.Eof do
    begin
      Status.Caption := Trim(IBRestoreService1.GetNextLine);
      Application.ProcessMessages
    end;
  finally
    IBRestoreService1.Active := false
  end;
  if FileExists(IBRestoreService1.DatabaseName[0]) then
  begin
    ModalResult := mrOK
  end
  else
    ModalResult := mrCancel
 except on E:Exception do
   raise
 end;
 Sleep(500);
end;

end.
