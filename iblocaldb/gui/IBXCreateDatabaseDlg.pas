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
unit IBXCreateDatabaseDlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBXServices, StdCtrls, ExtCtrls;

type

  { TCreateDatabaseDlg }

  TCreateDatabaseDlg = class(TForm)
    IBRestoreService1: TIBXServerSideRestoreService;
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure IBRestoreService1GetNextLine(Sender: TObject; var Line: string);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoRestore(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  CreateDatabaseDlg: TCreateDatabaseDlg;

function RestoreDatabaseFromArchive(
   aRestoreService: TIBXServerSideRestoreService;  aFilename: string): boolean;

function CreateNewDatabase(
   aRestoreService: TIBXServerSideRestoreService;  DBArchive: string): boolean;

implementation

function RestoreDatabaseFromArchive(
  aRestoreService: TIBXServerSideRestoreService; aFilename: string): boolean;
begin
 with TCreateDatabaseDlg.Create(Application) do
 try
   if (aFilename = '') or not FileExists(aFileName) then
   begin
    OpenDialog1.InitialDir := GetUserDir;
    if OpenDialog1.Execute then
      aFilename := OpenDialog1.FileName
    else
      Exit;
   end;
   IBRestoreService1.Assign(aRestoreService);
   IBRestoreService1.BackupFiles.Clear;
   IBRestoreService1.Options := [replace];
   IBRestoreService1.BackupFiles.Add(aFilename);
   Result := ShowModal = mrOK;
 finally
   Free
 end;
end;

function CreateNewDatabase(aRestoreService: TIBXServerSideRestoreService;
  DBArchive: string): boolean;
begin
 with TCreateDatabaseDlg.Create(Application) do
 try
  IBRestoreService1.Assign(aRestoreService);
  IBRestoreService1.BackupFiles.Clear;
  IBRestoreService1.BackupFiles.Add(DBArchive);
  IBRestoreService1.Options := [CreateNewDB];
  Result := ShowModal = mrOK;
 finally
   Free
 end
end;

{$R *.lfm}

procedure TCreateDatabaseDlg.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoRestore,0)
end;

procedure TCreateDatabaseDlg.IBRestoreService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Status.Caption := Line;
  Application.ProcessMessages;
end;

procedure TCreateDatabaseDlg.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  if FileExists(IBRestoreService1.DatabaseFiles[0]) then
  begin
    ModalResult := mrOK
  end
  else
    ModalResult := mrCancel
end;

procedure TCreateDatabaseDlg.DoRestore(Data: PtrInt);
begin
 try
   IBRestoreService1.Execute(nil);
   Timer1.Interval := 500;
 except on E:Exception do
   raise
 end;
end;

end.
