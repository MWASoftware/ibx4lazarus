unit SaveDatabaseUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBServices, StdCtrls, ExtCtrls, IBDatabase;

type

  { TSaveDatabase }

  TSaveDatabase = class(TForm)
    Panel1: TPanel;
    Status: TLabel;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    IBBackupService1: TIBBackupService;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure DoBackup(Data: PtrInt);
    procedure SetDBParams(DBParams: TStrings);
  public
    { Public declarations }
  end;

var
  SaveDatabase: TSaveDatabase;

function SaveDatabaseToArchive(DBName: string; DBParams:TStrings; aFilename: string): boolean;

implementation

uses Registry, IBHeader;

{$IFDEF WINDOWS}
const
  rgShellFolders      = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  rgPersonal          = 'Personal';
{$ENDIF}

function SaveDatabaseToArchive(DBName: string; DBParams: TStrings;
  aFilename: string): boolean;
begin
  Result := false;
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
   IBBackupService1.BackupFile.Clear;
   IBBackupService1.DatabaseName := DBName;
   SetDBParams(DBParams);
   IBBackupService1.BackupFile.Add(aFilename);
   Result := ShowModal = mrOK
  finally
    Free
  end;
end;

{$R *.lfm}

{ TSaveDatabase }

procedure TSaveDatabase.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(DoBackup,0);
end;

procedure TSaveDatabase.DoBackup(Data: PtrInt);
begin
 try
  IBBackupService1.Active := true;
  IBBackupService1.ServiceStart;
  try
    while not IBBackupService1.Eof do
    begin
      Status.Caption := IBBackupService1.GetNextLine;
      Application.ProcessMessages
    end;
  finally
    IBBackupService1.Active := false
  end;
  if FileExists(IBBackupService1.BackupFile[0]) { *Converted from FileExists*  } then
    ModalResult := mrOK
  else
    ModalResult := mrCancel
 except
   ModalResult := mrCancel;
   raise
 end;
 Sleep(500)
end;

procedure TSaveDatabase.SetDBParams(DBParams: TStrings);
var i: integer;
    j: integer;
begin
  IBBackupService1.Params.Clear;
  for i := 0 to DBParams.Count - 1 do
  begin
    for j := 1 to isc_spb_last_spb_constant do
      if DBParams[i] = SPBConstantNames[j] then
      begin
        IBBackupService1.Params.Add(DBParams[i]);
        break;
      end;
  end;
end;

end.
