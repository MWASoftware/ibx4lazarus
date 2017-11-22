unit IBXCreateDatabaseFromSQLDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ibxscript, IB, IBDatabase;

type

  { TIBXCreateDatabaseFromSQLDlg }

  TIBXCreateDatabaseFromSQLDlg = class(TForm)
    Bevel1: TBevel;
    IBXScript: TIBXScript;
    Label1: TLabel;
    ProgressBar: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure IBXScriptCreateDatabase(Sender: TObject;
      var DatabaseFileName: string);
    procedure IBXScriptProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
  private
    FDatabasePath: string;
    FFileName: string;
    procedure DoRunScript(Data: PtrInt);
  public
    property FileName: string read FFileName write FFileName;
    property DatabasePath: string read FDatabasePath write FDatabasePath;
  end;

var
  IBXCreateDatabaseFromSQLDlg: TIBXCreateDatabaseFromSQLDlg;

function CreateNewDatabase(aDatabase: TIBDatabase; DBArchive: string): boolean;

implementation

uses IBErrorCodes;

function CreateNewDatabase(aDatabase: TIBDatabase;
  DBArchive: string): boolean;
begin
  with TIBXCreateDatabaseFromSQLDlg.Create(Application) do
  try
    FileName := DBArchive;
    IBXScript.Database := aDatabase;
    DatabasePath := aDatabase.DatabaseName;
    Result := ShowModal = mrOK;
  finally
    Free
  end
end;

{$R *.lfm}

{ TIBXCreateDatabaseFromSQLDlg }

procedure TIBXCreateDatabaseFromSQLDlg.IBXScriptProgressEvent(Sender: TObject;
  Reset: boolean; value: integer);
begin
  if Reset then
    ProgressBar.Max := value
  else
    ProgressBar.StepIt;
  Application.ProcessMessages;
end;

procedure TIBXCreateDatabaseFromSQLDlg.DoRunScript(Data: PtrInt);
begin
  try
    ModalResult := mrCancel;
    IBXScript.Database.CreateDatabase; {try to create the database}
    repeat
      try
        if IBXScript.RunScript(FileName) then
          ModalResult := mrOK;
        break;
      except on E:EIBInterBaseError do
      begin
        writeln(E.IBErrorCode);
        if (E.IBErrorCode = isc_io_error) or  (E.IBErrorCode = isc_db_or_file_exists) then
          {script contains Create Database Statement}
        begin
          IBXScript.Database.Connected := true;
          IBXScript.Database.DropDatabase;
          {repeat above and let script create database}
        end
        else
          raise;
      end;
      end;
    until false;
    IBXScript.Database.Connected := false;
  except on E:Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      Close;
    end;
  end;
end;

procedure TIBXCreateDatabaseFromSQLDlg.FormShow(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Application.QueueAsyncCall(@DoRunScript,0);
end;

procedure TIBXCreateDatabaseFromSQLDlg.IBXScriptCreateDatabase(Sender: TObject;
  var DatabaseFileName: string);
begin
  DatabaseFileName := DatabasePath;
end;

end.

