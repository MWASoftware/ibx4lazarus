unit IBXCreateDatabaseFromSQLDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ibxscript;

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

implementation

{$R *.lfm}

{ TIBXCreateDatabaseFromSQLDlg }

procedure TIBXCreateDatabaseFromSQLDlg.IBXScriptProgressEvent(Sender: TObject;
  Reset: boolean; value: integer);
begin
  if Reset then
    ProgressBar.Max := value
  else
    ProgressBar.StepIt;
end;

procedure TIBXCreateDatabaseFromSQLDlg.DoRunScript(Data: PtrInt);
begin
  try
    ModalResult := mrCancel;
    if IBXScript.RunScript(FileName) then
      ModalResult := mrOK;
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

