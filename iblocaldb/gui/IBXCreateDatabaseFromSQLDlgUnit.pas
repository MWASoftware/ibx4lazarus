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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
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
    IBTransaction1: TIBTransaction;
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
    IBTransaction1.DefaultDatabase := aDatabase;
    IBXScript.Transaction := IBTransaction1;
    DatabasePath := aDatabase.Attachment.GetConnectString;
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
    ProgressBar.StepBy(Value);
  Application.ProcessMessages;
end;

procedure TIBXCreateDatabaseFromSQLDlg.DoRunScript(Data: PtrInt);
begin
  try
    ModalResult := mrCancel;
    if IBXScript.RunScript(FileName) then
          ModalResult := mrOK;
    with IBXScript.Transaction do
      if InTransaction then Commit;
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
  IBXScript.Database.DropDatabase;
end;

end.

