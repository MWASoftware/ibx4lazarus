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
unit DBACreateDatabaseDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IBXServices, StdCtrls, ExtCtrls, ComCtrls;

type

  { TDBACreateDatabaseDlg }

  TDBACreateDatabaseDlg = class(TForm)
    IBXClientSideRestoreService1: TIBXClientSideRestoreService;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Status: TLabel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure IBRestoreService1GetNextLine(Sender: TObject; var Line: string);
  private
    { Private declarations }
    FFileName: string;
    procedure DoRestore(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  DBACreateDatabaseDlg: TDBACreateDatabaseDlg;

function RestoreDatabaseFromArchive(
   aRestoreService: TIBXClientSideRestoreService;  aFilename: string): boolean;

implementation

{$R *.lfm}

function RestoreDatabaseFromArchive(
  aRestoreService: TIBXClientSideRestoreService; aFilename: string): boolean;
begin
 with TDBACreateDatabaseDlg.Create(Application) do
 try
   FFileName := aFileName;
   IBXClientSideRestoreService1.Assign(aRestoreService);
   IBXClientSideRestoreService1.Options := [replace];
   Result := ShowModal = mrOK;
 finally
   Free
 end;
end;

procedure TDBACreateDatabaseDlg.FormShow(Sender: TObject);
begin
 Status.Caption := '';
 Application.QueueAsyncCall(@DoRestore,0)
end;

procedure TDBACreateDatabaseDlg.IBRestoreService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Status.Caption := Line;
  Application.ProcessMessages;
end;

procedure TDBACreateDatabaseDlg.DoRestore(Data: PtrInt);
begin
 try
   IBXClientSideRestoreService1.RestoreFromFile(FFileName,nil);
   ModalResult := mrOK;
 except on E:Exception do
   begin
     Application.ShowException(E);
     ModalResult := mrCancel;
   end;
 end;
end;

end.
