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
            
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, db,
  IBDynamicGrid, IBDatabase, IBStoredProc, IBQuery, IBDatabaseInfo, IB,
  Unit2;

{$DEFINE LOCALDATABASE}

const
  sDatabaseName = 'sptest.fdb'; {If LOCALDATABASE defined then prepended with
                               path to temp folder}

  {If you want to explicitly define the test database location then undefine
  LOCALDATABASE and set explicit path e.g.

  sDatabaseName = 'myserver:/databases/test.fdb';
  }

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    GetLinesBtn: TButton;
    DataSource1: TDataSource;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    IBDynamicGrid1: TIBDynamicGrid;
    IBQuery1: TIBQuery;
    IBStoredProc1: TIBStoredProc;
    IBStoredProc2: TIBStoredProc;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetLinesBtnClick(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
  private
    procedure DoConnectDatabase(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFNDEF LOCALDATABASE}
  IBDatabase1.DatabaseName := sDatabaseName
  {$ENDIF}
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  GetLinesBtn.Enabled := IBQuery1.Active and (IBQuery1.RecordCount > 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IBTransaction1.Active := true;
  IBStoredProc2.ExecProc;
  IBTransaction1.Commit;
  IBTransaction1.Active := true;
  IBQuery1.Active := true;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Application.QueueAsyncCall(@DoConnectDatabase,0);
end;

procedure TForm1.GetLinesBtnClick(Sender: TObject);
begin
  IBStoredProc1.ExecProc;
  Memo1.Lines.Add(IBStoredProc1.ParamByName('LINES').AsString);
end;

procedure TForm1.IBDatabase1CreateDatabase(Sender: TObject);
begin
  if IBDatabaseInfo1.ODSMajorVersion < 12 then
  begin
    IBDatabase1.DropDatabase;
    raise EIBClientError.Create(0,'This example requires Firebird 3');
  end
  else
   DBCreateForm.ShowModal;
end;

procedure TForm1.DoConnectDatabase(Data: PtrInt);
begin
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Close;
        Exit
      end;
    On E:Exception do
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
end;

end.

