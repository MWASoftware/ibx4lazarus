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
  IBDynamicGrid, IBDatabase, IBCustomDataSet, IBDatabaseInfo, IB;

{$DEFINE LOCALDATABASE}

const
  sDatabaseName = 'IDTest.fdb'; {If LOCALDATABASE defined then prepended with
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
    IBDatabaseInfo1: TIBDatabaseInfo;
    Label1: TLabel;
    PostBtn: TButton;
    DataSource1: TDataSource;
    IBDatabase1: TIBDatabase;
    IBDataSet1: TIBDataSet;
    IBDynamicGrid1: TIBDynamicGrid;
    IBTransaction1: TIBTransaction;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
    procedure PostBtnClick(Sender: TObject);
  private
    procedure DoConnectDatabase(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  sqlCreateTable = 'CREATE TABLE IDTEST ('+
                   '"KEY" integer GENERATED BY DEFAULT AS IDENTITY, '+
                   'SOMETEXT varchar(64), '+
                   'COMPTEXT Computed By (SOMETEXT || '' has the key '' || "KEY"), '+
                   'PRIMARY KEY ("KEY") )';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFNDEF LOCALDATABASE}
  IBDatabase1.DatabaseName := sDatabaseName
  {$ENDIF}
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoConnectDatabase,0);
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBTransaction1.Active := true;
  IBDataset1.Active := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IBDataset1.Append;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  PostBtn.Enabled := IBDataset1.State in [dsInsert,dsEdit];
end;

procedure TForm1.IBDatabase1CreateDatabase(Sender: TObject);
begin
  if IBDatabaseInfo1.ODSMajorVersion < 12 then
  begin
    IBDatabase1.DropDatabase;
    raise EIBClientError.Create(0,'This example requires Firebird 3 or later');
  end
  else
  with IBDatabase1.Attachment do
    ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable); {Create the table}
end;

procedure TForm1.PostBtnClick(Sender: TObject);
begin
  IBDataset1.Post;
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

