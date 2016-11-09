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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls, db,
  IBArrayGrid, IBDatabase, IBQuery, IBCustomDataSet, IB;

{$DEFINE LOCALDATABASE}

const
  sDatabaseName = '2Dtest.fdb'; {If LOCALDATABASE defined then prepended with
                               path to temp folder}

  {If you want to explicitly define the test database location then undefine
  LOCALDATABASE and set explicit path e.g.

  sDatabaseName = 'myserver:/databases/test.fdb';
  }

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBNavigator1: TDBNavigator;
    IBArrayGrid1: TIBArrayGrid;
    IBDatabase1: TIBDatabase;
    IBDataSet1: TIBDataSet;
    IBTransaction1: TIBTransaction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF LOCALDATABASE}
  IBDatabase1.DatabaseName := GetTempDir + sDatabaseName
  {$else}
  IBDatabase1.DatabaseName := sDatabaseName
  {$ENDIF}
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBDataSet1.Active := true;
end;

const
  sqlCreateTable =
    'Create Table TestData ('+
    'RowID Integer not null,'+
    'Title VarChar(32) Character Set UTF8,'+
    'MyArray VarChar(16) [0:16, -1:7] Character Set UTF8,'+
    'Primary Key(RowID)'+
    ')';

  sqlCreateGenerator = 'Create Generator ROWNUMBER';
  sqlSetGenerator = 'Set Generator ROWNUMBER to ';

  sqlInsert = 'Insert into TestData(RowID,Title) Values(:RowID,:Title)';

  sqlUpdate = 'Update TestData Set MyArray = ? Where RowID = ?';

procedure TForm1.IBDatabase1CreateDatabase(Sender: TObject);
var Transaction: ITransaction;
    Statement: IStatement;
    ResultSet: IResultSet;
    row, i,j,k : integer;
    ar: IArray;
    c: char;
begin
  with IBDatabase1.Attachment do
  begin
    ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateTable); {Create the table}
    ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateGenerator); {Create the table}
    {Now Populate it}
    Transaction := StartTransaction([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
    Statement := Prepare(Transaction,'Select * from TestData');
    for row := 1 to 3 do
    begin
      Statement := PrepareWithNamedParameters(Transaction,sqlInsert);
      with Statement.GetSQLParams do
      begin
        ByName('rowid').AsInteger := row;
        ByName('title').AsString := 'Row ' + IntToStr(row);
      end;
      Statement.Execute;

      Statement := Prepare(Transaction,sqlUpdate);
      if row > 2 then continue;
      ar := CreateArray(Transaction,'TestData','MyArray');
      if ar <> nil then
      begin
        k := 0;
        c := chr(ord('A') + row - 1);
        for i := 0 to 16 do
          for j := -1 to 7 do
          begin
            ar.SetAsString([i,j],c + IntToStr(k));
            Inc(k);
          end;
        Statement.SQLParams[0].AsArray := ar;
        Statement.SQLParams[1].AsInteger := row;
        Statement.Execute;
      end;
    end;
    ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlSetGenerator + '4'); {update the generator}
  end;
end;

end.

