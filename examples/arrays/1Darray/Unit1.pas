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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DbCtrls, StdCtrls, db, DBControlGrid, IBArrayGrid,  IBDatabase,
  IBCustomDataSet, IB;

{$DEFINE LOCALDATABASE}

const
  sDatabaseName = '1Dtest.fdb'; {If LOCALDATABASE defined then prepended with
                               path to temp folder}

  {If you want to explicitly define the test database location then undefine
  LOCALDATABASE and set explicit path e.g.

  sDatabaseName = 'myserver:/databases/test.fdb';
  }

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IBArrayGrid1: TIBArrayGrid;
    IBDataSet1MYARRAY: TIBArrayField;
    IBDataSet1ROWID: TIntegerField;
    IBDataSet1TITLE: TIBStringField;
    Panel2: TPanel;
    SaveBtn: TButton;
    CancelBtn: TButton;
    DataSource1: TDataSource;
    DBControlGrid1: TDBControlGrid;
    DBEdit1: TDBEdit;
    IBDatabase1: TIBDatabase;
    IBDataSet1: TIBDataSet;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
    procedure IBDataSet1AfterEdit(DataSet: TDataSet);
    procedure IBDataSet1AfterOpen(DataSet: TDataSet);
    procedure IBTransaction1AfterTransactionEnd(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { private declarations }
    procedure DoConnectDatabase(Data: PtrInt);
    procedure ReOpen(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  sqlCreateTable =
    'Create Table TestData ('+
    'RowID Integer not null,'+
    'Title VarChar(32) Character Set UTF8,'+
    'MyArray Double Precision [1:12],'+
    'Primary Key(RowID)'+
    ')';

  sqlCreateGenerator = 'Create Generator ROWNUMBER';
  sqlSetGenerator = 'Set Generator ROWNUMBER to ';

  sqlInsert = 'Insert into TestData(RowID,Title) Values(:RowID,:Title)';

  sqlUpdate = 'Update TestData Set MyArray = ? Where RowID = ?';


{ TForm1 }

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
        ByName('title').AsString := 'Sales Agent ' + IntToStr(row);
      end;
      Statement.Execute;

      Statement := Prepare(Transaction,sqlUpdate);
      ar := CreateArray(Transaction,'TestData','MyArray');
      if ar <> nil then
      begin
        for i := 1 to 12 do
          ar.SetAsDouble([i], abs(row + 16.45 * (6-i))); {sort of randomish formula}
        Statement.SQLParams[0].AsArray := ar;
        Statement.SQLParams[1].AsInteger := row;
        Statement.Execute;
      end;
    end;
    ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlSetGenerator + '4'); {update the generator}
  end;
end;

procedure TForm1.IBDataSet1AfterEdit(DataSet: TDataSet);
begin
  SaveBtn.Enabled := true;
  CancelBtn.Enabled := true;
end;

procedure TForm1.IBDataSet1AfterOpen(DataSet: TDataSet);
begin
  SaveBtn.Enabled := false;
  CancelBtn.Enabled := false;
end;

procedure TForm1.IBTransaction1AfterTransactionEnd(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    Application.QueueAsyncCall(@ReOpen,0);
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  with IBTransaction1 do
    if InTransaction then Commit;
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

procedure TForm1.ReOpen(Data: PtrInt);
begin
  if not (csDestroying in ComponentState) then
    IBDataSet1.Active := true;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoConnectDatabase,0);
end;

procedure TForm1.CancelBtnClick(Sender: TObject);
begin
  with IBTransaction1 do
    if InTransaction then Rollback;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF LOCALDATABASE}
  IBDatabase1.DatabaseName := GetTempDir + sDatabaseName
  {$else}
  IBDatabase1.DatabaseName := sDatabaseName
  {$ENDIF}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IBDataSet1.Append;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete this row?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    IBDataSet1.Delete;
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBDataSet1.Active := true
end;

end.

