unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DbCtrls, StdCtrls, db, DBControlGrid, IBArrayGrid, IBTable, IBDatabase,
  IBCustomDataSet, IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    SaveBtn: TButton;
    CancelBtn: TButton;
    DataSource1: TDataSource;
    DBControlGrid1: TDBControlGrid;
    DBEdit1: TDBEdit;
    IBArrayGrid1: TIBArrayGrid;
    IBDatabase1: TIBDatabase;
    IBDataSet1: TIBDataSet;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
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
  Application.QueueAsyncCall(@ReOpen,0);
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  with IBTransaction1 do
    if InTransaction then Commit;
end;

procedure TForm1.DoConnectDatabase(Data: PtrInt);
begin
  try
    IBDatabase1.Connected := true;
  except on E: Exception do
    begin
      ShowMessage(E.Message);
      Close;
    end;
  end;
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

