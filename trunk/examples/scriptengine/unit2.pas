unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, db,
  IBDynamicGrid, IBQuery, IBDatabase;

type

  { TSelectSQLResults }

  TSelectSQLResults = class(TForm)
    DataSource1: TDataSource;
    IBDynamicGrid1: TIBDynamicGrid;
    IBTransaction1: TIBTransaction;
    SelectQuery: TIBQuery;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Show(SelectSQLText: string);
  end;

var
  SelectSQLResults: TSelectSQLResults;

implementation

{$R *.lfm}

{ TSelectSQLResults }

procedure TSelectSQLResults.FormShow(Sender: TObject);
begin
  SelectQuery.Active := true;
end;

procedure TSelectSQLResults.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SelectQuery.ACtive := false;
  CloseAction := caFree;
end;

procedure TSelectSQLResults.Show(SelectSQLText: string);
begin
  SelectQuery.SQL.Text := SelectSQLText;
  inherited Show;
end;

end.

