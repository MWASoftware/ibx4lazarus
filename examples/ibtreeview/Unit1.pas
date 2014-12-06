unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, db,
  IBTreeView, IBDatabase, IBCustomDataSet;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DepartmentsBUDGET: TIBBCDField;
    DepartmentsDEPARTMENT: TIBStringField;
    DepartmentsDEPT_NO: TIBStringField;
    DepartmentsHEAD_DEPT: TIBStringField;
    DepartmentsLOCATION: TIBStringField;
    DepartmentsMNGR_NO: TSmallintField;
    DepartmentsPHONE_NO: TIBStringField;
    IBDatabase1: TIBDatabase;
    Departments: TIBDataSet;
    IBTransaction1: TIBTransaction;
    IBTreeView1: TIBTreeView;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Departments.Active := false
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Departments.Active := true
end;

end.

