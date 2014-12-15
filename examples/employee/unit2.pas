unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, db,
  IBTreeView, IBQuery;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    Departments: TIBQuery;
    IBTreeView1: TIBTreeView;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBTreeView1DblClick(Sender: TObject);
  private
    { private declarations }
    FSelectedDept_No: string;
  public
    { public declarations }
    function ShowModal(var Dept_No: string): TModalResult;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin
  Departments.ACtive := true
end;

procedure TForm2.IBTreeView1DblClick(Sender: TObject);
begin
  ModalResult := mrOK
end;

function TForm2.ShowModal(var Dept_No: string): TModalResult;
begin
  Result := inherited ShowModal;
  Dept_No := FSelectedDept_No;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (ModalResult = mrOK) and (IBTreeView1.Selected <> nil) then
    FSelectedDept_No := TIBTreeNode(IBTreeView1.Selected).KeyValue;
  Departments.Active := false
end;

end.
