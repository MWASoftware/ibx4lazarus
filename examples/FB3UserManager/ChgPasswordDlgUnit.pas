unit ChgPasswordDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TChgPasswordDlg }

  TChgPasswordDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var Password: string): TModalResult;
  end;

var
  ChgPasswordDlg: TChgPasswordDlg;

implementation

{$R *.lfm}

{ TChgPasswordDlg }

procedure TChgPasswordDlg.FormShow(Sender: TObject);
begin
  Edit2.SetFocus;
end;

procedure TChgPasswordDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if Edit2.Text <> Edit3.Text then
    begin
      MessageDlg('Passwords do not match',mtError,[mbOK],0);
      CloseAction := caNone;
    end;
  end;
end;

function TChgPasswordDlg.ShowModal(var Password: string): TModalResult;
begin
  Edit2.Text := '';
  Edit3.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
    Password := Edit2.Text;
end;

end.

