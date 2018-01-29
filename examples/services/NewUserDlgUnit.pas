unit NewUserDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TNewUserDlg }

  TNewUserDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var UserName, Password: string): TModalResult;
  end;

var
  NewUserDlg: TNewUserDlg;

implementation

{$R *.lfm}

{ TNewUserDlg }

procedure TNewUserDlg.FormShow(Sender: TObject);
begin
  Edit1.SetFocus
end;

procedure TNewUserDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    if Edit1.Text = '' then
    begin
      MessageDlg('A User Name must be given',mtError,[mbOK],0);
      CloseAction := caNone;
    end
    else
    if Edit2.Text <> Edit3.Text then
    begin
      MessageDlg('Passwords do not match',mtError,[mbOK],0);
      CloseAction := caNone;
    end;
  end;
end;

function TNewUserDlg.ShowModal(var UserName, Password: string): TModalResult;
begin
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    UserName := AnsiUpperCase(Edit1.Text);
    Password := Edit2.Text;
  end;
end;

end.

