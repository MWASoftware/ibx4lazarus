unit RegisterServerDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TRegisterServerDlg }

  TRegisterServerDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(var aServerName: string): TModalResult;
  end;

var
  RegisterServerDlg: TRegisterServerDlg;

implementation

{$R *.lfm}

{ TRegisterServerDlg }

procedure TRegisterServerDlg.FormShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

function TRegisterServerDlg.ShowModal(var aServerName: string): TModalResult;
begin
  Edit1.Text := aServerName;
  Result := inherited ShowModal;
  if Result = mrOK then
    aServerName := Edit1.text;
end;

end.

