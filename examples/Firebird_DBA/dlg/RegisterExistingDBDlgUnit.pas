unit RegisterExistingDBDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TRegisterExistingDBDlg }

  TRegisterExistingDBDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    ServerName: TEdit;
    DatabaseName: TEdit;
    DatabasePath: TEdit;
    DefaultUserName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(aServerName: string): TModalResult;
  end;

var
  RegisterExistingDBDlg: TRegisterExistingDBDlg;

implementation

{$R *.lfm}

{ TRegisterExistingDBDlg }

procedure TRegisterExistingDBDlg.FormShow(Sender: TObject);
begin
  DatabaseName.SetFocus;
end;

function TRegisterExistingDBDlg.ShowModal(aServerName: string): TModalResult;
begin
  ServerName.Text := aServerName;
  DatabaseName.Text := '';
  DatabasePath.Text := '';
  DefaultUserName.Text := '';
  Result := inherited ShowModal;
end;

end.

