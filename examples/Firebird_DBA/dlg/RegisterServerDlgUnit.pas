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
    DomainName: TEdit;
    DefaultUserName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ServerName: TEdit;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
  end;

var
  RegisterServerDlg: TRegisterServerDlg;

implementation

{$R *.lfm}

{ TRegisterServerDlg }

procedure TRegisterServerDlg.FormShow(Sender: TObject);
begin
  ServerName.Text := 'My Server';
  DomainName.Text := '';
  DefaultUserName.Text := 'SYSDBA';
  ServerName.SetFocus;
end;

procedure TRegisterServerDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
end;

end.

