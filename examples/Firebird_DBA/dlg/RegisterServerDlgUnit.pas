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
    LocalBtn: TRadioButton;
    RemoteBtn: TRadioButton;
    ServerName: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure RemoteBtnChange(Sender: TObject);
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
  LocalBtn.Checked := true;
  RemoteBtnChange(nil);
  ServerName.SetFocus;
end;

procedure TRegisterServerDlg.RemoteBtnChange(Sender: TObject);
begin
  DomainName.Enabled := RemoteBtn.Checked;
  if not DomainName.Enabled then
    DomainName.Text := '';
end;

end.

