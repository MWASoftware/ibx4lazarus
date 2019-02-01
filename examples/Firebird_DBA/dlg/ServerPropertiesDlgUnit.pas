unit ServerPropertiesDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, RegisterServerDlgUnit,
  ServerDataUnit;

type

  { TServerPropertiesDlg }

  TServerPropertiesDlg = class(TRegisterServerDlg)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FServerData: TServerData;
  public
    function ShowModal(ServerData: TServerData): TModalResult;
  end;

var
  ServerPropertiesDlg: TServerPropertiesDlg;

implementation

{$R *.lfm}

{ TServerPropertiesDlg }

procedure TServerPropertiesDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    FServerData.ServerName := ServerName.Text;
    FServerData.DomainName := DomainName.Text;
    FServerData.DefaultUserName := DefaultUserName.Text;
  end;
end;

procedure TServerPropertiesDlg.FormShow(Sender: TObject);
begin
  ServerName.Text := FServerData.ServerName;
  DomainName.Text := FServerData.DomainName;
  LocalBtn.Checked := DomainName.Text = '';
  RemoteBtnChange(nil);
  DefaultUserName.Text := FServerData.DefaultUserName;
  if DomainName.Enabled then
    DomainName.SetFocus
  else
    ServerName.SetFocus;
end;

function TServerPropertiesDlg.ShowModal(ServerData: TServerData): TModalResult;
begin
  FServerData := ServerData;
  Result := inherited ShowModal;
end;

end.

