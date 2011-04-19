unit dblogindlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TIBXLoginDlg }

  TIBXLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    DatabaseName: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

function LoginDialogEx(const ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
var
  IBXLoginDlg: TIBXLoginDlg;

implementation

{$R *.lfm}

function LoginDialogEx(const ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
begin
  with TIBXLoginDlg.Create(Application) do
  try
    DatabaseName.Caption := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if NameReadOnly then
      UserName.Enabled := False
    else
      if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end
  finally
    Free;
  end;
end;


end.

