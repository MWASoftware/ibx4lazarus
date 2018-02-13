unit ServicesLoginDlgUnit;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, StdCtrls, ExtCtrls, Buttons, IB, IBDialogs;

type
  { TSvcLoginDlg }

  TSvcLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    ServerName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(aServerName: string; var aUserName, aPassword: string
      ): TModalResult;
  end;

var SvcLoginDlg: TSvcLoginDlg;

implementation

{$R *.lfm}

{ TSvcLoginDlg }

function TSvcLoginDlg.ShowModal(aServerName: string; var aUserName,
  aPassword: string): TModalResult;
begin
  ServerName.Text := aServerName;
  UserName.Text := aUserName;
  Password.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aUserName := UserName.Text;
    aPassword := Password.Text;
  end;
end;


end.
