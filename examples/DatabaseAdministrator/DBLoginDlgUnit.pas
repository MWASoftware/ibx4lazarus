unit DBLoginDlgUnit;

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
  { TDBLoginDlg }

  TDBLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    DatabaseName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(var aDatabaseName, aUserName, aPassword: string
      ): TModalResult;
  end;

var DBLoginDlg: TDBLoginDlg;

implementation

{$R *.lfm}

{ TDBLoginDlg }

function TDBLoginDlg.ShowModal(var aDatabaseName, aUserName, aPassword: string
  ): TModalResult;
begin
  DatabaseName.Text := aDatabaseName;
  UserName.Text := aUserName;
  Password.Text := '';
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDatabaseName := DatabaseName.Text;
    aUserName := UserName.Text;
    aPassword := Password.Text;
  end;
end;


end.
