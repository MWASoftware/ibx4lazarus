unit RegisterExistingDBDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ServerDataUnit;

type

  { TRegisterExistingDBDlg }

  TRegisterExistingDBDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    UsesAltSecDB: TCheckBox;
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
    FServerData: TServerData;

  public
    function ShowModal(aServerData: TServerData): TModalResult;
    property ServerData: TServerData read FServerData;
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

function TRegisterExistingDBDlg.ShowModal(aServerData: TServerData
  ): TModalResult;
begin
  FServerData := aServerData;
  if aServerData = nil then
    ServerName.Text := ''
  else
    ServerName.Text := FServerData.ServerName;
  DatabaseName.Text := '';
  DatabasePath.Text := '';
  DefaultUserName.Text := 'SYSDBA';
  UsesAltSecDB.Checked := false;
  Result := inherited ShowModal;
end;

end.

