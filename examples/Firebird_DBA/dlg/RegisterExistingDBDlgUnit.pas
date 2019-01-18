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
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(aServerName: string;
            var aDatabaseName, aDatabasePath, aUserName: string): TModalResult;
  end;

var
  RegisterExistingDBDlg: TRegisterExistingDBDlg;

implementation

{$R *.lfm}

{ TRegisterExistingDBDlg }

procedure TRegisterExistingDBDlg.FormShow(Sender: TObject);
begin
  Edit2.SetFocus;
end;

function TRegisterExistingDBDlg.ShowModal(aServerName: string;
  var aDatabaseName, aDatabasePath, aUserName: string): TModalResult;
begin
  Edit1.Text := aServerName;
  Edit2.Text := aDatabaseName;
  Edit3.Text := aDatabasePath;
  Edit4.Text := aUserName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDatabaseName := Edit2.Text;
    aDatabasePath := Edit3.Text;
    aUserName := Edit4.Text;
  end;
end;

end.

