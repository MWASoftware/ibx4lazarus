unit ShutdownRegDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IBServices;

type

  { TShutdownReqDlg }

  TShutdownReqDlg = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    DatabaseName: TEdit;
    Delay: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OKBtn: TButton;
    ShutdownOptions: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(const aDatabaseName: string; var aShutDownmode: TShutdownMode;
      var aDelay: integer): TModalResult;
  end;

var
  ShutdownReqDlg: TShutdownReqDlg;

implementation

{$R *.lfm}

{ TShutdownReqDlg }

procedure TShutdownReqDlg.FormShow(Sender: TObject);
begin
  Delay.Text := '60';
end;

function TShutdownReqDlg.ShowModal(const aDatabaseName: string;
  var aShutDownmode: TShutdownMode; var aDelay: integer): TModalResult;
begin
  ShutdownOptions.ItemIndex := ord(aShutDownmode);
  DatabaseName.Text := aDatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDelay := StrToInt(Delay.Text);
    aShutDownmode := TShutdownMode(ShutdownOptions.ItemIndex);
  end;
end;

end.

