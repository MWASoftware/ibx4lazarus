unit SelectValidationDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TSelectValidationDlg }

  TSelectValidationDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OnlineValidation: TRadioButton;
    FullValidation: TRadioButton;
  private

  public
    function ShowModal(aServerName: string;
      var DatabaseName: string; var aOnlineValidation: boolean): TModalResult;
  end;

var
  SelectValidationDlg: TSelectValidationDlg;

implementation

{$R *.lfm}

{ TSelectValidationDlg }

function TSelectValidationDlg.ShowModal(aServerName: string;
  var DatabaseName: string; var aOnlineValidation: boolean): TModalResult;
begin
  Edit1.Text := DatabaseName;
  Edit2.text := aServerName;
  OnlineValidation.checked := aOnlineValidation;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    DatabaseName := Edit1.Text;
    aOnlineValidation := OnlineValidation.checked;
  end;
end;

end.

