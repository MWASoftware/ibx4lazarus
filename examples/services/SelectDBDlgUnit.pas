unit SelectDBDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TSelectDBDlg }

  TSelectDBDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
  private

  public
    function ShowModal(var DatabaseName: string): TModalResult;

  end;

var
  SelectDBDlg: TSelectDBDlg;

implementation

{$R *.lfm}

{ TSelectDBDlg }

function TSelectDBDlg.ShowModal(var DatabaseName: string): TModalResult;
begin
  Edit1.Text := DatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
   DatabaseName := Edit1.Text;
  end;

end;

end.

