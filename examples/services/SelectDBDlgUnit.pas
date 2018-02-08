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
    UseAltSecDB: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
  private

  public
    function ShowModal(var DatabaseName: string; var IsExpectedDB: boolean): TModalResult;

  end;

var
  SelectDBDlg: TSelectDBDlg;

implementation

{$R *.lfm}

{ TSelectDBDlg }

function TSelectDBDlg.ShowModal(var DatabaseName: string;
  var IsExpectedDB: boolean): TModalResult;
begin
  Edit1.Text := DatabaseName;
  UseAltSecDB.Checked := IsExpectedDB;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
   DatabaseName := Edit1.Text;
   IsExpectedDB := UseAltSecDB.Checked;
  end;

end;

end.

