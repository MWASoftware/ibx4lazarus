unit ViewLogDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TViewLogDlg }

  TViewLogDlg = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(Log: TStrings): TModalResult;
  end; 

var
  ViewLogDlg: TViewLogDlg;

function ShowViewLogDlg(Log: TStrings): TModalResult;

implementation

function ShowViewLogDlg(Log: TStrings): TModalResult;
begin
  with TViewLogDlg.Create(Application) do
  try
    Result := ShowModal(Log);
  finally
    Free
  end;
end;

{$R *.lfm}

{ TViewLogDlg }

function TViewLogDlg.ShowModal(Log: TStrings): TModalResult;
begin
  Memo1.Lines.Assign(Log);
  Result := inherited ShowModal;
  Memo1.Lines.Clear
end;

end.

