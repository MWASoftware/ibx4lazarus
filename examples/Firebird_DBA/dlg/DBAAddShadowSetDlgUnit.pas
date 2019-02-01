unit DBAAddShadowSetDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, AddShadowSetDlgUnit;

type

  { TDBAAddShadowSetDlg }

  TDBAAddShadowSetDlg = class(TAddShadowSetDlg)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DBAAddShadowSetDlg: TDBAAddShadowSetDlg;

implementation

{$R *.lfm}

{ TDBAAddShadowSetDlg }

procedure TDBAAddShadowSetDlg.FormCreate(Sender: TObject);
begin
  AddShadowSetDlg := self;
end;

end.

