unit DBARestoreDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, RestoreDlgUnit;

type

  { TDBARestoreDlg }

  TDBARestoreDlg = class(TRestoreDlg)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DBARestoreDlg: TDBARestoreDlg;

implementation

{$R *.lfm}

{ TDBARestoreDlg }

procedure TDBARestoreDlg.FormCreate(Sender: TObject);
begin
  RestoreDlg := self;
end;

end.

