unit DBAExecuteSQLScriptDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, 
    ExecuteSQLScriptDlgUnit;

type

  { TDBAExecuteSQLScriptDlg }

  TDBAExecuteSQLScriptDlg = class(TExecuteSQLScriptDlg)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DBAExecuteSQLScriptDlg: TDBAExecuteSQLScriptDlg;

implementation

{$R *.lfm}

{ TDBAExecuteSQLScriptDlg }

procedure TDBAExecuteSQLScriptDlg.FormCreate(Sender: TObject);
begin
  ExecuteSQLScriptDlg := self;
end;

end.

