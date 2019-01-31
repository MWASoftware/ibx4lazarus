unit DBAShutdownDatabaseDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, 
    ShutdownDatabaseDlgUnit;

type

  { TDBAShutdownDatabaseDlg }

  TDBAShutdownDatabaseDlg = class(TShutdownDatabaseDlg)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DBAShutdownDatabaseDlg: TDBAShutdownDatabaseDlg;

implementation

{$R *.lfm}

{ TDBAShutdownDatabaseDlg }

procedure TDBAShutdownDatabaseDlg.FormCreate(Sender: TObject);
begin
  ShutdownDatabaseDlg := self;
end;

end.

