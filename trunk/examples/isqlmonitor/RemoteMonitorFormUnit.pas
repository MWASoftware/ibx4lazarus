unit RemoteMonitorFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBSQLMonitor;

type

  { TForm1 }

  TForm1 = class(TForm)
    IBSQLMonitor1: TIBSQLMonitor;
    Memo1: TMemo;
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
begin
  Memo1.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz',EventTime) + ': ' + EventText);
  Application.ProcessMessages;
end;

end.

