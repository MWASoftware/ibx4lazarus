unit MonitorFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBSQLMonitor;

type

  { TMonitorForm }

  TMonitorForm = class(TForm)
    IBSQLMonitor1: TIBSQLMonitor;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MonitorForm: TMonitorForm;

implementation

{$R *.lfm}

{ TMonitorForm }

procedure TMonitorForm.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
begin
  Memo1.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz',EventTime) + ': ' + EventText);
end;

procedure TMonitorForm.FormShow(Sender: TObject);
begin
  EnableMonitoring;
end;

end.

