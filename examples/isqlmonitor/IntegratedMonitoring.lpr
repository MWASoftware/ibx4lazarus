program IntegratedMonitoring;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, SelectDeptDlgUnit, ibexpress, MonitorFormUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSelectDeptDlg, SelectDeptDlg);
  Application.CreateForm(TMonitorForm, MonitorForm);
  Application.Run;
end.

