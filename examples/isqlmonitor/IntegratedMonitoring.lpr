program IntegratedMonitoring;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, SelectDeptDlgUnit, ibexpress, MonitorFormUnit;

{$R *.res}

begin   Application.Title := 'Integrated Monitoring Example';

{
  if you want to open a console window in Windows (for writeln debugging
  enabled the following

   AllocConsole;      // in Windows unit
   IsConsole := True; // in System unit
   SysInitStdIO;      // in System unit
    }

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSelectDeptDlg, SelectDeptDlg);
  Application.CreateForm(TMonitorForm, MonitorForm);
  Application.Run;
end.

