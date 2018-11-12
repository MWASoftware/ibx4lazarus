(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
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
  Application.Title := 'Integrated Monitoring Example';

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

