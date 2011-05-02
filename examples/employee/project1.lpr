program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, ibexpress, Unit2, Unit3;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TEditEmployeeDlg, EditEmployeeDlg);
  Application.CreateForm(TAddEmployeeDlg, AddEmployeeDlg);
  Application.Run;
end.

