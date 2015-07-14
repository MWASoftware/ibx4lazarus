program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Unit2, Unit4, Unit5, ibexpress;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSelectDeptDlg, SelectDeptDlg);
  Application.CreateForm(TEditLocation, EditLocation);
  Application.CreateForm(TEditJobCode, EditJobCode);
  Application.Run;
end.

