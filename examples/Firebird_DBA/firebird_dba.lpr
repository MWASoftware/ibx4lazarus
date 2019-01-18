program firebird_dba;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DBAMainFormUnit, ibexpress
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDBAMainForm, DBAMainForm);
  Application.CreateForm(TRegisterExistingDBDlg, RegisterExistingDBDlg);
  Application.CreateForm(TRegisterServerDlg, RegisterServerDlg);
  Application.CreateForm(TCreateNewDBDlg, CreateNewDBDlg);
  Application.Run;
end.

