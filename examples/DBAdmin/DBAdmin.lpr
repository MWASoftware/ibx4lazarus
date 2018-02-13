program DBAdmin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, MainFormUnit, DataModule, ibexpress,
  DBLoginDlgUnit, ShutdownRegDlgUnit, ShutdownDatabaseDlgUnit, BackupDlgUnit,
  RestoreDlgUnit, AddSecondaryFileDlgUnit, AddShadowSetDlgUnit, AddShadowFileDlgUnit,
  ServicesLoginDlgUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDatabaseData, DatabaseData);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TAddSecondaryFileDlg, AddSecondaryFileDlg);
  Application.CreateForm(TAddShadowFileDlg, AddShadowFileDlg);
  Application.CreateForm(TAddShadowSetDlg, AddShadowSetDlg);
  Application.CreateForm(TSvcLoginDlg, SvcLoginDlg);
  Application.Run;
end.

