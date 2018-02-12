program services;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, ibexpress,
  ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit,
  AltDBSvcLoginDlgUnit,NewUserDlgUnit, ChgPasswordDlgUnit, BringOnlineDlgUnit,
  ShutdownDatabaseDlgUnit, ShutdownRegDlgUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAltDBSvcLoginDlg, AltDBSvcLoginDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TLimboTransactionsForm, LimboTransactionsForm);
  Application.CreateForm(TListUsersForm, ListUsersForm);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TSvcLoginDlg, SvcLoginDlg);
  Application.CreateForm(TSelectValidationDlg, SelectValidationDlg);
  Application.CreateForm(TSelectDBDlg, SelectDBDlg);
  Application.CreateForm(TBringOnlineDlg, BringOnlineDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.Run;
end.

