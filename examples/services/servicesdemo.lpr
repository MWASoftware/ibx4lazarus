program servicesdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  ibexpress, Forms, memdslaz, Unit1, BackupDlgUnit, RestoreDlgUnit, ListUsersUnit,
  LimboTransactionsUnit, NewUserDlgUnit, ChgPasswordDlgUnit,
  ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TListUsersForm, ListUsersForm);
  Application.CreateForm(TLimboTransactionsForm, LimboTransactionsForm);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TSvcLoginDlg, SvcLoginDlg);
  Application.CreateForm(TSelectValidationDlg, SelectValidationDlg);
  Application.CreateForm(TSelectDBDlg, SelectDBDlg);
  Application.Run;
end.

