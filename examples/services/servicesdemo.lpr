program servicesdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  ibexpress, Forms, memdslaz, MainFormUnit, BackupDlgUnit, RestoreDlgUnit, ListUsersUnit,
  LimboTransactionsUnit, NewUserDlgUnit, ChgPasswordDlgUnit,
  ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit, DatabasePropertiesUnit,
  DBLoginDlgUnit, AltDBSvcLoginDlgUnit
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
  Application.CreateForm(TDatabaseProperties, DatabaseProperties);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TAltDBSvcLoginDlg, AltDBSvcLoginDlg);
  Application.Run;
end.

