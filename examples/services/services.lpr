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
  AltDBSvcLoginDlgUnit,NewUserDlgUnit, ChgPasswordDlgUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
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
  Application.Run;
end.

