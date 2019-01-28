program firebird_dba;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, DBAMainFormUnit, ibexpress, DBADataModule, LocalDataModule,
  RegisterServerDlgUnit, RegisterExistingDBDlgUnit, CreateNewDBDlgUnit,
  SelectSQLResultsUnit, DBLoginDlgUnit, ShutdownRegDlgUnit,
  ShutdownDatabaseDlgUnit, BackupDlgUnit, RestoreDlgUnit, AddSecondaryFileDlgUnit,
  AddShadowSetDlgUnit, AddShadowFileDlgUnit, NewUserDlgUnit, ChgPasswordDlgUnit,
  ExecuteSQLScriptDlgUnit, ServerPropertiesDlgUnit
 { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TLocalData, LocalData);
  Application.CreateForm(TDBAMainForm, DBAMainForm);
  Application.CreateForm(TDBADatabaseData, DBADatabaseData);
  Application.CreateForm(TRegisterExistingDBDlg, RegisterExistingDBDlg);
  Application.CreateForm(TRegisterServerDlg, RegisterServerDlg);
  Application.CreateForm(TCreateNewDBDlg, CreateNewDBDlg);
  Application.CreateForm(TAddSecondaryFileDlg, AddSecondaryFileDlg);
  Application.CreateForm(TAddShadowFileDlg, AddShadowFileDlg);
  Application.CreateForm(TAddShadowSetDlg, AddShadowSetDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TExecuteSQLScriptDlg, ExecuteSQLScriptDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TServerPropertiesDlg, ServerPropertiesDlg);
  Application.Run;
end.

