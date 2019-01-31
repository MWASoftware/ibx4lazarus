program firebird_dba;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, DBAMainFormUnit, ibexpress, DBADataModule, LocalDataModule,
  RegisterServerDlgUnit, RegisterExistingDBDlgUnit, CreateNewDBDlgUnit,
  SelectSQLResultsUnit, ShutdownRegDlgUnit, ShutdownDatabaseDlgUnit,
  BackupDlgUnit, RestoreDlgUnit, AddSecondaryFileDlgUnit, AddShadowSetDlgUnit,
  AddShadowFileDlgUnit, NewUserDlgUnit, ChgPasswordDlgUnit,
  ExecuteSQLScriptDlgUnit, DBLoginDlgUnit, ServerPropertiesDlgUnit,
  DatabasePropertiesDlgUnit, DBABackupDlgUnit, DBACreateDatabaseDlgUnit,
  DBAShutdownDatabaseDlgUnit, DBAAddShadowSetDlgUnit,
  DBAExecuteSQLScriptDlgUnit, DBARestoreDlgUnit
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
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TServerPropertiesDlg, ServerPropertiesDlg);
  Application.CreateForm(TDatabasePropertiesDlg, DatabasePropertiesDlg);
  Application.CreateForm(TDBABackupDlg, DBABackupDlg);
  Application.CreateForm(TDBAShutdownDatabaseDlg, DBAShutdownDatabaseDlg);
  Application.CreateForm(TDBAAddShadowSetDlg, DBAAddShadowSetDlg);
  Application.CreateForm(TDBAExecuteSQLScriptDlg, DBAExecuteSQLScriptDlg);
  Application.CreateForm(TDBARestoreDlg, DBARestoreDlg);
  Application.Run;
end.

