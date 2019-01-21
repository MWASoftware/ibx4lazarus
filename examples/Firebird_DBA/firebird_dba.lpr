program firebird_dba;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, DBAMainFormUnit, ibexpress, DBADataModule
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
  Application.CreateForm(TAddSecondaryFileDlg, AddSecondaryFileDlg);
  Application.CreateForm(TAddShadowFileDlg, AddShadowFileDlg);
  Application.CreateForm(TLocalData, LocalData);
  Application.CreateForm(TDatabaseData, DatabaseData);
  Application.CreateForm(TAddShadowSetDlg, AddShadowSetDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TExecuteSQLScriptDlg, ExecuteSQLScriptDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TDBADatabaseData, DBADatabaseData);
  Application.Run;
end.

