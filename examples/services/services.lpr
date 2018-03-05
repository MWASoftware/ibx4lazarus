(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
program services;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, MainFormUnit, ibexpress,
  ServicesLoginDlgUnit, SelectValidationDlgUnit, SelectDBDlgUnit,
  BackupDlgUnit, RestoreDlgUnit,  ListUsersUnit, LimboTransactionsUnit,
  NewUserDlgUnit, ChgPasswordDlgUnit,
  ShutdownDatabaseDlgUnit, ShutdownRegDlgUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TLimboTransactionsForm, LimboTransactionsForm);
  Application.CreateForm(TListUsersForm, ListUsersForm);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TSvcLoginDlg, SvcLoginDlg);
  Application.CreateForm(TSelectValidationDlg, SelectValidationDlg);
  Application.CreateForm(TSelectDBDlg, SelectDBDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.Run;
end.

