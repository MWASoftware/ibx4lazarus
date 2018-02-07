program servicesdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  ibexpress, Forms, memdslaz, Unit1, Unit2, Unit3, ListUsersUnit,
  LimboTransactionsUnit, NewUserDlgUnit, ChgPasswordDlgUnit,
  ServicesLoginDlgUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TListUsersForm, ListUsersForm);
  Application.CreateForm(TLimboTransactionsForm, LimboTransactionsForm);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TSvcLoginDlg, SvcLoginDlg);
  Application.Run;
end.

