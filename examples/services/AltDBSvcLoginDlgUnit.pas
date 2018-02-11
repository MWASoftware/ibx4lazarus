unit AltDBSvcLoginDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ServicesLoginDlgUnit;

type

  { TAltDBSvcLoginDlg }

  TAltDBSvcLoginDlg = class(TSvcLoginDlg)
    Label1: TLabel;
  private

  public

  end;

var
  AltDBSvcLoginDlg: TAltDBSvcLoginDlg;

implementation

{$R *.lfm}

end.

