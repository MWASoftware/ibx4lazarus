unit DBABackupDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BackupDlgUnit;

type

  { TDBABackupDlg }

  TDBABackupDlg = class(TBackupDlg)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DBABackupDlg: TDBABackupDlg;

implementation

{$R *.lfm}

{ TDBABackupDlg }

procedure TDBABackupDlg.FormCreate(Sender: TObject);
begin
  BackupDlg := self;
end;

end.

