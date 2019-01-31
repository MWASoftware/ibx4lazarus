unit CreateNewDBDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, db, IBLookupComboEditBox, RegisterExistingDBDlgUnit;

type

  { TCreateNewDBDlg }

  TCreateNewDBDlg = class(TRegisterExistingDBDlg)
    AppDBSource: TDataSource;
    AppDBLookup: TIBLookupComboEditBox;
    Label5: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  end;

var
  CreateNewDBDlg: TCreateNewDBDlg;

implementation

uses Variants;

{$R *.lfm}

{ TCreateNewDBDlg }

procedure TCreateNewDBDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if VarIsNull(AppDBLookup.KeyValue) then
  begin
    MessageDlg('An Application Database must be selected',mtError,[mbOK],0);
    CloseAction := caNone;
  end;
end;

end.

