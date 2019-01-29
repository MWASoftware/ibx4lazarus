unit DatabasePropertiesDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, 
    RegisterExistingDBDlgUnit, DatabaseDataUnit;

type

  { TDatabasePropertiesDlg }

  TDatabasePropertiesDlg = class(TRegisterExistingDBDlg)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FDatabaseData: TDatabaseData;
  public
    function ShowModal(DatabaseData: TDatabaseData): TModalResult;
  end;

var
  DatabasePropertiesDlg: TDatabasePropertiesDlg;

implementation

{$R *.lfm}

{ TDatabasePropertiesDlg }

procedure TDatabasePropertiesDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    FDatabaseData.DatabaseName := DatabaseName.Text;
    FDatabaseData.DatabasePath := DatabasePath.Text;
    FDatabaseData.DefaultUserName := DefaultUserName.Text;
    FDatabaseData.UsesDefaultSecDatabase := not UsesAltSecDB.Checked;
  end;
end;

procedure TDatabasePropertiesDlg.FormShow(Sender: TObject);
begin
  inherited;
  DatabaseName.Text := FDatabaseData.DatabaseName;
  DatabasePath.Text := FDatabaseData.DatabasePath;
  DefaultUserName.Text := FDatabaseData.DefaultUserName;
  UsesAltSecDB.Checked := not FDatabaseData.UsesDefaultSecDatabase;
end;

function TDatabasePropertiesDlg.ShowModal(DatabaseData: TDatabaseData
  ): TModalResult;
begin
  FDatabaseData := DatabaseData;
  Result := inherited ShowModal(DatabaseData.ServerData);
end;

end.

