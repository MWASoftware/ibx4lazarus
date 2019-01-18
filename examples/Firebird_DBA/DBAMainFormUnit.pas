unit DBAMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, DBCtrls, Menus, ActnList, db, SynEdit, SynHighlighterSQL,
  MainFormUnit, IBLookupComboEditBox, IBDynamicGrid, IBTreeView, IBDatabaseInfo,
  IBExtract;

type

  { TDBAMainForm }

  TDBAMainForm = class(TMainForm)
    IBTreeView1: TIBTreeView;
    Splitter6: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  DBAMainForm: TDBAMainForm;

implementation

{$R *.lfm}

{ TDBAMainForm }

procedure TDBAMainForm.FormShow(Sender: TObject);
begin
  LocalDatabase.Connected := true;
  inherited;
end;

procedure TDBAMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  LocalDatabase.Connected := false;
  inherited;
end;

end.

