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
  private

  public

  end;

var
  DBAMainForm: TDBAMainForm;

implementation

{$R *.lfm}

end.

