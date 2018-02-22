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
            
unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, db,
  IBDynamicGrid, IBQuery, IBDatabase;

type

  { TSelectSQLResults }

  TSelectSQLResults = class(TForm)
    DataSource1: TDataSource;
    IBDynamicGrid1: TIBDynamicGrid;
    IBTransaction1: TIBTransaction;
    SelectQuery: TIBQuery;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Show(SelectSQLText: string);
  end;

var
  SelectSQLResults: TSelectSQLResults;

implementation

{$R *.lfm}

{ TSelectSQLResults }

procedure TSelectSQLResults.FormShow(Sender: TObject);
begin
  SelectQuery.Active := true;
end;

procedure TSelectSQLResults.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SelectQuery.ACtive := false;
  CloseAction := caFree;
end;

procedure TSelectSQLResults.Show(SelectSQLText: string);
begin
  SelectQuery.SQL.Text := SelectSQLText;
  inherited Show;
end;

end.

