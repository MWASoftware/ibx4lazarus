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
            
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    HavingAllUnions: TCheckBox;
    Button1: TButton;
    WhereAllUnions: TCheckBox;
    WhereCondition: TEdit;
    HavingCondition: TEdit;
    HavingConditionType: TRadioGroup;
    OrderBy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OriginalSQL: TMemo;
    GeneratedSQL: TMemo;
    WhereConditionType: TRadioGroup;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses IBSQLParser;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var Parser: TSelectSQLParser;
begin
  Parser := TSelectSQLParser.Create(OriginalSQL.Lines);
  try
    if WhereCondition.Text <> '' then
      Parser.Add2WhereClause(WhereCondition.Text,WhereConditionType.ItemIndex <> 0,WhereAllUnions.Checked);
    if HavingCondition.Text <> '' then
      Parser.Add2HavingClause(HavingCondition.Text,HavingConditionType.ItemIndex <> 0,HavingAllUnions.Checked);
    if OrderBy.Text <> ''then
      Parser.OrderByClause := OrderBy.Text;
    GeneratedSQL.Lines.Text := Parser.SQLText
  finally
  end;
end;

end.

