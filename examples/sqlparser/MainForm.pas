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
  Parser := TSelectSQLParser.Create(nil,OriginalSQL.Lines);
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

