unit ListUsersUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, IBServices;

type

  { TListUsersForm }

  TListUsersForm = class(TForm)
    IBSecurityService1: TIBSecurityService;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
  private
    { private declarations }
    procedure DoRefresh(Data: PtrInt);
  public
    { public declarations }
  end;

var
  ListUsersForm: TListUsersForm;

implementation

{$R *.lfm}

{ TListUsersForm }

procedure TListUsersForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh,0);
end;

procedure TListUsersForm.StringGrid1EditingDone(Sender: TObject);
begin
  writeln('Editing Done ', StringGrid1.row);
end;

procedure TListUsersForm.DoRefresh(Data: PtrInt);
var i: integer;
begin
  with IBSecurityService1 do
  begin
    Active := true;
    DisplayUsers;
    StringGrid1.RowCount := UserInfoCount + 1;
    for i := 0 to UserInfoCount - 1 do
    with UserInfo[i] do
    begin
      StringGrid1.Cells[0,i+1] := IntToStr(UserID);
      StringGrid1.Cells[1,i+1] := IntToStr(GroupID);
      StringGrid1.Cells[2,i+1] := UserName;
      StringGrid1.Cells[3,i+1] := FirstName;
      StringGrid1.Cells[4,i+1] := MiddleName;
      StringGrid1.Cells[5,i+1] := LastName;
    end;
  end;
end;

end.

