unit ListUsersUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, IBServices;

type

  { TListUsersForm }

  TListUsersForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    IBSecurityService1: TIBSecurityService;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
  private
    { private declarations }
    procedure DoRefresh(Data: PtrInt);
    procedure UpdateUser(row: integer);
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

procedure TListUsersForm.Button1Click(Sender: TObject);
begin
  if MessageDlg(Format('Do you really want delete user %s',[StringGrid1.Cells[2,StringGrid1.row]]),
        mtConfirmation,[mbYes,mbNo],0) = mrYes then
  with IBSecurityService1 do
  begin
    UserName := StringGrid1.Cells[2,StringGrid1.row];
    DeleteUser;
    Application.QueueAsyncCall(@DoRefresh,0);
  end;
end;

procedure TListUsersForm.Button2Click(Sender: TObject);
var NewUserName: string;
    NewPassword: string;
    NewRow: integer;
begin
  NewUserName := '';
  if InputQuery('Add New User','Enter UserName',NewUserName) and
     InputQuery('Add New User ','Enter password',true,NewPassword) then
  with IBSecurityService1 do
  begin
    UserName := NewUserName;
    Password := NewPassword;
    AddUser;
    Application.QueueAsyncCall(@DoRefresh,0);
  end;
end;

procedure TListUsersForm.Button3Click(Sender: TObject);
var NewPassword: string;
begin
  NewPassword := '';
  if InputQuery('Change Password for user ' + StringGrid1.Cells[2,StringGrid1.row],
           'Enter new password',true,NewPassword) then
  begin
    IBSecurityService1.Password := NewPassword;
    UpdateUser(StringGrid1.row);
  end;
end;

procedure TListUsersForm.StringGrid1EditingDone(Sender: TObject);
begin
  if StringGrid1.RowCount > 1 then
    UpdateUser(StringGrid1.row);
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

procedure TListUsersForm.UpdateUser(row: integer);
begin
  with IBSecurityService1 do
  begin
    UserID := StrToInt(StringGrid1.Cells[0,row]);
    GroupID := StrToInt(StringGrid1.Cells[1,row]);
    UserName := StringGrid1.Cells[2,row];
    FirstName := StringGrid1.Cells[3,row];
    MiddleName := StringGrid1.Cells[4,row];
    LastName := StringGrid1.Cells[5,row];
    ModifyUser
  end;
end;

end.

