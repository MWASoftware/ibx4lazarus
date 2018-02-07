unit ListUsersUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ActnList, Menus, db, memds, IBServices, IBCustomDataSet, IBDatabase,
  IBDynamicGrid;

type

  { TListUsersForm }

  TListUsersForm = class(TForm)
    Button4: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveChanges: TAction;
    DeleteUser: TAction;
    ChangePassword: TAction;
    AddUser: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    IBDynamicGrid1: TIBDynamicGrid;
    IBSecurityService1: TIBSecurityService;
    Label1: TLabel;
    UserList: TMemDataset;
    UserListSource: TDataSource;
    procedure AddUserExecute(Sender: TObject);
    procedure ChangePasswordExecute(Sender: TObject);
    procedure ChangePasswordUpdate(Sender: TObject);
    procedure DeleteUserExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
    procedure UserListAfterInsert(DataSet: TDataSet);
    procedure UserListAfterOpen(DataSet: TDataSet);
    procedure UserListAfterPost(DataSet: TDataSet);
    procedure UserListBeforeClose(DataSet: TDataSet);
    procedure UserListBeforeDelete(DataSet: TDataSet);
    procedure UserListBeforePost(DataSet: TDataSet);
  private
    { private declarations }
    FLoading: boolean;
    procedure DoRefresh(Data: PtrInt);
  public
    { public declarations }
  end;

var
  ListUsersForm: TListUsersForm;

implementation

{$R *.lfm}

uses NewUserDlgUnit, ChgPasswordDlgUnit;

{ TListUsersForm }

procedure TListUsersForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh,0);
end;

procedure TListUsersForm.SaveChangesExecute(Sender: TObject);
begin
  UserList.Post;
end;

procedure TListUsersForm.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserList.State in [dsInsert,dsEdit];
end;

procedure TListUsersForm.UserListAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('UserID').AsInteger := 0;
  DataSet.FieldByName('GroupID').AsInteger := 0;
  DataSet.FieldByName('Admin').AsBoolean := false;
  DataSet.FieldByName('Password').Clear;
end;

procedure TListUsersForm.AddUserExecute(Sender: TObject);
var NewUserName: string;
    NewPassword: string;
begin
  NewUserName := '';
  if NewUserDlg.ShowModal(NewUserName,NewPassword) = mrOK then
  with UserList do
  begin
    Append;
    FieldByName('UserName').AsString := NewUserName;
    FieldByName('Password').AsString := NewPassword;
  end;
end;

procedure TListUsersForm.ChangePasswordExecute(Sender: TObject);
var NewPassword: string;
begin
  NewPassword := '';
  if ChgPasswordDlg.ShowModal(NewPassword) = mrOK then
  with UserList do
  begin
    Edit;
    FieldByName('Password').AsString := NewPassword;
  end;
end;

procedure TListUsersForm.ChangePasswordUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserList.Active and (UserList.RecordCount > 0);
end;

procedure TListUsersForm.DeleteUserExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you really want delete user %s',[UserList.FieldByName('UserName').AsString]),
        mtConfirmation,[mbYes,mbNo],0) = mrYes then
    UserList.Delete;
end;

procedure TListUsersForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  UserList.Active := false;
end;

procedure TListUsersForm.UserListAfterOpen(DataSet: TDataSet);
var i: integer;
begin
  with IBSecurityService1 do
  begin
    IBSecurityService1.Active := true;
    IBDynamicGrid1.Columns[6].ReadOnly := not HasAdminRole;
    DisplayUsers;
    FLoading := true;
    try
      for i := 0 to UserInfoCount - 1 do
      with UserInfo[i],UserList do
      begin
        Append;
        FieldByName('UserID').AsInteger := UserID;
        FieldByName('GroupID').AsInteger := GroupID;
        FieldByName('UserName').AsString := UserName;
        FieldByName('FirstName').AsString := FirstName;
        FieldByName('MiddleName').AsString := MiddleName;
        FieldByName('LastName').AsString := LastName;
        FieldByName('Password').Clear;
        FieldByName('Admin').AsBoolean := AdminRole;
        Post;
      end;
    finally
      IBSecurityService1.Active := false;
      FLoading := false;
    end;
  end;
end;

procedure TListUsersForm.UserListAfterPost(DataSet: TDataSet);
begin
  if not FLoading then
    Application.QueueAsyncCall(@DoRefresh,0);
end;

procedure TListUsersForm.UserListBeforeClose(DataSet: TDataSet);
begin
  with UserList do
  begin
    if State in [dsEdit,dsInsert] then Post;
    Clear(false);
  end;
end;

procedure TListUsersForm.UserListBeforeDelete(DataSet: TDataSet);
begin
  with IBSecurityService1 do
  begin
    Active := true;
    try
      UserName := UserList.FieldByName('UserName').AsString;
      DeleteUser;
    finally
      Active := false;
    end;
  end;
end;

procedure TListUsersForm.UserListBeforePost(DataSet: TDataSet);
  procedure SetParams;
  begin
    with UserList, IBSecurityService1 do
    begin
      UserID := FieldByName('UserID').AsInteger;
      GroupID := FieldByName('GroupID').AsInteger;
      UserName := FieldByName('UserName').AsString;
      FirstName := FieldByName('FirstName').AsString;
      MiddleName := FieldByName('MiddleName').AsString;
      LastName := FieldByName('LastName').AsString;
      if not FieldByName('Password').IsNull then
        Password := FieldByName('Password').AsString;
      AdminRole := FieldByName('Admin').AsBoolean;
    end;
  end;

 begin
    if FLoading then Exit;
    IBSecurityService1.Active := true;
    case UserList.State of
    dsEdit:
      begin
        SetParams;
        IBSecurityService1.ModifyUser;
      end;
    dsInsert:
      begin
        SetParams;
        IBSecurityService1.AddUser;
      end;
    end;
    IBSecurityService1.Active := false;
end;

procedure TListUsersForm.DoRefresh(Data: PtrInt);
begin
  UserList.Active := false;
  UserList.Active := true;
end;

end.

