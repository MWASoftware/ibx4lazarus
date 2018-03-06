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
            
unit ListUsersUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Menus, db, IBDatabase,
  IBXServices, IBDynamicGrid;

type

  { TListUsersForm }

  TListUsersForm = class(TForm)
    Button4: TButton;
    IBXSecurityService1: TIBXSecurityService;
    UserList: TIBXServicesUserList;
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
    Label1: TLabel;
    UserListSource: TDataSource;
    procedure AddUserExecute(Sender: TObject);
    procedure ChangePasswordExecute(Sender: TObject);
    procedure ChangePasswordUpdate(Sender: TObject);
    procedure DeleteUserExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
  private
    { private declarations }
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
  UserList.Active := true;
end;

procedure TListUsersForm.SaveChangesExecute(Sender: TObject);
begin
  UserList.Post;
end;

procedure TListUsersForm.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserList.State in [dsInsert,dsEdit];
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

end.

