unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ExtCtrls, DbCtrls, StdCtrls, ComCtrls, db, IBDatabase, IBQuery,
  IBCustomDataSet, IBUpdate, IBSQL, IBDatabaseInfo, IBDynamicGrid, IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    DBEdit5: TDBEdit;
    DisconnectUser: TAction;
    DropConnection: TIBSQL;
    IBDatabaseInfo: TIBDatabaseInfo;
    Label6: TLabel;
    MenuItem5: TMenuItem;
    RoleNameListGRANTED: TBooleanField;
    SaveChanges: TAction;
    ActionList1: TActionList;
    AddUser: TAction;
    ApplyUserChange: TIBSQL;
    Button1: TButton;
    ChgPassword: TAction;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBQuerySource: TDataSource;
    DBEdit1: TDBEdit;
    DBText1: TDBText;
    DeleteUser: TAction;
    IBDatabase1: TIBDatabase;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    DatabaseQuery: TIBQuery;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MenuItem1: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    UpdateUsers: TIBUpdate;
    UpdateUserRoles: TIBUpdate;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RoleNameList: TIBQuery;
    RoleNameListRDBDESCRIPTION: TIBMemoField;
    RoleNameListRDBOWNER_NAME: TIBStringField;
    RoleNameListRDBPRIVILEGE: TIBStringField;
    RoleNameListRDBROLE_NAME: TIBStringField;
    RoleNameListRDBSECURITY_CLASS: TIBStringField;
    RoleNameListRDBSYSTEM_FLAG: TSmallintField;
    RoleNameListRDBUSER: TIBStringField;
    RoleNameListUSERNAME: TIBStringField;
    RoleSource: TDataSource;
    Splitter1: TSplitter;
    UserList: TIBQuery;
    UserListCURRENT_CONNECTION: TIBLargeIntField;
    UserListDBCREATOR: TBooleanField;
    UserListLOGGEDIN: TBooleanField;
    UserListMONATTACHMENT_ID: TIBLargeIntField;
    UserListMONREMOTE_HOST: TIBStringField;
    UserListROLENAME: TIBStringField;
    UserListSECACTIVE: TBooleanField;
    UserListSECADMIN: TBooleanField;
    UserListSECDESCRIPTION: TIBMemoField;
    UserListSECFIRST_NAME: TIBStringField;
    UserListSECLAST_NAME: TIBStringField;
    UserListSECMIDDLE_NAME: TIBStringField;
    UserListSECPLUGIN: TIBStringField;
    UserListSource: TDataSource;
    UserListUSERNAME: TIBStringField;
    UserListUSERPASSWORD: TIBStringField;
    procedure AddUserExecute(Sender: TObject);
    procedure AddUserUpdate(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ChgPasswordExecute(Sender: TObject);
    procedure ChgPasswordUpdate(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DeleteUserExecute(Sender: TObject);
    procedure DisconnectUserExecute(Sender: TObject);
    procedure DisconnectUserUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1CreateDatabase(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure IBTransaction1AfterTransactionEnd(Sender: TObject);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
    procedure UpdateUserRolesApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
    procedure UpdateUsersApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure UserListAfterInsert(DataSet: TDataSet);
    procedure UserListAfterOpen(DataSet: TDataSet);
    procedure UserListAfterPost(DataSet: TDataSet);
    procedure UserListBeforeClose(DataSet: TDataSet);
  private
      FIsAdmin: boolean;
     FDisconnecting: boolean;
     procedure DoReopen(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses NewUserDlgUnit, ChgPasswordDlgUnit, DBLoginDlgUnit, FBMessages;

{ TForm1 }

procedure TForm1.UpdateUsersApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

  function FormatStmtOptions: string;
  var Param: ISQLParam;
  begin
    Result := Trim(Params.ByName('UserName').AsString);
    Param := Params.ByName('USERPASSWORD');
    if (Param <> nil) and not Param.IsNull  then
      Result += ' PASSWORD ''' + Param.AsString + '''';
    Param := Params.ByName('SEC$FIRST_NAME');
    if Param <> nil then
      Result += ' FIRSTNAME ''' + Param.AsString + '''';
    Param := Params.ByName('SEC$MIDDLE_NAME');
    if Param <> nil then
      Result += ' MIDDLENAME ''' + Param.AsString + '''';
    Param := Params.ByName('SEC$LAST_NAME');
    if Param <> nil then
      Result += ' LASTNAME ''' + Param.AsString + '''';
    Param := Params.ByName('SEC$ACTIVE');
    if Param <> nil then
    begin
      if Param.AsBoolean then
        Result += ' ACTIVE'
      else
        Result += ' INACTIVE';
    end;
  end;

{  procedure ShowParams;
  var i: integer;
  begin
    for i := 0 to Params.Count - 1 do
      writeln(Params[i].Name,' = ',Params[i].AsString);
  end; }

begin
//  ShowParams;
  begin
    case UpdateKind of
    ukInsert:
      ApplyUserChange.SQL.Text := 'CREATE USER ' + FormatStmtOptions;
    ukModify:
        ApplyUserChange.SQL.Text := 'ALTER USER ' + FormatStmtOptions;
    ukDelete:
      ApplyUserChange.SQL.Text := 'DROP USER ' + Trim(Params.ByName('UserName').AsString);
    end;
    ApplyUserChange.ExecQuery;
  end;
  if UpdateKind = ukModify then
  {Update Admin Role if allowed}
  begin
    if Params.ByName('SEC$ADMIN').AsBoolean and not Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ApplyUserChange.SQL.Text := 'ALTER USER ' + Trim(Params.ByName('UserName').AsString) + ' GRANT ADMIN ROLE';
      ApplyUserChange.ExecQuery;
    end
    else
    if not Params.ByName('SEC$ADMIN').AsBoolean and Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ApplyUserChange.SQL.Text := 'ALTER USER ' + Trim(Params.ByName('UserName').AsString) + ' REVOKE ADMIN ROLE';
      ApplyUserChange.ExecQuery;
    end
  end;
  {Update DB Creator Role}
  if Params.ByName('DBCreator').AsBoolean and not Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ApplyUserChange.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + Trim(Params.ByName('UserName').AsString);
    ApplyUserChange.ExecQuery;
  end
  else
  if not Params.ByName('DBCreator').AsBoolean and Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ApplyUserChange.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + Trim(Params.ByName('UserName').AsString);
    ApplyUserChange.ExecQuery;
  end
end;

procedure TForm1.UserListAfterInsert(DataSet: TDataSet);
begin
  UserList.FieldByName('SEC$ADMIN').AsBoolean := false;
  UserList.FieldByName('SEC$ACTIVE').AsBoolean := false;
end;

procedure TForm1.UserListAfterOpen(DataSet: TDataSet);
begin
  RoleNameList.Active := true;
end;

procedure TForm1.UserListAfterPost(DataSet: TDataSet);
begin
  IBTransaction1.Commit;
end;

procedure TForm1.UserListBeforeClose(DataSet: TDataSet);
begin
  RoleNameList.Active := false;
end;

procedure TForm1.DoReopen(Data: PtrInt);
begin
  if not FDisconnecting and IBDatabase1.Connected then
  begin
    IBTransaction1.Active := true;
    DatabaseQuery.Active := true;
    UserList.Active := true;
  end;
end;

procedure TForm1.UpdateUserRolesApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

  procedure Grant(Params: ISQLParams);
  begin
    ApplyUserChange.SQL.Text := 'Grant ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' to ' + Params.ByName('USERNAME').AsString;
    ApplyUserChange.ExecQuery;
  end;

  procedure Revoke(Params: ISQLParams);
  begin
    ApplyUserChange.SQL.Text := 'Revoke ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' from ' + Params.ByName('USERNAME').AsString;
    ApplyUserChange.ExecQuery;
  end;

begin
  if UpdateKind = ukModify then
  begin
    if Params.ByName('GRANTED').AsInteger = 0 then
      Revoke(Params)
    else
      Grant(Params);
  end;
end;

procedure TForm1.AddUserExecute(Sender: TObject);
var NewUserName: string;
    NewPassword: string;
begin
  NewUserName := '';
  if NewUserDlg.ShowModal(NewUserName,NewPassword) = mrOK then
  with UserList do
  begin
    Append;
    FieldByName('UserName').AsString := NewUserName;
    FieldByName('USERPASSWORD').AsString := NewPassword;
  end;
end;

procedure TForm1.AddUserUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FIsAdmin;
end;

procedure TForm1.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  if E is EIBInterBaseError then
  begin
    if RoleNameList.State in [dsInsert,dsEdit] then
      RoleNameList.Cancel;
    if UserList.State in [dsInsert,dsEdit] then
      UserList.Cancel;
  end;
  MessageDlg(E.Message,mtError,[mbOK],0);
  Application.QueueAsyncCall(@DoReopen,0);
end;

procedure TForm1.ChgPasswordExecute(Sender: TObject);
var NewPassword: string;
begin
  NewPassword := '';
  if ChgPasswordDlg.ShowModal(NewPassword) = mrOK then
  with UserList do
  begin
    Edit;
    FieldByName('USERPASSWORD').AsString := NewPassword;
  end;
end;

procedure TForm1.ChgPasswordUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FIsAdmin and UserList.Active and (UserList.RecordCount > 0);
end;

procedure TForm1.DatabaseQueryAfterOpen(DataSet: TDataSet);
begin
  FIsAdmin := DatabaseQuery.FieldByName('SEC$ADMIN').AsBoolean;
  IBDynamicGrid1.Columns[1].ReadOnly := not FIsAdmin;
  IBDynamicGrid1.Columns[2].ReadOnly := not FIsAdmin;
  IBDynamicGrid1.Columns[3].ReadOnly := not FIsAdmin;
  IBDynamicGrid1.Columns[4].ReadOnly := not FIsAdmin;
  IBDynamicGrid1.Columns[5].ReadOnly := not FIsAdmin;
  IBDynamicGrid1.Columns[6].ReadOnly := not FIsAdmin;
  StatusBar1.SimpleText := 'Logged in to ' + IBDatabase1.DatabaseName + ' as user ' +
       DatabaseQuery.FieldByName('UserName').AsString;
  if FIsAdmin then
    StatusBar1.SimpleText := StatusBar1.SimpleText + ' with Administrator Privileges';
end;

procedure TForm1.DeleteUserExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete user ' + Trim(UserList.FieldByName('UserName').AsString),
     mtConfirmation,[mbYes,mbNo],0) = mrYes then
     UserList.Delete;
end;

procedure TForm1.DisconnectUserExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to disconnect user ' + Trim(UserList.FieldByName('UserName').AsString),
     mtConfirmation,[mbYes,mbNo],0) = mrYes then
  with DropConnection do
  begin
    ParamByName('ID').AsInteger := UserList.FieldByName('MON$ATTACHMENT_ID').AsInteger;
    ExecQuery;
    Transaction.Commit;
  end;
end;

procedure TForm1.DisconnectUserUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserList.Active and (UserList.RecordCount > 0) and
     not UserList.FieldByName('MON$ATTACHMENT_ID').IsNull and
     (UserList.FieldByName('MON$ATTACHMENT_ID').AsInteger <> UserList.FieldByName('CURRENT_CONNECTION').AsInteger);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IBDatabase1.ForceClose;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(self);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Close;
        Exit
      end;
    On E:Exception do
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  DoReopen(0);
end;

procedure TForm1.IBDatabase1AfterDisconnect(Sender: TObject);
begin
  FDisconnecting := false;
end;

procedure TForm1.IBDatabase1BeforeDisconnect(Sender: TObject);
begin
  FDisconnecting := true;
end;

procedure TForm1.IBDatabase1CreateDatabase(Sender: TObject);
begin
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    IBDatabase1.DropDatabase;
    raise EIBClientError.Create(0,'This example requires Firebird 3');
  end
end;

procedure TForm1.IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
begin
  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword) = mrOK then
  begin
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TForm1.IBTransaction1AfterTransactionEnd(Sender: TObject);
begin
  if not FDisconnecting then
    Application.QueueAsyncCall(@DoReopen,0);
end;

procedure TForm1.SaveChangesExecute(Sender: TObject);
begin
  IBTransaction1.Commit;
end;

procedure TForm1.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserList.State in [dsInsert,dsEdit]) or
                                 (RoleNameList.State in [dsInsert,dsEdit]);
end;

end.

