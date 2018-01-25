unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ExtCtrls, DbCtrls, StdCtrls, db, IBDatabase, IBQuery, IBCustomDataSet,
  IBUpdate, IBSQL, IBDatabaseInfo, IBDynamicGrid, IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBEdit5: TDBEdit;
    DisconnectUser: TAction;
    DropConnection: TIBSQL;
    IBDatabaseInfo: TIBDatabaseInfo;
    Label6: TLabel;
    MenuItem5: TMenuItem;
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
    UpdateUsers: TIBUpdate;
    UpdateUserRoles: TIBUpdate;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RoleNameList: TIBQuery;
    RoleNameListGRANTED: TIntegerField;
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
    procedure ChgPasswordExecute(Sender: TObject);
    procedure ChgPasswordUpdate(Sender: TObject);
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
  var sql: string;
  begin
    sql := 'Grant ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' to ' + Params.ByName('USERNAME').AsString;
    with Sender as TIBUpdate do
    (DataSet.Database as TIBDatabase).Attachment.ExecImmediate(DataSet.Transaction.TransactionIntf,sql);
  end;

  procedure Revoke(Params: ISQLParams);
  var sql: string;
  begin
    sql := 'Revoke ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' from ' + Params.ByName('USERNAME').AsString;
    with Sender as TIBUpdate do
    (DataSet.Database as TIBDatabase).Attachment.ExecImmediate(DataSet.Transaction.TransactionIntf,sql);
  end;

begin
  case UpdateKind of
  ukModify:
    if Params.ByName('GRANTED').AsInteger = 0 then
      Revoke(Params)
    else
      Grant(Params);
  ukInsert:
    Grant(Params);
  ukDelete:
    Revoke(Params);
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
  (Sender as TAction).Enabled := UserList.Active and (UserList.RecordCount > 0);
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
  IBDatabase1.Connected := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(self);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
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

