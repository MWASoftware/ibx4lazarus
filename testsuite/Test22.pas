(*
 *  IBX Test suite. This program is used to test the IBX non-visual
 *  components and provides a semi-automated pass/fail check for each test.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)      
unit Test22;

{$mode objfpc}{$H+}

{Test 22: TIBUpdate Tests}

{ This test uses TIBUpdate to allow a list of database users to be presented
  as a table and edited using normal insert/edit/delete/post methods.
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, DB, IB, IBSQL, IBUpdate,
  IBQuery, IBCustomDataset;

const
  aTestID    = '22';
  aTestTitle = 'TIBUpdate Tests';

type

{ TTest22 }

  TTest22 = class(TIBXTestBase)
  private
    FIBUpdate: TIBUpdate;
    ExecDDL: TIBSQL;
    procedure UserListAfterInsert(DataSet: TDataSet);
    procedure UpdateUsersApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
    function SkipTest: boolean; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

uses IBUtils;

const
  UsersQuery =
    'Select A.SEC$DESCRIPTION, Trim(A.SEC$PLUGIN) as SEC$PLUGIN, A.SEC$ADMIN, '+
    'A.SEC$ACTIVE, Trim(A.SEC$USER_NAME) as SEC$USER_NAME, '+
    'Trim(A.SEC$FIRST_NAME) as SEC$FIRST_NAME, '+
    'Trim(A.SEC$MIDDLE_NAME) as SEC$MIDDLE_NAME, '+
    'Trim(A.SEC$LAST_NAME) as SEC$LAST_NAME, '+
    'cast(NULL as VarChar(32)) as SEC$PASSWORD, '+
    'case when Count(B.MON$ATTACHMENT_ID) > 0 then true else false end as LoggedIn, '+
    'case When C.SEC$USER is not null then true else false end as DBCreator '+
    'From SEC$USERS A '+
    'Left Outer Join MON$ATTACHMENTS B '+
    'On A.SEC$USER_NAME = B.MON$USER '+
    'Left Outer Join SEC$DB_CREATORS  C on C.SEC$USER = A.SEC$USER_NAME';
  UsersQueryGroupBy =
    'Group By A.SEC$DESCRIPTION, A.SEC$PLUGIN, A.SEC$ADMIN, '+
    'A.SEC$ACTIVE, A.SEC$USER_NAME, A.SEC$MIDDLE_NAME, '+
    'A.SEC$FIRST_NAME, A.SEC$LAST_NAME, C.SEC$USER';

{ TTest22 }

procedure TTest22.UserListAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('SEC$ADMIN').AsBoolean := false;
  DataSet.FieldByName('SEC$ACTIVE').AsBoolean := false;
  DataSet.FieldByName('DBCreator').AsBoolean := false;
  DataSet.FieldByName('SEC$PLUGIN').AsString := 'Srp';
  DataSet.FieldByName('SEC$PASSWORD').Clear;
end;

procedure TTest22.UpdateUsersApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

var UserName: string;

  function FormatStmtOptions: string;
  var Param: ISQLParam;
  begin
    Result := UserName;
    Param := Params.ByName('SEC$PASSWORD');
    if (Param <> nil) and not Param.IsNull  then
      Result += ' PASSWORD ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$FIRST_NAME');
    if Param <> nil then
      Result += ' FIRSTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$MIDDLE_NAME');
    if Param <> nil then
      Result += ' MIDDLENAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$LAST_NAME');
    if Param <> nil then
      Result += ' LASTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$ACTIVE');
    if Param <> nil then
    begin
      if Param.AsBoolean then
        Result += ' ACTIVE'
      else
        Result += ' INACTIVE';
    end;
    Param := Params.ByName('SEC$PLUGIN');
    if Param <> nil then
      Result += ' USING PLUGIN ' + QuoteIdentifierIfNeeded((Sender as TIBUpdate).DataSet.Database.SQLDialect,Param.AsString);
  end;

  function GetAlterPasswordStmt: string;
  var Param: ISQLParam;
  begin
    Result := '';
    Param := Params.ByName('SEC$PASSWORD');
    if (UpdateKind = ukModify) and not Param.IsNull then
    begin
      Result := 'ALTER USER ' + UserName +
          ' PASSWORD ''' + SQLSafeString(Param.AsString) + '''';
      Param := Params.ByName('SEC$PLUGIN');
     if Param <> nil then
       Result += ' USING PLUGIN ' + QuoteIdentifierIfNeeded((Sender as TIBUpdate).DataSet.Database.SQLDialect,Param.AsString);
    end;
  end;

begin
  UserName := Trim(Params.ByName('SEC$USER_NAME').AsString);
  {non SYSDBA user not an RDB$ADMIN can only change their password}
  if (Owner.GetUserName <> 'SYSDBA') and (RoleName <> 'RDB$ADMIN') then
  begin
   ExecDDL.SQL.Text := GetAlterPasswordStmt;
   if ExecDDL.SQL.Text <> '' then
     ExecDDL.ExecQuery;
   Exit;
  end;

  case UpdateKind of
  ukInsert:
      ExecDDL.SQL.Text := 'CREATE USER ' + FormatStmtOptions;
  ukModify:
      ExecDDL.SQL.Text := 'ALTER USER ' + FormatStmtOptions;
  ukDelete:
    ExecDDL.SQL.Text := 'DROP USER ' + UserName;
  end;
//  writeln(OutFile,'Query Text = ',ExecDDL.SQL.Text);
  ExecDDL.ExecQuery;

  if UpdateKind = ukInsert then
  begin
    {if new user is also given the admin role then we need to add this}
    if Params.ByName('SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end;
    if Params.ByName('DBCreator').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + UserName;
      ExecDDL.ExecQuery;
    end
    else
    begin
      ExecDDL.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + UserName;
      ExecDDL.ExecQuery;
    end;
  end
  else
  if UpdateKind = ukModify then
  {Update Admin Role if allowed}
  begin
    if Params.ByName('SEC$ADMIN').AsBoolean and not Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end
    else
    if not Params.ByName('SEC$ADMIN').AsBoolean and Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' REVOKE ADMIN ROLE';
      ExecDDL.ExecQuery;
    end;

    {Update DB Creator Role}
    if Params.ByName('DBCreator').AsBoolean and not Params.ByName('OLD_DBCreator').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + UserName;
      ExecDDL.ExecQuery;
    end
    else
    if not Params.ByName('DBCreator').AsBoolean and Params.ByName('OLD_DBCreator').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + UserName;
      ExecDDL.ExecQuery;
    end;
  end;
end;

procedure TTest22.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBUpdate := TIBUpdate.Create(Application);
  FIBUpdate.RefreshSQL.Text := UsersQuery + ' Where  A.SEC$USER_NAME = :SEC$USER_NAME ' + UsersQueryGroupBy;
  FIBUpdate.OnApplyUpdates := @UpdateUsersApplyUpdates;
  IBQuery.SQL.Text := UsersQuery + ' ' + UsersQueryGroupBy;
  IBQuery.AfterInsert:= @UserListAfterInsert;
  IBQuery.UpdateObject := FIBUpdate;
  IBQuery.AutoCommit := acCommitRetaining;
  ExecDDL := TIBSQL.Create(Application);
  ExecDDL.Database := IBDatabase;
  ExecDDL.Transaction := IBTransaction;
end;

function TTest22.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest22.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest22.InitTest;
begin
  inherited InitTest;
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadWriteTransaction;
end;

function TTest22.SkipTest: boolean;
begin
  Result := FirebirdAPI.GetClientMajor < 3;
  if Result then
    writeln(OutFile,'Skipping ',TestTitle);
end;

procedure TTest22.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  IBTransaction.Active := true;
  try
     writeln(Outfile,'RoleName = ',RoleName);
     IBQuery.Active := true;
     writeln(Outfile,'User List');
     PrintDataSet(IBQuery);
     writeln(Outfile,'Add a user');
     with IBQuery do
     begin
       Append;
       FieldByName('SEC$USER_NAME').AsString := 'TESTER';
       FieldByName('SEC$FIRST_NAME').AsString := 'Chief';
       FieldByName('SEC$LAST_NAME').AsString := 'Tester';
       FieldByName('SEC$PASSWORD').AsString := 'LetMeIn';
       Post;
       IBTransaction.Commit;
       IBTransaction.Active := true;
       Active := true;
     end;
     writeln(Outfile,'Updated User List');
     PrintDataSet(IBQuery);
     writeln(Outfile,'Modify a User');
     with IBQuery do
     if Locate('SEC$USER_NAME','TESTER',[]) then
     begin
       Edit;
       FieldByName('SEC$MIDDLE_NAME').AsString := 'Database';
       FieldByName('DBCreator').AsBoolean := true;
       Post;
       IBTransaction.Commit;
       IBTransaction.Active := true;
       Active := true;
     end
     else
       writeln(Outfile,'Error: unable to located new user');
     writeln(Outfile,'Updated User List');
     PrintDataSet(IBQuery);
     writeln(Outfile,'Delete a user');
     with IBQuery do
       if Locate('SEC$USER_NAME','TESTER',[]) then
         Delete;
     IBTransaction.Commit;
     IBTransaction.Active := true;
     IBQuery.Active := true;
     writeln(Outfile,'Updated User List');
     PrintDataSet(IBQuery);
  finally
    IBDatabase.ReConnect;
    IBTransaction.Active := true;
    with IBQuery do
    begin {make sure user is removed}
      Active := true;
      if Locate('SEC$USER_NAME','TESTER',[]) then
        Delete;
      IBTransaction.Commit;
    end;
    IBDatabase.Connected := false;
  end;
end;

initialization
  RegisterTest(TTest22);

end.

