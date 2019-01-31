Create Database '/tmp/databasemgr.fdb' Default Character Set UTF8;

Create Table Servers (
  ServerID Integer not null,
  ServerName VarChar(256),
  DomainName VarChar(256),
  DefaultUserName VarChar(32) Default 'SYSDBA',
  Primary Key (ServerID)
);

Create Sequence UniqueID;

Create Table Databases (
  DatabaseID Integer not null,
  DatabasePath VarChar(256),
  DatabaseName VarChar(64),
  DefaultUserName VarChar(32) Default 'SYSDBA',
  Server Integer not null References Servers(ServerID)
    On Update Cascade
    On Delete Cascade,
  Primary Key(DatabaseID)
);

Create Table DBCONTROL (
  VERSIONNO smallint DEFAULT 0,
  MinVersionNo smallint Default 0,
  UUID char(16) CHARACTER SET OCTETS
);

Create Table UserConfig (
  KeyName VarChar(32),
  KeyValue VarChar(128),
  Primary Key KeyName;
);

Create Table SQLHistory (
  SeqNo Smallint not null,
  SQLText Blob sub_type text,
  Primary Key(SeqNo)
);

Create Table SQLStatementGroups (
  GroupID integer not null,
  Parent integer,
  GroupName VarChar(64),
  Primary Key (GroupID)
);

Alter Table SQLStatementGroups
  Add Foreign Key (Parent) References SQLStatementGroups(GroupID)
    On Update cascade
    On Delete cascade;

Create Sequence GroupID;

Create Table SavedSQLStatements (
  StatementID integer not null,
  GroupID integer ,
  StatementName VarChar(64),
  SQLText Blob sub_type text,
  Primary Key (StatementID)
);

Alter Table SavedSQLStatements
  Add Foreign Key (GroupID) References SQLStatementGroups(GroupID)
    On Update cascade
    On Delete cascade;

Create Sequence StatementID;

Create Global Temporary Table ServerUsers (
  UserID Integer,
  GroupID Integer,
  UserName VarChar(32) not null,
  FirstName VarChar(32),
  MiddleName VarChar(32),
  LastName VarChar(32),
  LoggedIn Boolean,
  Primary Key(UserName)
) On Commit Delete Rows;

Create Global Temporary Table PasswordCache (
  ServerName VarChar(128) not null,
  DatabaseName VarChar(512),
  DBAUser VarChar(32),
  DBAPasswordIndex Integer,
  SecurityDatabase VarChar(512),
  Primary Key (DBAPasswordIndex)
) On Commit Preserve Rows;

Create View DatabasesByServer As

with recursive ServerInfo As (
  Select -1 as ItemType, -1 as ID, 'Servers' as ItemName,
    cast (Null as VarChar(256)) as DomainName, null as Parent From RDB$Database
  Union
  Select 0 as ItemType, ServerID as ID, ServerName as ItemName, DomainName, -1 as Parent
   From Servers
  Union all
  Select 1, DB.DatabaseID, DB.DatabaseName, NULL, DB.Server
    From Databases DB
    Join ServerInfo SI On SI.ID = DB.Server
    Where SI.ItemType = 0
)
Select * From ServerInfo;

Set term ^;
Create trigger Insert_DatabasesByServer 
Active before Insert on DatabasesByServer
As
Begin
  if (new.ItemType = 0) then
    Insert Into Servers(ServerID,ServerName) Values(new.ID,new.ItemName);
  else
    Insert Into Databases(DatabaseID,DatabaseName,Server) Values(new.ID,new.ItemName,new.Parent);
End^

Create Trigger Update_DatabasesByServer
Active before Update on DatabasesByServer
As
Begin
  if (new.ItemType = 0) then
  begin
    if (new.ItemName is not distinct from new.DomainName) then
      Update Servers Set ServerName = new.ItemName, DomainName = NULL, ServerID = new.ID
        Where ServerID = old.ID;
    else
      Update Servers Set ServerName = new.ItemName, DomainName = coalesce(new.DomainName,old.ItemName), ServerID = new.ID
        Where ServerID = old.ID;
  end
  else
    Update Databases Set DatabaseName = new.ItemName, DatabaseID = new.ID
      Where DatabaseID = old.ID;
End^

Create Trigger Delete_DatabasesByServer
 Active before Delete on DatabasesByServer
As
Begin
  if (old.ItemType = 0) then
    Delete From Servers Where ServerID = old.ID;
  else
    Delete From Databases Where DatabaseID = old.ID;
End^

set term ;^
Commit;

Insert into DBControl(VERSIONNO) Values(1);
Insert into SQLStatementGroups(GroupID,GroupName) Values(1,'Saved SQL Statements');
ALTER SEQUENCE GroupID restart with 10;
Commit;

