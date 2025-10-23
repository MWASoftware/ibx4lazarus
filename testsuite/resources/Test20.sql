Create Table IBDataSetTest (
  KeyField Integer not null,
  PlainText VarChar(128),
  TextAndKey Computed By (KeyField || ': ' || PlainText),
  ServerSideText VarChar(256),
  Primary Key (KeyField)
);

Create Generator AGenerator;

Create Trigger BeforeInsertTest for IBDataSetTest
Before Insert or Update
As
Begin
  new.ServerSideText = new.KeyField || ' - ' || new.PlainText;
End

Create Table IBXTest2 (
 Key1 integer not null,
 Key2 integer not null,
 primary key(key1,key2)
);
