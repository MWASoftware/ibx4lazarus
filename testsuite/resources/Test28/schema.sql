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
End;

Create Table DBVersionInfo(
  VersionNo Integer,
  primary key (VersionNo)
);

INSERT INTO DBVersionInfo(VersionNo) VALUES (1);
