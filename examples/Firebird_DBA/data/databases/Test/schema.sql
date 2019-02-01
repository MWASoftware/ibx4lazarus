Create Table TEST_DB (
  KeyName VarChar(32),
  KeyValue VarChar(32),
  Primary Key (KeyName)
);

CREATE TABLE "DBVERSIONINFO" 
(
  "VERSIONNO"	SMALLINT DEFAULT 0,
  "UUID"	CHAR(16) CHARACTER SET OCTETS
);
