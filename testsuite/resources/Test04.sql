Create Table IBXTest (
 TableKey Integer not null,
 F1 Smallint,
 F2 Float,
 F3 Double Precision,
 F4 Decimal(9,3),
 F5 Decimal(18,4),
 F6 Date,
 F7 Timestamp,
 F8 Char (2),
 F9 VarChar(256),
 F10 Blob,
 F11 BigInt,
 "f12" Integer,
 F13 Time,
 F14 Blob sub_type text,
 F15 Double Precision Computed by (F2 + F3),
 MyArray Integer [0:16],
 "GRANTS" VarChar(20),
 "My Field" VarChar(32),
 "MY Field" VarChar(32),
 Primary Key(TableKey)
);

Create Generator IBXGen;
