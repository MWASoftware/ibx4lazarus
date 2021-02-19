Create Table IBXTest (
 TableKey Integer not null,
 F1 Timestamp with Time Zone,
 F2 Time with Time Zone,
 F3 DecFloat(16),
 F4 DecFloat(34),
 F5 Numeric (24,6),
 F6 INT128,
 Primary Key(TableKey)
) SQL SECURITY DEFINER;

ALTER DATABASE SET DEFAULT SQL SECURITY DEFINER;

Create Generator IBXGen;
