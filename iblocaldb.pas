{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iblocaldb;

interface

uses
  IBLocalDBSupport, NewDatabaseUnit, SaveDatabaseUnit, UpdateDatabaseUnit, 
  ViewLogDlgUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('iblocaldb', @Register);
end.
