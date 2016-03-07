{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclibx;

interface

uses
  IBDBReg, IBEventsEditor, IBTransactionEdit, IBDatabaseEdit, IBSystemTables, 
  ibdeletesqleditor, IBGeneratorEditor, ibinsertsqleditor, ibmodifysqleditor, 
  ibselectsqleditor, IBUpdateSQLEditor, IBSQLEditor, IBDataSetEditor, 
  dbFieldLinkPropEditor, dbFieldListPropEditor, IBDSDialogs, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IBDBReg', @IBDBReg.Register);
end;

initialization
  RegisterPackage('dclibx', @Register);
end.
