{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclibx;

interface

uses
  IBDBReg, IBEventsEditor, IBTransactionEdit, IBDatabaseEdit, 
  ibdeletesqleditor, IBGeneratorEditor, ibinsertsqleditor, ibmodifysqleditor, 
  ibselectsqleditor, ibupdatesqleditor, ibsqleditor, dbFieldLinkPropEditor, 
  dbFieldListPropEditor, IBDSDialogs, IBSQLEditFrame, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IBDBReg', @IBDBReg.Register);
end;

initialization
  RegisterPackage('dclibx', @Register);
end.
