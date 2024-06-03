{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ibcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  IBLookupComboEditBox, IBDynamicGrid, IBTreeView, DBControlGrid, IBArrayGrid, 
  DBTreeView, IBDynamicInterfaces, IBControlsRegisterUnit, 
  dbFieldListPropEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IBControlsRegisterUnit' , @IBControlsRegisterUnit.Register);
end;

initialization
  RegisterPackage('ibcontrols' , @Register);
end.
