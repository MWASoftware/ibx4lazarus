{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ibcontrols;

interface

uses
  IBLookupComboEditBox, IBDynamicGrid, IBTreeView, DBControlGrid, IBArrayGrid, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ibcontrols', @Register);
end.
