(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iblocaldb;

interface

uses
  IBXCustomIBLocalDBSupport, IBXCreateDatabaseDlg, IBXSaveDatabaseDlg, 
  IBXUpgradeDatabaseDlg, IBXViewLogDig, IBLocalDBSupport, IBXUpgradeConfFile, 
  IBXCreateDatabaseFromSQLDlgUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('iblocaldb', @Register);
end.
