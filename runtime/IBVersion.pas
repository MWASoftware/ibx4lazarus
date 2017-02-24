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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBVersion;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage UTF8}
{$IF FPC_FULLVERSION < 30000 }
{$ERROR FPC Version 3.0.0 or later is required}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils;

const
  IBX_MAJOR = 2;
  IBX_MINOR = 0;
  IBX_RELEASE = 2;
  IBX_VERSION = '2.0.2';

implementation

end.

