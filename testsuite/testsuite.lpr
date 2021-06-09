(*
 *  IBX Test suite. This program is used to test the IBX non-visual
 *  components and provides a semi-automated pass/fail check for each test.
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
 *  The Original Code is (C) 2021 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
program testsuite;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage utf8}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, IB, Test01, IBXTestBase, Test03, Test10, Test09,
  Test04, Test05, Test23, Test14, Test15, Test12, Test02, Test06, Test11,
  Test13, Test07, Test08, Test24, Test16, Test17, Test19, Test18, Test20,
  Test21, Test22, Test25, Test26, Test27, Test28, TestApplication;

type

  { TIBXTestSuite }

  TIBXTestSuite = class(TTestApplication)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TIBXTestSuite }

constructor TIBXTestSuite.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

var
  Application: TIBXTestSuite;
begin
  Application := TIBXTestSuite.Create(nil);
  Application.Title := 'IBX Test Suite';
  Application.Run;
  Application.Free;
end.

