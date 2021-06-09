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
unit TestTemplate;

{$mode objfpc}{$H+}

{Test 1: Titlecursor}

{ Description
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, IB;

const
  aTestID    = '1';
  aTestTitle = 'Test 1';

type

{ TTest1 }

  TTest1 = class(TIBXTestBase)
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest1 }

procedure TTest1.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
end;

function TTest1.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest1.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest1.InitTest;
begin
end;

procedure TTest1.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
end;

initialization
  RegisterTest(TTest1);

end.

