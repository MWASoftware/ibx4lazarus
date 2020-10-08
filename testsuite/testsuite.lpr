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
  Test04, Test05, Test23, Test14, Test12, Test30, Test02, Test06, Test11,
  Test13, Test07, Test08, Test24, Test16, Test17, Test19, Test18, Test20,
  TestApplication;

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

