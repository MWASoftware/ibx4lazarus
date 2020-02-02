program testsuite;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage utf8}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, TestApplication, IB, Test01, IBXTestManager,
  Test02, Test03;

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

