unit TestTemplate;

{$mode objfpc}{$H+}

{Test 1: Titlecursor}

{ Description
}

interface

uses
  Classes, SysUtils, CustApp, TestManager, IBXTestManager, IB;

const
  aTestID    = '1';
  aTestTitle = 'Test 1';

type

{ TTest1 }

  TTest1 = class(TIBXTestBase)
  protected
    procedure CreateObjects(Application: TCustomApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest1 }

procedure TTest1.CreateObjects(Application: TCustomApplication);
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

