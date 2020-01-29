unit TestTemplate;
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$codepage utf8}
{$ENDIF}

{Test 1: Titlecursor}

{ Description
}

interface

uses
  Classes, SysUtils, TestManager, IBXTestManager IB;

const
  aTestID    = '1';
  aTestTitle = 'Test 1';

type

{ TTest1 }

  TTest1 = class(TIBXTestBase)
  private
    procedure DoQuery(Attachment: IAttachment);
  protected
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest1 }

procedure TTest1.DoQuery(Attachment: IAttachment);
begin

end;

function TTest1.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest1.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest1.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
end;

initialization
  RegisterTest(TTest1);

end.

