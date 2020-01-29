program testsuite;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$codepage utf8}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, TestManager, IB, Firebird, Test01, IBXTestManager,
  Test02;

type

  { TIBXTestSuite }

  TIBXTestSuite = class(TCustomApplication)
  private
    FTestID: AnsiString;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TIBXTestSuite }

procedure TIBXTestSuite.DoRun;
var
  ErrorMsg: String;
  DoPrompt: boolean;
begin
  TestMgr.Application := self;
  OutFile := stdout;
  // quick check parameters
  ErrorMsg := CheckOptions('htupensbolrSPX', 'help test user passwd employeedb newdbname secondnewdbname backupfile outfile fbclientlibrary server stats port prompt');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('t') then
    FTestID := GetOptionValue('t');

  DoPrompt := HasOption('X','prompt');

  if TestMgr <> nil then
  begin
    if HasOption('u','user') then
      TestMgr.SetUserName(GetOptionValue('u'));

    if HasOption('p','passwd') then
      TestMgr.SetPassword(GetOptionValue('p'));

    if HasOption('e','employeedb') then
      TestMgr.SetEmployeeDatabaseName(GetOptionValue('e'));

    if HasOption('n','newdbname') then
      TestMgr.SetNewDatabaseName(GetOptionValue('n'));

    if HasOption('s','secondnewdbname') then
      TestMgr.SetSecondNewDatabaseName(GetOptionValue('s'));

    if HasOption('b','backupfile') then
      TestMgr.SetBackupFileName(GetOptionValue('b'));

    if HasOption('l','fbclientlibrary') then
      TestMgr.SetClientLibraryPath(GetOptionValue('l'));

    if HasOption('r','server') then
      TestMgr.SetServerName(GetOptionValue('r'));

    if HasOption('o','outfile') then
    begin
      system.Assign(outFile,GetOptionValue('o'));
      ReWrite(outFile);
    end;

    if HasOption('P','port') then
      TestMgr.SetPortNum(GetOptionValue('P'));

    TestMgr.ShowStatistics := HasOption('S','stats');
    {$IF declared(SetTextCodePage)}
    {Ensure consistent UTF-8 output}
    SetTextCodePage(OutFile,cp_utf8);
    {$IFEND}

    {Ensure consistent date reporting across platforms}
    DefaultFormatSettings.ShortDateFormat := 'yyyy/m/d';
    DefaultFormatSettings.LongTimeFormat := 'HH:MM:SS';
    DefaultFormatSettings.DateSeparator := '/';

    writeln(OutFile,Title);
    writeln(OutFile,'Copyright MWA Software 2020');
    writeln(OutFile);
    writeln(OutFile,'Starting Tests');
    writeln(OutFile,'Client API Version = ',FirebirdAPI.GetImplementationVersion);
    writeln(OutFile,'Firebird Environment Variable = ',GetEnvironmentVariable('FIREBIRD'));
    if TestMgr.FirebirdAPI.GetClientMajor >= 3 then
    begin
      writeln(OutFile,'Firebird Bin Directory = ', IMaster(TestMgr.FirebirdAPI.GetIMaster).getConfigManager.getDirectory(IConfigManager.DIR_BIN));
      writeln(OutFile,'Firebird Conf Directory = ', IMaster(TestMgr.FirebirdAPI.GetIMaster).getConfigManager.getDirectory(IConfigManager.DIR_CONF));
    end;

    try
      if FTestID = '' then
        TestMgr.RunAll
      else
        TestMgr.Run(FTestID);
    except on E: Exception do
      writeln('Exception: ',E.Message);
    end;
    TestMgr.Free;
  end;

  writeln(OutFile,'Test Suite Ends');
  Flush(OutFile);
  {$IFDEF WINDOWS}
  if DoPrompt then
  begin
    write('Press Entry to continue');
    readln; {uncomment if running from IDE and console window closes before you can view results}
  end;
  {$ENDIF}

  // stop program loop
  Terminate;
end;

constructor TIBXTestSuite.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

procedure TIBXTestSuite.WriteHelp;
begin
  { add your help code here }
  writeln(OutFile,'Usage: ', ExeName, ' -h');
end;

var
  Application: TIBXTestSuite;
begin
  Application := TIBXTestSuite.Create(nil);
  Application.Title := 'IBX Test Suite';
  Application.Run;
  Application.Free;
end.

