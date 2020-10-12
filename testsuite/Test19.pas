unit Test19;

{$mode objfpc}{$H+}

{Test 19: Master/Detail Queries}

{ Description
  Two IBQueries are opened in a master detail relationship. The master is scrolled
  and the secondary shown to follow suite.
}

interface

uses
  Classes, SysUtils, CustApp,  TestApplication, IBXTestBase, DB, IB, IBQuery,
  fpTimer, IBInternals;

const
  aTestID    = '19';
  aTestTitle = 'TIBDataset Master/Detail Queries';

type

{ TTest19 }

  TTest19 = class(TIBXTestBase)
  private
    FDetailQuery: TIBQuery;
    FMasterSource: TDataSource;
    procedure InitDetailQuery(Application: TTestApplication);
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;

  { TNonGUITimer }

  TNonGUITimer = class(TInterfacedObject,IIBTimerInf)
  private
    FTimer: TFPTimer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetEnabled: boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);
    function GetOnStartTimer: TNotifyEvent;
    procedure SetOnStartTimer(Value: TNotifyEvent);
    function GetOnStopTimer: TNotifyEvent;
    procedure SetOnStopTimer(Value: TNotifyEvent);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property OnStartTimer: TNotifyEvent read GetOnStartTimer write SetOnStartTimer;
    property OnStopTimer: TNotifyEvent read GetOnStopTimer write SetOnStopTimer;
  end;

  { TDummyGUIInterface }

  TDummyGUIInterface = class(TInterfacedObject,IIBGUIInterface)
  public
    function ServerLoginDialog(var AServerName: string;
                               var AUserName, APassword: string): Boolean;
    function LoginDialogEx(var ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean;
    procedure SetCursor;
    procedure RestoreCursor;
    function CreateTimer: IIBTimerInf;
  end;


implementation

{ TDummyGUIInterface }

function TDummyGUIInterface.ServerLoginDialog(var AServerName: string;
  var AUserName, APassword: string): Boolean;
begin
  Result := false;
end;

function TDummyGUIInterface.LoginDialogEx(var ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
begin
  Result := false;
end;

procedure TDummyGUIInterface.SetCursor;
begin

end;

procedure TDummyGUIInterface.RestoreCursor;
begin

end;

function TDummyGUIInterface.CreateTimer: IIBTimerInf;
begin
  Result := TNonGUITimer.Create;
end;

{ TNonGUITimer }

constructor TNonGUITimer.Create;
begin
  inherited Create;
  FTimer := TFPTimer.Create(nil);
end;

destructor TNonGUITimer.Destroy;
begin
  if FTimer <> nil then FTimer.Free;
  inherited Destroy;
end;

function TNonGUITimer.GetEnabled: boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TNonGUITimer.SetEnabled(Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

function TNonGUITimer.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TNonGUITimer.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TNonGUITimer.GetOnTimer: TNotifyEvent;
begin
  Result := FTimer.OnTimer;
end;

procedure TNonGUITimer.SetOnTimer(Value: TNotifyEvent);
begin
  FTimer.OnTimer := Value;
end;

function TNonGUITimer.GetOnStartTimer: TNotifyEvent;
begin
  Result := FTimer.OnStartTimer;
end;

procedure TNonGUITimer.SetOnStartTimer(Value: TNotifyEvent);
begin
  FTimer.OnStartTimer := Value;
end;

function TNonGUITimer.GetOnStopTimer: TNotifyEvent;
begin
  Result := FTimer.OnStopTimer;
end;

procedure TNonGUITimer.SetOnStopTimer(Value: TNotifyEvent);
begin
  FTimer.OnStopTimer := Value;
end;

{ TTest19 }

procedure TTest19.InitDetailQuery(Application: TTestApplication);
begin
  FDetailQuery := TIBQuery.Create(Application);
  FDetailQuery.Database := IBDatabase;
  FDetailQuery.Transaction := IBTransaction;
  FDetailQuery.SQL.Text := 'Select * From EMPLOYEE Where DEPT_NO = :DEPT_NO';
  FDetailQuery.DataSource := FMasterSource;
end;

procedure TTest19.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  IBQuery.SQL.Text := 'Select * From DEPARTMENT';
  FMasterSource := TDataSource.Create(Application);
  FMasterSource.DataSet := IBQuery;
  InitDetailQuery(Application);
end;

function TTest19.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest19.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest19.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetEmployeeDatabaseName;
  ReadOnlyTransaction;
end;

procedure TTest19.RunTest(CharSet: AnsiString; SQLDialect: integer);
begin
  IBDatabase.Connected := true;
  try
    IBTransaction.Active := true;
    IBQuery.Active := true;
    FDetailQuery.Active := true;
    writeln(Outfile,'Department Record:');
    PrintDataSetRow(IBQuery);
    writeln(Outfile);
    writeln(Outfile,'Employee Records:');
    PrintDataSet(FDetailQuery);
    writeln(Outfile);
    writeln(OutFile,'Advance to next department');
    IBQuery.Next;
    writeln(Outfile,'Department Record:');
    PrintDataSetRow(IBQuery);
    writeln(Outfile);
    writeln(Outfile,'Employee Records:');
    PrintDataSet(FDetailQuery);
    FDetailQuery.Active := false;
    IBQuery.Active := false;

    FreeAndNil(FDetailQuery);
    IBGUIInterface := TDummyGUIInterface.Create;
    try
      InitDetailQuery(Owner);
      FDetailQuery.MasterDetailDelay := 500;
      writeln(Outfile,'Repeat test with an active master/detail timer');
      IBQuery.Active := true;
      FDetailQuery.Active := true;
      writeln(Outfile,'Department Record:');
      PrintDataSetRow(IBQuery);
      writeln(Outfile);
      writeln(Outfile,'Employee Records:');
      PrintDataSet(FDetailQuery);
      writeln(Outfile);
      writeln(OutFile,'Advance to next department');
      IBQuery.Next;
      writeln(Outfile,'Department Record:');
      PrintDataSetRow(IBQuery);
      writeln(Outfile);
      writeln(Outfile,'Employee Records:');
      writeln(Outfile,'Result - before checksynchronise');
      PrintDataSet(FDetailQuery);
      CheckSynchronize(5000);
      writeln(Outfile,'Result - after checksynchronise');
      PrintDataSet(FDetailQuery);
    finally
      IBGUIInterface := nil;
    end;
  finally
    IBDatabase.Connected := false;
  end;
end;

initialization
  RegisterTest(TTest19);

end.

