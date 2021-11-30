unit IBJournal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBDatabase, IBUtils, IBInternals, IBSQL;

{ Database Journalling.

  This component is intended to support a client side journal of all database
  updates, inserts and deletes made by the client during a session. It also records
  the transaction each update was made under. If a database connection fails
  during a session, the connection can then be re-established and any incomplete
  transactions repeated (by re-applying the updates) when the connection is
  restored. The user can then restart from where they were.

  The database schema is required to include a control table "IBX$JOURNALS" and
  an SQL Sequence IBX$SESSIONS. These are created by the component when the
  database is opened, if they are not already present. However, it is recommended
  that they are created as an orginal part of the database schema in order to
  unnecessarily avoid each user being given sufficient priviledge to create tables
  and Sequences.

  The Journal file is saved in:

  Unix:
  $HOME/.<vendor name>/<application name>/ibxjournal<nnn>.log

  Windows:
  <User Application Data Dir>\<vendor name>\<application name>\ibxjournal<nnn>.log

  Syntax:

  Transaction Start:
  !S<transaction no.>,<session id>,TPB

  Transaction Commit (retaining) Called :
  !C[R]<transaction no.>

  Transaction Rollback (retaining) Called:
  !R[R]<transaction no.>

  Transaction Commit/Rollback (retaining) Completed:
  !E<transaction no.>

  Update/Insert/Delete
  !Q<transaction no.>,<length of query text in bytes>:<query text>

}

type

  { TIBJournal }

  TIBJournal = class(TComponent,IJournallingHook)
  private
    const DefaultVendor          = 'Snake Oil (Sales) Ltd';
    const DefaultJournalTemplate = 'Journal.%d.log';
    const sQueryJournal          = '!Q%d,%d:%s' + LineEnding;
    const sTransStartJnl         = '!S%d,%d,%s' + LineEnding;
    const sTransCommitJnl        = '!C%s%d' + LineEnding;
    const sTransRollBackJnl      = '!R%s%d' + LineEnding;
    const sTransEndJnl           = '!E%d' + LineEnding;
  private
    FApplicationName: string;
    FBase: TIBBase;
    FEnabled: boolean;
    FJournalAll: boolean;
    FJournalFileTemplate: string;
    FJournalFilePath: string;
    FJournalFileStream: TStream;
    FVendorName: string;
    FSessionId: integer;
    procedure EnsurePathExists(FileName: string);
    function GetDatabase: TIBDatabase;
    function GetJournalFilePath: string;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetEnabled(AValue: boolean);
    procedure HandleBeforeDatabaseDisconnect(Sender: TObject);
    procedure HandleDatabaseConnect(Sender: TObject);
    procedure HandleDatabaseDisconnect(Sender: TObject);
    function HasTable(TableName: string): boolean;
    procedure InitDatabaseConnection;
    procedure StopJournalling;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    {IJournallingHook}
    procedure ExecQuery(aIBSQL: TObject);
    procedure TransactionStart(Tr: TIBTransaction);
    procedure TransactionEnd(Tr: TIBTransaction; Action: TTransactionAction);
    procedure TransactionEndDone(TransactionID: integer);
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Enabled: boolean read FEnabled write SetEnabled;
    property JournalFileTemplate: string read FJournalFileTemplate write FJournalFileTemplate;
    property JournalAll: boolean read FJournalAll write FJournalAll;
    {JournalFileTemplate should include a %d where %d is replaced with the session no.}
    property VendorName: string read FVendorName write FVendorName;
    property ApplicationName: string read FApplicationName write FApplicationName;
  end;


implementation

uses IBMessages,IBXScript;

const
  sJournalTableName = 'IBX$JOURNALS';
  sqlCreateJournalTable =
    'Create Table ' + sJournalTableName + '(' +
    '  IBX$SessionID Integer not null, '+
    '  IBX$LastTransactionID Integer not null, '+
    '  IBX$USER VarChar(32) Default CURRENT_USER,'+
    '  IBX$TIMESTAMP TIMESTAMP Default CURRENT_TIMESTAMP,'+
    '  Primary Key(IBX$SessionID)' +
    ')';

  sqlCreateSequence = 'CREATE SEQUENCE IBX$SESSIONS';

  sqlGetNextSessionID = 'Select Gen_ID(IBX$SESSIONS,1) as SessionID From RDB$DATABASE';

  sqlInitJournalEntry = 'Insert into ' + sJournalTableName + '(IBX$SessionID,IBX$LastTransactionID) '+
                        'Values(?,0)';

  sqlUpdateJournalEntry = 'Update or Insert into ' + sJournalTableName + ' (IBX$SessionID,IBX$LastTransactionID) '+
                          'Values(?,?) Matching (IBX$SessionID)';

type

  { TQueryProcessor }

  TQueryProcessor=class(TSQLTokeniser)
  private
    FInString: AnsiString;
    FIndex: integer;
    FIBSQL: TIBSQL;
    function DoExecute: AnsiString;
    function GetParamValue(ParamIndex: integer): AnsiString;
  protected
    function GetChar: AnsiChar; override;
  public
    class function Execute(IBSQL: TIBSQL): AnsiString;
  end;

{ TQueryProcessor }

function TQueryProcessor.DoExecute: AnsiString;
var token: TSQLTokens;
    ParamIndex: integer;
begin
  Result := '';
  ParamIndex := 0;

  while not EOF do
  begin
    token := GetNextToken;
    case token of
    sqltPlaceHolder:
      begin
        Result := Result + GetParamValue(ParamIndex);
        Inc(ParamIndex);
      end;
    else
      Result := Result + TokenText;
    end;
  end;
end;

function TQueryProcessor.GetParamValue(ParamIndex: integer): AnsiString;
begin
  with FIBSQL.Params[ParamIndex] do
  begin
    if IsNull then
      Result := 'NULL'
    else
    case GetSQLType of
    SQL_BLOB:
      if getSubType = 1 then {string}
        Result := '''' + SQLSafeString(GetAsString) + ''''
      else
        Result := TSQLXMLReader.FormatBlob(GetAsString,getSubType);
    SQL_ARRAY:
      Result := TSQLXMLReader.FormatArray(FIBSQL.Database,getAsArray);

    SQL_VARYING,
    SQL_TEXT,
    SQL_TIMESTAMP,
    SQL_TYPE_DATE,
    SQL_TYPE_TIME,
    SQL_TIMESTAMP_TZ_EX,
    SQL_TIME_TZ_EX,
    SQL_TIMESTAMP_TZ,
    SQL_TIME_TZ:
      Result := '''' + SQLSafeString(GetAsString) + '''';
    else
      Result := GetAsString;
    end;
  end;
end;

function TQueryProcessor.GetChar: AnsiChar;
begin
  if FIndex <= Length(FInString) then
  begin
    Result := FInString[FIndex];
    Inc(FIndex);
  end
  else
    Result := #0;
end;

class function TQueryProcessor.Execute(IBSQL: TIBSQL): AnsiString;
begin
  if not IBSQL.Prepared then
    IBError(ibxeSQLClosed,[]);
  with self.Create do
  try
    FIBSQL := IBSQL;
    FInString := IBSQL.Statement.GetProcessedSQLText;
    FIndex := 1;
    Result := DoExecute;
  finally
    Free;
  end;
end;


{ TIBJournal }

procedure TIBJournal.EnsurePathExists(FileName: string);
var Path: string;
begin
  Path := ExtractFileDir(FileName);
  if (Path <> '') and not DirectoryExists(Path) then
    EnsurePathExists(Path);
  CreateDir(Path);
end;

function TIBJournal.GetDatabase: TIBDatabase;
begin
  Result := FBase.Database;
end;

function TIBJournal.GetJournalFilePath: string;
begin
  Result := Format(FJournalFileTemplate,[FSessionId]);
  if FApplicationName <> '' then
    Result := ApplicationName +  DirectorySeparator + Result
  else
  if Sysutils.ApplicationName <> '' then
    Result := Sysutils.ApplicationName +  DirectorySeparator + Result
  else
    Result := ExtractFileName(ParamStr(0));
  if VendorName <> '' then
    Result := VendorName + DirectorySeparator + Result
  else
  if Sysutils.VendorName <> '' then
    Result := SysUtils.VendorName + DirectorySeparator + Result;
  {$IFDEF UNIX}
  Result := GetUserDir + '.' + Result;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA) + Result;
  {$ENDIF}
end;

procedure TIBJournal.SetDatabase(AValue: TIBDatabase);
begin
  if AValue = FBase.Database then Exit;
  FBase.Database := AValue;
end;

procedure TIBJournal.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if not (csDesigning in ComponentState) and Database.Connected then
  begin
    if FEnabled then
    begin
      InitDatabaseConnection;
      FBase.JournalHook := self
    end
    else
      StopJournalling;
  end;
end;

procedure TIBJournal.HandleBeforeDatabaseDisconnect(Sender: TObject);
begin
  StopJournalling;
end;

procedure TIBJournal.HandleDatabaseConnect(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and Enabled then
  begin
    InitDatabaseConnection;
    FBase.JournalHook := self;
  end;
  FJournalFilePath := GetJournalFilePath;
  EnsurePathExists(FJournalFilePath);
  FJournalFileStream := TFileStream.Create(FJournalFilePath,fmCreate);
end;

procedure TIBJournal.HandleDatabaseDisconnect(Sender: TObject);
begin
  {Delete the Journal File and IBX$JOURNAL entry}
end;

function TIBJournal.HasTable(TableName: string): boolean;
begin
  Result := Database.Attachment.OpenCursorAtStart(
       'Select count(*) From RDB$RELATIONS Where RDB$RELATION_NAME = ?',
          [TableName])[0].AsInteger > 0;
end;

procedure TIBJournal.InitDatabaseConnection;
begin
  with Database.Attachment do
  begin
    if not HasTable(sJournalTableName) then
    begin
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateJournalTable);
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateSequence);
    end;
    FSessionID := OpenCursorAtStart(sqlGetNextSessionID)[0].AsInteger;
    ExecuteSQL([isc_tpb_write,isc_tpb_nowait,isc_tpb_concurrency],sqlInitJournalEntry,[FSessionID]);
  end;
end;

procedure TIBJournal.StopJournalling;
begin
  FreeAndNil(FJournalFileStream);
  FSessionID := -1;
  FBase.JournalHook := nil;
end;

constructor TIBJournal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := @HandleBeforeDatabaseDisconnect;
  FBase.AfterDatabaseConnect := @HandleDatabaseConnect;
  FBase.AfterDatabaseDisconnect := @HandleDatabaseDisconnect;
  FVendorName := DefaultVendor;
  FJournalFileTemplate := DefaultJournalTemplate;
  FJournalAll := false;
end;

destructor TIBJournal.Destroy;
begin
  Enabled := false;
  FBase.Free;
  inherited Destroy;
end;

procedure TIBJournal.ExecQuery(aIBSQL: TObject);
var SQL: AnsiString;
    LogEntry: AnsiString;
    IBSQL: TIBSQL;
begin
  IBSQL := aIBSQL as TIBSQL;
  if assigned(FJournalFileStream) and (FJournalAll or
    (not IBSQL.Transaction.IsReadOnly and (IBSQL.RowsAffected > 0))) then
  begin
    SQL := TQueryProcessor.Execute(IBSQL);
    LogEntry := Format(sQueryJournal,[IBSQL.Transaction.TransactionID,Length(SQL),SQL]);
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
  end;
end;

procedure TIBJournal.TransactionStart(Tr: TIBTransaction);
var LogEntry: AnsiString;
    TPBText: AnsiString;
    i: integer;
begin
  TPBText := '[';
  with Tr.TPB do
    for i := 0 to getCount - 1 do
    begin
      TPBText := TPBText + GetParamTypeName(Items[i].getParamType);
      if i < getCount - 1 then
        TPBText := TPBText + ',';
    end;
  TPBText := TPBText + ']';
  LogEntry := Format(sTransStartJnl,[FSessionID,Tr.TransactionID,TPBText]);
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

procedure TIBJournal.TransactionEnd(Tr: TIBTransaction;
  Action: TTransactionAction);
var LogEntry: AnsiString;
begin
  case Action of
  TARollback:
    LogEntry := Format(sTransRollbackJnl,['',Tr.TransactionID]);
  TACommit:
    LogEntry := Format(sTransCommitJnl,['',Tr.TransactionID]);
  TACommitRetaining:
    LogEntry := Format(sTransCommitJnl,['R',Tr.TransactionID]);
  TARollbackRetaining:
    LogEntry := Format(sTransRollbackJnl,['R',Tr.TransactionID]);
  end;
  Database.Attachment.ExecuteSQL(Tr.TransactionIntf,
           sqlUpdateJournalEntry,[FSessionID,Tr.TransactionID]);
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

procedure TIBJournal.TransactionEndDone(TransactionID: integer);
var LogEntry: AnsiString;
begin
  LogEntry := Format(sTransEndJnl,[TransactionID]);
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

end.

