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
  !S:<session id>,<transaction no.>,<string length>:<transaction Name or TPB>

  Transaction Commit (retaining) Called :
  !C:<session id>,<transaction no.>,<phase no.>

  Transaction Commit retaining Called :
  !c:<session id>,<transaction no.>,<phase no.>

  Transaction Rollback Called:
  !R:<session id>,<transaction no.>,<phase no.>

  Transaction Rollback retaining Called:
  !r:<session id>,<transaction no.>,<phase no.>

  Transaction Commit/Rollback  Completed:
  !E:<session id>,<transaction no.>

  Update/Insert/Delete
  !Q:<session id>,<transaction no.>,<phase no.>,<length of query text in bytes>:<query text>

}

type
  TGetArrayFieldIndex = procedure (Sender: TObject; aIBSQL: TIBSQL; ParamIndex: integer;
                                var FieldIndex: integer) of object;

  { TIBJournal }

  TIBJournal = class(TComponent,IJournallingHook)
  private
    const DefaultVendor          = 'Snake Oil (Sales) Ltd';
    const DefaultJournalTemplate = 'Journal.%d.log';
    const sQueryJournal          = '*Q:%d,%d,%d,%d:%s' + LineEnding;
    const sTransStartJnl         = '*S:%d,%d,%d:%s' + LineEnding;
    const sTransCommitJnl        = '*C:%d,%d,%d' + LineEnding;
    const sTransCommitRetJnl     = '*c:%d,%d,%d' + LineEnding;
    const sTransRollBackJnl      = '*R:%d,%d,%d' + LineEnding;
    const sTransRollBackRetJnl   = '*e:%d,%d,%d' + LineEnding;
    const sTransEndJnl           = '*E:%d,%d' + LineEnding;
  private
    FApplicationName: string;
    FBase: TIBBase;
    FEnabled: boolean;
    FJournalAll: boolean;
    FJournalFileTemplate: string;
    FJournalFilePath: string;
    FJournalFileStream: TStream;
    FRetainJournal: boolean;
    FVendorName: string;
    FSessionId: integer;
    FGetArrayFieldIndex: TGetArrayFieldIndex;
    procedure EnsurePathExists(FileName: string);
    function GetDatabase: TIBDatabase;
    function GetJournalFilePath: string;
    procedure SetDatabase(AValue: TIBDatabase);
    procedure SetEnabled(AValue: boolean);
    procedure HandleBeforeDatabaseDisconnect(Sender: TObject);
    procedure HandleDatabaseConnect(Sender: TObject);
    procedure HandleDatabaseDisconnect(Sender: TObject);
    function HasTable(TableName: string): boolean;
    procedure StartSession;
    procedure EndSession;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    {IJournallingHook}
    procedure ExecQuery(aIBSQL: TObject);
    procedure TransactionStart(Tr: TIBTransaction);
    procedure TransactionEnd(Tr: TIBTransaction; Action: TTransactionAction);
    procedure TransactionEndDone(TransactionID: integer);
    property JournalFilePath: string read FJournalFilePath;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Enabled: boolean read FEnabled write SetEnabled;
    property JournalFileTemplate: string read FJournalFileTemplate write FJournalFileTemplate;
    property JournalAll: boolean read FJournalAll write FJournalAll;
    {JournalFileTemplate should include a %d where %d is replaced with the session no.}
    property VendorName: string read FVendorName write FVendorName;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property RetainJournal: boolean read FRetainJournal write FRetainJournal;
    property OnGetArrayFieldIndex: TGetArrayFieldIndex read FGetArrayFieldIndex write FGetArrayFieldIndex;
  end;


  TJnlEntryType = (jeTransStart, jeTransCommit, jeTransCommitRet, jeTransRollback,
                 jeTransRollbackRet, jeTransEnd, jeQuery,jeUnknown);

  TOnNextJournalEntry = procedure(JnlEntryType: TJnlEntryType; SessionID, TransactionID, PhaseNo: integer;
                                 Description: AnsiString) of object;

  { TJournalProcessor }

  TJournalProcessor = class(TSQLTokeniser)
  private
    type TLineState = (lsInit, lsJnlFound, lsGotJnlType,  lsGotSessionID, lsGotTransactionID, lsGotPhaseNo, lsGotLength);
  private
    FOnNextJournalEntry: TOnNextJournalEntry;
    FOwner: TIBJournal;
    FInStream: TStream;
    procedure DoExecute;
    function IdentifyJnlEntry(aTokenText: AnsiString): TJnlEntryType;
  protected
    function GetChar: AnsiChar; override;
  public
    destructor Destroy; override;
    class procedure Execute(Owner: TIBJournal; aFileName: string);
    property OnNextJournalEntry: TOnNextJournalEntry read FOnNextJournalEntry write FOnNextJournalEntry;
  end;


implementation

uses IBMessages,IBXScript;

const
  sJournalTableName = 'IBX$JOURNALS';
  sqlCreateJournalTable =
    'Create Table ' + sJournalTableName + '(' +
    '  IBX$SessionID Integer not null, '+
    '  IBX$TransactionID Integer not null, '+
    '  IBX$USER VarChar(32) Default CURRENT_USER,'+
    '  IBX$TIMESTAMP TIMESTAMP Default CURRENT_TIMESTAMP,'+
    '  Primary Key(IBX$SessionID,IBX$TransactionID)' +
    ')';

  sqlCreateSequence = 'CREATE SEQUENCE IBX$SESSIONS';

  sqlGetNextSessionID = 'Select Gen_ID(IBX$SESSIONS,1) as SessionID From RDB$DATABASE';

  sqlRecordJournalEntry = 'Insert into ' + sJournalTableName + '(IBX$SessionID,IBX$TransactionID) '+
                        'Values(?,?)';

  sqlCleanUpSession = 'Delete From ' + sJournalTableName + ' Where IBX$SESSIONID = ?';

type

  { TQueryProcessor }

  TQueryProcessor=class(TSQLTokeniser)
  private
    FOwner: TIBJournal;
    FInString: AnsiString;
    FIndex: integer;
    FIBSQL: TIBSQL;
    function DoExecute: AnsiString;
    function GetParamValue(ParamIndex: integer): AnsiString;
  protected
    function GetChar: AnsiChar; override;
  public
    class function Execute(Owner: TIBJournal; IBSQL: TIBSQL): AnsiString;
  end;

{ TJournalProcessor }

procedure TJournalProcessor.DoExecute;
var token: TSQLTokens;
    LineState: TLineState;
    JnlEntryType: TJnlEntryType;
    SessionID, TransactionID, PhaseNo: integer;
    Len: integer;
    Description: AnsiString;
begin
  LineState := lsInit;
  while not EOF do
  begin
    if LineState = lsInit then
    begin
      SessionID := 0;
      TransactionID := 0;
      PhaseNo := 0;
      Len := 0;
      Description := '';
      JnlEntryType := jeUnknown;
    end;
    token := GetNextToken;
    case token of
    sqltAsterisk:
      if LineState = lsInit then
        LineState := lsJnlFound;

    sqltIdentifier:
      if LineState = lsJnlFound then
        begin
          JnlEntryType := IdentifyJnlEntry(TokenText);
          LineState := lsGotJnlType;
        end
      else
        LineState := lsInit;

    sqltColon:
      if LineState = lsGotLength then
      begin
        Setlength(Description,Len);
        FInStream.Read(Description[1],Len);
        if assigned(FOnNextJournalEntry) then
          OnNextJournalEntry(JnlEntryType,SessionID, TransactionID, PhaseNo, Description);
        LineState := lsInit;
      end
      else
      if LineState <> lsGotJnlType then
        LineState := lsInit;

   sqltComma:
     if not (LineState in [lsGotSessionID,lsGotTransactionID,lsGotPhaseNo]) then
       LineState := lsInit;

   sqltNumberString:
     case LineState of
     lsGotJnlType:
       begin
         SessionID := StrToInt(TokenText);
         LineState := lsGotSessionID;
       end;

     lsGotSessionID:
       begin
         TransactionID := StrToInt(TokenText);
         LineState := lsGotTransactionID;
     end;

     lsGotTransactionID:
       begin
         case JnlEntryType of
         jeTransStart:
           begin
             len := StrToInt(TokenText);
             LineState := lsGotLength;
           end;

           jeTransCommit,
           jeTransCommitRet,
           jeTransRollback,
           jeTransRollbackRet:
             begin
               PhaseNo := StrToInt(TokenText);
               if assigned(FOnNextJournalEntry) then
                 OnNextJournalEntry(JnlEntryType,SessionID, TransactionID, PhaseNo, Description);
               LineState := lsInit;
             end;

           jeTransEnd:
             begin
               if assigned(FOnNextJournalEntry) then
                 OnNextJournalEntry(JnlEntryType,SessionID, TransactionID, PhaseNo, Description);
               LineState := lsInit;
             end;

           jeQuery:
             begin
               PhaseNo := StrToInt(TokenText);
               LineState := lsGotPhaseNo;
             end;
           else
             LineState := lsInit;
         end; {case JnlEntryType}
       end;

     lsGotPhaseNo:
       if JnlEntryType = jeQuery then
       begin
         len :=  StrToInt(TokenText);
         LineState := lsGotLength;
       end
       else
         LineState := lsInit;

     end; {case LineState}
    end; {case token}
  end; {while}
end;

function TJournalProcessor.IdentifyJnlEntry(aTokenText: AnsiString
  ): TJnlEntryType;
begin
  Result := jeUnknown;
  if Length(aTokenText) > 0 then
  case aTokenText[1] of
  'S':
    Result := jeTransStart;
  'C':
    Result := jeTransCommit;
  'c':
    Result := jeTransCommitRet;
  'R':
    Result := jeTransRollback;
  'r':
    Result := jeTransRollbackRet;
  'E':
    Result := jeTransEnd;
  'Q':
    Result := jeQuery;
  end;
end;

function TJournalProcessor.GetChar: AnsiChar;
begin
  FInStream.Read(Result,1);
end;

destructor TJournalProcessor.Destroy;
begin
  FInStream.Free;
  inherited Destroy;
end;

class procedure TJournalProcessor.Execute(Owner: TIBJournal; aFileName: string
  );
begin
  with TJournalProcessor.Create do
  try
    FInStream := TFileStream.Create(aFileName,fmOpenRead);
    DoExecute;
  finally
    Free
  end;
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
var arIndex: integer;
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
      if assigned(FOwner) and assigned(FOwner.OnGetArrayFieldIndex) then
      begin
        arIndex := -1;
        FOwner.OnGetArrayFieldIndex(FOwner,FIBSQL,ParamIndex,arIndex);
        if arIndex >= 0 then
          Result := TSQLXMLReader.FormatArray(FIBSQL.Database,FIBSQL.Fields[arIndex].getAsArray)
        else
          Result := 'NULL'
      end
      else
        Result := 'NULL';

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

class function TQueryProcessor.Execute(Owner: TIBJournal; IBSQL: TIBSQL
  ): AnsiString;
begin
  if not IBSQL.Prepared then
    IBError(ibxeSQLClosed,[]);
  with self.Create do
  try
    FIBSQL := IBSQL;
    FInString := IBSQL.Statement.GetProcessedSQLText;
    FIndex := 1;
    FOwner := Owner;
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
      StartSession
    else
      EndSession;
  end;
end;

procedure TIBJournal.HandleBeforeDatabaseDisconnect(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and Enabled then
    EndSession;
end;

procedure TIBJournal.HandleDatabaseConnect(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and Enabled then
    StartSession;
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

procedure TIBJournal.StartSession;
begin
  with Database.Attachment do
  begin
    if not HasTable(sJournalTableName) then
    begin
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateJournalTable);
      ExecImmediate([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],sqlCreateSequence);
    end;
    FSessionID := OpenCursorAtStart(sqlGetNextSessionID)[0].AsInteger;
  end;
  FJournalFilePath := GetJournalFilePath;
  EnsurePathExists(FJournalFilePath);
  FJournalFileStream := TFileStream.Create(FJournalFilePath,fmCreate);
  FBase.JournalHook := self;
end;

procedure TIBJournal.EndSession;
begin
  FreeAndNil(FJournalFileStream);
  FSessionID := -1;
  FBase.JournalHook := nil;
  if not FRetainJournal then
  begin
      Database.Attachment.ExecuteSQL([isc_tpb_write,isc_tpb_wait,isc_tpb_consistency],
           sqlCleanUpSession,[FSessionID]);
      DeleteFile(FJournalFilePath);
  end;
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
    SQL := TQueryProcessor.Execute(self,IBSQL);
    LogEntry := Format(sQueryJournal,[FSessionID,
                                      IBSQL.Transaction.TransactionID,
                                      IBSQL.Transaction.PhaseNo,
                                      Length(SQL),SQL]);
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
  end;
end;

procedure TIBJournal.TransactionStart(Tr: TIBTransaction);
var LogEntry: AnsiString;
    Description: AnsiString;
    i: integer;
begin
  if not Tr.IsReadOnly then
    Database.Attachment.ExecuteSQL(Tr.TransactionIntf,
           sqlRecordJournalEntry,[FSessionID,Tr.TransactionID]);
  if Tr.TransactionName <> '' then
    Description := Tr.TransactionName
  else
  begin
    Description := '[';
    with Tr.TPB do
      for i := 0 to getCount - 1 do
      begin
        Description := Description + GetParamTypeName(Items[i].getParamType);
        if i < getCount - 1 then
          Description := Description + ',';
      end;
    Description := Description + ']';
  end;
  LogEntry := Format(sTransStartJnl,[FSessionID,
                                     Tr.TransactionID,
                                     Length(Description),
                                     Description]);
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

procedure TIBJournal.TransactionEnd(Tr: TIBTransaction;
  Action: TTransactionAction);
var LogEntry: AnsiString;
begin
  case Action of
  TARollback:
    LogEntry := Format(sTransRollbackJnl,[FSessionID,Tr.TransactionID,Tr.PhaseNo]);
  TACommit:
    LogEntry := Format(sTransCommitJnl,[FSessionID,Tr.TransactionID,Tr.PhaseNo]);
  TACommitRetaining:
    LogEntry := Format(sTransCommitRetJnl,[FSessionID,Tr.TransactionID,Tr.PhaseNo]);
  TARollbackRetaining:
    LogEntry := Format(sTransRollbackRetJnl,[FSessionID,Tr.TransactionID,Tr.PhaseNo]);
  end;
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

procedure TIBJournal.TransactionEndDone(TransactionID: integer);
var LogEntry: AnsiString;
begin
  LogEntry := Format(sTransEndJnl,[FSessionID,TransactionID]);
  if assigned(FJournalFileStream) then
    FJournalFileStream.Write(LogEntry[1],Length(LogEntry));
end;

end.

