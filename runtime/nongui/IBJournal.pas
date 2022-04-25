(*
 *  IBX For Lazarus (Firebird Express)
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
 *  The Original Code is (C) 2015-2022 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit IBJournal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBDatabase, IBUtils, IBInternals;

{ Database Journalling.

  This component is intended to support a client side journal of all database
  updates, inserts and deletes made by the client during a session. Support for
  creating the Journal is provided by the fbintf package. This component is an
  IBX front end.

  Journal playback is also supported. This should be viewed as an experimental
  feature.
}

type
  TOnJournalEntry = procedure(Sender: TObject; aJnlEntry: PJnlEntry) of object;
  { TIBJournal }

  TIBJournal = class(TComponent)
  private
    const DefaultVendor          = 'Snake Oil (Sales) Ltd';
    const DefaultJournalTemplate = 'Journal.%d.log';
  private
    FApplicationName: string;
    FBase: TIBBase;
    FEnabled: boolean;
    FJournalFileTemplate: string;
    FJournalFilePath: string;
    FOnJournalEntry: TOnJournalEntry;
    FOptions: TJournalOptions;
    FRetainJournal: boolean;
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
    procedure StartSession;
    procedure EndSession;
    function HasJournalHandler: boolean;
    procedure DoJournalHandler(aJnlEntry: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReplayJournal(aJournalFile: string);
    property JournalFilePath: string read FJournalFilePath;
    property SessionID: integer read FSessionID;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
      {When enabled is true, journaling is performed. Enabled may be set before
       a database is opened, in which case, journaling starts as soon as
       }
    property Enabled: boolean read FEnabled write SetEnabled;
     {JournalFileTemplate determines the name of the journal file and should
      include a %d where %d is replaced with the session no at run time.}
    property JournalFileTemplate: string read FJournalFileTemplate write FJournalFileTemplate;
     {Journaling options - see fbintf/doc/README.ClientSideJournaling.pdf }
    property Options: TJournalOptions read FOptions write FOptions;
     {Vendor Name and ApplicationName determine the location of the Journal file.
      The Journal file is saved in:

      Unix:
      $HOME/.<vendor name>/<application name>/

      Windows:
      <User Application Data Dir>\<vendor name>\<application name>\
     }
    property VendorName: string read FVendorName write FVendorName;
    property ApplicationName: string read FApplicationName write FApplicationName;
     {If RetainJournal is true then when journaling terminats Enabled := false,
      or a normal database close, the journal file and journal table (IBX$JOURNALS)
      entries are retained. Otherwise, they are discarded. Note: always retained
      on a Force Disconnect or a lost connection}
    property RetainJournal: boolean read FRetainJournal write FRetainJournal;
    property OnJournalEntry: TOnJournalEntry read FOnJournalEntry write FOnJournalEntry;
  end;

  { TJournalPlayer }

  TJournalPlayer = class
  private type
    PJnlTransaction = ^TJnlTransaction;
    TJnlTransaction = record
      tr: ITransaction;
      TransactionName: AnsiString;
      ReadOnly: boolean;
      TPB: ITPB;
      SessionID: integer;
      TransactionID: integer;
      DefaultCompletion: TTransactionCompletion;
      CompletionType: TJnlEntryType;
      ReplayRequired: boolean;
      RetainedTransaction: PJnlTransaction; {Link to next transaction for commit/Rollack Retained}
    end;

    PJnlListItem = ^TJnlListItem;
    TJnlListItem = record
      JnlEntry: TJnlEntry;
      Transaction: PJnlTransaction;
    end;
  private
    FTransactionList: TList; {Used when replaying journals}
    FJnlEntryList: TList;  {Used when replaying journals}
    function FindTransaction(aSessionID, aTransactionID: integer): PJnlTransaction;
    procedure HandleOnJnlEntry(JnlEntry: TJnlEntry);
    procedure SetTransactionStatus(att: IAttachment);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    {The LoadJournalFile method reads a journal file and creates an internal
     list of log entries. It also identifies database transactions and links
     them to log entries. Finally, the IBX$JOURNALS table, if present, is
     checked to determine which transactions, if any, have not been completed.}
    procedure LoadJournalFile(aFilename: string; Database: TIBDatabase);

    {The Playback method repeats all log entries in order from the loaded
     journal file that are part of uncompleted transactions, and commits/Rollbacks the
     transactions as determined by their Default Completion.}
    procedure PlayBack(Database: TIBDatabase);
  end;


implementation

uses IBMessages {$IFDEF WINDOWS}, Windows ,Windirs {$ENDIF};

const
  sqlFindTransaction = 'Select * From IBX$JOURNALS Where IBX$SessionID = ? and IBX$TransactionID = ?';

type

  { TJournalEntryParser }

  TJournalEntryParser = class(TCustomJournalProcessor)
  private
    FJnlEntry: TJnlEntry;
    FText: AnsiString;
    FIndex: integer;
  protected
    procedure DoNextJournalEntry(JnlEntry: TJnlEntry);
    function GetChar: AnsiChar; override;
  public
    function ParseJnlEntry(aText: AnsiString; var JnlEntry: TJnlEntry): boolean;
  end;

  {The TJournalStream is intended to be used to receive Journal entries from
   an IAttachment interface. When called in a GUI application, the
   TApplication.QueueAsyncCall method is used to process each entry in the context
   of the main loop. Entries are also written through to the FOutStream, if present.}

  { TJournalStream }

  TJournalStream = class(TStream)
  private
    FParent: TIBJournal;
    FOutStream: TStream;
    FParser: TJournalEntryParser;
  public
    constructor Create(aParent: TIBJournal; OutStream: TStream);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TJournalStream }

function TJournalStream.Write(const Buffer; Count: Longint): Longint;
var JnlEntry: PJnlEntry;
    aText: AnsiString;
begin
  if not FParent.HasJournalHandler then
  begin
    if FOutStream <> nil then
      Result := FOutStream.Write(Buffer,Count);
  end
  else
  begin
    SetLength(aText,Count);
    Move(Buffer,aText[1],Count);
    new(JnlEntry);
    if FParser.ParseJnlEntry(aText,JnlEntry^) then
    begin
      if assigned(IBGUIInterface) then
      begin
        IBGUIInterface.QueueAsyncCall(@FParent.DoJournalHandler, PtrInt(JnlEntry));
        Exit
      end
      else
        FParent.DoJournalHandler(PtrInt(JnlEntry));
    end
    else
      dispose(JnlEntry);
    repeat
      Dec(Count,FOutStream.Write(Buffer,Count));
    until Count = 0;
    Result := Count;
  end;
end;

constructor TJournalStream.Create(aParent: TIBJournal; OutStream: TStream);
begin
  inherited Create;
  FParent := aParent;
  FOutStream := OutStream;
  if aParent.Database = nil then
    IBError(ibxeDatabaseNotAssigned,[nil]);
  FParser := TJournalEntryParser.Create(aParent.Database.FirebirdAPI);
end;

destructor TJournalStream.Destroy;
begin
  if FOutStream <> nil then FOutStream.Free;
  if FParser <> nil then FParser.Free;
  inherited Destroy;
end;

{ TJournalEntryParser }

procedure TJournalEntryParser.DoNextJournalEntry(JnlEntry: TJnlEntry);
begin
  FJnlEntry := JnlEntry;
end;

function TJournalEntryParser.GetChar: AnsiChar;
begin
  if FIndex > Length(FText) then
    Result := #0
  else
  begin
    Result := FText[FIndex];
    Inc(FIndex);
  end;
end;

function TJournalEntryParser.ParseJnlEntry(aText: AnsiString;
  var JnlEntry: TJnlEntry): boolean;
begin
  FText := aText;
  FIndex := 1;
  DoExecute;
  JnlEntry := FJnlEntry;
  Result := JnlEntry.JnlEntryType <> jeUnknown;
end;

{ TJournalPlayer }

function TJournalPlayer.FindTransaction(aSessionID, aTransactionID: integer
  ): PJnlTransaction;
var i: integer;
begin
  Result := nil;
  for i := 0 to FTransactionList.Count - 1 do
    with PJnlTransaction(FTransactionList[i])^ do
      if (SessionID = aSessionID) and (TransactionID = aTransactionID) then
      begin
        Result := PJnlTransaction(FTransactionList[i]);
        Exit;
      end;
end;

procedure TJournalPlayer.HandleOnJnlEntry(JnlEntry: TJnlEntry);

  function NewTransaction: PJnlTransaction;
  begin
    new(Result);
    with Result^ do
    begin
      tr := nil;
      TPB := nil;
      CompletionType := jeUnknown;
      SessionID := JnlEntry.SessionID;
      TransactionID := JnlEntry.TransactionID;
      RetainedTransaction := nil;
    end;
    FTransactionList.Add(Result);
  end;

var item: PJnlListItem;
begin
  new(item);
  item^.JnlEntry := JnlEntry;
  item^.Transaction := nil;
  FJnlEntryList.Add(Item);

  {assumption: transactions are always started before use i.e. log is sequencial}
  case JnlEntry.JnlEntryType of
  jeTransStart:
    begin
      item^.Transaction := NewTransaction;
      with item^.Transaction^ do
      begin
        TPB := JnlEntry.TPB;
        ReadOnly := TPB.Find(isc_tpb_read) <> nil;
        TransactionName := JnlEntry.TransactionName;
        DefaultCompletion := JnlEntry.DefaultCompletion;
      end;
    end;

  jeTransCommitRet,
  jeTransRollbackRet:
    begin
      item^.Transaction := FindTransaction(JnlEntry.SessionID,JnlEntry.OldTransactionID);
      if item^.Transaction <> nil then
      begin
        item^.Transaction^.CompletionType := JnlEntry.JnlEntryType;
        item^.Transaction^.RetainedTransaction := NewTransaction;
        with item^.Transaction^.RetainedTransaction^ do
        begin
          TPB := item^.Transaction^.TPB;
          ReadOnly := TPB.Find(isc_tpb_read) <> nil;
          TransactionName := item^.Transaction^.TransactionName;
          DefaultCompletion := item^.Transaction^.DefaultCompletion;
        end;
      end;
    end;

  jeTransCommit,
  jeTransCommitFail,
  jeTransRollback,
  jeTransRollbackFail:
    begin
      item^.Transaction := FindTransaction(JnlEntry.SessionID,JnlEntry.TransactionID);
      if item^.Transaction <> nil then
        item^.Transaction^.CompletionType := JnlEntry.JnlEntryType;
    end;

  else
    item^.Transaction := FindTransaction(JnlEntry.SessionID,JnlEntry.TransactionID);
  end;
end;

procedure TJournalPlayer.SetTransactionStatus(att: IAttachment);
var i: integer;
    tr: ITransaction;
    Cursor: IResultset;
begin
  if not att.HasTable('IBX$JOURNALS') then Exit;

  tr := att.StartTransaction([isc_tpb_read,isc_tpb_nowait,isc_tpb_concurrency],taCommit);
  for i := 0 to FTransactionList.Count - 1 do
    with PJnlTransaction(FTransactionList[i])^ do
    begin
      Cursor := att.OpenCursor(tr,sqlFindTransaction,[SessionID,TransactionID]);
      {No entry found then transaction did not complete}
      ReplayRequired := Cursor.isEOF or not Cursor.FetchNext;
    end;
end;

constructor TJournalPlayer.Create;
begin
  inherited Create;
  FTransactionList := TList.Create;
  FJnlEntryList := TList.Create;
end;

destructor TJournalPlayer.Destroy;
begin
  Clear;
  if assigned(FJnlEntryList) then FJnlEntryList.Free;
  if assigned(FTransactionList) then FTransactionList.Free;
  inherited Destroy;
end;

procedure TJournalPlayer.Clear;
var i: integer;
begin
  for i := 0 to FTransactionList.Count - 1 do
    dispose(PJnlTransaction(FTransactionList[i]));
  FTransactionList.Clear;
  for i := 0 to FJnlEntryList.Count - 1 do
    dispose(PJnlListItem(FJnlEntryList[i]));
  FJnlEntryList.Clear;
end;

procedure TJournalPlayer.LoadJournalFile(aFilename: string;
  Database: TIBDatabase);
begin
  Clear;
  if Database = nil then
    IBError(ibxeDatabaseNotAssigned,[nil]);
  with TJournalProcessor.Create(Database.FirebirdAPI) do
  try
     Execute(aFilename,Database.FirebirdAPI,@HandleOnJnlEntry);
  finally
    Free
  end;
  if assigned(Database) and assigned(Database.Attachment) then
    SetTransactionStatus(Database.Attachment);
end;

procedure TJournalPlayer.PlayBack(Database: TIBDatabase);

var i: integer;
    item: PJnlListItem;
begin
  for i := 0 to FJnlEntryList.Count - 1 do
  begin
    item := PJnlListItem(FJnlEntryList[i]);
    if (item^.Transaction <> nil) and (item^.Transaction^.ReplayRequired) then
    case item^.JnlEntry.JnlEntryType of
    jeTransStart:
      if item^.Transaction <> nil then
        with item^.Transaction^ do
          tr := Database.Attachment.StartTransaction(TPB,DefaultCompletion);

    jeTransCommit,
    jeTransCommitFail:
      if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
        item^.Transaction^.tr.Commit;

    jeTransRollback,
    jeTransRollbackFail:
      if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
        item^.Transaction^.tr.Rollback;

    jeTransCommitRet:
      if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
        item^.Transaction^.tr.CommitRetaining;

    jeTransRollbackRet:
      if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
        item^.Transaction^.tr.RollbackRetaining;

    jeQuery:
      if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
        Database.Attachment.ExecuteSQL(item^.Transaction^.tr,
                                       item^.JnlEntry.QueryText,[]);

    end;
  end;
  {Complete using Default Completion any open transaction }
  for i := 0 to FJnlEntryList.Count - 1 do
  begin
    item := PJnlListItem(FJnlEntryList[i]);
    if (item^.Transaction <> nil) and (item^.Transaction^.tr <> nil) then
      item^.Transaction^.tr := nil;
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
  Result := Format(FJournalFileTemplate,[Database.Attachment.GetAttachmentID]);
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
  EndSession;
end;

procedure TIBJournal.StartSession;
var S: TJournalStream;
    F: TFileStream;
begin
  FJournalFilePath := GetJournalFilePath;
  EnsurePathExists(FJournalFilePath);
  F := TFileStream.Create(JournalFilePath,fmCreate);
  try
    S := TJournalStream.Create(self,F);
    try
     FSessionID := Database.Attachment.StartJournaling(S,Options);
    except
      S.Free;
      raise;
    end;
  except
    F.Free;
    raise;
  end;
end;

procedure TIBJournal.EndSession;
begin
  FSessionID := -1;
  Database.Attachment.StopJournaling(RetainJournal);
end;

function TIBJournal.HasJournalHandler: boolean;
begin
  Result := assigned(FOnJournalEntry);
end;

procedure TIBJournal.DoJournalHandler(aJnlEntry: PtrInt);
begin
  if HasJournalHandler then
    OnJournalEntry(self,PJnlEntry(aJnlEntry));
  dispose(PJnlEntry(aJnlEntry));
end;

constructor TIBJournal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := @HandleBeforeDatabaseDisconnect;
  FBase.AfterDatabaseConnect := @HandleDatabaseConnect;
  FBase.BeforeDatabaseDisconnect := @HandleDatabaseDisconnect;
  FVendorName := DefaultVendor;
  FJournalFileTemplate := DefaultJournalTemplate;
  FOptions := [joReadWriteTransactions,joModifyQueries];
end;

destructor TIBJournal.Destroy;
begin
  Enabled := false;
  FBase.Free;
  inherited Destroy;
end;

procedure TIBJournal.ReplayJournal(aJournalFile: string);
var player: TJournalPlayer;
begin
  player := TJournalPlayer.Create;
  try
   player.LoadJournalFile(aJournalFile,Database);
   player.PlayBack(Database);
  finally
    player.Free;
  end;
end;

end.

