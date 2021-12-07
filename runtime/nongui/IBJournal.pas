unit IBJournal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IB, IBDatabase, IBUtils, IBInternals, IBSQL;

{ Database Journalling.

  This component is intended to support a client side journal of all database
  updates, inserts and deletes made by the client during a session. Support for
  creating the Journal is provided by the fbintf package. This component is an
  IBX front end.
}

type
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
    FJournalFileStream: TStream;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  end;


implementation

uses IBMessages {$IFDEF WINDOWS}, Windows ,Windirs {$ENDIF};

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
begin
  FJournalFilePath := GetJournalFilePath;
  EnsurePathExists(FJournalFilePath);
  FSessionID := Database.Attachment.StartJournaling(JournalFilePath,Options);
end;

procedure TIBJournal.EndSession;
begin
  FSessionID := -1;
  Database.Attachment.StopJournaling(RetainJournal);
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

end.

