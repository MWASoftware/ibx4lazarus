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

  The Journal file is saved in:

  Unix:
  $HOME/.<vendor name>/<application name>/ibxjournal<nnn>.log

  Windows:
  <User Application Data Dir>\<vendor name>\<application name>\ibxjournal<nnn>.log

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
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Enabled: boolean read FEnabled write SetEnabled;
    property JournalFileTemplate: string read FJournalFileTemplate write FJournalFileTemplate;
    property Options: TJournalOptions read FOptions write FOptions;
    {JournalFileTemplate should include a %d where %d is replaced with the session no.}
    property VendorName: string read FVendorName write FVendorName;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property RetainJournal: boolean read FRetainJournal write FRetainJournal;
  end;


implementation

uses IBMessages;

type

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
  FSessionID := Database.Attachment.StartJournaling(JournalFilePath,RetainJournal,Options);
end;

procedure TIBJournal.EndSession;
begin
  FSessionID := -1;
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

