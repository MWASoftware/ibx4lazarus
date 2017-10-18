{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011 - 2014                                                }
{                                                                        }
{************************************************************************}

unit IBSQL;

{$Mode Delphi}

{$codepage UTF8}

(* Define IBXQUERYSTATS to write to stdout a summary of query execution
   statistics each time a query is executed

   Define IBXQUERYTIME to write to stdout The local execution time for each
   query
   *)

{ $DEFINE IBXQUERYSTATS}
{ $DEFINE IBXQUERYTIME}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  baseunix, unix,
{$ENDIF}
  SysUtils, Classes, IBExternals, IB, IBDatabase, IBUtils;

type
  { TIBBatch }

  TIBBatch = class(TObject)
  protected
    FFilename: String;
    FColumns: IResults;
    FParams: ISQLParams;
  public
    procedure ReadyFile; virtual; abstract;
    property Columns: IResults read FColumns;
    property Filename: String read FFilename write FFilename;
    property Params: ISQLParams read FParams;
  end;

  TIBBatchInput = class(TIBBatch)
  public
    function ReadParameters: Boolean; virtual; abstract;
  end;

  TIBBatchOutput = class(TIBBatch)
  public
    function WriteColumns: Boolean; virtual; abstract;
  end;


  { TIBOutputDelimitedFile }
  TIBOutputDelimitedFile = class(TIBBatchOutput)
  protected
  {$IFDEF UNIX}
    FHandle: cint;
  {$ELSE}
    FHandle: THandle;
  {$ENDIF}
    FOutputTitles: Boolean;
    FColDelimiter,
    FRowDelimiter: string;
  public
    destructor Destroy; override;
    procedure ReadyFile; override;
    function WriteColumns: Boolean; override;
    property ColDelimiter: string read FColDelimiter write FColDelimiter;
    property OutputTitles: Boolean read FOutputTitles
                                   write FOutputTitles;
    property RowDelimiter: string read FRowDelimiter write FRowDelimiter;
  end;

  { TIBInputDelimitedFile }
  TIBInputDelimitedFile = class(TIBBatchInput)
  protected
    FColDelimiter,
    FRowDelimiter: string;
    FEOF: Boolean;
    FFile: TFileStream;
    FLookAhead: Char;
    FReadBlanksAsNull: Boolean;
    FSkipTitles: Boolean;
  public
    destructor Destroy; override;
    function GetColumn(var Col: string): Integer;
    function ReadParameters: Boolean; override;
    procedure ReadyFile; override;
    property ColDelimiter: string read FColDelimiter write FColDelimiter;
    property ReadBlanksAsNull: Boolean read FReadBlanksAsNull
                                       write FReadBlanksAsNull;
    property RowDelimiter: string read FRowDelimiter write FRowDelimiter;
    property SkipTitles: Boolean read FSkipTitles write FSkipTitles;
  end;

  { TIBOutputRawFile }
  TIBOutputRawFile = class(TIBBatchOutput)
  protected
  {$IFDEF UNIX}
    FHandle: cint;
  {$ELSE}
    FHandle: THandle;
  {$ENDIF}
  public
    destructor Destroy; override;
    procedure ReadyFile; override;
    function WriteColumns: Boolean; override;
  end;

  { TIBInputRawFile }
  TIBInputRawFile = class(TIBBatchInput)
  protected
   {$IFDEF UNIX}
    FHandle: cint;
  {$ELSE}
    FHandle: THandle;
  {$ENDIF}
  public
    destructor Destroy; override;
    function ReadParameters: Boolean; override;
    procedure ReadyFile; override;
  end;

     { TIBSQL }

  TIBSQL = class(TComponent)
  private
    FMetaData: IMetaData;
    FSQLParams: ISQLParams;
    FStatement: IStatement;
    FOnSQLChanged: TNotifyEvent;
    FUniqueParamNames: Boolean;
    FBOF: boolean;
    FEOF: boolean;
    function GetFieldCount: integer;
    function GetOpen: Boolean;
    function GetPrepared: Boolean;
    function GetSQLStatementType: TIBSQLStatementTypes;
    procedure SetUniqueParamNames(AValue: Boolean);
  protected
    FBase: TIBBase;
    FGoToFirstRecordOnExecute: boolean;     { Automatically position record on first record after executing }
    FRecordCount: Integer;         { How many records have been read so far? }
    FOnSQLChanging: TNotifyEvent;  { Call this when the SQL is changing }
    FSQL: TStrings;                { SQL Query (by user) }
    FParamCheck: Boolean;          { Check for parameters? (just like TQuery) }
    FResults: IResults;            {Single row results from exec}
    FResultSet: IResultSet;        {Multi-row results from open cursor}
    FGenerateParamNames: Boolean;  { Auto generate param names ?}
    procedure DoBeforeDatabaseDisconnect(Sender: TObject);
    function GetDatabase: TIBDatabase;
    function GetEOF: Boolean;
    function GetFields(const Idx: Integer): ISQLData;
    function GetFieldIndex(FieldName: String): Integer;
    function GetPlan: String;
    function GetRecordCount: Integer;
    function GetRowsAffected: Integer;
    function GetSQLParams: ISQLParams;
    function GetTransaction: TIBTransaction;
    procedure SetDatabase(Value: TIBDatabase);
    procedure SetSQL(Value: TStrings);
    procedure SetTransaction(Value: TIBTransaction);
    procedure SQLChanging(Sender: TObject);
    procedure SQLChanged(Sender: TObject);
    procedure BeforeTransactionEnd(Sender: TObject; Action: TTransactionAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BatchInput(InputObject: TIBBatchInput);
    procedure BatchOutput(OutputObject: TIBBatchOutput);
    procedure CheckClosed;           { raise error if query is not closed. }
    procedure CheckOpen;             { raise error if query is not open.}
    procedure CheckValidStatement;   { raise error if statement is invalid.}
    procedure Close;
    procedure ExecQuery;
    function HasField(FieldName: String): boolean;
    function FieldByName(FieldName: String): ISQLData;
    function ParamByName(ParamName: String): ISQLParam;
    procedure FreeHandle;
    function Next: boolean;
    procedure Prepare;
    function GetUniqueRelationName: String;
    property Bof: Boolean read FBOF;
    property Eof: Boolean read GetEOF;
    property Current: IResults read FResults;
    property Fields[const Idx: Integer]: ISQLData read GetFields; default;
    property FieldIndex[FieldName: String]: Integer read GetFieldIndex;
    property FieldCount: integer read GetFieldCount;
    property Open: Boolean read GetOpen;
    property Params: ISQLParams read GetSQLParams;
    property Plan: String read GetPlan;
    property Prepared: Boolean read GetPrepared;
    property RecordCount: Integer read GetRecordCount;
    property RowsAffected: Integer read GetRowsAffected;
    property SQLStatementType: TIBSQLStatementTypes read GetSQLStatementType;
    property UniqueRelationName: String read GetUniqueRelationName;
    property Statement: IStatement read FStatement;
    property MetaData: IMetaData read FMetaData;
  published
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property GenerateParamNames: Boolean read FGenerateParamNames write FGenerateParamNames;
    property UniqueParamNames: Boolean read FUniqueParamNames write SetUniqueParamNames;
    property GoToFirstRecordOnExecute: Boolean read FGoToFirstRecordOnExecute
                                               write FGoToFirstRecordOnExecute
                                               default True;
    property ParamCheck: Boolean read FParamCheck write FParamCheck;
    property SQL: TStrings read FSQL write SetSQL;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
    property OnSQLChanging: TNotifyEvent read FOnSQLChanging write FOnSQLChanging;
    property OnSQLChanged: TNotifyEvent read FOnSQLChanged write FOnSQLChanged;
  end;

procedure IBAlloc(var P; OldSize, NewSize: Integer);

implementation

uses
   Variants, IBSQLMonitor, FBMessages, IBCustomDataSet;

procedure IBAlloc(var P; OldSize, NewSize: Integer);
var
  i: Integer;
begin
  ReallocMem(Pointer(P), NewSize);
  for i := OldSize to NewSize - 1 do PChar(P)[i] := #0;
end;

{ TIBOutputDelimitedFile }

destructor TIBOutputDelimitedFile.Destroy;
begin
{$IFDEF UNIX}
  if FHandle <> -1 then
     fpclose(FHandle);
{$ELSE}
  if FHandle <> 0 then
  begin
    FlushFileBuffers(FHandle);
    CloseHandle(FHandle);
  end;
{$ENDIF}
  inherited Destroy;
end;

procedure TIBOutputDelimitedFile.ReadyFile;
var
  i: Integer;
  {$IFDEF UNIX}
  BytesWritten: cint;
  {$ELSE}
  BytesWritten: DWORD;
  {$ENDIF}
  st: string;
begin
  if FColDelimiter = '' then
    FColDelimiter := TAB;
  if FRowDelimiter = '' then
    FRowDelimiter := CRLF;
  {$IFDEF UNIX}
  FHandle := FpOpen(Filename,O_WrOnly or O_Creat);
  {$ELSE}
  FHandle := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                        FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
  {$ENDIF}
  if FOutputTitles then
  begin
    for i := 0 to Columns.Count - 1 do
      if i = 0 then
        st := Columns[i].GetAliasname
      else
        st := st + FColDelimiter + Columns[i].GetAliasname;
    st := st + FRowDelimiter;
    {$IFDEF UNIX}
    if FHandle <> -1 then
       BytesWritten := FpWrite(FHandle,st[1],Length(st));
    if BytesWritten = -1 then
       raise Exception.Create('File Write Error');
    {$ELSE}
    WriteFile(FHandle, st[1], Length(st), BytesWritten, nil);
    {$ENDIF}
  end;
end;

function TIBOutputDelimitedFile.WriteColumns: Boolean;
var
  i: Integer;
  {$IFDEF UNIX}
  BytesWritten: cint;
  {$ELSE}
  BytesWritten: DWORD;
  {$ENDIF}
  st: string;
begin
  result := False;
  {$IFDEF UNIX}
  if FHandle <> -1 then
  {$ELSE}
  if FHandle <> 0 then
  {$ENDIF}
  begin
    st := '';
    for i := 0 to Columns.Count - 1 do
    begin
      if i > 0 then
        st := st + FColDelimiter;
      st := st + StripString(Columns[i].AsString, FColDelimiter + FRowDelimiter);
    end;
    st := st + FRowDelimiter;
  {$IFDEF UNIX}
    BytesWritten := FpWrite(FHandle,st[1],Length(st));
  {$ELSE}
    WriteFile(FHandle, st[1], Length(st), BytesWritten, nil);
  {$ENDIF}
    if BytesWritten = DWORD(Length(st)) then
      result := True;
  end
end;

 { TIBInputDelimitedFile }

destructor TIBInputDelimitedFile.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

function TIBInputDelimitedFile.GetColumn(var Col: string): Integer;
var
  c: Char;
  BytesRead: Integer;

  procedure ReadInput;
  begin
    if FLookAhead <> NULL_TERMINATOR then
    begin
      c := FLookAhead;
      BytesRead := 1;
      FLookAhead := NULL_TERMINATOR;
    end else
      BytesRead := FFile.Read(c, 1);
  end;

  procedure CheckCRLF(Delimiter: string);
  begin
    if (c = CR) and (Pos(LF, Delimiter) > 0) then {mbcs ok}
    begin
      BytesRead := FFile.Read(c, 1);
      if (BytesRead = 1) and (c <> #10) then
        FLookAhead := c
    end;
  end;

begin
  Col := '';
  result := 0;
  ReadInput;
  while BytesRead <> 0 do begin
    if Pos(c, FColDelimiter) > 0 then {mbcs ok}
    begin
      CheckCRLF(FColDelimiter);
      result := 1;
      break;
    end else if Pos(c, FRowDelimiter) > 0 then {mbcs ok}
    begin
      CheckCRLF(FRowDelimiter);
      result := 2;
      break;
    end else
      Col := Col + c;
    ReadInput;
  end;
end;

function TIBInputDelimitedFile.ReadParameters: Boolean;
var
  i, curcol: Integer;
  Col: string;
begin
  result := False;
  if not FEOF then begin
    curcol := 0;
    repeat
      i := GetColumn(Col);
      if (i = 0) then
        FEOF := True;
      if (curcol < Params.Count) then
      begin
        try
          if (Col = '') and
             (ReadBlanksAsNull) then
            Params[curcol].IsNull := True
          else
            Params[curcol].AsString := Col;
          Inc(curcol);
        except
          on E: Exception do begin
            if not (FEOF and (curcol = Params.Count)) then
              raise;
          end;
        end;
      end;
    until (FEOF) or (i = 2);
    result := ((FEOF) and (curcol = Params.Count)) or
              (not FEOF);
  end;
end;

procedure TIBInputDelimitedFile.ReadyFile;
begin
  if FColDelimiter = '' then
    FColDelimiter := TAB;
  if FRowDelimiter = '' then
    FRowDelimiter := CRLF;
  FLookAhead := NULL_TERMINATOR;
  FEOF := False;
  if FFile <> nil then
    FFile.Free;
  FFile := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
  if FSkipTitles then
    ReadParameters;
end;

{ TIBOutputRawFile }
destructor TIBOutputRawFile.Destroy;
begin
{$IFDEF UNIX}
  if FHandle <> -1 then
     fpclose(FHandle);
{$ELSE}
  if FHandle <> 0 then
  begin
    FlushFileBuffers(FHandle);
    CloseHandle(FHandle);
  end;
{$ENDIF}
  inherited Destroy;
end;

procedure TIBOutputRawFile.ReadyFile;
begin
  {$IFDEF UNIX}
  FHandle := FpOpen(Filename,O_WrOnly or O_Creat);
  {$ELSE}
  FHandle := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                        FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
  {$ENDIF}
end;

function TIBOutputRawFile.WriteColumns: Boolean;
var
  i: Integer;
  BytesWritten: DWord;
begin
  result := False;
  if FHandle <> 0 then
  begin
    for i := 0 to Columns.Count - 1 do
    begin
      {$IFDEF UNIX}
      BytesWritten := FpWrite(FHandle,Columns[i].GetAsPointer^, Columns[i].GetSize);
      {$ELSE}
      WriteFile(FHandle, Columns[i].GetAsPointer^, Columns[i].GetSize,
                BytesWritten, nil);
      {$ENDIF}
      if BytesWritten <> DWORD(Columns[i].GetSize) then
        exit;
    end;
    result := True;
  end;
end;

{ TIBInputRawFile }
destructor TIBInputRawFile.Destroy;
begin
{$IFDEF UNIX}
  if FHandle <> -1 then
     fpclose(FHandle);
{$ELSE}
  if FHandle <> 0 then
    CloseHandle(FHandle);
{$ENDIF}
  inherited Destroy;
end;

function TIBInputRawFile.ReadParameters: Boolean;
var
  i: Integer;
  BytesRead: DWord;
begin
  result := False;
{$IFDEF UNIX}
  if FHandle <> -1 then
{$ELSE}
  if FHandle <> 0 then
{$ENDIF}
  begin
    for i := 0 to Params.Count - 1 do
    begin
      {$IFDEF UNIX}
      BytesRead := FpRead(FHandle,Params[i].GetAsPointer^,Params[i].GetSize);
      {$ELSE}
      ReadFile(FHandle, Params[i].GetAsPointer^, Params[i].GetSize,
               BytesRead, nil);
      {$ENDIF}
      if BytesRead <> DWORD(Params[i].GetSize) then
        exit;
    end;
    result := True;
  end;
end;

procedure TIBInputRawFile.ReadyFile;
begin
{$IFDEF UNIX}
  if FHandle <> -1 then
     fpclose(FHandle);
  FHandle := FpOpen(Filename,O_RdOnly);
  if FHandle = -1 then
     raise Exception.CreateFmt('Unable to open file %s',[Filename]);
{$ELSE}
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := CreateFile(PChar(Filename), GENERIC_READ, 0, nil, OPEN_EXISTING,
                        FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0;
{$ENDIF}
end;

{ TIBSQL }
constructor TIBSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGenerateParamNames := False;
  FGoToFirstRecordOnExecute := True;
  FBase := TIBBase.Create(Self);
  FBase.BeforeDatabaseDisconnect := DoBeforeDatabaseDisconnect;
  FBase.BeforeTransactionEnd := BeforeTransactionEnd;
  FRecordCount := 0;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChanging := SQLChanging;
  TStringList(FSQL).OnChange := SQLChanged;
  FParamCheck := True;
  if AOwner is TIBDatabase then
    Database := TIBDatabase(AOwner)
  else
    if AOwner is TIBTransaction then
      Transaction := TIBTransaction(AOwner);
end;

destructor TIBSQL.Destroy;
begin
  FreeHandle;
  FSQL.Free;
  FBase.Free;
  inherited Destroy;
end;

procedure TIBSQL.BatchInput(InputObject: TIBBatchInput);
begin
  if not Prepared then
    Prepare;
  InputObject.FParams := Self.GetSQLParams;
  InputObject.ReadyFile;
  if GetSQLStatementType in [SQLInsert, SQLUpdate, SQLDelete, SQLExecProcedure] then
    while InputObject.ReadParameters do
      ExecQuery;
end;

procedure TIBSQL.BatchOutput(OutputObject: TIBBatchOutput);
begin
  CheckClosed;
  if not Prepared then
    Prepare;
  if GetSQLStatementType = SQLSelect then begin
    try
      ExecQuery;
      OutputObject.FColumns := Self.FResults;
      OutputObject.ReadyFile;
      if not FGoToFirstRecordOnExecute then
        Next;
      while (not Eof) and (OutputObject.WriteColumns) do
        Next;
    finally
      Close;
    end;
  end;
end;

procedure TIBSQL.CheckClosed;
begin
  if FResultSet <> nil  then IBError(ibxeSQLOpen, [nil]);
end;

procedure TIBSQL.CheckOpen;
begin
  if FResultSet = nil then IBError(ibxeSQLClosed, [nil]);
end;

procedure TIBSQL.CheckValidStatement;
begin
  FBase.CheckTransaction;
  if (FStatement = nil) then
    IBError(ibxeInvalidStatementHandle, [nil]);
end;

procedure TIBSQL.Close;
begin
  if FResults <> nil then
    FResults.SetRetainInterfaces(false);
  FResultSet := nil;
  FResults := nil;
  FBOF := false;
  FEOF := false;
  FRecordCount := 0;
end;

function TIBSQL.GetFieldCount: integer;
begin
  if FResults <> nil then
    Result := FResults.GetCount
  else
  if FMetaData <> nil then
    Result := FMetaData.GetCount
  else
    Result := 0;
end;

function TIBSQL.GetOpen: Boolean;
begin
  Result := FResultSet <> nil;
end;

function TIBSQL.GetPrepared: Boolean;
begin
  Result := (FStatement <> nil) and FStatement.IsPrepared;
end;

function TIBSQL.GetSQLStatementType: TIBSQLStatementTypes;
begin
  if FStatement = nil then
    Result := SQLUnknown
  else
    Result := FStatement.GetSQLStatementType;
end;

 procedure TIBSQL.SetUniqueParamNames(AValue: Boolean);
begin
  if FUniqueParamNames = AValue then Exit;
  FreeHandle;
  FUniqueParamNames := AValue;
end;

procedure TIBSQL.DoBeforeDatabaseDisconnect(Sender: TObject);
begin
  FreeHandle;
end;

procedure TIBSQL.ExecQuery;
var
  fetch_res: ISC_STATUS;
  {$IFDEF IBXQUERYSTATS}
  stats: TPerfCounters;
  {$ENDIF}
  {$IFDEF IBXQUERYTIME}
  tmsecs: comp;
  {$ENDIF}
begin
  CheckClosed;
  if not Prepared then Prepare;
  CheckValidStatement;
  {$IFDEF IBXQUERYTIME}
  tmsecs := TimeStampToMSecs(DateTimeToTimeStamp(Now));
  {$ENDIF}
  if SQLStatementType = SQLSelect then
  begin
    FResultSet := FStatement.OpenCursor;
    FResults := FResultSet;
    FResults.SetRetainInterfaces(true);
    FBOF := True;
    FEOF := False;
    FRecordCount := 0;
    if not (csDesigning in ComponentState) then
      MonitorHook.SQLExecute(Self);
    if FGoToFirstRecordOnExecute then
      Next;
  end
  else
  begin
    FResults := FStatement.Execute;
    if not (csDesigning in ComponentState) then
      MonitorHook.SQLExecute(Self);
  end;
  {$IFDEF IBXQUERYTIME}
  writeln('Executing ',FStatement.GetSQLText,
    ' Response time= ',Format('%f msecs',[TimeStampToMSecs(DateTimeToTimeStamp(Now)) - tmsecs]));
  {$ENDIF}
  {$IFDEF IBXQUERYSTATS}
  if FStatement.GetPerfStatistics(stats) then
    writeln('Executing ',FStatement.GetSQLText,
    ' Elapsed time= ', FormatFloat('#0.000',stats[psRealTime]/1000),' sec');
  {$ENDIF}
  FBase.DoAfterExecQuery(self);
end;

function TIBSQL.HasField(FieldName: String): boolean;
begin
  if FResults = nil then
    IBError(ibxeNoFieldAccess,[nil]);

  Result := FResults.ByName(FieldName) <> nil;
end;

function TIBSQL.GetEOF: Boolean;
begin
  result := FEOF or (FResultSet = nil);
end;

function TIBSQL.FieldByName(FieldName: String): ISQLData;
begin
  if FResults = nil then
    IBError(ibxeNoFieldAccess,[nil]);

  Result := FResults.ByName(FieldName);

  if Result = nil then
    IBError(ibxeFieldNotFound, [FieldName]);
end;

function TIBSQL.ParamByName(ParamName: String): ISQLParam;
begin
  Result := Params.ByName(ParamName);
end;

function TIBSQL.GetFields(const Idx: Integer): ISQLData;
begin
  if FResults = nil then
    IBError(ibxeNoFieldAccess,[nil]);

  if (Idx < 0) or (Idx >= FResults.GetCount) then
    IBError(ibxeFieldNotFound, [IntToStr(Idx)]);
  result := FResults[Idx];
end;

function TIBSQL.GetFieldIndex(FieldName: String): Integer;
var Field: IColumnMetaData;
begin
  if FMetaData = nil then
    IBError(ibxeNoFieldAccess,[nil]);

  Field := FMetaData.ByName(FieldName);

  if Field = nil then
    result := -1
  else
    result := Field.GetIndex;
end;

function TIBSQL.Next: boolean;
begin
  result := false;
  if not FEOF then
  begin
    CheckOpen;
    try
      Result := FResultSet.FetchNext;
    except
      Close;
      raise;
    end;

    if Result then
    begin
      Inc(FRecordCount);
      FBOF := False;
    end
    else
      FEOF := true;

    if not (csDesigning in ComponentState) then
      MonitorHook.SQLFetch(Self);
  end;
end;

procedure TIBSQL.FreeHandle;
begin
  if FStatement <> nil then
    FStatement.SetRetainInterfaces(false);
  Close;
  FStatement := nil;
  FResults := nil;
  FResultSet := nil;
  FMetaData := nil;
  FSQLParams := nil;
end;

function TIBSQL.GetDatabase: TIBDatabase;
begin
  result := FBase.Database;
end;

function TIBSQL.GetPlan: String;
begin
  if (not Prepared) or
     (not (GetSQLStatementType in [SQLSelect, SQLSelectForUpdate,
       {TODO: SQLExecProcedure, }
       SQLUpdate, SQLDelete])) then
    result := ''
  else
    Result := FStatement.GetPlan;
end;

function TIBSQL.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

 function TIBSQL.GetRowsAffected: Integer;
var
  SelectCount, InsertCount, UpdateCount, DeleteCount: integer;
begin
  if not Prepared then
    Result := -1
  else
  begin
    FStatement.GetRowsAffected(SelectCount, InsertCount, UpdateCount, DeleteCount);
    Result := InsertCount + UpdateCount + DeleteCount;
  end;
end;

function TIBSQL.GetSQLParams: ISQLParams;
begin
  if not Prepared then
    Prepare;
  result := Statement.SQLParams;
end;

function TIBSQL.GetTransaction: TIBTransaction;
begin
  result := FBase.Transaction;
end;

procedure TIBSQL.SetDatabase(Value: TIBDatabase);
begin
  FBase.Database := Value;
end;

procedure TIBSQL.Prepare;
begin
  CheckClosed;
  FBase.CheckDatabase;
  FBase.CheckTransaction;
  Close;
  if Prepared then
    exit;
  if (FSQL.Text = '') then
    IBError(ibxeEmptyQuery, [nil]);

  if FStatement <> nil then
    FStatement.Prepare(Transaction.TransactionIntf)
  else
  if not ParamCheck then
    FStatement := Database.Attachment.Prepare(Transaction.TransactionIntf,SQL.Text)
  else
    FStatement := Database.Attachment.PrepareWithNamedParameters(
                     Transaction.TransactionIntf,
                     SQL.Text,
                     GenerateParamNames);
  {$IFDEF IBXQUERYSTATS}
  FStatement.EnableStatistics(true);
  {$ENDIF}
  FMetaData := FStatement.GetMetaData;
  FSQLParams := FStatement.GetSQLParams;
  FStatement.SetRetainInterfaces(true);
  if not (csDesigning in ComponentState) then
      MonitorHook.SQLPrepare(Self);
end;

function TIBSQL.GetUniqueRelationName: String;
begin
  if Prepared and (GetSQLStatementType = SQLSelect) then
    result := FMetaData.GetUniqueRelationName
  else
    result := '';
end;

procedure TIBSQL.SetSQL(Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
  begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TIBSQL.SetTransaction(Value: TIBTransaction);
begin
  FBase.Transaction := Value;
end;

procedure TIBSQL.SQLChanging(Sender: TObject);
begin
  if Assigned(OnSQLChanging) then
    OnSQLChanging(Self);

  FreeHandle;
end;

procedure TIBSQL.SQLChanged(Sender: TObject);
begin
  if assigned(OnSQLChanged) then
    OnSQLChanged(self);
end;

procedure TIBSQL.BeforeTransactionEnd(Sender: TObject;
  Action: TTransactionAction);
begin
  if not (Owner is TIBCustomDataSet) then
    FreeHandle;
end;

end.
