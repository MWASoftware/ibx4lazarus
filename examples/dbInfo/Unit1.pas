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
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IB, IBDatabase, IBDatabaseInfo, IBQuery, IBExternals;

type

  { TForm1 }

  TForm1 = class(TForm)
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    IBTransaction1: TIBTransaction;
    Memo1: TMemo;
    TableNameLookup: TIBQuery;
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
  private
    procedure AddPerfStats(Heading: string; stats: TStrings);
    { private declarations }
    procedure ShowBoolValue(aValue: Long; WhenTrue, WhenFalse: string);
    procedure ShowStrings(aCaption: string; List: TStrings);
    function HexString(s: AnsiString): string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Memo1.Lines.Clear;
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Close;
        Exit
      end;
    On E:Exception do
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
var S: TStrings;
begin
  IBTransaction1.Active := true;
  TableNameLookup.Active := true;
  Memo1.Lines.Add('Authentication Method = '+ IBDatabase1.AuthenticationMethod);
  Memo1.Lines.Add('Remote Protocol = ' + IBDatabase1.RemoteProtocol);
  Memo1.Lines.Add('Attachment SQLDialect = ' + IntToStr(IBDatabase1.DBSQLDialect));
  S := TStringList.Create;
  try
    IBDatabase1.Attachment.getFBVersion(S);
    Memo1.Lines.AddStrings(S);
  finally
    S.Free;
  end;
  with IBDatabaseInfo1 do
  begin
    Memo1.Lines.Add('Firebird Library Pathname = ' + IBDatabase1.FirebirdAPI.GetFBLibrary.GetLibraryFilePath);
    Memo1.Lines.Add('DB SQLDialect = ' + IntToStr(DBSQLDialect));
    Memo1.Lines.Add('Allocation = ' + IntToStr(Allocation));
    Memo1.Lines.Add('Base Level = ' + IntToStr(BaseLevel));
    Memo1.Lines.Add('DB File Name = ' + DBFileName);
    Memo1.Lines.Add('DB Site Name = ' + DBSiteName);
    Memo1.Lines.Add('DB Implementation No = ' + IntToStr(DBImplementationNo));
    Memo1.Lines.Add('Database Created: ' + DateTimeToStr(DateDBCreated));
    Memo1.Lines.Add('DB Implementation Class = ' + IntToStr(DBImplementationClass));
    ShowBoolValue(NoReserve, 'No Space Reserved','Space is Reserved');
    Memo1.Lines.Add('ODS Minor Version = ' + IntToStr(ODSMinorVersion));
    Memo1.Lines.Add('ODS Major Version = ' + IntToStr(ODSMajorVersion));
    Memo1.Lines.Add('Page Size = ' + IntToStr(PageSize));
    Memo1.Lines.Add('Version = ' + Version);
    Memo1.Lines.Add('Current Memory = ' + IntToStr(CurrentMemory));
    ShowBoolValue(ForcedWrites,'Forced Writes Enabled','Forced Writes Disabled');
    Memo1.Lines.Add('Max Memory = ' + IntToStr(MaxMemory));
    Memo1.Lines.Add('Number of Buffers = ' + IntToStr(NumBuffers));
    Memo1.Lines.Add('Sweep Interval = ' + IntToStr(SweepInterval));
    ShowStrings('User Names',UserNames);
    Memo1.Lines.Add('Fetches = ' + IntToStr(Fetches));
    Memo1.Lines.Add('Marks = ' + IntToStr(Marks));
    Memo1.Lines.Add('Reads = ' + IntToStr(Reads));
    Memo1.Lines.Add('Writes = ' + IntToStr(Writes));
    if ODSMajorVersion >= 12 then
    begin
      Memo1.Lines.Add('Pages Free = ' + IntToStr(PagesFree));
      Memo1.Lines.Add('Pages Used = ' + IntToStr(PagesUsed));
    end;
    Memo1.Lines.Add('Transaction Count = ' + IntToStr(TransactionCount));
    AddPerfStats('Backout Count',BackoutCount);
    AddPerfStats('Delete Count',DeleteCount);
    AddPerfStats('Expunge Count',ExpungeCount);
    AddPerfStats('Insert Count',InsertCount);
    AddPerfStats('Purge Count',PurgeCount);
    AddPerfStats('Read Idx Count',ReadIdxCount);
    AddPerfStats('Read Seq Count',ReadSeqCount);
    AddPerfStats('Update Count',UpdateCount);
    Memo1.Lines.Add('');
    ShowBoolValue(ReadOnly,'Database is Read Only','Database is Read/Write');
    Memo1.Lines.Add('Hex Dump of Database Page 100:');
    Memo1.Lines.Add(HexString(GetDatabasePage(100)));
  end;
end;

procedure TForm1.ShowBoolValue(aValue: Long; WhenTrue, WhenFalse: string);
begin
  if aValue <> 0 then
    Memo1.Lines.Add(WhenTrue)
  else
    Memo1.Lines.Add(WhenFalse);
end;

procedure TForm1.ShowStrings(aCaption: string; List: TStrings);
var s: string;
    i: integer;
begin
  s := aCaption + ': ';
  for i := 0 to List.Count - 1 do
  begin
    if i > 0 then
      s := s + ', ';
    s := s + List[i];
  end;
  Memo1.Lines.Add(s);
end;

function TForm1.HexString(s: AnsiString): string;
var i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
    Result += Format('%x ',[byte(s[i])]);
end;

procedure TForm1.AddPerfStats(Heading: string;
  stats: TStrings);
var i: integer;
begin
  with Memo1.Lines do
  begin
    if stats.count = 0 then exit;
    Add('');
    Add(Heading);
    for i := 0 to stats.Count - 1 do
    begin
      if TableNameLookup.Locate('RDB$RELATION_ID',stats.Names[i],[]) then
        Add('  ' + TableNameLookup.FieldByName('RDB$RELATION_NAME').AsString + ' = ' + stats.ValueFromIndex[i]);
    end;
  end;
end;


end.

