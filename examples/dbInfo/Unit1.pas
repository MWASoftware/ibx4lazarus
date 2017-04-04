unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IB, IBDatabase, IBDatabaseInfo, IBExternals;

type

  { TForm1 }

  TForm1 = class(TForm)
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
  private
    { private declarations }
    procedure ShowBoolValue(aValue: Long; WhenTrue, WhenFalse: string);
    procedure ShowStrings(aCaption: string; List: TStrings);
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
begin
  with IBDatabaseInfo1 do
  begin
    Memo1.Lines.Add('Allocation = ' + IntToStr(Allocation));
    Memo1.Lines.Add('Base Level = ' + IntToStr(BaseLevel));
    Memo1.Lines.Add('DB File Name = ' + DBFileName);
    Memo1.Lines.Add('DB Site Name = ' + DBSiteName);
    Memo1.Lines.Add('DB Implementation No = ' + IntToStr(DBImplementationNo));
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
    ShowStrings('Backout Count',BackoutCount);
    ShowStrings('Delete Count',DeleteCount);
    ShowStrings('Expunge Count',ExpungeCount);
    ShowStrings('Insert Count',InsertCount);
    ShowStrings('Purge Count',PurgeCount);
    ShowStrings('Read Idx Count',ReadIdxCount);
    ShowStrings('Read Seq Count',ReadSeqCount);
    ShowStrings('Update Count',UpdateCount);
    Memo1.Lines.Add('DB SQLDialect = ' + IntToStr(DBSQLDialect));
    ShowBoolValue(ReadOnly,'Database is Read Only','Database is Read/Write');
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

end.

