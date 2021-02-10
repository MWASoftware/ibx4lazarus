unit Test23;

{$mode objfpc}{$H+}

{Test 23: Transliteration Tests

 A test database is created with a row containing text data types and then
 initialised to text is European Character sets i.e. not just ASCII.

 The text is read back both as text and as hex characters with various
 connection character sets.
}

interface

uses
  Classes, SysUtils,   TestApplication, IBXTestBase, IB, IBSQL;

const
  aTestID    = '23';
  aTestTitle = 'Transliteration Tests';

type

{ TTest23 }

  TTest23 = class(TIBXTestBase)
  private
    FIBSQL: TIBSQL;
    procedure AddRow;
    procedure ShowData;
  protected
    procedure CreateObjects(Application: TTestApplication); override;
    function GetTestID: AnsiString; override;
    function GetTestTitle: AnsiString; override;
    procedure InitTest; override;
  public
    procedure RunTest(CharSet: AnsiString; SQLDialect: integer); override;
  end;


implementation

{ TTest23 }

procedure TTest23.AddRow;
var b: IBlob;
begin
  with FIBSQL do
  begin
    SQL.Text := 'Insert into TestData(RowID,Title,Notes, BlobData,InClear) Values(:RowID,:Title,:Notes,:BlobData,:InClear)';
    Transaction.Active := true;
    ParamByName('rowid').AsInteger := 1;
    ParamByName('title').AsString := 'Blob Test ©€';
    ParamByName('Notes').AsString := 'Écoute moi';
    b := IBDatabase.Attachment.CreateBlob(Transaction.TransactionIntf,'TestData','BlobData');
    b. AsString :='Some German Special Characters like ÖÄÜöäüß';
    ParamByName('BlobData').AsBlob := b;
    ParamByName('InClear').AsString := #$01'Test'#$0D#$C3;
    ExecQuery;
  end;
end;

procedure TTest23.ShowData;
begin
  writeln(Outfile,'Default Character Set = ' + IBDatabase.DefaultCharSetName);
  writeln(Outfile);
  with FIBSQL do
  begin
    SQL.Text := 'Select A.ROWID, A.TITLE, A.NOTES, A.BLOBDATA, A.INCLEAR From TESTDATA A';
    Transaction.Active := true;
    ExecQuery;
    ReportResult(Current);
  end;
  writeln(Outfile);
end;

procedure TTest23.CreateObjects(Application: TTestApplication);
begin
  inherited CreateObjects(Application);
  FIBSQL := TIBSQL.Create(Application);
  FIBSQL.Database := IBDatabase;
end;

function TTest23.GetTestID: AnsiString;
begin
  Result := aTestID;
end;

function TTest23.GetTestTitle: AnsiString;
begin
  Result := aTestTitle;
end;

procedure TTest23.InitTest;
begin
  IBDatabase.DatabaseName := Owner.GetNewDatabaseName;
  IBDatabase.CreateIfNotExists := true;
  ReadWriteTransaction;
end;

procedure TTest23.RunTest(CharSet: AnsiString; SQLDialect: integer);
var index: integer;
begin
  IBDatabase.Connected := true;
  try
    AddRow;
    ShowData;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=WIN1252';
    IBDatabase.Connected := true;
    ShowData;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=NONE';
    IBDatabase.Connected := true;
    ShowData;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=UTF8';
    IBDatabase.Connected := true;
    ShowData;
    FHexStrings := true;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=WIN1252';
    IBDatabase.Connected := true;
    ShowData;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=NONE';
    IBDatabase.Connected := true;
    ShowData;
    IBDatabase.Connected := false;
    index := IBDatabase.Params.IndexOfName('lc_ctype');
    IBDatabase.Params[index] := 'lc_ctype=UTF8';
    IBDatabase.Connected := true;
    ShowData;
  finally
    IBDatabase.DropDatabase;
  end;
end;

initialization
  RegisterTest(TTest23);

end.

