unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, ExtCtrls, ibxscript, IBDatabase, IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    EchoInput: TCheckBox;
    OpenBlobDialog: TOpenDialog;
    StopOnError: TCheckBox;
    RunScript: TAction;
    LoadScript: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    IBXScript1: TIBXScript;
    Label1: TLabel;
    Label2: TLabel;
    IBScript: TMemo;
    Label3: TLabel;
    DBName: TLabel;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    ResultsLog: TMemo;
    Timer1: TTimer;
    procedure EchoInputChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1BeforeConnect(Sender: TObject);
    procedure IBXScript1GetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure IBXScript1LogProc(Sender: TObject; Msg: string);
    procedure IBXScript1ProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
    procedure IBXScript1SelectSQL(Sender: TObject; SQLText: string);
    procedure LoadScriptExecute(Sender: TObject);
    procedure RunScriptExecute(Sender: TObject);
    procedure RunScriptUpdate(Sender: TObject);
    procedure StopOnErrorChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure DoOpen(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IBBlob, DB, Unit2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  ResultsLog.Lines.Clear;
  IBScript.Lines.Clear;
  DBName.Caption := IBDatabase1.DatabaseName;
  StopOnError.Checked := IBXScript1.StopOnFirstError;
  EchoInput.Checked :=  IBXScript1.Echo;
//  Application.QueueAsyncCall(@DoOpen,0);
end;

procedure TForm1.IBDatabase1BeforeConnect(Sender: TObject);
begin
  with (Sender as TIBDatabase) do
  begin
    LoginPrompt := (Params.IndexOfName('user_name') = -1) or
                   (Params.IndexOfName('password') = -1);
  end;
end;

procedure TForm1.EchoInputChange(Sender: TObject);
begin
  IBXScript1.Echo :=  EchoInput.Checked;
end;

procedure TForm1.IBXScript1GetParamValue(Sender: TObject; ParamName: string;
  var BlobID: TISC_QUAD);
var Blob: TIBBlobStream;
    Source: TStream;
begin
  OpenBlobDialog.Title := 'Resolve Query Parameter: ''' + ParamName + '''';
  if OpenBlobDialog.Execute then
  begin
    ResultsLog.Lines.Add('Loading ' + ParamName + ' from ' + OpenBlobDialog.FileName);
    Blob := TIBBlobStream.Create;
    try
      Blob.Database := (Sender as TIBXScript).Database;
      Blob.Mode := bmWrite;
      Source := TFileStream.Create(OpenBlobDialog.FileName,fmOpenRead or fmShareDenyNone);
      try
        Blob.CopyFrom(Source,0)
      finally
        Source.Free;
      end;
      Blob.Finalize;
      BlobID := Blob.BlobID;
    finally
      Blob.Free;
    end;
  end
  else
    raise Exception.Create('Unable to resolve statement parameter');
end;

procedure TForm1.IBXScript1LogProc(Sender: TObject; Msg: string);
begin
  ResultsLog.Lines.Add(Msg);
end;

procedure TForm1.IBXScript1ProgressEvent(Sender: TObject; Reset: boolean;
  value: integer);
begin
  if Reset then
  begin
    ProgressBar1.Position :=  0;
    ProgressBar1.Max := value;
  end
  else
    ProgressBar1.StepIt;
end;

procedure TForm1.IBXScript1SelectSQL(Sender: TObject; SQLText: string);
begin
  with TSelectSQLResults.Create(Application) do
    Show(SQLText);
end;

procedure TForm1.LoadScriptExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    IBScript.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.RunScriptExecute(Sender: TObject);
begin
  ResultsLog.Lines.Clear;
  IBXScript1.RunScript(IBScript.Lines);
  Timer1.Interval := 1000;
  EchoInput.Checked := IBXScript1.Echo;
  StopOnError.Checked := IBXScript1.StopOnFirstError;
  DBName.Caption := IBDatabase1.DatabaseName;
end;

procedure TForm1.RunScriptUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBScript.Lines.Text <> '';
end;

procedure TForm1.StopOnErrorChange(Sender: TObject);
begin
   IBXScript1.StopOnFirstError := StopOnError.Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  ProgressBar1.Position := 0;
end;

procedure TForm1.DoOpen(Data: PtrInt);
begin
  try
    IBDatabase1.Connected := true;
  except on E: Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      Application.Terminate;
    end;
  end;
end;

end.

