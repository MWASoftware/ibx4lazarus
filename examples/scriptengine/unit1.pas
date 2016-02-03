unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, ibxscript, IBDatabase, IBHeader;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure FormShow(Sender: TObject);
    procedure IBXScript1GetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure IBXScript1LogProc(Sender: TObject; Msg: string);
    procedure LoadScriptExecute(Sender: TObject);
    procedure RunScriptExecute(Sender: TObject);
    procedure RunScriptUpdate(Sender: TObject);
    procedure StopOnErrorChange(Sender: TObject);
  private
    { private declarations }
    procedure DoOpen(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IBBlob, DB;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  ResultsLog.Lines.Clear;
  IBScript.Lines.Clear;
  DBName.Caption := IBDatabase1.DatabaseName;
  StopOnError.Checked := IBXScript1.StopOnFirstError;
  Application.QueueAsyncCall(@DoOpen,0);
end;

procedure TForm1.IBXScript1GetParamValue(Sender: TObject; ParamName: string;
  var BlobID: TISC_QUAD);
var Blob: TIBBlobStream;
    Source: TStream;
begin
  OpenBlobDialog.Title := 'Resolve Query Parameter: ''' + ParamName + '''';
  if OpenBlobDialog.Execute then
  begin
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

procedure TForm1.LoadScriptExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    IBScript.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.RunScriptExecute(Sender: TObject);
var S: TMemoryStream;
begin
  ResultsLog.Lines.Clear;
  S := TMemoryStream.Create;
  try
   IBScript.Lines.SaveToStream(S);
   S.Position := 0;
   IBXScript1.PerformUpdate(S,ProgressBar1,true);
  finally
    S.Free;
  end;
end;

procedure TForm1.RunScriptUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBScript.Lines.Text <> '';
end;

procedure TForm1.StopOnErrorChange(Sender: TObject);
begin
   IBXScript1.StopOnFirstError := StopOnError.Checked;
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

