unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, ibxscript, IBDatabase;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure IBXScript1LogProc(Sender: TObject; Msg: string);
    procedure LoadScriptExecute(Sender: TObject);
    procedure RunScriptExecute(Sender: TObject);
    procedure RunScriptUpdate(Sender: TObject);
  private
    { private declarations }
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
  ResultsLog.Lines.Clear;
  IBScript.Lines.Clear;
  DBName.Caption := IBDatabase1.DatabaseName;
  IBDatabase1.Connected := true;
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

end.

