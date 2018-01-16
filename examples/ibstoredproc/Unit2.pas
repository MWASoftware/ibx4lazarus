unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ibxscript;

type

  { TDBCreateForm }

  TDBCreateForm = class(TForm)
    IBXScript1: TIBXScript;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure FormShow(Sender: TObject);
  private
    procedure DoCreateDatabase(Data: PtrInt);
    procedure NotifyDBCreated;
  public

  end;

var
  DBCreateForm: TDBCreateForm;

implementation

{$R *.lfm}

type

  { TCreateDBThread }

  TCreateDBThread = class(TThread)
  private
    FOwner: TDBCreateForm;
    procedure NotifyDBCreated;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TDBCreateForm);
  end;

{ TCreateDBThread }

procedure TCreateDBThread.NotifyDBCreated;
begin
  FOwner.NotifyDBCreated;
end;

procedure TCreateDBThread.Execute;
begin
  FOwner.IBXScript1.Transaction.Active := true;
  FOwner.IBXScript1.RunScript('fbout-header.sql');
  FOwner.IBXScript1.RunScript('fbout-body.sql');
  FOwner.IBXScript1.RunScript('fbout-test.sql');
  FOwner.IBXScript1.Transaction.Commit;
  Synchronize(@NotifyDBCreated);
end;

constructor TCreateDBThread.Create(aOwner: TDBCreateForm);
begin
  inherited Create(true);
  FOwner := aOwner;
  FreeOnTerminate := true;
end;

{ TDBCreateForm }

procedure TDBCreateForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoCreateDatabase,0);
end;

procedure TDBCreateForm.DoCreateDatabase(Data: PtrInt);
var aThread: TCreateDBThread;
begin
  aThread := TCreateDBThread.Create(self);
  aThread.Resume;
end;

procedure TDBCreateForm.NotifyDBCreated;
begin
  ModalResult := mrOK;
end;

end.

