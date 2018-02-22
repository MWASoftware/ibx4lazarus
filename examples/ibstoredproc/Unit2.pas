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

