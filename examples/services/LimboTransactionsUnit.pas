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
            
unit LimboTransactionsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, IBServices;

type
  { TLimboTransactionsForm }

  TLimboTransactionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    LimboTransactionValidation: TIBValidationService;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
  private
    { private declarations }
    procedure DoRefresh(Data: PtrInt);
    function StateToStr(State: TTransactionState): string;
    function AdviseToStr(Advise: TTransactionAdvise): string;
    function ActionToStr(anAction: TTransactionAction): string;
    procedure RunGFix;
  public
    { public declarations }
  end;

var
  LimboTransactionsForm: TLimboTransactionsForm;

implementation

{$R *.lfm}

uses MainFormUnit;

{ TLimboTransactionsForm }

procedure TLimboTransactionsForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh,0);
end;

procedure TLimboTransactionsForm.Button1Click(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := CommitGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.Button2Click(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := RollbackGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.Button3Click(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := NoGlobalAction;
  RunGFix;
end;

procedure TLimboTransactionsForm.Button4Click(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := RecoverTwoPhaseGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
end;

procedure TLimboTransactionsForm.StringGrid1EditingDone(Sender: TObject);
begin
  with StringGrid1, LimboTransactionValidation do
  if col = 7 then
  begin
    if Cells[7,row] = 'Commit' then
      LimboTransactionInfo[row-1].Action := CommitAction
    else
      if Cells[7,row] = 'Rollback' then
        LimboTransactionInfo[row-1].Action := RollbackAction
  end;
end;

procedure TLimboTransactionsForm.DoRefresh(Data: PtrInt);
var i: integer;
begin
  with LimboTransactionValidation do
  begin
    Active := true;
    ServiceStart;
    FetchLimboTransactionInfo;
    StringGrid1.RowCount := LimboTransactionInfoCount + 1;
    for i := 0 to LimboTransactionInfoCount - 1 do
    with LimboTransactionInfo[i] do
    begin
      StringGrid1.Cells[0,i+1] := IntToStr(ID);
      if MultiDatabase then
        StringGrid1.Cells[1,i+1] := 'Multi DB'
      else
        StringGrid1.Cells[1,i+1] := 'Single DB';
      StringGrid1.Cells[2,i+1] := HostSite;
      StringGrid1.Cells[3,i+1] := RemoteSite;
      StringGrid1.Cells[4,i+1] := RemoteDatabasePath;
      StringGrid1.Cells[5,i+1] := StateToStr(State);
      StringGrid1.Cells[6,i+1] := AdviseToStr(Advise);
      StringGrid1.Cells[7,i+1] := ActionToStr(Action);
    end;
  end;
end;

function TLimboTransactionsForm.StateToStr(State: TTransactionState): string;
begin
  case State of
  LimboState:
    Result := 'Limbo';
  CommitState:
    Result := 'Commit';
  RollbackState:
    Result := 'Rollback';
  else
    Result := 'Unknown';
  end;
end;

function TLimboTransactionsForm.AdviseToStr(Advise: TTransactionAdvise): string;
begin
  case Advise of
  CommitAdvise:
    Result := 'Commit';
  RollbackAdvise:
    Result := 'Rollback';
  else
    Result := 'Unknown';
  end;
end;

function TLimboTransactionsForm.ActionToStr(anAction: TTransactionAction
  ): string;
begin
  case anAction of
  CommitAction:
    Result := 'Commit';
  RollbackAction:
    Result := 'Rollback';
  end;
end;

procedure TLimboTransactionsForm.RunGFix;
begin
  with LimboTransactionValidation do
  begin
    MainForm.Memo1.Lines.Add('Starting Limbo transaction resolution');
    FixLimboTransactionErrors;
    while not Eof do
    begin
      MainForm.Memo1.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
    MainForm.Memo1.Lines.Add('Limbo Transaction resolution complete');
    Application.QueueAsyncCall(@DoRefresh,0);
  end;
end;

end.

