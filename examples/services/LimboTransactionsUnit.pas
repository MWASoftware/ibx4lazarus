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
  Grids, ActnList, db, memds, IBServices, IBDynamicGrid;

type
  { TLimboTransactionsForm }

  TLimboTransactionsForm = class(TForm)
    ApplySelectedAction: TAction;
    Commit2PhaseAll: TAction;
    RollbackAll: TAction;
    CommitAll: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    IBDynamicGrid3: TIBDynamicGrid;
    InLimboList: TMemDataset;
    Label38: TLabel;
    Label39: TLabel;
    LimboListSource: TDataSource;
    LimboReport: TMemo;
    LimboTransactionValidation: TIBValidationService;
    procedure ApplySelectedActionExecute(Sender: TObject);
    procedure Commit2PhaseAllExecute(Sender: TObject);
    procedure CommitAllExecute(Sender: TObject);
    procedure CommitAllUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure InLimboListAfterOpen(DataSet: TDataSet);
    procedure InLimboListBeforeClose(DataSet: TDataSet);
    procedure InLimboListBeforePost(DataSet: TDataSet);
    procedure RollbackAllExecute(Sender: TObject);
  private
    { private declarations }
    FLoadingLimboTr: boolean;
    procedure DoRefresh(Data: PtrInt);
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
  LimboReport.Lines.Clear;
end;

procedure TLimboTransactionsForm.InLimboListAfterOpen(DataSet: TDataSet);

function TypeToStr(MultiDatabase: boolean): string;
begin
  if MultiDatabase then
    Result := 'Multi DB'
  else
    Result := 'Single DB';
end;

function StateToStr(State: TTransactionState): string;
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

function AdviseToStr(Advise: TTransactionAdvise): string;
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

function ActionToStr(anAction: IBServices.TTransactionAction): string;
begin
  case anAction of
  CommitAction:
    Result := 'Commit';
  RollbackAction:
    Result := 'Rollback';
  end;
end;

var i: integer;
begin
  if FLoadingLimboTr then Exit;
  FLoadingLimboTr := true;
  with LimboTransactionValidation do
  try
    Active := true;
    ServiceStart;
    FetchLimboTransactionInfo;
    for i := 0 to LimboTransactionInfoCount - 1 do
    with LimboTransactionInfo[i] do
    begin
      InLimboList.Append;
      InLimboList.FieldByName('TransactionID').AsInteger := ID;
      InLimboList.FieldByName('TransactionType').AsString := TypeToStr(MultiDatabase);
      InLimboList.FieldByName('HostSite').AsString := HostSite;
      InLimboList.FieldByName('RemoteSite').AsString := RemoteSite;
      InLimboList.FieldByName('DatabasePath').AsString := RemoteDatabasePath;
      InLimboList.FieldByName('State').AsString := StateToStr(State);
      InLimboList.FieldByName('RecommendedAction').AsString := AdviseToStr(Advise);
      InLimboList.FieldByName('RequestedAction').AsString := ActionToStr(Action);
      InLimboList.Post;
    end;
  finally
    FLoadingLimboTr := false;
  end;
end;

procedure TLimboTransactionsForm.InLimboListBeforeClose(DataSet: TDataSet);
begin
  InLimboList.Clear(false);
end;

procedure TLimboTransactionsForm.InLimboListBeforePost(DataSet: TDataSet);
var i: integer;
begin
  if FLoadingLimboTr then Exit;
  with LimboTransactionValidation do
  for i := 0 to LimboTransactionInfoCount - 1 do
    with LimboTransactionInfo[i] do
    begin
      if ID = InLimboList.FieldByName('TransactionID').AsInteger then
      begin
       if InLimboList.FieldByName('RequestedAction').AsString = 'Commit' then
         Action := CommitAction
       else
         if InLimboList.FieldByName('RequestedAction').AsString = 'Rollback' then
           Action := RollbackAction;
       break;
      end;
    end;
end;

procedure TLimboTransactionsForm.RollbackAllExecute(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := RollbackGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.ApplySelectedActionExecute(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := NoGlobalAction;
  RunGFix;
end;

procedure TLimboTransactionsForm.Commit2PhaseAllExecute(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := RecoverTwoPhaseGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.CommitAllExecute(Sender: TObject);
begin
  LimboTransactionValidation.GlobalAction := CommitGlobal;
  RunGFix;
end;

procedure TLimboTransactionsForm.CommitAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := InLimboList.Active and (InLimboList.RecordCount > 0);
end;

procedure TLimboTransactionsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  InLimboList.Active := false;
end;

procedure TLimboTransactionsForm.DoRefresh(Data: PtrInt);
begin
  InLimboList.Active := false;
  InLimboList.Active := true;
end;

procedure TLimboTransactionsForm.RunGFix;
begin
  if not InLimboList.Active then
    raise Exception.Create('Limbo Transactions List not available');

  with InLimboList do
    if State = dsEdit then Post;
  LimboReport.Lines.Clear;

  with LimboTransactionValidation do
  begin
    LimboReport.Lines.Add('Starting Limbo transaction resolution');
    FixLimboTransactionErrors;
    while not Eof do
    begin
      LimboReport.Lines.Add(GetNextLine);
      Application.ProcessMessages;
    end;
    LimboReport.Lines.Add('Limbo Transaction resolution complete');
    Application.QueueAsyncCall(@DoRefresh,0);
  end;
end;

end.

