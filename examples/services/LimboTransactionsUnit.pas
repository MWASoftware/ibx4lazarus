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
  Grids, ActnList, db, IBXServices, IBDynamicGrid;

type
  { TLimboTransactionsForm }

  TLimboTransactionsForm = class(TForm)
    ApplySelectedAction: TAction;
    Commit2PhaseAll: TAction;

      IBXLimboTransactionResolutionService1: TIBXLimboTransactionResolutionService;
      InLimboList: TIBXServicesLimboTransactionsList;
    RollbackAll: TAction;
    CommitAll: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    IBDynamicGrid3: TIBDynamicGrid;
    Label38: TLabel;
    Label39: TLabel;
    LimboListSource: TDataSource;
    LimboReport: TMemo;
    procedure ApplySelectedActionExecute(Sender: TObject);
    procedure Commit2PhaseAllExecute(Sender: TObject);
    procedure CommitAllExecute(Sender: TObject);
    procedure CommitAllUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RollbackAllExecute(Sender: TObject);
  private
    { private declarations }
    procedure DoRefresh(Data: PtrInt);
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

procedure TLimboTransactionsForm.RollbackAllExecute(Sender: TObject);
begin
  InLimboList.FixErrors(RollbackGlobal,LimboReport.Lines);
end;

procedure TLimboTransactionsForm.ApplySelectedActionExecute(Sender: TObject);
begin
  InLimboList.FixErrors(NoGlobalAction,LimboReport.Lines);
end;

procedure TLimboTransactionsForm.Commit2PhaseAllExecute(Sender: TObject);
begin
  InLimboList.FixErrors(RecoverTwoPhaseGlobal,LimboReport.Lines);
end;

procedure TLimboTransactionsForm.CommitAllExecute(Sender: TObject);
begin
  InLimboList.FixErrors(CommitGlobal,LimboReport.Lines);
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

end.

