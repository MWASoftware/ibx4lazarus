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
            
(*
 * ShutdownDatabaseDlgUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit ShutdownDatabaseDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBServices, IB;

type

  { TShutdownDatabaseDlg }

  TShutdownDatabaseDlg = class(TForm)
    Bevel1: TBevel;
    CloseBtn: TButton;
    IBConfigService: TIBConfigService;
    ProgressBar1: TProgressBar;
    StatusMsg: TLabel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FShutDownmode: TShutdownMode;
    FDelay: integer;
    FAborting: boolean;
    FSecContextError: boolean;
    FShutdownWaitThread: TThread;
    procedure OnWaitCompleted(Sender: TObject);
  public
     procedure Shutdown(aService: TIBConfigService; aShutDownmode: TShutdownMode;
       aDelay: integer);
     property Aborting: boolean read FAborting;
  end;

var
  ShutdownDatabaseDlg: TShutdownDatabaseDlg;

implementation

{$R *.lfm}

uses IBErrorCodes;

resourcestring
  sWaitStatusMsg = 'Waiting for %s to shutdown';
  sDatabaseShutdown  = 'Database has been successfully shutdown';
  sOnCompleted = 'Shutdown of %s completed with response: %s';

type
    { TShutdownWaitThread }

  TShutdownWaitThread = class(TThread)
  private
    FErrorMessage: string;
    FIBConfigService: TIBConfigService;
    FOptions: TShutdownMode;
    FSecContextError: boolean;
    FSuccess: boolean;
    FWait: integer;
    FOnCompleted: TNotifyEvent;
    procedure DoCallback;
  protected
    procedure Execute; override;
  public
    constructor Create(aService: TIBConfigService; Options: TShutdownMode;
      Wait: Integer; OnCompleted: TNotifyEvent);
    destructor Destroy; override;
    procedure Abort;
    property Success: boolean read FSuccess;
    property SecContextError: boolean read FSecContextError;
    property ErrorMessage: string read FErrorMessage;
  end;



{ TShutdownWaitThread }

procedure TShutdownWaitThread.DoCallback;
begin
  if assigned(FOnCompleted) then
    FOnCompleted(self);
end;

procedure TShutdownWaitThread.Execute;
begin
  FSuccess := false;
  FIBConfigService.Active := true;
  try
    try
      FIBConfigService.ShutDownDatabase(FOptions,FWait);
      FErrorMessage := 'Completed without error';
      FSuccess := true;
    except
      on E: EIBInterBaseError do
        if E.IBErrorCode = isc_sec_context then
          FSecContextError := true
        else
          FErrorMessage := E.Message;

      on E: Exception do
        FErrorMessage := E.Message;
    end;
  finally
    if not FSecContextError then
    try
      while FIBConfigService.IsServiceRunning do;
    except   end;
    if Terminated and FSuccess then
      FIBConfigService.BringDatabaseOnline;
    FIBConfigService.Active := false;
  end;
  Synchronize(@DoCallback);
end;

constructor TShutdownWaitThread.Create(aService: TIBConfigService;
  Options: TShutdownMode; Wait: Integer; OnCompleted: TNotifyEvent);
begin
  inherited Create(false);
  FOptions := Options;
  FWait := Wait;
  FOnCompleted := OnCompleted;
  FreeOnTerminate := true;
  FIBConfigService := TIBConfigService.Create(nil);
  FIBConfigService.Assign(aService);
  FIBConfigService.DatabaseName := aService.DatabaseNAme;
end;

destructor TShutdownWaitThread.Destroy;
begin
  if FIBConfigService <> nil then FIBConfigService.Free;
  inherited Destroy;
end;

procedure TShutdownWaitThread.Abort;
begin
  Terminate;
end;

{ TShutdownDatabaseDlg }

procedure TShutdownDatabaseDlg.FormShow(Sender: TObject);
begin
  FAborting := false;
  StatusMsg.Caption := Format(sWaitStatusMsg,[IBConfigService.DatabaseName]);
  FShutdownWaitThread := TShutdownWaitThread.Create(IBConfigService,FShutDownMode,FDelay,@OnWaitCompleted);
end;

procedure TShutdownDatabaseDlg.CloseBtnClick(Sender: TObject);
begin
  FAborting := true;
  FShutdownWaitThread.Terminate;
  Close;
end;

procedure TShutdownDatabaseDlg.OnWaitCompleted(Sender: TObject);
begin
  with TShutdownWaitThread(Sender) do
    if not Success and SecContextError then
      self.FSecContextError := true
    else
    if not FAborting then
      MessageDlg(Format(sOnCompleted,[IBConfigService.DatabaseName,ErrorMessage]),
               mtInformation,[mbOK],0);
  FAborting := false;
  Close;
end;

procedure TShutdownDatabaseDlg.Shutdown(aService: TIBConfigService;
  aShutDownmode: TShutdownMode; aDelay: integer);
begin
  IBConfigService.Assign(aService);
  IBConfigService.DatabaseName := aService.DatabaseName;
  FShutDownmode := aShutDownmode;
  FDelay := aDelay;
  FSecContextError := false;
  if aDelay <= 0 then
  begin
    IBConfigService.Active := true;
    try
      IBConfigService.ShutDownDatabase(aShutDownmode,0);
      while IBConfigService.IsServiceRunning do;
      if aDelay = 0 then
        MessageDlg(sDatabaseShutdown,mtInformation,[mbOK],0);
    finally
      IBConfigService.Active := false;
    end
  end
  else
  begin
    ShowModal;
    if FSecContextError  then
      raise EIBInterBaseError.Create(FirebirdAPI.getStatus); {re-raise the error}
  end;
end;

end.

