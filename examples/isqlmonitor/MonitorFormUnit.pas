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
            
unit MonitorFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBSQLMonitor;

type

  { TMonitorForm }

  TMonitorForm = class(TForm)
    IBSQLMonitor1: TIBSQLMonitor;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MonitorForm: TMonitorForm;

implementation

{$R *.lfm}

{ TMonitorForm }

procedure TMonitorForm.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
begin
  Memo1.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz',EventTime) + ': ' + EventText);
end;

procedure TMonitorForm.FormShow(Sender: TObject);
begin
  EnableMonitoring;
end;

end.

