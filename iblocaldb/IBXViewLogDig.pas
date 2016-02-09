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
 *  The Original Code is (C) 2014 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit IBXViewLogDig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TViewLogDlg }

  TViewLogDlg = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(Log: TStrings): TModalResult;
  end; 

var
  ViewLogDlg: TViewLogDlg;

function ShowViewLogDlg(Log: TStrings): TModalResult;

implementation

function ShowViewLogDlg(Log: TStrings): TModalResult;
begin
  with TViewLogDlg.Create(Application) do
  try
    Result := ShowModal(Log);
  finally
    Free
  end;
end;

{$R *.lfm}

{ TViewLogDlg }

function TViewLogDlg.ShowModal(Log: TStrings): TModalResult;
begin
  Memo1.Lines.Assign(Log);
  Result := inherited ShowModal;
  Memo1.Lines.Clear
end;

end.

