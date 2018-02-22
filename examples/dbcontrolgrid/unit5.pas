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
            
unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, db, IBQuery, IBLookupComboEditBox;

type

  { TEditJobCode }

  TEditJobCode = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    JobCodes: TIBQuery;
    JobCodeSource: TDataSource;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure JobCodesAfterOpen(DataSet: TDataSet);
    procedure JobCodesBeforeOpen(DataSet: TDataSet);
  private
    { private declarations }
    FGrade: integer;
    FCountry: string;
    FJobCode: string;
  public
    { public declarations }
    function ShowModal(Grade: integer; Country: string; var JobCode:string): TModalResult;
  end;

var
  EditJobCode: TEditJobCode;

implementation

{$R *.lfm}

{ TEditJobCode }

procedure TEditJobCode.FormShow(Sender: TObject);
begin
  JobCodes.Active := true;
end;

procedure TEditJobCode.JobCodesAfterOpen(DataSet: TDataSet);
begin
  IBLookupComboEditBox1.KeyValue := FJobCode;
end;

procedure TEditJobCode.JobCodesBeforeOpen(DataSet: TDataSet);
begin
  JobCodes.ParamByName('JOB_GRADE').AsInteger := FGrade;
  JobCodes.ParamByName('JOB_COUNTRY').AsString := FCountry;
end;

function TEditJobCode.ShowModal(Grade: integer; Country: string; var JobCode: string
  ): TModalResult;
begin
  FGrade := Grade;
  FCountry := Country;
  FJobCode := JobCode;
  Result := inherited ShowModal;
  if Result = mrOK then
    JobCode := FJobCode;
end;

procedure TEditJobCode.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
    FJobCode := IBLookupComboEditBox1.KeyValue;
  JobCodes.Active := false
end;

end.

