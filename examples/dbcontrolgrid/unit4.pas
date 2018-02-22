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
            
unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, db, IBQuery, IBLookupComboEditBox;

type

  { TEditLocation }

  TEditLocation = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Countries: TIBQuery;
    CountrySource: TDataSource;
    IBLookupComboEditBox2: TIBLookupComboEditBox;
    procedure CountriesAfterOpen(DataSet: TDataSet);
    procedure CountriesBeforeOpen(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
     FGrade: integer;
    FCountry: string;
    FJobCode: string;
public
    { public declarations }
    function ShowModal(Grade: integer; JobCode: string; var Country: string
      ): TModalResult;
  end;

var
  EditLocation: TEditLocation;

implementation

{$R *.lfm}

{ TEditLocation }

procedure TEditLocation.FormShow(Sender: TObject);
begin
  Countries.Active := true
end;

function TEditLocation.ShowModal(Grade: integer; JobCode: string;
  var Country: string): TModalResult;
begin
  FGrade := Grade;
  FCountry := Country;
  FJobCode := JobCode;
  Result := inherited ShowModal;
  if Result = mrOK then
    Country := FCountry;
end;

procedure TEditLocation.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
    FCountry := IBLookupComboEditBox2.KeyValue;
  Countries.Active := false
end;

procedure TEditLocation.CountriesBeforeOpen(DataSet: TDataSet);
begin
  Countries.ParamByName('JOB_GRADE').AsInteger := FGrade;
  Countries.ParamByName('JOB_CODE').AsString := FJobCode
end;

procedure TEditLocation.CountriesAfterOpen(DataSet: TDataSet);
begin
  IBLookupComboEditBox2.KeyValue := FCountry
end;

end.

