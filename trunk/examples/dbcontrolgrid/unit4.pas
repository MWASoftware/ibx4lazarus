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

