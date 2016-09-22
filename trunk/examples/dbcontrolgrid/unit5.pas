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

