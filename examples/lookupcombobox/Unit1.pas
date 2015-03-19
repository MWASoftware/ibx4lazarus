unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DbCtrls, EditBtn, Buttons, db, IBDatabase, IBCustomDataSet,
  IBLookupComboEditBox,  IBQuery, DBExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    DBEdit5: TDBEdit;
    DeleteBtn: TButton;
    CountriesSource: TDataSource;
    EmployeesDEPT_KEY_PATH: TIBStringField;
    EmployeesDEPT_PATH: TIBStringField;
    IBDateEdit1: TDBDateEdit;
    IBLookupComboEditBox2: TIBLookupComboEditBox;
    IBLookupComboEditBox3: TIBLookupComboEditBox;
    JobCodeSource: TDataSource;
    DBComboBox1: TDBComboBox;
    DBEdit4: TDBEdit;
    Countries: TIBQuery;
    JobCodes: TIBQuery;
    Label10: TLabel;
    Label11: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SaveBtn: TButton;
    CancelBtn: TButton;
    EmployeeSource: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    EmployeesDEPT_NO: TIBStringField;
    EmployeesEMP_NO: TSmallintField;
    EmployeesFIRST_NAME: TIBStringField;
    EmployeesFULL_NAME: TIBStringField;
    EmployeesHIRE_DATE: TDateTimeField;
    EmployeesJOB_CODE: TIBStringField;
    EmployeesJOB_COUNTRY: TIBStringField;
    EmployeesJOB_GRADE: TSmallintField;
    EmployeesLAST_NAME: TIBStringField;
    EmployeesPHONE_EXT: TIBStringField;
    EmployeesSALARY: TIBBCDField;
    IBDatabase1: TIBDatabase;
    Employees: TIBDataSet;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure CancelBtnClick(Sender: TObject);
    procedure CountriesBeforeOpen(DataSet: TDataSet);
    procedure EmployeeSourceDataChange(Sender: TObject; Field: TField);
    procedure DBComboBox1CloseUp(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EmployeesAfterDelete(DataSet: TDataSet);
    procedure EmployeesAfterInsert(DataSet: TDataSet);
    procedure EmployeesAfterPost(DataSet: TDataSet);
    procedure EmployeesAfterTransactionEnd(Sender: TObject);
    procedure EmployeesFULL_NAMEChange(Sender: TField);
    procedure EmployeesJOB_CODEChange(Sender: TField);
    procedure EmployeesJOB_GRADEChange(Sender: TField);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBLookupComboEditBox1CanAutoInsert(Sender: TObject;
      aText: string; var Accept: boolean);
    procedure JobCodesBeforeOpen(DataSet: TDataSet);
    procedure SaveBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FDirty: boolean;
    FClosing: boolean;
    FLastJobGrade: integer;
    FLastJobCode: string;
    procedure OpenDataSets(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2, IB;

{$R *.lfm}

{ TForm1 }

procedure TForm1.EmployeesSALARYGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if not Sender.IsNull and DisplayText then
    aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  else
    aText := Sender.AsString
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FClosing := false;
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Close;
        Exit
      end;
    On E:Exception do
     MessageDlg(E.Message,mtError,[mbOK],0);
    end;
  until IBDatabase1.Connected;
  OpenDataSets(0)
end;

procedure TForm1.IBLookupComboEditBox1CanAutoInsert(Sender: TObject;
  aText: string; var Accept: boolean);
begin
  Accept := MessageDlg(Format('Insert a new Employee record for ''%s''?',
              [aText]),mtConfirmation,[mbYes,mbNo],0) = mrYes
end;

procedure TForm1.JobCodesBeforeOpen(DataSet: TDataSet);
begin
  JobCodes.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  FLastJobGrade :=  EmployeesJOB_GRADE.AsInteger;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  IBTransaction1.Commit
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var DeptNo: string;
begin
  if SelectDeptDlg.ShowModal(EmployeesDEPT_KEY_PATH.AsString,DeptNo) = mrOK then
  begin
    Employees.Edit;
    EmployeesDEPT_NO.AsString := DeptNo;
    try
      Employees.Post;
    except
      Employees.Cancel;
      raise;
    end;
  end;
end;

procedure TForm1.OpenDataSets(Data: PtrInt);
begin
  FDirty := false;
  IBTransaction1.StartTransaction;
  JobCodes.Active := true;
  Countries.Active := true;
  Employees.Active := true;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  SaveBtn.Enabled := FDirty;
  CancelBtn.Enabled := FDirty;
  DeleteBtn.Enabled := Employees.Active and (Employees.RecordCount > 0)
end;

procedure TForm1.CancelBtnClick(Sender: TObject);
begin
  if Employees.State in [dsInsert,dsEdit] then
    Employees.Cancel;
  IBTransaction1.Rollback
end;

procedure TForm1.CountriesBeforeOpen(DataSet: TDataSet);
begin
  Countries.ParamByName('JOB_GRADE').AsInteger := EmployeesJOB_GRADE.AsInteger;
  Countries.ParamByName('JOB_CODE').AsString := EmployeesJOB_CODE.AsString;
  FLastJobGrade :=  EmployeesJOB_GRADE.AsInteger;
  FLastJobCode := EmployeesJOB_CODE.AsString;
end;

procedure TForm1.EmployeeSourceDataChange(Sender: TObject; Field: TField);
begin
  if FLastJobGrade <>  EmployeesJOB_GRADE.AsInteger then
  begin
    JobCodes.Active := false;
    JobCodes.Active := true;
    Countries.Active := false;
    Countries.Active := true;
  end
  else
  if FLastJobCode <> EmployeesJOB_CODE.AsString then
  begin
    Countries.Active := false;
    Countries.Active := true;
  end;
end;

procedure TForm1.DBComboBox1CloseUp(Sender: TObject);
begin
  (Sender as TCustomDBComboBox).EditingDone
end;

procedure TForm1.DeleteBtnClick(Sender: TObject);
begin
  if MessageDlg(Format('Do you really want to delete ''%s''?',
                           [EmployeesFULL_NAME.AsString]),
                           mtConfirmation,[mbYes,mbNo],0) = mrYes then
    Employees.Delete
end;

procedure TForm1.EmployeesAfterDelete(DataSet: TDataSet);
begin
  FDirty := true
end;

procedure TForm1.EmployeesAfterInsert(DataSet: TDataSet);
begin
  FDirty := true;
  EmployeesJOB_COUNTRY.AsString := 'USA';
  EmployeesJOB_CODE.AsString := 'SRep';
  EmployeesJOB_GRADE.AsInteger := 4;
  EmployeesSALARY.AsCurrency := 20000;
  EmployeesFIRST_NAME.AsString := 'John';
  EmployeesLAST_NAME.AsString := 'Doe';
  EmployeesHIRE_DATE.AsDateTime := now;
  EmployeesDEPT_NO.AsString := '000';
end;

procedure TForm1.EmployeesAfterPost(DataSet: TDataSet);
begin
  Employees.Refresh;
end;

procedure TForm1.EmployeesAfterTransactionEnd(Sender: TObject);
begin
  if not FClosing then
    Application.QueueAsyncCall(@OpenDataSets,0)
end;

procedure TForm1.EmployeesFULL_NAMEChange(Sender: TField);
var I: integer;
    aText: string;
begin
  aText := Sender.AsString;
  I := Pos(',',aText);
  Employees.Edit;
  if I > 0 then
  begin
    EmployeesLAST_NAME.AsString := system.copy(aText,1,I-1);
    EmployeesFIRST_NAME.AsString := Trim(system.copy(aText,I+1,Length(aText) - I));
  end
  else
  begin
    EmployeesLAST_NAME.AsString := aText;
    EmployeesFIRST_NAME.AsString := '!!unknown!!';
  end;
end;

procedure TForm1.EmployeesJOB_CODEChange(Sender: TField);
begin
  Countries.Active := false;
  Countries.Active := true;
end;

procedure TForm1.EmployeesJOB_GRADEChange(Sender: TField);
begin
  JobCodes.Active := false;
  JobCodes.Active := true;
  Countries.Active := false;
  Countries.Active := true;
end;

end.

