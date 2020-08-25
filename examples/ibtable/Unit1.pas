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
            
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, db, IBDatabase, IBTable, IBCustomDataSet, IBDynamicGrid,
  IB;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Datasource1: TDataSource;
    DataSource2: TDataSource;
    EmployeesFULL_NAME: TIBStringField;
    IBDatabase1: TIBDatabase;
    IBDynamicGrid1: TIBDynamicGrid;
    Employees: TIBTable;
    EmployeesDEPT_NO: TIBStringField;
    EmployeesEMP_NO: TSmallintField;
    EmployeesFIRST_NAME: TIBStringField;
    EmployeesHIRE_DATE: TDateTimeField;
    EmployeesJOB_CODE: TIBStringField;
    EmployeesJOB_COUNTRY: TIBStringField;
    EmployeesJOB_GRADE: TSmallintField;
    EmployeesLAST_NAME: TIBStringField;
    EmployeesPHONE_EXT: TIBStringField;
    EmployeesSALARY: TIBBCDField;
    Depts: TIBTable;
    DeptsBUDGET: TIBBCDField;
    DeptsDEPARTMENT: TIBStringField;
    DeptsDEPT_NO: TIBStringField;
    DeptsHEAD_DEPT: TIBStringField;
    DeptsLOCATION: TIBStringField;
    DeptsMNGR_NO: TSmallintField;
    DeptsPHONE_NO: TIBStringField;
    IBDynamicGrid2: TIBDynamicGrid;
    IBTransaction1: TIBTransaction;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    procedure CheckBox1Change(Sender: TObject);
    procedure EmployeesSALARYGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure DeptsAfterOpen(DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
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
end;

procedure TForm1.EmployeesSALARYGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if DisplayText and not Sender.IsNull then
    aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  else
    aText := Sender.AsString;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if (Sender as TCheckbox).Checked then
    Employees.Filter := 'Salary < 100000'
  else
    Employees.Filter := '';
end;

procedure TForm1.IBDatabase1AfterConnect(Sender: TObject);
begin
  Depts.Active := true;
end;

procedure TForm1.DeptsAfterOpen(DataSet: TDataSet);
begin
  Employees.Active := true;
end;

end.

