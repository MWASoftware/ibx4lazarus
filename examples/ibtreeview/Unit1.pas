unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, DbCtrls, ActnList, Menus, db, IBTreeView, IBDatabase,
  IBCustomDataSet, IBLookupComboEditBox, IBQuery, IBDynamicGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddSibling: TAction;
    AddChild: TAction;
    DeleteNode: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DepartmentsCHILDCOUNT: TIntegerField;
    IBDynamicGrid1: TIBDynamicGrid;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    Staff: TIBQuery;
    Label7: TLabel;
    Managers: TIBQuery;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel3: TPanel;
    SaveBtn: TButton;
    CancelBtn: TButton;
    DataSource1: TDataSource;
    DepartmentsBUDGET: TIBBCDField;
    DepartmentsDEPARTMENT: TIBStringField;
    DepartmentsDEPT_NO: TIBStringField;
    DepartmentsHEAD_DEPT: TIBStringField;
    DepartmentsLOCATION: TIBStringField;
    DepartmentsMNGR_NO: TSmallintField;
    DepartmentsPHONE_NO: TIBStringField;
    IBDatabase1: TIBDatabase;
    Departments: TIBDataSet;
    IBTransaction1: TIBTransaction;
    IBTreeView1: TIBTreeView;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StaffDEPT_NO: TIBStringField;
    StaffEMP_NO: TSmallintField;
    StaffFIRST_NAME: TIBStringField;
    StaffFULL_NAME: TIBStringField;
    StaffHIRE_DATE: TDateTimeField;
    StaffJOB_CODE: TIBStringField;
    StaffJOB_COUNTRY: TIBStringField;
    StaffJOB_GRADE: TSmallintField;
    StaffLAST_NAME: TIBStringField;
    StaffPHONE_EXT: TIBStringField;
    StaffSALARY: TIBBCDField;
    procedure AddChildExecute(Sender: TObject);
    procedure AddSiblingExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteNodeExecute(Sender: TObject);
    procedure DeleteNodeUpdate(Sender: TObject);
    procedure DepartmentsAfterDelete(DataSet: TDataSet);
    procedure DepartmentsAfterInsert(DataSet: TDataSet);
    procedure DepartmentsAfterTransactionEnd(Sender: TObject);
    procedure DepartmentsBeforeClose(DataSet: TDataSet);
    procedure DepartmentsBeforeScroll(DataSet: TDataSet);
    procedure DepartmentsBeforeTransactionEnd(Sender: TObject);
    procedure DepartmentsBUDGETGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { private declarations }
    FDirty: boolean;
    FClosing: boolean;
    FNodePath: TVariantArray;
    procedure Reopen(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  Staff.Active := false;
  Departments.Active := false;
  Managers.Active := false;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  SaveBtn.Enabled := FDirty;
  CancelBtn.Enabled := FDirty
end;

procedure TForm1.AddChildExecute(Sender: TObject);
begin
  IBTreeView1.Items.AddChild(IBTreeView1.Selected,'<a new department>');
  IBTreeView1.Selected.Expand(true)
end;

procedure TForm1.AddSiblingExecute(Sender: TObject);
begin
  IBTreeView1.Items.Add(IBTreeView1.Selected,'<a new department>');
end;

procedure TForm1.CancelBtnClick(Sender: TObject);
begin
  IBTransaction1.Rollback
end;

procedure TForm1.DeleteNodeExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you want to delete the %s department?',[IBTreeview1.Selected.Text]),
              mtConfirmation,[mbYes,mbNo],0) = mrYes then
    TIBTreeNode(IBTreeview1.Selected).DeleteAll
end;

procedure TForm1.DeleteNodeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBTreeView1.Selected <> nil
end;

procedure TForm1.DepartmentsAfterDelete(DataSet: TDataSet);
begin
  FDirty := true
end;

procedure TForm1.DepartmentsAfterInsert(DataSet: TDataSet);
begin
  FDirty := true;
end;

procedure TForm1.DepartmentsAfterTransactionEnd(Sender: TObject);
begin
  if not FClosing then
    Application.QueueAsyncCall(@Reopen,0);
end;

procedure TForm1.DepartmentsBeforeClose(DataSet: TDataSet);
begin
  with DataSet do
    try
      if State in [dsInsert,dsEdit] then Post
    except
      Cancel;
      raise
    end;
end;

procedure TForm1.DepartmentsBeforeScroll(DataSet: TDataSet);
begin
  with DataSet do
    try
      if State in [dsInsert,dsEdit] then Post
    except
      Cancel;
      raise
    end;
end;

procedure TForm1.DepartmentsBeforeTransactionEnd(Sender: TObject);
begin
  FNodePath := IBTreeView1.GetSelectNodePath;
end;

procedure TForm1.DepartmentsBUDGETGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  if DisplayText and not Sender.IsNull then
    aText := FormatFloat('$#,##0.00',Sender.AsFloat)
  else
    aText := Sender.AsString
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FDirty := false;
  IBDatabase1.Connected := true;
  IBTransaction1.StartTransaction;
  Managers.Active := true;
  Departments.Active := true;
  Staff.Active := true;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
begin
  IBtransaction1.Commit
end;

procedure TForm1.Reopen(Data: PtrInt);
begin
  FDirty := false;
  IBTransaction1.StartTransaction;
  Managers.Active := true;
  Departments.Active := true;
  Staff.Active := true;
  if Length(FNodePath) > 0 then
    IBTreeView1.FindNode(FNodePath,true);
end;

end.

