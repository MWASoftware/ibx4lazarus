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
  StdCtrls, DbCtrls, ActnList, Menus, db, DBTreeView, IBTreeView, IBDatabase,
  IBCustomDataSet, IBLookupComboEditBox, IBQuery, IBDynamicGrid, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddFirstChild: TAction;
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
    DepartmentsCHILDCOUNT: TIBLargeIntField;
    DepartmentsIMAGEINDEX: TIBIntegerField;
    IBDynamicGrid1: TIBDynamicGrid;
    IBLookupComboEditBox1: TIBLookupComboEditBox;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
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
    procedure AddFirstChildExecute(Sender: TObject);
    procedure AddSiblingExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteNodeExecute(Sender: TObject);
    procedure DeleteNodeUpdate(Sender: TObject);
    procedure DepartmentsAfterDelete(DataSet: TDataSet);
    procedure DepartmentsAfterInsert(DataSet: TDataSet);
    procedure DepartmentsAfterTransactionEnd(Sender: TObject);
    procedure DepartmentsBUDGETGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBTreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure IBTreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SaveBtnClick(Sender: TObject);
  private
    { private declarations }
    FDirty: boolean;
    FClosing: boolean;
    procedure Reopen(Data: PtrInt);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses IB;

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := true;
  if IBTransaction1.Intransaction then
    IBTransaction1.Commit;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  SaveBtn.Enabled := FDirty;
  CancelBtn.Enabled := FDirty
end;

procedure TForm1.AddChildExecute(Sender: TObject);
begin
  IBTreeView1.Selected.Expand(true);
  IBTreeView1.Selected := IBTreeView1.Items.AddChild(IBTreeView1.Selected,'');
  IBTreeView1.Selected.Expand(true);
  IBTreeView1.Selected.EditText;
end;

procedure TForm1.AddFirstChildExecute(Sender: TObject);
begin
  IBTreeView1.Selected.Expand(true);
  IBTreeView1.Selected := IBTreeView1.Items.AddChildFirst(IBTreeView1.Selected,'');
  IBTreeView1.Selected.Expand(true);
  IBTreeView1.Selected.EditText;
end;

procedure TForm1.AddSiblingExecute(Sender: TObject);
begin
  IBTreeView1.Selected := IBTreeView1.Items.Add(IBTreeView1.Selected,'');
  IBTreeView1.Selected.EditText;
end;

procedure TForm1.CancelBtnClick(Sender: TObject);
begin
  IBTransaction1.Rollback
end;

procedure TForm1.DeleteNodeExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you want to delete the %s department?',[IBTreeview1.Selected.Text]),
              mtConfirmation,[mbYes,mbNo],0) = mrYes then
    TDBTreeNode(IBTreeview1.Selected).DeleteAll
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
  DataSet.FieldByName('Department').AsString := 'Dept ' + DataSet.FieldByName('DEPT_NO').AsString
end;

procedure TForm1.DepartmentsAfterTransactionEnd(Sender: TObject);
begin
  if not FClosing then
    Application.QueueAsyncCall(@Reopen,0);
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
  Reopen(0);
end;

procedure TForm1.IBTreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var Node: TTreeNode;
    tv: TTreeView;
begin
  if Source = Sender then {Dragging within Tree View}
  begin
    tv := TTreeView(Sender);;
    Node := tv.GetNodeAt(X,Y); {Drop Point}
    if assigned(tv.Selected) and (tv.Selected <> Node) then
    begin
      if Node = nil then
        tv.Selected.MoveTo(nil,naAdd) {Move to Top Level}
      else
      begin
        if ssCtrl in GetKeyShiftState then
        begin
          Node.Expand(false);
          tv.Selected.MoveTo(Node,naAddChildFirst)
        end
        else
          tv.Selected.MoveTo(Node,naInsertBehind)
      end;
    end;
  end;
end;

procedure TForm1.IBTreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Sender
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
end;

end.

