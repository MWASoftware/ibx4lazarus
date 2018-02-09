unit DatabasePropertiesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Buttons, DBExtCtrls, ActnList, ExtCtrls, db, IBDatabaseInfo, IBQuery,
  IBSQL, IBDatabase, IBServices, IB;

type

  { TDatabaseProperties }

  TDatabaseProperties = class(TForm)
    AllocatedPages: TEdit;
    Bevel1: TBevel;
    Button1: TButton;
    DatabaseOnline: TCheckBox;
    DatabaseQuery: TIBQuery;
    DatabaseSource: TDataSource;
    DBDateEdit1: TDBDateEdit;
    DBEdit1: TDBEdit;
    DBEdit3: TDBEdit;
    DBIsReadOnly: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    IBConfigService1: TIBConfigService;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    IBStatisticalService1: TIBStatisticalService;
    IBTransaction1: TIBTransaction;
    IsShadowChk: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NoReserve: TCheckBox;
    PageSize: TEdit;
    SecDatabase: TDBEdit;
    SetLinger: TIBSQL;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeOpen(DataSet: TDataSet);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
  private
    FDatabaseOnline: boolean;
    FShadowDatabase: boolean;
    FLoading: boolean;
    procedure GetDBFlags;
    procedure LoadData;
    procedure Connect;
  public
    function ShowModal(ActiveService: TIBCustomService; DBName: string; aPassword: string): TModalResult;
  end;

var
  DatabaseProperties: TDatabaseProperties;

implementation

{$R *.lfm}

uses IBUtils, MainFormUnit, DBLoginDlgUnit, FBMessages, IBErrorCodes;

{ TDatabaseProperties }

procedure TDatabaseProperties.IBDatabase1AfterConnect(Sender: TObject);
begin
  IBTransaction1.Active := true;
  with IBDatabaseInfo do
    if (ODSMajorVersion > 11) or ((ODSMajorVersion = 11) and (ODSMinorVersion >= 1)) then
      DataBaseQuery.Active := true;
end;

procedure TDatabaseProperties.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
begin
  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  if DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword) = mrOK then
  begin
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    IBConfigService1.Params.Values['user_name'] := aUserName;
    IBConfigService1.Params.Values['password'] := aPassword;
    IBConfigService1.Params.Values['expected_db'] := IBConfigService1.DatabaseName;
    IBConfigService1.LoginPrompt := false;
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TDatabaseProperties.GetDBFlags;
var Line: string;
begin
  FDatabaseOnline := true;
  FShadowDatabase := false;
  with IBStatisticalService1 do
  begin
    Active := True;
    try
      ServiceStart;
      While not Eof do
      begin
         Line := GetNextLine;
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
           FDatabaseOnline := false;
         if (Pos('Attributes',Line) <> 0) and ((Pos('database shutdown',Line) <> 0)
                   or (Pos('multi-user maintenance',Line) <> 0)) then
           FShadowDatabase := true;

      end
    finally
      Active := False;
    end
  end;
end;

procedure TDatabaseProperties.LoadData;
begin
  if FLoading then Exit;
  FLoading := true;
  try
    Edit1.Text :=  IBDatabase1.DatabaseName;
    Edit2.Text :=  Format('%d.%d',[IBDatabaseInfo.ODSMajorVersion,IBDatabaseInfo.ODSMinorVersion]);
    Edit5.Text :=  IBDatabaseInfo.Version;
    Edit6.Text :=  IntToStr(IBDatabaseInfo.DBSQLDialect);
    Edit7.Text := IBConfigService1.ServerName;
    Edit8.Text := IBDatabaseInfo.DBFileName;
    Edit10.Text := IntToStr(IBDatabaseInfo.CurrentMemory);
    Edit11.Text := IntToStr(IBDatabaseInfo.MaxMemory);
    Edit12.Text := IntToStr(IBDatabaseInfo.NumBuffers);
    Edit3.Text := IntToStr(IBDatabaseInfo.PagesUsed);
    Edit4.Text := IntToStr(IBDatabaseInfo.PagesFree);
    DBIsReadOnly.Checked := IBDatabaseInfo.ReadOnly <> 0;
    SyncWrites.Checked := IBDatabaseInfo.ForcedWrites = 1;
    SweepInterval.Text := IntToStr(IBDatabaseInfo.SweepInterval);
    AllocatedPages.Text := IntToStr(IBDatabaseInfo.Allocation);
    PageSize.Text := IntToStr(IBDatabaseInfo.PageSize);
    NoReserve.Checked := IBDatabaseInfo.NoReserve = 0;
    GetDBFlags;
    DatabaseOnline.Checked := FDatabaseOnline;
    IsShadowChk.Checked := FShadowDatabase;
  finally
    FLoading := false;
  end;
end;

procedure TDatabaseProperties.Connect;
var index: integer;
begin
  try
    IBDatabase1.Connected := true;
  except on E:EIBInterBaseError do
    if E.IBErrorCode = isc_login then
    begin
      IBConfigService1.Active := false;
      IBStatisticalService1.Active := false;
      IBDatabase1.LoginPrompt := true;
      index := IBDatabase1.Params.IndexOfName('password');
      if index <> -1 then
        IBDatabase1.Params.Delete(index);
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
    end
    else
      raise;
  end;
  IBConfigService1.Active := true;
  IBStatisticalService1.Assign(IBConfigService1);
end;

procedure TDatabaseProperties.FormShow(Sender: TObject);
begin
  LoadData;
end;

procedure TDatabaseProperties.DatabaseQueryBeforeOpen(DataSet: TDataSet);
begin
  SecDatabase.Enabled := IBDatabaseInfo.ODSMajorVersion >= 12;
end;

procedure TDatabaseProperties.DatabaseQueryAfterOpen(DataSet: TDataSet);
var Linger: TField;
begin
   Linger := DataSet.FieldByName('RDB$LINGER');
   if Linger <> nil then
   begin
     if Linger.IsNull then
       Edit13.Text := '0'
     else
       Edit13.Text := Linger.AsString;
   end
   else
     Edit13.Text := 'n/a';
   Edit13.ReadOnly := (Linger = nil) or (IBDatabaseInfo.ODSMajorVersion < 12);
end;

function TDatabaseProperties.ShowModal(ActiveService: TIBCustomService;
  DBName: string; aPassword: string): TModalResult;
var index: integer;
begin
  IBConfigService1.Assign(ActiveService);
  IBConfigService1.DatabaseName := DBName;
  IBStatisticalService1.DatabaseName := DBName;

  with IBConfigService1 do
  begin
    IBDatabase1.DatabaseName := MakeConnectString(ServerName,DBName,Protocol,PortNo);
    index := Params.IndexOfName('user_name');
    if index <> -1 then
      IBDatabase1.Params.Values['user_name'] := Params.Values['user_name'];
    if aPassword <> '' then
    begin
      IBDatabase1.Params.Values['password'] := aPassword;
      IBDatabase1.LoginPrompt := false;
    end
    else
    begin
      IBDatabase1.LoginPrompt := true;
      IBConfigService1.Active := false;
    end;
  end;
  try
    Connect;
  except on E:Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      Exit;
    end;
  end;
  Result := inherited ShowModal;
  IBDatabase1.Connected := false;
end;

end.

