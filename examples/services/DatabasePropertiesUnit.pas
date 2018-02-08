unit DatabasePropertiesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Buttons, DBExtCtrls, ActnList, ExtCtrls, db, IBDatabaseInfo, IBQuery,
  IBSQL, IBDatabase, IBServices;

type

  { TDatabaseProperties }

  TDatabaseProperties = class(TForm)
    AllocatedPages: TEdit;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
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
  private
    FDatabaseOnline: boolean;
    FShadowDatabase: boolean;
    FLoading: boolean;
    procedure GetDBFlags;
    procedure LoadData;
  public
    function ShowModal(ActiveService: TIBCustomServer; ServerName, DBName: string; ForceLogin: boolean): TModalResult;
  end;

var
  DatabaseProperties: TDatabaseProperties;

implementation

{$R *.lfm}

uses IBUtils;

{ TDatabaseProperties }

procedure TDatabaseProperties.IBDatabase1AfterConnect(Sender: TObject);
begin
  with IBDatabaseInfo do
    if (ODSMajorVersion > 11) or ((ODSMajorVersion = 11( and (ODSMinorVersion >= 1) then
      DataBaseQuery.Active := true;
end;

procedure TDatabaseProperties.GetDBFlags;
var Line: string;
begin
  with IBStatisticalService1 do
  begin
    DatabaseName := IBDatabase1.DatabasePath;
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
    GetDBFlags;
    DatabaseOnline.Checked := FDatabaseOnline;
    IsShadowChk.Checked := FShadowDatabase;
    AllocatedPages.Text := IntToStr(IBDatabaseInfo.Allocation);
    PageSize.Text := IntToStr(IBDatabaseInfo.PageSize);
    NoReserve.Checked := IBDatabaseInfo.NoReserve = 0;
  finally
    FLoading := false;
  end;
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

function TDatabaseProperties.ShowModal(ActiveService: TIBCustomServer;
  ServerName, DBName: string; ForceLogin: boolean): TModalResult;
var index: integer;
begin
  IBConfigService1.Assign(ActiveService);
  IBStatisticalService1.Assign(ActiveService);
  with IBConfigService1 do
  begin
    IBDatabase1.DatabaseName := MakeConnectString(ServerName,DBName,Protocol,PortNo);
    index := Params.IndexOfName('user_name');
    if index <> -1 then
      IBDatabase1.Params.Values['user_name'] := Params.Values['user_name'];
    if not ForceLogin then
    begin
      index := Params.IndexOfName('password');
      if index <> -1 then
        IBDatabase1.Params.Values['password'] := Params.Values['password'];
      IBDatabase1.LoginPrompt := index = -1;
    end
    else
      IBDatabase1.LoginPrompt := true;
  end;
  IBDatabase1.Connected := true;
  Result := inherited ShowModal;
end;

end.

