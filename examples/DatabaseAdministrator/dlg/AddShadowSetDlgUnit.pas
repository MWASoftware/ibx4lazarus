unit AddShadowSetDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls,
  ExtCtrls, StdCtrls, ActnList, memds, db, IBDatabase, IBSQL,
  IBDynamicGrid, Forms, Dialogs, DbCtrls, ComCtrls, IB;

type

  { TAddShadowSetDlg }

  TAddShadowSetDlg = class(TForm)
    Add: TAction;
    Bevel1: TBevel;
    CancelBtn: TButton;
    Creating: TLabel;
    ExecSQL: TIBSQL;
    OKBtn: TButton;
    ProgressBar: TProgressBar;
    Remove: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ShadowFileSource: TDataSource;
    IBDynamicGrid1: TIBDynamicGrid;
    Label1: TLabel;
    Label2: TLabel;
    ShadowFileList: TMemDataset;
    RemoveBtn: TButton;
    ShadowMode: TRadioGroup;
    ShadowSet: TEdit;
    procedure AddExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RemoveExecute(Sender: TObject);
    procedure RemoveUpdate(Sender: TObject);
    procedure ShadowFileListBeforeClose(DataSet: TDataSet);
    procedure ShadowSetEditingDone(Sender: TObject);
  private
    FShadowSet: integer;
  public
    function ShowModal( aShadowSet: integer): TModalResult;
  end;

var
  AddShadowSetDlg: TAddShadowSetDlg;

implementation

uses AddShadowFileDlgUnit;

{$R *.lfm}

const
  sCreateShadow      = 'Create Shadow %d %s ''%s''';
  sCreateFirstShadow = 'Create Shadow %d %s ''%s'' LENGTH %d';
  sNextShadow        = 'FILE ''%s'' LENGTH %d';
  sCreateLastShadow  = 'FILE ''%s'' STARTING AT %d';

resourcestring
  sLengthIgnored = 'The Length is ignore for the last or only file in the Shadow Set';
  sNoLength = 'A Length must be specified for all but the last file in a multi-file set';

type

  { TCreateShadowThread }

  TCreateShadowThread = class(TThread)
  private
    FSQLText: string;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  public
    constructor Create(SQLText: string);
    property ErrorMessage: string read FErrorMessage;
  end;

{ TCreateShadowThread }

procedure TCreateShadowThread.Execute;
begin
  FErrorMessage := '';
  try
    with ExecSQL do
    begin
      Transaction.Active := true;
      SQL.Text := FSQLText;
      ExecQuery;
    end;
  except On E:Exception do
    FErrorMessage := E.Message;
  end;
end;

constructor TCreateShadowThread.Create(SQLText: string);
begin
  inherited Create(false);
  FErrorMessage := '';
  FSQLText := SQLText;
end;



{ TAddShadowSetDlg }

procedure TAddShadowSetDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var SQLText: string;
    Mode: string;
    StartAt: integer;
    FileName: string;
    FileLength: integer;
begin
  if ModalResult = mrOK then
  begin
    ShadowFileList.DisableControls;
    try
      ShadowFileList.Last;
      if not ShadowFileList.FieldByName('ShadowFileLength').IsNull then
      begin
        MessageDlg(sLengthIgnored,mtWarning,[mbOK],0);
        CloseAction := caNone;
        Exit;
      end;

      ShadowFileList.First;
      case ShadowMode.ItemIndex of
      0: Mode := 'AUTO';
      1: Mode := 'MANUAL';
      2: Mode := 'CONDITIONAL';
      end;
      if ShadowFileList.RecordCount = 1 then
        SQLText := Format(sCreateShadow,[FShadowSet,Mode,ShadowFileList.FieldByName('ShadowFileName').AsString])
      else
      begin
        if ShadowFileList.FieldByName('ShadowFileLength').AsInteger = 0 then
        begin
          MessageDlg(sNoLength,mtError,[mbOK],0);
          CloseAction := caNone;
          Exit;
        end;
        SQLText := Format(sCreateFirstShadow,[FShadowSet,Mode,
                                              ShadowFileList.FieldByName('ShadowFileName').AsString,
                                              ShadowFileList.FieldByName('ShadowFileLength').AsInteger]);
        StartAt := ShadowFileList.FieldByName('ShadowFileLength').AsInteger;
        ShadowFileList.Next;
        while not ShadowFileList.EOF do
        begin
          FileName := ShadowFileList.FieldByName('ShadowFileName').AsString;
          FileLength := ShadowFileList.FieldByName('ShadowFileLength').AsInteger;
          ShadowFileList.Next;
          if ShadowFileList.EOF then
            SQLText := SQLText + ' ' + Format(sCreateLastShadow,[FileName,StartAt+1])
          else
          begin
            if FileLength = 0 then
            begin
              MessageDlg(sNoLength,mtError,[mbOK],0);
              CloseAction := caNone;
              Exit;
            end;
            Inc(StartAt,FileLength);
            SQLText := SQLText + ' ' + Format(sNextShadow,[FileName,FileLength]);
          end;
        end;
      end;
      Creating.Visible := true;
      ProgressBar.Visible := true;
      Application.ProcessMessages;
 //     writeln(SQLText);
      with TCreateShadowThread.Create(SQLText) do
      try
        while not Finished do Application.ProcessMessages;
        if ErrorMessage <> '' then
        begin
          MessageDlg(ErrorMessage,mtError,[mbOK],0);
          ModalResult := mrCancel;
        end;
      finally
        Free
      end;
    finally
      ShadowFileList.EnableControls;
    end;
  end
end;

procedure TAddShadowSetDlg.FormShow(Sender: TObject);
begin
  Creating.Visible := false;
  ProgressBar.Visible := false;
end;

procedure TAddShadowSetDlg.AddExecute(Sender: TObject);
var aFileName: string;
    aFileLength: integer;
    Pages: boolean;
begin
  if AddShadowFileDlg.ShowModal(aFileName,aFileLength,Pages) = mrOK then
  begin
    if not Pages then
    begin
      if aFileLength <> -1 then
        aFileLength := aFileLength*1024*1024 div IBDatabaseInfo.PageSize;
    end;
    with ShadowFileList do
    begin
      Append;
      FieldByName('ShadowFileName').AsString := aFileName;
      if aFileLength <> -1 then
        FieldByName('ShadowFileLength').AsInteger := aFileLength;
      Post;
    end;
  end;
end;

procedure TAddShadowSetDlg.RemoveExecute(Sender: TObject);
begin
  ShadowFileList.Delete;
end;

procedure TAddShadowSetDlg.RemoveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ShadowFileList.Active and (ShadowFileList.RecordCount > 0);
end;

procedure TAddShadowSetDlg.ShadowFileListBeforeClose(DataSet: TDataSet);
begin
  ShadowFileList.Clear(false);
end;

procedure TAddShadowSetDlg.ShadowSetEditingDone(Sender: TObject);
begin
  FShadowSet := StrToInt(ShadowSet.Text);
end;

function TAddShadowSetDlg.ShowModal(aShadowSet: integer): TModalResult;
begin
  FShadowSet := aShadowSet;
  ShadowSet.Text := IntToStr(aShadowSet);
  Result := inherited ShowModal;
end;


end.

