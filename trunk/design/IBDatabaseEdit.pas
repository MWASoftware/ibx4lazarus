{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{************************************************************************}

unit IBDatabaseEdit;

{$MODE Delphi}

{$A+}                           (* Aligned records: On *)
{$B-}                           (* Short circuit boolean expressions: Off *)
{ $G+}                           (* Imported data: On *)
{$H+}                           (* Huge Strings: On *)
{$J-}                           (* Modification of Typed Constants: Off *)
{$M+}                           (* Generate run-time type information: On *)
{$O+}                           (* Optimization: On *)
{$Q-}                           (* Overflow checks: Off *)
{$R-}                           (* Range checks: Off *)
{$T+}                           (* Typed address: On *)
{ $U+}                           (* Pentim-safe FDIVs: On *)
{$W-}                           (* Always generate stack frames: Off *)
{$X+}                           (* Extended syntax: On *)
{$Z1}                           (* Minimum Enumeration Size: 1 Byte *)

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IBDataBase, IB,  LResources;

type

  { TIBDatabaseEditForm }

  TIBDatabaseEditForm = class(TForm)
    UseSystemDefaultCS: TCheckBox;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    DatabaseName: TEdit;
    Label1: TLabel;
    LocalRbtn: TRadioButton;
    RemoteRbtn: TRadioButton;
    Browse: TButton;
    GroupBox1: TGroupBox;
    UserName: TEdit;
    Password: TEdit;
    SQLRole: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DatabaseParams: TMemo;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label5: TLabel;
    LoginPrompt: TCheckBox;
    Label6: TLabel;
    CharacterSet: TComboBox;
    ServerName: TEdit;
    Protocol: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    Test: TButton;
    procedure RemoteRbtnClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure LocalRbtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure UserNameChange(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure SQLRoleChange(Sender: TObject);
    procedure CharacterSetChange(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure UseSystemDefaultCSChange(Sender: TObject);
  private
    { Private declarations }
    Database: TIBDatabase;
    function Edit: Boolean;
    function GetParam(Name: string): string;
    procedure AddParam(Name, Value: string);
    procedure DeleteParam(Name: string);
  public
    { Public declarations }
  end;

var
  IBDatabaseEditForm: TIBDatabaseEditForm;

  function EditIBDatabase(ADatabase: TIBDatabase): Boolean;

implementation

{$R *.lfm}

uses TypInfo, FBMessages;

function EditIBDatabase(ADatabase: TIBDatabase): Boolean;
begin
  with TIBDatabaseEditForm.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TIBDatabaseEditForm.GetParam(Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to DatabaseParams.Lines.Count - 1 do
  begin
    if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
    begin
      Result := DatabaseParams.Lines.Values[DatabaseParams.Lines.Names[i]];
      break;
    end;
  end;
end;

procedure TIBDatabaseEditForm.AddParam(Name, Value: string);
var
  i: Integer;
  found: boolean;
begin
  found := False;
  if Trim(Value) <> '' then
  begin
    DatabaseParams.Lines.NameValueSeparator := '=';
    for i := 0 to DatabaseParams.Lines.Count - 1 do
    begin
      if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        DatabaseParams.Lines.Values[DatabaseParams.Lines.Names[i]] := Value;
        found := True;
        break;
      end;
    end;
    if not found then
      DatabaseParams.Lines.Add(Name + '=' + Value);
  end
  else
    DeleteParam(Name);
end;

procedure TIBDatabaseEditForm.DeleteParam(Name: string);
var
  i: Integer;
begin
    for i := 0 to DatabaseParams.Lines.Count - 1 do
    begin
      if (Pos(Name, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        DatabaseParams.Lines.Delete(i);
        break;
      end;
    end;
end;

function TIBDatabaseEditForm.Edit: Boolean;
var
  st: string;

  procedure DecomposeDatabaseName;
  var
    Idx1, Idx2: Integer;
    st: string;
  begin
    if Pos('\\', Database.DatabaseName) <> 0 then {do not localize}
    begin
      LocalRBtn.Checked := False;
      RemoteRbtn.Checked := True;
      Protocol.ItemIndex := 1;
      st := copy(Database.DatabaseName, 3, Length(Database.DatabaseName));
      Idx1 := Pos('\', st); {do not localize}
      if Idx1 = 0 then
        IBError(ibxeUnknownError, [nil])
      else begin
        ServerName.Text := Copy(st, 1, Idx1 - 1);
        DatabaseName.Text:= Copy(st, Idx1 + 1, Length(st));
      end;
    end
    else begin
      Idx1 := Pos(':', Database.DatabaseName ); {do not localize}
      If (Idx1 = 0) or (Idx1 = 2) then
      begin
        DatabaseName.Text := Database.DatabaseName;
      end
      else
      begin
        LocalRBtn.Checked := False;
        RemoteRbtn.Checked := True;
        Idx2 := Pos('@', Database.DatabaseName); {do not localize}
        if Idx2 = 0 then
        begin
          Protocol.ItemIndex := 0;
          ServerName.Text := copy(Database.DatabaseName, 1, Idx1 - 1);
          DatabaseName.Text := copy(Database.DatabaseName, Idx1 + 1,
            Length(Database.DatabaseName));
        end
        else begin
          Protocol.ItemIndex := 2;
          ServerName.Text := copy(Database.DatabaseName, 1, Idx2 - 1);
          DatabaseName.Text := copy(Database.DatabaseName, Idx2 + 1,
            Length(Database.DatabaseName));
        end;
      end;
    end;
  end;
begin
  DecomposeDatabaseName;
  if Trim(Database.Params.Text) = '' then
    DatabaseParams.Clear
  else
    DatabaseParams.Lines.Assign(Database.Params);
  LoginPrompt.Checked := Database.LoginPrompt;
  UserName.Text := GetParam('user_name');
  Password.Text := GetParam('password');
  SQLRole.Text := GetParam('sql_role');
  st := GetParam('lc_ctype');
  if (st <> '') then
    CharacterSet.ItemIndex := CharacterSet.Items.IndexOf(st);
  if Database.UseDefaultSystemCodePage then
    UseSystemDefaultCS.Checked := true
  else
    UseSystemDefaultCS.Checked := false;
  Result := False;
  if ShowModal = mrOk then
  begin
    Database.DatabaseName := DatabaseName.Text;
    if LocalRbtn.Checked then
      DatabaseName.Text := Database.DatabaseName
    else
      case Protocol.ItemIndex of
        0: Database.DatabaseName := Format('%s:%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
        1: Database.DatabaseName := Format('\\%s\%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
        2: Database.DatabaseName := Format('%s@%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
      end;
    Database.Params := DatabaseParams.Lines;
    Database.LoginPrompt := LoginPrompt.Checked;
    Database.UseDefaultSystemCodePage := UseSystemDefaultCS.Checked;
    Result := True;
  end;
end;

procedure TIBDatabaseEditForm.RemoteRbtnClick(Sender: TObject);
begin
  Browse.Enabled := False;
  Label7.Enabled := True;
  Label8.Enabled := True;
  Protocol.Enabled := True;
  ServerName.Enabled := True;
  if Protocol.Text = '' then
    Protocol.Text := 'TCP';
end;

procedure TIBDatabaseEditForm.BrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
    try
      InitialDir := ExtractFilePath(DatabaseName.Text);
      Filter := SDatabaseFilter;
      if Execute then
        DatabaseName.Text := FileName;
    finally
      Free
    end;
end;

procedure TIBDatabaseEditForm.LocalRbtnClick(Sender: TObject);
begin
  Browse.Enabled := True;
  Label7.Enabled := False;
  Label8.Enabled := False;
  ServerName.Enabled := False;
  Protocol.Enabled := False;
end;

procedure TIBDatabaseEditForm.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Database.Connected then
  begin
    if MessageDlg(SDisconnectDatabase, mtConfirmation,
      mbOkCancel, 0) <> mrOk then Exit;
    Database.Close;
  end;
  ModalResult := mrOk;
end;

procedure TIBDatabaseEditForm.FormCreate(Sender: TObject);
begin
//  HelpContext := hcDIBDataBaseEdit;
end;

procedure TIBDatabaseEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TIBDatabaseEditForm.UserNameChange(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

procedure TIBDatabaseEditForm.PasswordChange(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TIBDatabaseEditForm.SQLRoleChange(Sender: TObject);
begin
  AddParam('sql_role_name', SQLRole.Text);
end;

procedure TIBDatabaseEditForm.CharacterSetChange(Sender: TObject);
begin
  if (CharacterSet.Text <> 'None') then {do not localize}
    AddParam('lc_ctype', CharacterSet.Text)
  else
    DeleteParam('lc_ctype');
end;

procedure TIBDatabaseEditForm.TestClick(Sender: TObject);
var
  tempDB : TIBDatabase;
begin
  Test.Enabled := false;
  tempDB := TIBDatabase.Create(nil);
  try
    if LocalRbtn.Checked then
      tempDB.DatabaseName := DatabaseName.Text
    else
      case Protocol.ItemIndex of
        0: tempDB.DatabaseName := Format('%s:%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
        1: tempDB.DatabaseName := Format('\\%s\%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
        2: tempDB.DatabaseName := Format('%s@%s', [ServerName.Text, DatabaseName.Text]); {do not localize}
      end;
    tempDB.Params.Assign(DatabaseParams.Lines);
    tempDB.LoginPrompt := LoginPrompt.Checked;
    try
      tempDB.Connected := true;
      ShowMessage('Successful Connection');
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  finally
    tempDB.Free;
    Test.Enabled := true;
  end;
end;

procedure TIBDatabaseEditForm.UseSystemDefaultCSChange(Sender: TObject);
begin
  CharacterSet.Enabled := not UseSystemDefaultCS.Checked;
  if UseSystemDefaultCS.Checked then
    DeleteParam('lc_ctype')
  else
  if (CharacterSet.Text <> 'None') then {do not localize}
      AddParam('lc_ctype', CharacterSet.Text)
end;


end.
