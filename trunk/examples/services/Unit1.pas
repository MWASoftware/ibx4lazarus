unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IBServices;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    IBServerProperties1: TIBServerProperties;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  with IBServerProperties1 do
  begin
    Attach;
    FetchVersionInfo;
    Memo1.Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Memo1.Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Memo1.Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close
end;

end.

