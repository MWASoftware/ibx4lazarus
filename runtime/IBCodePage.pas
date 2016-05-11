unit IBCodePage;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$DEFINE HAS_ANSISTRING_CODEPAGE}
{$ENDIF}

interface

uses
  Classes, SysUtils;

{$IFDEF HAS_ANSISTRING_CODEPAGE}
function IBGetCodePage(IBCP_Name: string): TSystemCodePage;
function IBGetCharacterSetName(CodePage: TSystemCodePage): RawByteString;
{$ENDIF}

implementation

{$IFDEF HAS_ANSISTRING_CODEPAGE}

type
  TIBCodePage = record
    IBCharacterSetName: string;
    cp: TSystemCodePage;
  end;

{ Code Page numbers should align with CodePageNames array in Sysutils}
const
  IBCodePages: array [0..51] of TIBCodePage = (
    (IBCharacterSetName: 'UTF8'; cp: CP_UTF8),
    (IBCharacterSetName: 'NONE'; cp: CP_NONE),
    (IBCharacterSetName: 'OCTETS'; cp: CP_NONE),
    (IBCharacterSetName: 'ASCII'; cp: CP_ASCII),
    (IBCharacterSetName: 'SJIS_0208'; cp: 932),
    (IBCharacterSetName: 'WIN1250'; cp: 1250),
    (IBCharacterSetName: 'WIN1251'; cp: 1251),
    (IBCharacterSetName: 'WIN1252'; cp: 1252),
    (IBCharacterSetName: 'WIN1253'; cp: 1253),
    (IBCharacterSetName: 'WIN1254'; cp: 1254),
    (IBCharacterSetName: 'WIN1255'; cp: 1255),
    (IBCharacterSetName: 'WIN1256'; cp: 1256),
    (IBCharacterSetName: 'WIN1257'; cp: 1257),
    (IBCharacterSetName: 'WIN1258'; cp: 1258),
    (IBCharacterSetName: 'ISO8859_1'; cp: 28591),
    (IBCharacterSetName: 'ISO8859_2'; cp: 28592),
    (IBCharacterSetName: 'ISO8859_3'; cp: 28593),
    (IBCharacterSetName: 'ISO8859_4'; cp: 28594),
    (IBCharacterSetName: 'ISO8859_5'; cp: 28595),
    (IBCharacterSetName: 'ISO8859_6'; cp: 28596),
    (IBCharacterSetName: 'ISO8859_7'; cp: 28597),
    (IBCharacterSetName: 'ISO8859_8'; cp: 28598),
    (IBCharacterSetName: 'ISO8859_9'; cp: 28599),
    (IBCharacterSetName: 'ISO8859_13'; cp: 28603),
    (IBCharacterSetName: 'EUCJ_0208'; cp: 20932),
    (IBCharacterSetName: 'DOS437'; cp: 437),
    (IBCharacterSetName: 'DOS850'; cp: 850),
    (IBCharacterSetName: 'DOS865'; cp: 865),
    (IBCharacterSetName: 'DOS852'; cp: 852),
    (IBCharacterSetName: 'DOS857'; cp: 857),
    (IBCharacterSetName: 'DOS860'; cp: 860),
    (IBCharacterSetName: 'DOS861'; cp: 861),
    (IBCharacterSetName: 'DOS863'; cp: 863),
    (IBCharacterSetName: 'CYRL'; cp: 28595),
    (IBCharacterSetName: 'DOS737'; cp: 737),
    (IBCharacterSetName: 'DOS775'; cp: 775),
    (IBCharacterSetName: 'DOS858'; cp: 858),
    (IBCharacterSetName: 'DOS862'; cp: 862),
    (IBCharacterSetName: 'DOS864'; cp: 864),
    (IBCharacterSetName: 'DOS866'; cp: 866),
    (IBCharacterSetName: 'DOS869'; cp: 869),
    (IBCharacterSetName: 'NEXT'; cp: CP_NONE),
    (IBCharacterSetName: 'KSC_5601'; cp: 949),
    (IBCharacterSetName: 'BIG_5'; cp: 950),
    (IBCharacterSetName: 'GB_2312'; cp: 52936),
    (IBCharacterSetName: 'KOI8R'; cp: 20866),
    (IBCharacterSetName: 'KOI8U'; cp: 21866),
    (IBCharacterSetName: 'TIS620'; cp: 20838),
    (IBCharacterSetName: 'GBK'; cp: 936),
    (IBCharacterSetName: 'CP943C'; cp: 50220),
    (IBCharacterSetName: 'UNICODE_FSS'; cp: CP_UTF8),
    (IBCharacterSetName: 'GB18030' ; cp: 54936)
  );

function IBGetCodePage(IBCP_Name: string): TSystemCodePage;
var I: integer;
begin
  Result := CP_NONE;
  for I := Low(IBCodePages) to High(IBCodePages) do
    if IBCodePages[I].IBCharacterSetName = IBCP_Name then
    begin
      Result :=  IBCodePages[I].cp;
      Exit;
    end;
end;

function IBGetCharacterSetName(CodePage: TSystemCodePage): RawByteString;
var I: integer;
begin
  Result := 'UTF8';
  for I := Low(IBCodePages) to High(IBCodePages) do
    if IBCodePages[I].cp = CodePage then
    begin
      Result :=  IBCodePages[I].IBCharacterSetName;
      Exit;
    end;
end;

{$ENDIF}

end.

