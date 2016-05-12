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
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{                                                                        }
{************************************************************************}

unit IBUtils;

{$Mode Delphi}

{$IF FPC_FULLVERSION >= 20700 }
{$codepage UTF8}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  Classes, SysUtils;

const
  CRLF = #13 + #10;
  CR   = #13;
  LF   = #10;
  TAB  = #9;
  NULL_TERMINATOR = #0;

  sqlReservedWords: array [0..166] of string = (
  'ADD','ADMIN','ALL','ALTER','AND','ANY','AS','AT','AVG','BEGIN','BETWEEN','BIGINT','BIT_LENGTH','BLOB','BOTH',
'BY','CASE','CAST','CHAR','CHAR_LENGTH','CHARACTER','CHARACTER_LENGTH','CHECK','CLOSE','COLLATE','COLUMN',
'COMMIT','CONNECT','CONSTRAINT','COUNT','CREATE','CROSS','CURRENT','CURRENT_CONNECTION','CURRENT_DATE',
'CURRENT_ROLE','CURRENT_TIME','CURRENT_TIMESTAMP','CURRENT_TRANSACTION','CURRENT_USER','CURSOR','DATE',
'DAY','DEC','DECIMAL','DECLARE','DEFAULT','DELETE','DISCONNECT','DISTINCT','DOUBLE','DROP','ELSE','END',
'ESCAPE','EXECUTE','EXISTS','EXTERNAL','EXTRACT','FETCH','FILTER','FLOAT','FOR','FOREIGN','FROM','FULL',
'FUNCTION','GDSCODE','GLOBAL','GRANT','GROUP','HAVING','HOUR','IN','INDEX','INNER','INSENSITIVE','INSERT',
'INT','INTEGER','INTO','IS','JOIN','LEADING','LEFT','LIKE','LONG','LOWER','MAX','MAXIMUM_SEGMENT','MERGE',
'MIN','MINUTE','MONTH','NATIONAL','NATURAL','NCHAR','NO','NOT','NULL','NUMERIC','OCTET_LENGTH','OF','ON',
'ONLY','OPEN','OR','ORDER','OUTER','PARAMETER','PLAN','POSITION','POST_EVENT','PRECISION','PRIMARY',
'PROCEDURE','RDB$DB_KEY','REAL','RECORD_VERSION','RECREATE','RECURSIVE','REFERENCES','RELEASE','RETURNING_VALUES',
'RETURNS','REVOKE','RIGHT','ROLLBACK','ROW_COUNT','ROWS','SAVEPOINT','SECOND','SELECT','SENSITIVE',
'SET','SIMILAR','SMALLINT','SOME','SQLCODE','SQLSTATE','START','SUM','TABLE','THEN','TIME',
'TIMESTAMP','TO','TRAILING','TRIGGER','TRIM','UNION','UNIQUE','UPDATE','UPPER','USER','USING',
'VALUE','VALUES','VARCHAR','VARIABLE','VARYING','VIEW','WHEN','WHERE','WHILE','WITH','YEAR');

function Max(n1, n2: Integer): Integer;
function Min(n1, n2: Integer): Integer;
function RandomString(iLength: Integer): String;
function RandomInteger(iLow, iHigh: Integer): Integer;
function StripString(st: String; CharsToStrip: String): String;
function FormatIdentifier(Dialect: Integer; Value: String): String;
function FormatIdentifierValue(Dialect: Integer; Value: String): String;
function FormatIdentifierValueNC(Dialect: Integer; Value: String): String;
function ExtractIdentifier(Dialect: Integer; Value: String): String;
function QuoteIdentifier(Dialect: Integer; Value: String): String;
function QuoteIdentifierIfNeeded(Dialect: Integer; Value: String): String;
function Space2Underscore(s: string): string;

implementation

function Max(n1, n2: Integer): Integer;
begin
  if (n1 > n2) then
    result := n1
  else
    result := n2;
end;

function Min(n1, n2: Integer): Integer;
begin
  if (n1 < n2) then
    result := n1
  else
    result := n2;
end;

function RandomString(iLength: Integer): String;
begin
  result := '';
  while Length(result) < iLength do
    result := result + IntToStr(RandomInteger(0, High(Integer)));
  if Length(result) > iLength then
    result := Copy(result, 1, iLength);
end;

function RandomInteger(iLow, iHigh: Integer): Integer;
begin
  result := Trunc(Random(iHigh - iLow)) + iLow;
end;

function StripString(st: String; CharsToStrip: String): String;
var
  i: Integer;
begin
  result := '';
  for i := 1 to Length(st) do begin
    if AnsiPos(st[i], CharsToStrip) = 0 then
      result := result + st[i];
  end;
end;

function FormatIdentifier(Dialect: Integer; Value: String): String;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
    if (Value <> '') and (Value[1] = '"') then
      Value := '"' + StringReplace (TrimRight(Value), '"', '""', [rfReplaceAll]) + '"'
    else
      Value := AnsiUpperCase(Value);
  Result := Value;
end;

function FormatIdentifierValue(Dialect: Integer; Value: String): String;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)  
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := StringReplace (Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

function FormatIdentifierValueNC(Dialect: Integer; Value: String): String;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := AnsiUpperCase(StringReplace (Value, '""', '"', [rfReplaceAll]));
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

function ExtractIdentifier(Dialect: Integer; Value: String): String;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := StringReplace (Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

function IsReservedWord(w: string): boolean;
var i: integer;
begin
     Result := true;
     for i := 0 to Length(sqlReservedWords) - 1 do
         if w = sqlReservedWords[i] then
            Exit;
     Result := false;
end;

function QuoteIdentifier(Dialect: Integer; Value: String): String;
begin
  if Dialect = 1 then
    Value := AnsiUpperCase(Trim(Value))
  else
    Value := '"' + Value + '"';
  Result := Value;
end;

function QuoteIdentifierIfNeeded(Dialect: Integer; Value: String): String;
begin
  if (Dialect = 3) and
    ((AnsiUpperCase(Value) <> Value) or IsReservedWord(Value)) then
     Result := '"' + Value + '"'
  else
    Result := Value
end;

function Space2Underscore(s: string): string;
var
   k: integer;
begin
     Result := s;
     for k := 1 to Length(s) do
         if not (Result[k] in ['0'..'9','A'..'Z','_','$'])  then
            Result[k] := '_';
end;


end.
