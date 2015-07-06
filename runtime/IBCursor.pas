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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit IBCursor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses IBDatabase, IBCustomDataSet, Forms, Controls;

const
  SetCursorDepth: integer = 0;

procedure SetCursor(Database: TIBDatabase);
begin
  if Assigned(Database) and not Database.SQLHourGlass then
     Exit;
  if (GetCurrentThreadID = MainThreadID) and (Screen.Cursor = crDefault) then
  begin
    if SetCursorDepth = 0 then
      Screen.Cursor := crHourGlass;
    Inc(SetCursorDepth);
  end;
end;

procedure RestoreCursor(Database: TIBDatabase);
begin
  if Assigned(Database) and not Database.SQLHourGlass then
     Exit;
  if SetCursorDepth > 0 then
  begin
     Dec(SetCursorDepth);
     if SetCursorDepth = 0 then
       Screen.Cursor := crDefault
  end;

end;

initialization
  OnSetCursor := @SetCursor;
  onRestoreCursor := @RestoreCursor;

end.

