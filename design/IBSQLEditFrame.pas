unit IBSQLEditFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, LResources, Forms,
  Controls, ActnList, Menus, Dialogs, ComCtrls, LazSynTextArea;

type

  { TIBSQLEditFrame }

  TIBSQLEditFrame = class(TFrame)
    Redo: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    Undo: TAction;
    SaveToFile: TAction;
    LoadFromFile: TAction;
    BtnImages: TImageList;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    WrapText: TAction;
    Clear: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    PopupMenu1: TPopupMenu;
    SelectAll: TAction;
    Paste: TAction;
    CopyText: TAction;
    Cut: TAction;
    ActionList1: TActionList;
    SQLText: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure ClearExecute(Sender: TObject);
    procedure CopyTextExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CutUpdate(Sender: TObject);
    procedure LoadFromFileExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure PasteUpdate(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure RedoUpdate(Sender: TObject);
    procedure SaveToFileExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure UndoUpdate(Sender: TObject);
    procedure WrapTextExecute(Sender: TObject);
    procedure WrapTextUpdate(Sender: TObject);
  private

  public
    procedure DoWrapText;
    procedure UnWrapText;
  end;

implementation

{$R *.lfm}


{ TIBSQLEditFrame }

procedure TIBSQLEditFrame.CutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.SelText <> '';
end;

procedure TIBSQLEditFrame.LoadFromFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    SQLText.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TIBSQLEditFrame.PasteExecute(Sender: TObject);
begin
  SQLText.PasteFromClipboard;
end;

procedure TIBSQLEditFrame.PasteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanPaste;
end;

procedure TIBSQLEditFrame.RedoExecute(Sender: TObject);
begin
  SQLText.Redo;
end;

procedure TIBSQLEditFrame.RedoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanRedo;
end;

procedure TIBSQLEditFrame.SaveToFileExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SQLText.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TIBSQLEditFrame.SelectAllExecute(Sender: TObject);
begin
  SQLText.SelectAll;
end;

procedure TIBSQLEditFrame.SelectAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.Lines.Count > 0;
end;

procedure TIBSQLEditFrame.UndoExecute(Sender: TObject);
begin
  SQLText.Undo;
end;

procedure TIBSQLEditFrame.UndoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.CanUndo;
end;

procedure TIBSQLEditFrame.WrapTextExecute(Sender: TObject);
begin
  UnWrapText;
  DoWrapText;
end;

procedure TIBSQLEditFrame.WrapTextUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SQLText.Lines.Count > 0;
end;

type
  THackedSynEdit = class(TSynEdit)
  public
    property TextArea: TLazSynTextArea read FTextArea;
  end;

const
  WhiteSpace = [' ',#$09];

procedure TIBSQLEditFrame.DoWrapText;

var NewLines: TStringList;
    i: integer;
    MaxWidth: integer;
    MaxChars: integer;
    Line: string;
begin
  NewLines := TStringList.Create;
  with SQLText do
  try
    with THackedSynEdit(SQLText).TextArea do
      MaxWidth := Right - Left;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      repeat
        if (Length(Line) = 0) or (Canvas.TextWidth(Line) <= MaxWidth) then
        begin
          NewLines.Add(Line);
          break; {next line}
        end
        else
        begin
          MaxChars := Canvas.TextFitInfo(Line,MaxWidth);
          if Line[MaxChars] in WhiteSpace then {consume whitespace}
          begin
            while (MaxChars < Length(Line)) and (Line[MaxChars] in WhiteSpace) do
               Inc(MaxChars);
            if MaxChars = Length(Line) then
            begin
               NewLines.Add(Line);
               break; {next line}
            end;
            Dec(MaxChars); {Now at last white space char}
          end
          else
          begin
            {Find start of word}
            while (MaxChars > 0) and not (Line[MaxChars] in WhiteSpace) do
              Dec(MaxChars);
            if MaxChars = 0 then
            begin
              NewLines.Add(Line); {has to overflow}
              break; {next line}
            end;
            {otherwise at last white space char}
          end;
          NewLines.Add(system.copy(Line,1,MaxChars));
          system.Delete(Line,1,MaxChars);
        end;
      until Length(Line) = 0;
    end;
    Lines.Assign(NewLines);
    if assigned(OnChange) then
      OnChange(self);
  finally
    NewLines.Free;
  end;
end;

procedure TIBSQLEditFrame.UnWrapText;
var Line: string;
    i: integer;
begin
  Line := '';
  with SQLText do
  begin
    for i := 0 to Lines.Count - 1 do
    begin
      if (Length(Line) > 0) and not (Line[Length(Line)] in WhiteSpace) then
        Line := Line + ' ';
     Line := Line + Lines[i];
    end;

    if assigned(OnChange) then
      OnChange(self);
    Lines.Text := Line;
  end;
end;

procedure TIBSQLEditFrame.CutExecute(Sender: TObject);
begin
  SQLText.CutToClipboard;
end;

procedure TIBSQLEditFrame.CopyTextExecute(Sender: TObject);
begin
  SQLText.CopyToClipboard;
end;

procedure TIBSQLEditFrame.ClearExecute(Sender: TObject);
begin
  SQLText.Lines.Clear;
end;

end.

