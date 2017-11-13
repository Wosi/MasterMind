unit MasterMind.View.Form;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, MasterMind.API;

type
  TFormMasterMind = class(TForm)
    pnlBoard: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    FBoardRows: array[0..MAX_GUESSES - 1] of TPanel;
    procedure CreateBoard;
    procedure CreateGuessRow(const RowIndex: Integer);
    function CreateRowPanel(const RowIndex: Integer): TPanel;
    procedure AddShapesToRowPanel(const RowPanel: TPanel);
    procedure AddCodeShapes(const RowPanel: TPanel);
    procedure AddHintShapes(const RowPanel: TPanel);
    procedure AddCodeShape(const CodeColorIndex: Integer; const RowPanel: TPanel);
    procedure AddHintShape(const HintIndex: Integer; const RowPanel: TPanel);
  end;

implementation

procedure TFormMasterMind.FormCreate(Sender: TObject);
begin
  Caption := 'MasterMind';
  CreateBoard;
end;

procedure TFormMasterMind.CreateBoard;
var
  I: Integer;
begin
  for I := Low(FBoardRows) to High(FBoardRows) do
    CreateGuessRow(I);
end;

procedure TFormMasterMind.CreateGuessRow(const RowIndex: Integer);
var
  RowPanel: TPanel;
begin
  RowPanel := CreateRowPanel(RowIndex);
  FBoardRows[RowIndex] := RowPanel;
end;

function TFormMasterMind.CreateRowPanel(const RowIndex: Integer): TPanel;
const
  ROW_HEIGHT = 40;
begin
  Result := TPanel.Create(pnlBoard);
  Result.Parent := pnlBoard;
  Result.Width := pnlBoard.Width;
  Result.Height := ROW_HEIGHT;
  Result.Top := pnlBoard.Height - ((RowIndex + 1) * ROW_HEIGHT);
  AddShapesToRowPanel(Result);
end;

procedure TFormMasterMind.AddShapesToRowPanel(const RowPanel: TPanel);
begin
  AddCodeShapes(RowPanel);
  AddHintShapes(RowPanel);
end;

procedure TFormMasterMind.AddCodeShapes(const RowPanel: TPanel);
var
  I: Integer;
begin
  for I := 0 to CODE_SIZE - 1 do
    AddCodeShape(I, RowPanel);
end;

procedure TFormMasterMind.AddCodeShape(const CodeColorIndex: Integer; const RowPanel: TPanel);
const
  CODE_SHAPE_START_LEFT = 150;
  CODE_SHAPE_SPACING = 20;
  CODE_SHAPE_SIZE = 20;
var
  Shape: TShape;
begin
  Shape := TShape.Create(RowPanel);
  Shape.Parent := RowPanel;
  Shape.Shape := stCircle;
  Shape.Width := CODE_SHAPE_SIZE;
  Shape.Height := CODE_SHAPE_SIZE;
  Shape.Top := (RowPanel.Height - CODE_SHAPE_SIZE) div 2;
  Shape.Left := CODE_SHAPE_START_LEFT + (CodeColorIndex * (CODE_SHAPE_SIZE + CODE_SHAPE_SPACING));
  Shape.Brush.Color := clBlack;
end;

procedure TFormMasterMind.AddHintShapes(const RowPanel: TPanel);
var
  I: Integer;
begin
  for I := 0 to CODE_SIZE - 1 do
    AddHintShape(I, RowPanel);
end;

procedure TFormMasterMind.AddHintShape(const HintIndex: Integer; const RowPanel: TPanel);
const
  HINT_SHAPE_START_LEFT = 10;
  HINT_SHAPE_SPACING = 12;
  HINT_SHAPE_SIZE = 10;
var
  Shape: TShape;
  RowMid, Column: Integer;
begin
  Shape := TShape.Create(RowPanel);
  Shape.Parent := RowPanel;
  Shape.Shape := stCircle;
  Shape.Width := HINT_SHAPE_SIZE;
  Shape.Height := HINT_SHAPE_SIZE;

  RowMid := (RowPanel.Height - HINT_SHAPE_SIZE) div 2;
  if HintIndex > 1 then
    Shape.Top := RowMid + (HINT_SHAPE_SPACING div 2)
  else
    Shape.Top := RowMid - (HINT_SHAPE_SPACING div 2);

  if Odd(HintIndex) then
    Column := 0
  else
    Column := 1;

  Shape.Left := HINT_SHAPE_START_LEFT + (Column * (HINT_SHAPE_SPACING));
  Shape.Brush.Color := clBlack;
end;

{$R *.lfm}

end.