unit MasterMind.View.Form;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, MasterMind.API, stdctrls;

type
  TBoardRow = record
    Panel: TPanel;
    Colors: array[0..CODE_SIZE - 1] of TShape;
    Hints: array[0..CODE_SIZE - 1] of TShape;
  end;

  TFormMasterMind = class(TForm, IGameView)
    pnlBoard: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    btnCommitGuess: TButton;
    FController: IGameController;
    FBoardRows: array[0..MAX_GUESSES - 1] of TBoardRow;
    FCurrentInputRow: Integer;
    procedure CreateBoard;
    function CreateRowPanel(const RowIndex: Integer): TPanel;
    procedure AddShapesToRowPanel(const RowPanel: TPanel; const RowIndex: Integer);
    procedure AddCodeShapes(const RowPanel: TPanel; const RowIndex: Integer);
    procedure AddHintShapes(const RowPanel: TPanel; const RowIndex: Integer);
    procedure AddCodeShape(const CodeColorIndex, RowIndex: Integer; const RowPanel: TPanel);
    procedure AddHintShape(const HintIndex, RowIndex: Integer; const RowPanel: TPanel);
    procedure ColorShapeClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetNextColor(const CodeShape: TShape): TColor;
    procedure CreateStartGameButton;
    procedure BtnNewGameClick(Sender: TObject);
    procedure BtnCommitGuessClick(Sender: TObject);
    procedure CreateBtnCommitGuess;
    function TryGetPlayersGuess(out Guess: TMasterMindCode): Boolean;
    function TryGetCodeColorFromShape(out Color: TMasterMindCodeColor; const ColorIndex: Integer): Boolean;
    procedure EnableGuessInput(const CurrentGuessIndex: Integer);
    procedure DisableGuessInput;
    procedure PaintEvaluatedGuess(const PreviousGuesses: TEvaluatedGuess; const RowIndex: Integer);
    procedure ResetBoardRow(const RowIndex: Integer);
  private
    const
      CODE_COLOR_MAPPING: array[TMasterMindCodeColor] of TColor = (
        clGreen,
        clYellow,
        clBlue,
        clRed,
        clWhite,
        clMaroon
      );

      HINT_COLOR_MAPPING: array[TMasterMindHint] of TColor = (
        clBlack,
        clWhite,
        clRed
      );
  public
    procedure Start;
    procedure StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
    procedure ShowGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
  end;

implementation

uses
  MasterMind.ControllerFactory;

procedure TFormMasterMind.FormCreate(Sender: TObject);
begin
  Caption := 'MasterMind';
  FController := TMasterMindControllerFactory.CreateController(Self);
  CreateBoard;
  CreateStartGameButton;
  CreateBtnCommitGuess;
  Start;
end;

procedure TFormMasterMind.CreateBoard;
var
  I: Integer;
begin
  for I := Low(FBoardRows) to High(FBoardRows) do
    CreateRowPanel(I);
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
  FBoardRows[RowIndex].Panel := Result;
  AddShapesToRowPanel(Result, RowIndex);
end;

procedure TFormMasterMind.AddShapesToRowPanel(const RowPanel: TPanel; const RowIndex: Integer);
begin
  AddCodeShapes(RowPanel, RowIndex);
  AddHintShapes(RowPanel, RowIndex);
end;

procedure TFormMasterMind.AddCodeShapes(const RowPanel: TPanel; const RowIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to CODE_SIZE - 1 do
    AddCodeShape(I, RowIndex, RowPanel);
end;

procedure TFormMasterMind.AddCodeShape(const CodeColorIndex, RowIndex: Integer; const RowPanel: TPanel);
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
  Shape.Tag := RowIndex;
  Shape.OnMouseDown := ColorShapeClick;

  FBoardRows[RowIndex].Colors[CodeColorIndex] := Shape;
end;

procedure TFormMasterMind.AddHintShapes(const RowPanel: TPanel; const RowIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to CODE_SIZE - 1 do
    AddHintShape(I, RowIndex, RowPanel);
end;

procedure TFormMasterMind.AddHintShape(const HintIndex, RowIndex: Integer; const RowPanel: TPanel);
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
    Column := 1
  else
    Column := 0;

  Shape.Left := HINT_SHAPE_START_LEFT + (Column * (HINT_SHAPE_SPACING));
  Shape.Brush.Color := clBlack;
  FBoardRows[RowIndex].Hints[HintIndex] := Shape;
end;

procedure TFormMasterMind.ColorShapeClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Shape: TShape;
  Guess: TMasterMindCode;
begin
  Shape := Sender as TShape;
  if Shape.Tag = FCurrentInputRow then
    Shape.Brush.Color := GetNextColor(Shape);

  btnCommitGuess.Enabled := TryGetPlayersGuess(Guess);
end;

function TFormMasterMind.GetNextColor(const CodeShape: TShape): TColor;
var
  ColorFound: Boolean;
  Color: TColor;
begin
  ColorFound := False;
  for Color in CODE_COLOR_MAPPING do
    if ColorFound then
      Exit(Color)
    else
      if CodeShape.Brush.Color = Color then
        ColorFound := True;

  Exit(CODE_COLOR_MAPPING[TMasterMindCodeColor(0)]);
end;

procedure TFormMasterMind.CreateStartGameButton;
var
  Button: TButton;
begin
  Button := TButton.Create(Self);
  Button.Parent := Self;
  Button.Caption := 'New Game';
  Button.Top := 20;
  Button.Left := Width - Button.Width - 10;
  Button.OnClick := BtnNewGameClick;
end;

procedure TFormMasterMind.BtnNewGameClick(Sender: TObject);
begin
  FController.NewGame;
end;

procedure TFormMasterMind.CreateBtnCommitGuess;
begin
  btnCommitGuess := TButton.Create(Self);
  btnCommitGuess.Parent := Self;
  btnCommitGuess.Caption := 'Commit guess';
  btnCommitGuess.Width := 90;
  btnCommitGuess.Top := Height - btnCommitGuess.Height - 10;
  btnCommitGuess.Left := Width - btnCommitGuess.Width - 10;
  btnCommitGuess.OnClick := BtnCommitGuessClick;
end;

procedure TFormMasterMind.BtnCommitGuessClick(Sender: TObject);
var
  Guess: TMasterMindCode;
begin
  if TryGetPlayersGuess(Guess) then
    FController.TakeGuess(Guess);
end;

function TFormMasterMind.TryGetPlayersGuess(out Guess: TMasterMindCode): Boolean;
var
  I: Integer;
begin
  if FCurrentInputRow < 0 then
    Exit(False);

  for I := Low(Guess) to High(Guess) do
    if not TryGetCodeColorFromShape(Guess[I], I) then
      Exit(False);

  Exit(True);
end;

function TFormMasterMind.TryGetCodeColorFromShape(out Color: TMasterMindCodeColor; const ColorIndex: Integer): Boolean;
var
  Shape: TShape;
  CurrentColor: TMasterMindCodeColor;
begin
  Shape := FBoardRows[FCurrentInputRow].Colors[ColorIndex];

  for CurrentColor := Low(TMasterMindCodeColor) to High(TMasterMindCodeColor) do
  begin
    if Shape.Brush.Color = CODE_COLOR_MAPPING[CurrentColor] then
    begin
      Color := CurrentColor;
      Exit(True);
    end;
  end;

  Exit(False);
end;

procedure TFormMasterMind.Start;
begin
  DisableGuessInput;
  FController.NewGame;
end;

procedure TFormMasterMind.StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
var
  CurrentGuessIndex: Integer;
begin
  CurrentGuessIndex := Length(PreviousGuesses);
  EnableGuessInput(CurrentGuessIndex);
end;

procedure TFormMasterMind.ShowGuesses(const PreviousGuesses: TPreviousGuesses);
var
  I: Integer;
begin
  DisableGuessInput;
  for I := Low(PreviousGuesses) to High(PreviousGuesses) do
    PaintEvaluatedGuess(PreviousGuesses[I], I);

  for I := Length(PreviousGuesses) to High(FBoardRows) do
    ResetBoardRow(I);
end;

procedure TFormMasterMind.PaintEvaluatedGuess(const PreviousGuesses: TEvaluatedGuess; const RowIndex: Integer);
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    FBoardRows[RowIndex].Hints[I].Brush.Color := HINT_COLOR_MAPPING[PreviousGuesses.GuessResult[I]];
end;

procedure TFormMasterMind.ResetBoardRow(const RowIndex: Integer);
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
  begin
    FBoardRows[RowIndex].Hints[I].Brush.Color := HINT_COLOR_MAPPING[mmhNoMatch];
    FBoardRows[RowIndex].Colors[I].Brush.Color := clBlack;
  end;
end;

procedure TFormMasterMind.ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
begin
  ShowMessage('You win!');
end;

procedure TFormMasterMind.ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
begin
  ShowMessage('You lose!');
end;

procedure TFormMasterMind.EnableGuessInput(const CurrentGuessIndex: Integer);
var
  I: Integer;
begin
  FCurrentInputRow := CurrentGuessIndex;
  for I := Low(FBoardRows) to High(FBoardRows) do
    if I = CurrentGuessIndex then
      FBoardRows[I].Panel.Color := clHighlight
    else
      FBoardRows[I].Panel.Color := clWindow;
end;

procedure TFormMasterMind.DisableGuessInput;
begin
  FCurrentInputRow := -1;
  btnCommitGuess.Enabled := False;
end;

{$R *.lfm}

end.