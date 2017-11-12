unit MasterMind.Controller;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindController = class(TInterfacedObject, IGameController)
  private
    FCodeSelector: ICodeSelector;
    FEvaluator: IGuessEvaluator;
    FView: IGameView;
    FCodeToBeGuessed: TMasterMindCode;
    FPreviousGuesses: TPreviousGuesses;
    procedure ClearPreviousGuesses;
    procedure AddGuess(const EvaluatedGuess: TEvaluatedGuess);
    procedure EvaluateGuess(const Guess: TMasterMindCode);
    function GuessWasCorrect: Boolean;
    procedure EndGameIfNecassary;
  public
    constructor Create(const CodeSelector: ICodeSelector; const Evaluator: IGuessEvaluator; const View: IGameView);
    procedure NewGame;
    function GetCodeToBeGuessed: TMasterMindCode;
    procedure TakeGuess(const Guess: TMasterMindCode);
  end;

implementation

constructor TMasterMindController.Create(const CodeSelector: ICodeSelector; const Evaluator: IGuessEvaluator; const View: IGameView);
begin
  inherited Create;
  FCodeSelector := CodeSelector;
  FEvaluator := Evaluator;
  FView := View;
end;

procedure TMasterMindController.NewGame;
begin
  ClearPreviousGuesses;
  FCodeToBeGuessed := FCodeSelector.SelectNewCode;
  FView.ShowGuesses(FPreviousGuesses);
  FView.StartRequestGuess(FPreviousGuesses);
end;

function TMasterMindController.GetCodeToBeGuessed: TMasterMindCode;
begin
  Result := FCodeToBeGuessed;
end;

procedure TMasterMindController.ClearPreviousGuesses;
begin
  SetLength(FPreviousGuesses, 0);
end;

procedure TMasterMindController.TakeGuess(const Guess: TMasterMindCode);
begin
  EvaluateGuess(Guess);
  FView.ShowGuesses(FPreviousGuesses);
  EndGameIfNecassary;
end;

procedure TMasterMindController.AddGuess(const EvaluatedGuess: TEvaluatedGuess);
begin
  SetLength(FPreviousGuesses, Length(FPreviousGuesses) + 1);
  FPreviousGuesses[High(FPreviousGuesses)] := EvaluatedGuess;
end;

procedure TMasterMindController.EvaluateGuess(const Guess: TMasterMindCode);
var
  EvaluatedGuess: TEvaluatedGuess;
begin
  EvaluatedGuess.GuessedCode := Guess;
  EvaluatedGuess.GuessResult := FEvaluator.EvaluateGuess(FCodeToBeGuessed, Guess);
  AddGuess(EvaluatedGuess);
end;

procedure TMasterMindController.EndGameIfNecassary;
begin
  if GuessWasCorrect then
    FView.ShowPlayerWinsMessage(FPreviousGuesses)
  else if Length(FPreviousGuesses) = MAX_GUESSES then
    FView.ShowPlayerLosesMessage(FPreviousGuesses);
end;

function TMasterMindController.GuessWasCorrect: Boolean;
var
  Hint: TMasterMindHint;
begin
  Result := True;
  for Hint in FPreviousGuesses[High(FPreviousGuesses)].GuessResult do
    if Hint <> mmhCorrect then
      Exit(False);
end;

end.