unit MasterMind.Presenter;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindPresenter = class(TInterfacedObject, IGamePresenter)
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
    procedure RequestNextGuessOrEndGameIfNecassary;
  public
    constructor Create(const CodeSelector: ICodeSelector; const Evaluator: IGuessEvaluator; const View: IGameView);
    procedure NewGame;
    function GetCodeToBeGuessed: TMasterMindCode;
    procedure TakeGuess(const Guess: TMasterMindCode);
  end;

implementation

constructor TMasterMindPresenter.Create(const CodeSelector: ICodeSelector; const Evaluator: IGuessEvaluator; const View: IGameView);
begin
  inherited Create;
  FCodeSelector := CodeSelector;
  FEvaluator := Evaluator;
  FView := View;
end;

procedure TMasterMindPresenter.NewGame;
begin
  ClearPreviousGuesses;
  FCodeToBeGuessed := FCodeSelector.SelectNewCode;
  FView.ShowGuesses(FPreviousGuesses);
  FView.StartRequestGuess(FPreviousGuesses);
end;

function TMasterMindPresenter.GetCodeToBeGuessed: TMasterMindCode;
begin
  Result := FCodeToBeGuessed;
end;

procedure TMasterMindPresenter.ClearPreviousGuesses;
begin
  SetLength(FPreviousGuesses, 0);
end;

procedure TMasterMindPresenter.TakeGuess(const Guess: TMasterMindCode);
begin
  EvaluateGuess(Guess);
  FView.ShowGuesses(FPreviousGuesses);
  RequestNextGuessOrEndGameIfNecassary;
end;

procedure TMasterMindPresenter.AddGuess(const EvaluatedGuess: TEvaluatedGuess);
begin
  SetLength(FPreviousGuesses, Length(FPreviousGuesses) + 1);
  FPreviousGuesses[High(FPreviousGuesses)] := EvaluatedGuess;
end;

procedure TMasterMindPresenter.EvaluateGuess(const Guess: TMasterMindCode);
var
  EvaluatedGuess: TEvaluatedGuess;
begin
  EvaluatedGuess.GuessedCode := Guess;
  EvaluatedGuess.GuessResult := FEvaluator.EvaluateGuess(FCodeToBeGuessed, Guess);
  AddGuess(EvaluatedGuess);
end;

procedure TMasterMindPresenter.RequestNextGuessOrEndGameIfNecassary;
begin
  if GuessWasCorrect then
    FView.ShowPlayerWinsMessage(FPreviousGuesses)
  else if Length(FPreviousGuesses) = MAX_GUESSES then
    FView.ShowPlayerLosesMessage(FPreviousGuesses)
  else
    FView.StartRequestGuess(FPreviousGuesses);
end;

function TMasterMindPresenter.GuessWasCorrect: Boolean;
var
  Hint: TMasterMindHint;
begin
  Result := True;
  for Hint in FPreviousGuesses[High(FPreviousGuesses)].GuessResult do
    if Hint <> mmhCorrect then
      Exit(False);
end;

end.