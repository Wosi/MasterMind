unit MasterMind.Evaluator;

interface

uses
  MasterMind.API;

type
  TMasterMindGuessEvaluator = class(TInterfacedObject, IGuessEvaluator)
  private
    function ColorInCode(const Guess: TMasterMindCodeColor; const CodeToBeGuessed: TMasterMindCode): Boolean;
  public
    function EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
  end;

implementation

function TMasterMindGuessEvaluator.EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if CodeToBeGuessed[I] = Guess[I] then
      Result[I] := mmhCorrect
    else if ColorInCode(Guess[I], CodeToBeGuessed) then
      Result[I] := mmhWrongPlace
    else
      Result[I] := mmhNoMatch;
end;

function TMasterMindGuessEvaluator.ColorInCode(const Guess: TMasterMindCodeColor; const CodeToBeGuessed: TMasterMindCode): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if CodeToBeGuessed[I] = Guess then
      Exit(True);
end;

end.