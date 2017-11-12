unit MasterMind.Evaluator;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindGuessEvaluator = class(TInterfacedObject, IGuessEvaluator)
  private
    function ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
      const CurrentEvaluationResult: TMasterMindGuessEvaluationResult; const ColorIndex: Integer): Boolean;
      procedure SortResult(var EvaluationResult: TMasterMindGuessEvaluationResult);
      function CountExactMatches(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
      function CountWrongPlaces(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
      function CountElements(const EvaluationResult: TMasterMindGuessEvaluationResult; const HintType: TMasterMindHint): Integer;
  public
    function EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
  end;

implementation

uses
  EnumHelper;

function CompareHint(const Left, Right: TMasterMindHint): Integer;
begin
  if Left > Right then
    Result := 1
  else if Left < Right then
    Result := -1
  else
    Result := 0;

  WriteLn(TEnumHelper<TMasterMindHint>.EnumToStr(Left), ' <-> ', TEnumHelper<TMasterMindHint>.EnumToStr(Right), ' = ', Result);
end;

function TMasterMindGuessEvaluator.EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    Result[I] := mmhNoMatch;

  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if CodeToBeGuessed[I] = Guess[I] then
      Result[I] := mmhCorrect;

  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(Guess, CodeToBeGuessed, Result, I) then
      Result[I] := mmhWrongPlace;

  SortResult(Result);
end;

function TMasterMindGuessEvaluator.ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
  const CurrentEvaluationResult: TMasterMindGuessEvaluationResult; const ColorIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if (CodeToBeGuessed[I] = Guess[ColorIndex]) and (CodeToBeGuessed[I] <> Guess[I]) and (CodeToBeGuessed[ColorIndex] <> Guess[ColorIndex]) then
      Exit(True);
end;

procedure TMasterMindGuessEvaluator.SortResult(var EvaluationResult: TMasterMindGuessEvaluationResult);
var
  Matches, WrongPlaces, I: Integer;
begin
  Matches := CountExactMatches(EvaluationResult);
  WrongPlaces := CountWrongPlaces(EvaluationResult);

  for I := 0 to Matches - 1 do
    EvaluationResult[I] := mmhCorrect;

  for I := 0 to WrongPlaces - 1 do
    EvaluationResult[Matches + I] := mmhWrongPlace;

  for I := Matches + WrongPlaces to High(EvaluationResult) do
    EvaluationResult[I] := mmhNoMatch;
end;

function TMasterMindGuessEvaluator.CountExactMatches(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
begin
  Result := CountElements(EvaluationResult, mmhCorrect);
end;

function TMasterMindGuessEvaluator.CountWrongPlaces(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
begin
  Result := CountElements(EvaluationResult, mmhWrongPlace);
end;

function TMasterMindGuessEvaluator.CountElements(const EvaluationResult: TMasterMindGuessEvaluationResult; const HintType: TMasterMindHint): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(EvaluationResult) to High(EvaluationResult) do
    if EvaluationResult[I] = HintType then
      Inc(Result);
end;

end.