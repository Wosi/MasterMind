unit MasterMind.Evaluator;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindGuessEvaluator = class(TInterfacedObject, IGuessEvaluator)
  private
    function ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
      const GuessIndex: Integer): Boolean;
    function GuessedColorIsAtCurrentPositionButIsNoCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
      const CurrentPosition, GuessPosition: Integer): Boolean;
    function ColorCorrectAtPosition(const Guess, CodeToBeGuessed: TMasterMindCode; const Position: Integer): Boolean;
    function ColorCorrectAtAnyPosition(const Guess, CodeToBeGuessed: TMasterMindCode; const Positions: array of Integer): Boolean;
    procedure InitializeResultWithNoMatches(out EvaluationResult: TMasterMindGuessEvaluationResult);
    procedure SetCorrectMatchesInResult(var EvaluationResult: TMasterMindGuessEvaluationResult; const CodeToBeGuessed, Guess: TMasterMindCode);
    procedure SetWrongPlacesInResult(var EvaluationResult: TMasterMindGuessEvaluationResult; const CodeToBeGuessed, Guess: TMasterMindCode);
    procedure SortResult(var EvaluationResult: TMasterMindGuessEvaluationResult);
    function CountExactMatches(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
    function CountWrongPlaces(const EvaluationResult: TMasterMindGuessEvaluationResult): Integer;
    function CountElements(const EvaluationResult: TMasterMindGuessEvaluationResult; const HintType: TMasterMindHint): Integer;
    procedure SetMatchesAndWrongPlaces(var EvaluationResult: TMasterMindGuessEvaluationResult; const Matches, WrongPlaces: Integer);
  public
    function EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
  end;

implementation

function TMasterMindGuessEvaluator.EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
begin
  InitializeResultWithNoMatches(Result);
  SetCorrectMatchesInResult(Result, CodeToBeGuessed, Guess);
  SetWrongPlacesInResult(Result, CodeToBeGuessed, Guess);
  SortResult(Result);
end;

procedure TMasterMindGuessEvaluator.InitializeResultWithNoMatches(out EvaluationResult: TMasterMindGuessEvaluationResult);
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    EvaluationResult[I] := mmhNoMatch;
end;

procedure TMasterMindGuessEvaluator.SetCorrectMatchesInResult(var EvaluationResult: TMasterMindGuessEvaluationResult;
  const CodeToBeGuessed, Guess: TMasterMindCode);
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if CodeToBeGuessed[I] = Guess[I] then
      EvaluationResult[I] := mmhCorrect;
end;

procedure TMasterMindGuessEvaluator.SetWrongPlacesInResult(var EvaluationResult: TMasterMindGuessEvaluationResult;
  const CodeToBeGuessed, Guess: TMasterMindCode);
var
  I: Integer;
begin
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(Guess, CodeToBeGuessed, I) then
      EvaluationResult[I] := mmhWrongPlace;
end;


function TMasterMindGuessEvaluator.ColorInCodeButWasNotAlreadyMarkedAsCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
  const GuessIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    if GuessedColorIsAtCurrentPositionButIsNoCorrectMatch(Guess, CodeToBeGuessed, I, GuessIndex) then
      Exit(True);
end;

function TMasterMindGuessEvaluator.GuessedColorIsAtCurrentPositionButIsNoCorrectMatch(const Guess, CodeToBeGuessed: TMasterMindCode;
  const CurrentPosition, GuessPosition: Integer): Boolean;
begin
  Result := (CodeToBeGuessed[CurrentPosition] = Guess[GuessPosition])
    and (not ColorCorrectAtAnyPosition(CodeToBeGuessed, Guess, [CurrentPosition, GuessPosition]));
end;

function TMasterMindGuessEvaluator.ColorCorrectAtAnyPosition(const Guess, CodeToBeGuessed: TMasterMindCode;
  const Positions: array of Integer): Boolean;
var
  Position: Integer;
begin
  Result := False;
  for Position in Positions do
    if ColorCorrectAtPosition(Guess, CodeToBeGuessed, Position) then
      Exit(True);
end;

function TMasterMindGuessEvaluator.ColorCorrectAtPosition(const Guess, CodeToBeGuessed: TMasterMindCode; const Position: Integer): Boolean;
begin
  Result := CodeToBeGuessed[Position] = Guess[Position];
end;

procedure TMasterMindGuessEvaluator.SortResult(var EvaluationResult: TMasterMindGuessEvaluationResult);
var
  Matches, WrongPlaces: Integer;
begin
  Matches := CountExactMatches(EvaluationResult);
  WrongPlaces := CountWrongPlaces(EvaluationResult);
  InitializeResultWithNoMatches(EvaluationResult);
  SetMatchesAndWrongPlaces(EvaluationResult, Matches, WrongPlaces);
end;

procedure TMasterMindGuessEvaluator.SetMatchesAndWrongPlaces(var EvaluationResult: TMasterMindGuessEvaluationResult; const Matches, WrongPlaces: Integer);
var
  I: Integer;
begin
  for I := 0 to Matches - 1 do
    EvaluationResult[I] := mmhCorrect;

  for I := 0 to WrongPlaces - 1 do
    EvaluationResult[Matches + I] := mmhWrongPlace;
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