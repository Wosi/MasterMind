unit MasterMind.Evaluator.Tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, MasterMind.API;

type
  TTestMasterMindGuessEvaluator = class(TTestCase)
  private
    FEvaluator: IGuessEvaluator;
    procedure CheckResultsEqual(const Expected, Actual: TMasterMindGuessEvaluationResult);
    function MakeResult(const Hints: array of TMasterMindHint): TMasterMindGuessEvaluationResult;
    function MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;
    procedure CheckEvaluation(const LeftCode, RightCode: array of TMasterMindCodeColor; const ExpectedResult: array of TMasterMindHint);
  protected
    procedure Setup; override;
  published
    procedure TestExactMatch;
    procedure TestNoMatch;
    procedure TestAllColorsAtWrongPlace;
  end;

implementation

uses
  MasterMind.Evaluator;

procedure TTestMasterMindGuessEvaluator.Setup;
begin
  FEvaluator := TMasterMindGuessEvaluator.Create;
end;


procedure TTestMasterMindGuessEvaluator.CheckResultsEqual(const Expected, Actual: TMasterMindGuessEvaluationResult);
var
  I: Integer;
begin
  for I := Low(Expected) to High(Expected) do
    if not (Expected[I] = Actual[I]) then
      Fail('Results do not match');
end;

function TTestMasterMindGuessEvaluator.MakeResult(const Hints: array of TMasterMindHint): TMasterMindGuessEvaluationResult;
var
  I: Integer;
begin
  for I := Low(Result) to High(Result) do
    Result[I] := Hints[I];
end;

function TTestMasterMindGuessEvaluator.MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;
var
  I: Integer;
begin
  for I := Low(Result) to High(Result) do
    Result[I] := Colors[I];
end;

procedure TTestMasterMindGuessEvaluator.CheckEvaluation(const LeftCode, RightCode: array of TMasterMindCodeColor; const ExpectedResult: array of TMasterMindHint);
var
  Expected, Actual: TMasterMindGuessEvaluationResult;
begin
  Actual := FEvaluator.EvaluateGuess(MakeCode(LeftCode), MakeCode(RightCode));
  Expected := MakeResult(ExpectedResult);
  CheckResultsEqual(Expected, Actual);
end;

procedure TTestMasterMindGuessEvaluator.TestExactMatch;
begin
  CheckEvaluation(
    [mmcGreen, mmcGreen, mmcGreen, mmcGreen],
    [mmcGreen, mmcGreen, mmcGreen, mmcGreen],
    [mmhCorrect, mmhCorrect, mmhCorrect, mmhCorrect]
  );
end;

procedure TTestMasterMindGuessEvaluator.TestNoMatch;
begin
  CheckEvaluation(
    [mmcGreen, mmcGreen, mmcGreen, mmcGreen],
    [mmcRed, mmcRed, mmcRed, mmcRed],
    [mmhNoMatch, mmhNoMatch, mmhNoMatch, mmhNoMatch]
  );
end;

procedure TTestMasterMindGuessEvaluator.TestAllColorsAtWrongPlace;
begin
  CheckEvaluation(
    [mmcGreen, mmcYellow, mmcOrange, mmcRed],
    [mmcYellow, mmcOrange, mmcRed, mmcGreen],
    [mmhWrongPlace, mmhWrongPlace, mmhWrongPlace, mmhWrongPlace]
  );
end;

initialization
  RegisterTest(TTestMasterMindGuessEvaluator);

end.