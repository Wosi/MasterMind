unit MasterMind.Evaluator.Tests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MasterMind.API;

type
  TTestMasterMindGuessEvaluator = class(TTestCase)
  private
    FEvaluator: IGuessEvaluator;
    procedure CheckResultsEqual(const Expected, Actual: TGuessEvaluationResult);
    procedure CheckEvaluation(const LeftCode, RightCode: array of TMasterMindCodeColor; const ExpectedResult: array of TMasterMindHint);
  protected
    procedure Setup; override;
  published
    procedure TestExactMatch;
    procedure TestNoMatch;
    procedure TestAllColorsAtWrongPlace;
    procedure TestFirstColorAtWrongPlaceAndSecondColorCorrect;
    procedure TestDuplicateColorsInGuessAreRewaredOnlyOneTimeForEachColorInTheCodeToBeGuessed;
  end;

implementation

uses
  MasterMind.Evaluator, EnumHelper, MasterMind.TestHelper;

procedure TTestMasterMindGuessEvaluator.Setup;
begin
  FEvaluator := TMasterMindGuessEvaluator.Create;
end;

procedure TTestMasterMindGuessEvaluator.CheckResultsEqual(const Expected, Actual: TGuessEvaluationResult);
begin
  TEnumHelper<TMasterMindHint>.CheckArraysEqual(Expected, Actual);
end;

procedure TTestMasterMindGuessEvaluator.CheckEvaluation(const LeftCode, RightCode: array of TMasterMindCodeColor; const ExpectedResult: array of TMasterMindHint);
var
  Expected, Actual: TGuessEvaluationResult;
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

procedure TTestMasterMindGuessEvaluator.TestFirstColorAtWrongPlaceAndSecondColorCorrect;
begin
  CheckEvaluation(
    [mmcGreen, mmcYellow, mmcOrange, mmcRed],
    [mmcOrange, mmcYellow, mmcBrown, mmcBrown],
    [mmhCorrect, mmhWrongPlace, mmhNoMatch, mmhNoMatch]
  );
end;

procedure TTestMasterMindGuessEvaluator.TestDuplicateColorsInGuessAreRewaredOnlyOneTimeForEachColorInTheCodeToBeGuessed;
begin
  CheckEvaluation(
    [mmcGreen, mmcGreen, mmcRed, mmcRed],
    [mmcGreen, mmcGreen, mmcGreen, mmcRed],
    [mmhCorrect, mmhCorrect, mmhCorrect, mmhNoMatch]
  );
end;

initialization
  RegisterTest(TTestMasterMindGuessEvaluator);

end.