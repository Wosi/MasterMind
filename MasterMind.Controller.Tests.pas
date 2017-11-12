unit MasterMind.Controller.Tests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MasterMind.API, MasterMind.View.Mock, MasterMind.Evaluator.Mock;

type
  TTestMasterMindController = class(TTestCase)
  private
    FController: IGameController;
    FViewMock: IGameViewMock;
    FShowGuessesCallCounter: Integer;
    FStartRequestGuessCallCounter: Integer;
    FEvaluatorMock: IGuessEvaluatorMock;
    FCurrentlyGuessedCode: TMasterMindCode;
    FShowPlayerWinsCallCounter: Integer;
    FShowPlayerLosesCallCounter: Integer;
    procedure CheckShowGuessesIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
    procedure CheckShowPlayerWinsIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
    procedure CheckShowPlayerLosesIsCalledWithTwelvePreviousGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure NoOperation(const PreviousGuesses: TPreviousGuesses);
    procedure CheckPreviousGuessesIsEmpty(const PreviousGuesses: TPreviousGuesses);
    procedure CheckShowGuessWasCalledOneTime;
    procedure CheckShowPlayerWinsCalledOneTime;
    procedure CheckShowPlayerLosesCalledOneTime;
    procedure CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses; const MethodName: String);
  protected
    procedure Setup; override;
  published
    procedure TestNewGameClearsTheBoardAndSelectsANewCodeAndStartsRequestNewGuess;
    procedure TestTakeGuessEvaluatesAndAddsWrongGuessToTheVisibleBoard;
    procedure TestTakeGuessCallsShowPlayerWinsMessageWhenGuessWasCorrect;
    procedure TestTakeTwelveWrongGuessesCallsShowPlayerLosesMessage;
  end;

implementation

uses
  MasterMind.Controller, MasterMind.CodeSelector.Mock, MasterMind.TestHelper, EnumHelper;

procedure TTestMasterMindController.Setup;
var
  CodeSelector: ICodeSelector;
  View: IGameView;
  Evaluator: IGuessEvaluator;
begin
  CodeSelector := TMasterMindCodeSelectorMock.Create(MakeCode([mmcGreen, mmcGreen, mmcRed, mmcRed]));
  Evaluator := TMasterMindGuessEvaluatorMock.Create;
  FEvaluatorMock := Evaluator as IGuessEvaluatorMock;
  View := TMasterMindViewMock.Create;
  FViewMock := View as IGameViewMock;
  FController := TMasterMindController.Create(CodeSelector, Evaluator, View);
end;

procedure TTestMasterMindController.CheckShowGuessesIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowGuessesCallCounter);
  CheckPreviousGuessesIsEmpty(PreviousGuesses);
end;

procedure TTestMasterMindController.CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FStartRequestGuessCallCounter);
  CheckPreviousGuessesIsEmpty(PreviousGuesses);
end;

procedure TTestMasterMindController.CheckPreviousGuessesIsEmpty(const PreviousGuesses: TPreviousGuesses);
begin
  CheckEquals(0, Length(PreviousGuesses), 'Expected previous guesses to be empty!');
end;

procedure TTestMasterMindController.NoOperation(const PreviousGuesses: TPreviousGuesses);
begin
  // Nop
end;

procedure TTestMasterMindController.CheckShowGuessWasCalledOneTime;
begin
  CheckEquals(1, FShowGuessesCallCounter, 'View.ShowGuesses has not been called');
end;

procedure TTestMasterMindController.CheckShowPlayerWinsCalledOneTime;
begin
  CheckEquals(1, FShowPlayerWinsCallCounter, 'View.ShowPlayerWins has not been called');
end;

procedure TTestMasterMindController.CheckShowPlayerLosesCalledOneTime;
begin
  CheckEquals(1, FShowPlayerLosesCallCounter, 'View.ShowPlayerLoses has not been called');
end;

procedure TTestMasterMindController.CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowGuessesCallCounter);
  CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(PreviousGuesses, 'ShowGuesses');
end;

procedure TTestMasterMindController.CheckShowPlayerWinsIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowPlayerWinsCallCounter);
  CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(PreviousGuesses, 'ShowPlayerWins');
end;

procedure TTestMasterMindController.CheckShowPlayerLosesIsCalledWithTwelvePreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowPlayerLosesCallCounter);
  CheckEquals(12, Length(PreviousGuesses));
end;

procedure TTestMasterMindController.CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses; const MethodName: String);
begin
  CheckEquals(1, Length(PreviousGuesses), MethodName +' called with PreviousGuesses having wrong length');
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual(FCurrentlyGuessedCode, PreviousGuesses[0].GuessedCode);
  TEnumHelper<TMasterMindHint>.CheckArraysEqual(FEvaluatorMock.EvaluationResult, PreviousGuesses[0].GuessResult)
end;

procedure TTestMasterMindController.TestNewGameClearsTheBoardAndSelectsANewCodeAndStartsRequestNewGuess;
begin
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithEmptyPreviousGuesses;
  FViewMock.OnStartRequestGuess := CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses;
  FController.NewGame;
  CheckShowGuessWasCalledOneTime;
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual([mmcGreen, mmcGreen, mmcRed, mmcRed], FController.CodeToBeGuessed);
end;

procedure TTestMasterMindController.TestTakeGuessEvaluatesAndAddsWrongGuessToTheVisibleBoard;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhCorrect, mmhWrongPlace, mmhNoMatch, mmhNoMatch]);
  FCurrentlyGuessedCode := MakeCode([mmcRed, mmcRed, mmcRed, mmcRed]);
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FController.TakeGuess(FCurrentlyGuessedCode);
  CheckShowGuessWasCalledOneTime;
end;

procedure TTestMasterMindController.TestTakeGuessCallsShowPlayerWinsMessageWhenGuessWasCorrect;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhCorrect, mmhCorrect, mmhCorrect, mmhCorrect]);
  FCurrentlyGuessedCode := MakeCode([mmcRed, mmcRed, mmcRed, mmcRed]);
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FViewMock.OnShowPlayerWins := CheckShowPlayerWinsIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FController.TakeGuess(FCurrentlyGuessedCode);
  CheckShowGuessWasCalledOneTime;
  CheckShowPlayerWinsCalledOneTime;
end;

procedure TTestMasterMindController.TestTakeTwelveWrongGuessesCallsShowPlayerLosesMessage;
var
  I: Integer;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhNoMatch, mmhNoMatch, mmhNoMatch, mmhNoMatch]);
  FViewMock.OnStartRequestGuess := NoOperation;
  FViewMock.OnShowGuesses := NoOperation;
  FViewMock.OnShowPlayerLoses := CheckShowPlayerLosesIsCalledWithTwelvePreviousGuesses;
  for I := 0 to 12 - 1 do
    FController.TakeGuess(MakeCode([mmcGreen, mmcGreen, mmcGreen, mmcGreen]));

  CheckShowPlayerLosesCalledOneTime;
end;

initialization
  RegisterTest(TTestMasterMindController);

end.