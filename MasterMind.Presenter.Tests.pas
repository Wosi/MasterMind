unit MasterMind.Presenter.Tests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MasterMind.API, MasterMind.View.Mock, MasterMind.Evaluator.Mock;

type
  TTestMasterMindPresenter = class(TTestCase)
  private
    FPresenter: IGamePresenter;
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
    procedure CheckStartRequestGuessIsCalledWithPreviousGuess(const PreviousGuesses: TPreviousGuesses);
    procedure NoOperation(const PreviousGuesses: TPreviousGuesses);
    procedure CheckPreviousGuessesIsEmpty(const PreviousGuesses: TPreviousGuesses);
    procedure CheckShowGuessWasCalledOneTime;
    procedure CheckShowPlayerWinsCalledOneTime;
    procedure CheckShowPlayerLosesCalledOneTime;
    procedure CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses; const MethodName: String);
    procedure CheckStartRequestGuessWasCalledOneTime;
    procedure CheckShowGuessWasCalledNTimes(const N: Integer);
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
  MasterMind.Presenter, MasterMind.CodeSelector.Mock, MasterMind.TestHelper, EnumHelper;

procedure TTestMasterMindPresenter.Setup;
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
  FPresenter := TMasterMindPresenter.Create(CodeSelector, Evaluator, View);
end;

procedure TTestMasterMindPresenter.CheckShowGuessesIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowGuessesCallCounter);
  CheckPreviousGuessesIsEmpty(PreviousGuesses);
end;

procedure TTestMasterMindPresenter.CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FStartRequestGuessCallCounter);
  CheckPreviousGuessesIsEmpty(PreviousGuesses);
end;

procedure TTestMasterMindPresenter.CheckStartRequestGuessIsCalledWithPreviousGuess(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FStartRequestGuessCallCounter);
  CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(PreviousGuesses);
end;

procedure TTestMasterMindPresenter.CheckPreviousGuessesIsEmpty(const PreviousGuesses: TPreviousGuesses);
begin
  CheckEquals(0, Length(PreviousGuesses), 'Expected previous guesses to be empty!');
end;

procedure TTestMasterMindPresenter.NoOperation(const PreviousGuesses: TPreviousGuesses);
begin
  // Nop
end;

procedure TTestMasterMindPresenter.CheckShowGuessWasCalledNTimes(const N: Integer);
begin
  CheckEquals(N, FShowGuessesCallCounter, 'View.ShowGuesses has not been called');
end;

procedure TTestMasterMindPresenter.CheckShowGuessWasCalledOneTime;
begin
  CheckShowGuessWasCalledNTimes(1);
end;

procedure TTestMasterMindPresenter.CheckShowPlayerWinsCalledOneTime;
begin
  CheckEquals(1, FShowPlayerWinsCallCounter, 'View.ShowPlayerWins has not been called');
end;

procedure TTestMasterMindPresenter.CheckShowPlayerLosesCalledOneTime;
begin
  CheckEquals(1, FShowPlayerLosesCallCounter, 'View.ShowPlayerLoses has not been called');
end;

procedure TTestMasterMindPresenter.CheckStartRequestGuessWasCalledOneTime;
begin
  CheckEquals(1, FStartRequestGuessCallCounter, 'View.StartRequestGuess has not been called');
end;

procedure TTestMasterMindPresenter.CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowGuessesCallCounter);
  CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(PreviousGuesses, 'ShowGuesses');
end;

procedure TTestMasterMindPresenter.CheckShowPlayerWinsIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowPlayerWinsCallCounter);
  CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(PreviousGuesses, 'ShowPlayerWins');
end;

procedure TTestMasterMindPresenter.CheckShowPlayerLosesIsCalledWithTwelvePreviousGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  Inc(FShowPlayerLosesCallCounter);
  CheckEquals(12, Length(PreviousGuesses));
end;

procedure TTestMasterMindPresenter.CheckOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult(const PreviousGuesses: TPreviousGuesses; const MethodName: String);
begin
  CheckEquals(1, Length(PreviousGuesses), MethodName +' called with PreviousGuesses having wrong length');
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual(FCurrentlyGuessedCode, PreviousGuesses[0].GuessedCode);
  TEnumHelper<TMasterMindHint>.CheckArraysEqual(FEvaluatorMock.EvaluationResult, PreviousGuesses[0].GuessResult)
end;

procedure TTestMasterMindPresenter.TestNewGameClearsTheBoardAndSelectsANewCodeAndStartsRequestNewGuess;
begin
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithEmptyPreviousGuesses;
  FViewMock.OnStartRequestGuess := CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses;
  FPresenter.NewGame;
  CheckShowGuessWasCalledOneTime;
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual([mmcGreen, mmcGreen, mmcRed, mmcRed], FPresenter.CodeToBeGuessed);
end;

procedure TTestMasterMindPresenter.TestTakeGuessEvaluatesAndAddsWrongGuessToTheVisibleBoard;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhCorrect, mmhWrongPlace, mmhNoMatch, mmhNoMatch]);
  FCurrentlyGuessedCode := MakeCode([mmcRed, mmcRed, mmcRed, mmcRed]);
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FViewMock.OnStartRequestGuess := CheckStartRequestGuessIsCalledWithPreviousGuess;
  FPresenter.TakeGuess(FCurrentlyGuessedCode);
  CheckShowGuessWasCalledNTimes(2);
  CheckStartRequestGuessWasCalledOneTime;
end;

procedure TTestMasterMindPresenter.TestTakeGuessCallsShowPlayerWinsMessageWhenGuessWasCorrect;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhCorrect, mmhCorrect, mmhCorrect, mmhCorrect]);
  FCurrentlyGuessedCode := MakeCode([mmcRed, mmcRed, mmcRed, mmcRed]);
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FViewMock.OnShowPlayerWins := CheckShowPlayerWinsIsCalledWithOnePreviousGuessHavingTheGuessedCodeAndTheEvaluatorsResult;
  FPresenter.TakeGuess(FCurrentlyGuessedCode);
  CheckShowGuessWasCalledOneTime;
  CheckShowPlayerWinsCalledOneTime;
end;

procedure TTestMasterMindPresenter.TestTakeTwelveWrongGuessesCallsShowPlayerLosesMessage;
var
  I: Integer;
begin
  FEvaluatorMock.EvaluationResult := MakeResult([mmhNoMatch, mmhNoMatch, mmhNoMatch, mmhNoMatch]);
  FViewMock.OnStartRequestGuess := NoOperation;
  FViewMock.OnShowGuesses := NoOperation;
  FViewMock.OnShowPlayerLoses := CheckShowPlayerLosesIsCalledWithTwelvePreviousGuesses;
  for I := 0 to 12 - 1 do
    FPresenter.TakeGuess(MakeCode([mmcGreen, mmcGreen, mmcGreen, mmcGreen]));

  CheckShowPlayerLosesCalledOneTime;
end;

initialization
  RegisterTest(TTestMasterMindPresenter);

end.