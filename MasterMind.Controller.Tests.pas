unit MasterMind.Controller.Tests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MasterMind.API, MasterMind.View.Mock;

type
  TTestMasterMindController = class(TTestCase)
  private
    FController: IGameController;
    FViewMock: IGameViewMock;
    FShowGuessesCallCounter: Integer;
    FStartRequestGuessCallCounter: Integer;
    procedure CheckShowGuessesIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure CheckPreviousGuessesIsEmpty(const PreviousGuesses: TPreviousGuesses);
  protected
    procedure Setup; override;
  published
    procedure TestNewGameClearsTheBoardAndSelectsANewCodeAndStartsRequestNewGuess;
  end;

implementation

uses
  MasterMind.Controller, MasterMind.CodeSelector.Mock, MasterMind.TestHelper, EnumHelper;

procedure TTestMasterMindController.Setup;
var
  CodeSelector: ICodeSelector;
  View: IGameView;
begin
  CodeSelector := TMasterMindCodeSelectorMock.Create(MakeCode([mmcGreen, mmcGreen, mmcGreen, mmcGreen]));
  View := TMasterMindViewMock.Create;
  FViewMock := View as IGameViewMock;
  FController := TMasterMindController.Create(CodeSelector, View);
end;

procedure TTestMasterMindController.TestNewGameClearsTheBoardAndSelectsANewCodeAndStartsRequestNewGuess;
begin
  FViewMock.OnShowGuesses := CheckShowGuessesIsCalledWithEmptyPreviousGuesses;
  FViewMock.OnStartRequestGuess := CheckStartRequestGuessIsCalledWithEmptyPreviousGuesses;
  FController.NewGame;
  CheckEquals(1, FShowGuessesCallCounter, 'View.ShowGuesses has not been called');
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual([mmcGreen, mmcGreen, mmcGreen, mmcGreen], FController.CodeToBeGuessed);
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

initialization
  RegisterTest(TTestMasterMindController);

end.