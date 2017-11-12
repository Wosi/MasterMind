unit MasterMind.View.Mock;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TPreviousGuessesProc = procedure(const PreviousGuesses: TPreviousGuesses) of object;

IGameViewMock = interface
    ['{E6FBEA3B-01F7-41AA-8BA4-A374323C39A5}']
    procedure SetOnStartRequestGuess(const Value: TPreviousGuessesProc);
    procedure SetOnShowGuesses(const Value: TPreviousGuessesProc);
    procedure SetOnShowPlayerWins(const Value: TPreviousGuessesProc);
    procedure SetOnShowPlayerLoses(const Value: TPreviousGuessesProc);
    property OnStartRequestGuess: TPreviousGuessesProc write SetOnStartRequestGuess;
    property OnShowGuesses: TPreviousGuessesProc write SetOnShowGuesses;
    property OnShowPlayerWins: TPreviousGuessesProc write SetOnShowPlayerWins;
    property OnShowPlayerLoses: TPreviousGuessesProc write SetOnShowPlayerLoses;
  end;

  TMasterMindViewMock = class(TInterfacedObject, IGameView, IGameViewMock)
  private
    FOnStartRequestGuess: TPreviousGuessesProc;
    FOnShowGuesses: TPreviousGuessesProc;
    FOnShowPlayerWinsMessage: TPreviousGuessesProc;
    FOnShowPlayerLoses: TPreviousGuessesProc;
  public
    procedure StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
    procedure ShowGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure SetOnStartRequestGuess(const Value: TPreviousGuessesProc);
    procedure SetOnShowGuesses(const Value: TPreviousGuessesProc);
    procedure ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
    procedure SetOnShowPlayerWins(const Value: TPreviousGuessesProc);
    procedure ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
    procedure SetOnShowPlayerLoses(const Value: TPreviousGuessesProc);
  end;

implementation

procedure TMasterMindViewMock.StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
begin
  FOnStartRequestGuess(PreviousGuesses);
end;

procedure TMasterMindViewMock.ShowGuesses(const PreviousGuesses: TPreviousGuesses);
begin
  FOnShowGuesses(PreviousGuesses);
end;

procedure TMasterMindViewMock.SetOnStartRequestGuess(const Value: TPreviousGuessesProc);
begin
  FOnStartRequestGuess := Value;
end;

procedure TMasterMindViewMock.SetOnShowGuesses(const Value: TPreviousGuessesProc);
begin
  FOnShowGuesses := Value;
end;

procedure TMasterMindViewMock.ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
begin
  FOnShowPlayerWinsMessage(PreviousGuesses);
end;

procedure TMasterMindViewMock.SetOnShowPlayerWins(const Value: TPreviousGuessesProc);
begin
  FOnShowPlayerWinsMessage := Value;
end;

procedure TMasterMindViewMock.ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
begin
  FOnShowPlayerLoses(PreviousGuesses);
end;

procedure TMasterMindViewMock.SetOnShowPlayerLoses(const Value: TPreviousGuessesProc);
begin
  FOnShowPlayerLoses := Value;
end;

end.