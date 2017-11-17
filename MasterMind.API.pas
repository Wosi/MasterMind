unit MasterMind.API;

interface

const
  CODE_SIZE = 4;
  MAX_GUESSES = 12;

type
  TMasterMindCodeColor = (mmcGreen, mmcYellow, mmcOrange, mmcRed, mmcWhite, mmcBrown);
  TMasterMindCode = array[0..CODE_SIZE - 1] of TMasterMindCodeColor;
  TMasterMindHint = (mmhNoMatch, mmhWrongPlace, mmhCorrect);
  TGuessEvaluationResult = array[0..CODE_SIZE - 1] of TMasterMindHint;

  TEvaluatedGuess = record
    GuessedCode: TMasterMindCode;
    GuessResult: TGuessEvaluationResult;
  end;

  TPreviousGuesses = array of TEvaluatedGuess;

  IGuessEvaluator = interface
    ['{168D4F90-D778-4BCF-A401-D32241932779}']
    function EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TGuessEvaluationResult;
  end;

  ICodeSelector = interface
    ['{909D1C37-3715-4C0E-8BA4-703F0068426A}']
    function SelectNewCode: TMasterMindCode;
  end;

  IGamePresenter = interface
    ['{E5412473-10EB-4B5F-B0A7-88C66FEE6A1A}']
    procedure NewGame;
    function GetCodeToBeGuessed: TMasterMindCode;
    procedure TakeGuess(const Guess: TMasterMindCode);
    property CodeToBeGuessed: TMasterMindCode read GetCodeToBeGuessed;
  end;

  IGameView = interface
    ['{C188DF5F-1B9D-423C-8619-FA00C617B601}']
    procedure Start;
    procedure StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
    procedure ShowGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
  end;

implementation

end.