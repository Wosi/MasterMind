unit MasterMind.API;

interface

const
  CODE_SIZE = 4;
  MAX_GUESSES = 12;

type
  TMasterMindCodeColor = (mmcGreen, mmcYellow, mmcOrange, mmcRed, mmcBlue, mmcBrown);
  TMasterMindCode = array[0..CODE_SIZE - 1] of TMasterMindCodeColor;
  TMasterMindHint = (mmhNoMatch, mmhWrongPlace, mmhCorrect);
  TMasterMindGuessEvaluationResult = array[0..CODE_SIZE - 1] of TMasterMindHint;

  IGuessEvaluator = interface
    ['{168D4F90-D778-4BCF-A401-D32241932779}']
    function EvaluateGuess(const CodeToBeGuessed, Guess: TMasterMindCode): TMasterMindGuessEvaluationResult;
  end;

implementation

end.