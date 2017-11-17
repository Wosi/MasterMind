unit MasterMind.Evaluator.Mock;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  IGuessEvaluatorMock = interface
    ['{21139CC3-FD94-4DC2-957D-7324C5CA00FB}']
    procedure SetEvaluationResult(const Value: TGuessEvaluationResult);
    function GetEvaluationResult: TGuessEvaluationResult;
    property EvaluationResult: TGuessEvaluationResult read GetEvaluationResult write SetEvaluationResult;
  end;

  TMasterMindGuessEvaluatorMock = class(TInterfacedObject, IGuessEvaluator, IGuessEvaluatorMock)
  private
    FEvaluation: TGuessEvaluationResult;
  public
    function EvaluateGuess(const CodeToBeGuessed: TMasterMindCode; const Guess: TMasterMindCode): TGuessEvaluationResult;
    procedure SetEvaluationResult(const Value: TGuessEvaluationResult);
    function GetEvaluationResult: TGuessEvaluationResult;
  end;

implementation

function TMasterMindGuessEvaluatorMock.EvaluateGuess(const CodeToBeGuessed: TMasterMindCode; const Guess: TMasterMindCode): TGuessEvaluationResult;
begin
  Result := FEvaluation;
end;

procedure TMasterMindGuessEvaluatorMock.SetEvaluationResult(const Value: TGuessEvaluationResult);
begin
  FEvaluation := Value;
end;

function TMasterMindGuessEvaluatorMock.GetEvaluationResult: TGuessEvaluationResult;
begin
  Result := FEvaluation;
end;

end.