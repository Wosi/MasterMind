unit MasterMind.TestHelper;

interface

uses
  MasterMind.API;

function MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;
function MakeResult(const Hints: array of TMasterMindHint): TGuessEvaluationResult;

implementation

function MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;
var
  I: Integer;
begin
  for I := Low(Result) to High(Result) do
    Result[I] := Colors[I];
end;

function MakeResult(const Hints: array of TMasterMindHint): TGuessEvaluationResult;
var
  I: Integer;
begin
  for I := Low(Result) to High(Result) do
    Result[I] := Hints[I];
end;

end.