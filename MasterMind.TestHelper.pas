unit MasterMind.TestHelper;

interface

uses
  MasterMind.API;

function MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;

implementation

function MakeCode(const Colors: array of TMasterMindCodeColor): TMasterMindCode;
var
  I: Integer;
begin
  for I := Low(Result) to High(Result) do
    Result[I] := Colors[I];
end;

end.