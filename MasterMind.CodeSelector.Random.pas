unit MasterMind.CodeSelector.Random;

interface

uses
  MasterMind.API;

type
  TMasterMindRandomCodeSelector = class(TInterfacedObject, ICodeSelector)
  public
    function SelectNewCode: TMasterMindCode;
  end;

implementation

function TMasterMindRandomCodeSelector.SelectNewCode: TMasterMindCode;
var
  I: Integer;
begin
  Randomize;
  for I := Low(TMasterMindCode) to High(TMasterMindCode) do
    Result[I] := TMasterMindCodeColor(Random(Length(TMasterMindCode)));
end;

end.