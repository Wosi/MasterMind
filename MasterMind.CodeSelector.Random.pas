unit MasterMind.CodeSelector.Random;

interface

uses
  MasterMind.API;

type
  TMasterMindRandomCodeSelector = class(TInterfacedObject, ICodeSelector)
  private
    function GetNumberOfAvailableColors: Integer;
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
    Result[I] := TMasterMindCodeColor(Random(GetNumberOfAvailableColors));
end;

function TMasterMindRandomCodeSelector.GetNumberOfAvailableColors: Integer;
var
  Colors: array[TMasterMindCodeColor] of Byte;
begin
  Result := Length(Colors);
end;

end.