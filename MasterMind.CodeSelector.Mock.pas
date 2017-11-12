unit MasterMind.CodeSelector.Mock;

interface

uses
  MasterMind.API;

type
  TMasterMindCodeSelectorMock = class(TInterfacedObject, ICodeSelector)
  private
    FCodeToSelect: TMasterMindCode;
  public
    constructor Create(const CodeToSelect: TMasterMindCode);
    function SelectNewCode: TMasterMindCode;
  end;
implementation

constructor TMasterMindCodeSelectorMock.Create(const CodeToSelect: TMasterMindCode);
begin
  inherited Create;
  FCodeToSelect := CodeToSelect;
end;

function TMasterMindCodeSelectorMock.SelectNewCode: TMasterMindCode;
begin
  Result := FCodeToSelect;
end;

end.