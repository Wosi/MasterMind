unit MasterMind.ControllerFactory;

interface

uses
  MasterMind.API;

type
  TMasterMindControllerFactory = class
    class function CreateController(const View: IGameView): IGameController;
  end;

implementation

uses
  MasterMind.Controller, MasterMind.CodeSelector.Random, MasterMind.Evaluator;

class function TMasterMindControllerFactory.CreateController(const View: IGameView): IGameController;
var
  Selector: ICodeSelector;
  Evaluator: IGuessEvaluator;
begin
  Selector := TMasterMindRandomCodeSelector.Create;
  Evaluator := TMasterMindGuessEvaluator.Create;
  Result := TMasterMindController.Create(Selector, Evaluator, View);
end;

end.