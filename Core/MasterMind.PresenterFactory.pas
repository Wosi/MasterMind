unit MasterMind.PresenterFactory;

interface

uses
  MasterMind.API;

type
  TMasterMindPresenterFactory = class
    class function CreatePresenter(const View: IGameView): IGamePresenter;
  end;

implementation

uses
  MasterMind.Presenter, MasterMind.CodeSelector.Random, MasterMind.Evaluator;

class function TMasterMindPresenterFactory.CreatePresenter(const View: IGameView): IGamePresenter;
var
  Selector: ICodeSelector;
  Evaluator: IGuessEvaluator;
begin
  Selector := TMasterMindRandomCodeSelector.Create;
  Evaluator := TMasterMindGuessEvaluator.Create;
  Result := TMasterMindPresenter.Create(Selector, Evaluator, View);
end;

end.