unit MasterMind.Controller;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindController = class(TInterfacedObject, IGameController)
  private
    FCodeSelector: ICodeSelector;
    FView: IGameView;
    FCodeToBeGuessed: TMasterMindCode;
    FPreviousGuesses: TPreviousGuesses;
    procedure ClearPreviousGuesses;
  public
    constructor Create(const CodeSelector: ICodeSelector; const View: IGameView);
    procedure NewGame;
    function GetCodeToBeGuessed: TMasterMindCode;
  end;

implementation

constructor TMasterMindController.Create(const CodeSelector: ICodeSelector; const View: IGameView);
begin
  inherited Create;
  FCodeSelector := CodeSelector;
  FView := View;
end;

procedure TMasterMindController.NewGame;
begin
  ClearPreviousGuesses;
  FCodeToBeGuessed := FCodeSelector.SelectNewCode;
  FView.ShowGuesses(FPreviousGuesses);
  FView.StartRequestGuess(FPreviousGuesses);
end;

function TMasterMindController.GetCodeToBeGuessed: TMasterMindCode;
begin
  Result := FCodeToBeGuessed;
end;

procedure TMasterMindController.ClearPreviousGuesses;
begin
  SetLength(FPreviousGuesses, 0);
end;

end.