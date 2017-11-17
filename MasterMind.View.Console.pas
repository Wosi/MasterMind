unit MasterMind.View.Console;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  MasterMind.API;

type
  TMasterMindConsoleView = class(TInterfacedObject, IGameView)
  private
    FController: IGameController;
    procedure WriteEvaluatedGuess(const PreviousGuess: TEvaluatedGuess);
    procedure WriteGuessResult(const GuessResult: TGuessEvaluationResult);
    procedure WriteHint(const Hint: TMasterMindHint);
    function GetColorForHint(const Hint: TMasterMindHint): Integer;
    function GetCharForHint(const Hint: TMasterMindHint): Char;
    procedure WriteCode(const Code: TMasterMindCode);
    procedure WriteCodeColor(const Color: TMasterMindCodeColor);
    function CodeColorToTextColor(const Color: TMasterMindCodeColor): Integer;
    procedure AskToStartNewGame;
  public
    constructor Create;
    procedure Start;
    procedure StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
    procedure ShowGuesses(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
    procedure ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
  end;

implementation

uses
  MasterMind.ControllerFactory, MasterMind.ConsoleUtils, crt;

constructor TMasterMindConsoleView.Create;
begin
  inherited Create;
  FController := TMasterMindControllerFactory.CreateController(Self);
end;

procedure TMasterMindConsoleView.Start;
begin
  FController.NewGame;
end;

procedure TMasterMindConsoleView.StartRequestGuess(const PreviousGuesses: TPreviousGuesses);
var
  Code: TMasterMindCode;
  CodeString: String;
begin
  repeat
    WriteLn(Output);
    Writeln(Output, 'Make a guess!');
    WriteLn(Output);
    Writeln(Output, 'A guess consists of ', Length(Code), ' colors.');
    Writeln(Output, 'Type "robw" if you want to guess red, orange, brown, white.');
    Writeln(Output, 'Select your code from these colors: ');
    TextColor(Green);
    Writeln(Output, '  (G)reen');
    TextColor(Yellow);
    Writeln(Output, '  (Y)ello');
    TextColor(LightRed);
    Writeln(Output, '  (O)range');
    TextColor(Red);
    Writeln(Output, '  (R)ed');
    TextColor(White);
    Writeln(Output, '  (W)hite');
    TextColor(Brown);
    Writeln(Output, '  (B)rown');
    TextColor(White);
    WriteLn(Output);
    ReadLn(Input, CodeString);
  until TryStringToCode(CodeString, Code);

  FController.TakeGuess(Code);
end;

procedure TMasterMindConsoleView.ShowGuesses(const PreviousGuesses: TPreviousGuesses);
var
  I: Integer;
begin
  for I := High(PreviousGuesses) downto Low(PreviousGuesses) do
    WriteEvaluatedGuess(PreviousGuesses[I]);
end;

procedure TMasterMindConsoleView.ShowPlayerWinsMessage(const PreviousGuesses: TPreviousGuesses);
begin
  WriteLn(Output, 'You win!');
  AskToStartNewGame;
end;

procedure TMasterMindConsoleView.ShowPlayerLosesMessage(const PreviousGuesses: TPreviousGuesses);
begin
  WriteLn(Output, 'You lose!');
  WriteLn(Output);
  Write('Searched code was: ');
  WriteCode(FController.CodeToBeGuessed);
  WriteLn(Output);
  AskToStartNewGame;
end;

procedure TMasterMindConsoleView.WriteEvaluatedGuess(const PreviousGuess: TEvaluatedGuess);
begin
  WriteGuessResult(PreviousGuess.GuessResult);
  Write(Output, '    ');
  WriteCode(PreviousGuess.GuessedCode);
  WriteLn(Output);
end;

procedure TMasterMindConsoleView.WriteGuessResult(const GuessResult: TGuessEvaluationResult);
var
  Hint: TMasterMindHint;
begin
  for Hint in GuessResult do
    WriteHint(Hint);
end;

procedure TMasterMindConsoleView.WriteHint(const Hint: TMasterMindHint);
var
  Color: Integer;
  Chr: Char;
begin
  Color := GetColorForHint(Hint);
  TextColor(Color);
  Chr := GetCharForHint(Hint);
  Write(Output, Chr);
  TextColor(White);
end;

function TMasterMindConsoleView.GetColorForHint(const Hint: TMasterMindHint): Integer;
const
  COLORS: array[TMasterMindHint] of Integer = (
    DarkGray,
    White,
    LightRed
  );
begin
  Result := COLORS[Hint];
end;

function TMasterMindConsoleView.GetCharForHint(const Hint: TMasterMindHint): Char;
begin
  if Hint = mmhNoMatch then
    Result := ' '
  else
    Result := 'o';
end;

procedure TMasterMindConsoleView.WriteCode(const Code: TMasterMindCode);
var
  Color: TMasterMindCodeColor;
begin
  for Color in Code do
    WriteCodeColor(Color);
end;

procedure TMasterMindConsoleView.WriteCodeColor(const Color: TMasterMindCodeColor);
var
  CharColor: Integer;
begin
  CharColor := CodeColorToTextColor(Color);
  TextColor(CharColor);
  Write(Output, 'O');
  TextColor(White);
end;

function TMasterMindConsoleView.CodeColorToTextColor(const Color: TMasterMindCodeColor): Integer;
const
  COLORS: array[TMasterMindCodeColor] of Integer = (
    Green,
    Yellow,
    LightRed,
    Red,
    White,
    Brown
  );
begin
  Result := COLORS[Color];
end;

procedure TMasterMindConsoleView.AskToStartNewGame;
var
  Chr: Char;
begin
  repeat
    Write('Do you want to start a new game? (y/n)');
    Read(Input, Chr);
  until (Chr = 'y') or (Chr = 'n');

  if Chr = 'y' then
    FController.NewGame
  else
    FController := Nil;
end;

end.