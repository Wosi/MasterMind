unit MasterMind.ConsoleUtils;

interface

uses
  MasterMind.API;

function TryStringToCode(const CodeString: String; out Code: TMasterMindCode): Boolean;

implementation

uses
  sysutils;

function TryCharToColor(const AChar: Char; out Color: TMasterMindCodeColor): Boolean;
begin
  Result := True;

  case UpperCase(AChar) of
    'G': Color := mmcGreen;
    'Y': Color := mmcYellow;
    'O': Color := mmcOrange;
    'R': Color := mmcRed;
    'W': Color := mmcWhite;
    'B': Color := mmcBrown;
    else
      Exit(False);
  end;
end;

function TryStringToCode(const CodeString: String; out Code: TMasterMindCode): Boolean;
var
  Color: TMasterMindCodeColor;
  I: Integer;
begin
  if Length(CodeString) <> Length(Code) then
    Exit(False);

  for I := Low(Code) to High(Code) do
    if TryCharToColor(CodeString[I + 1], Color) then
      Code[I] := Color
    else
      Exit(False);

  Result := True;
end;

end.