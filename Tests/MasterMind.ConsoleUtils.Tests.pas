unit MasterMind.ConsoleUtils.Tests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MasterMind.API;

type
  TTestMasterMindConsoleUtils = class(TTestCase)
  published
    procedure TestTryStringToCodeWithInvalidCode;
    procedure TestTryStringToCodeWithValidCode;
  end;

implementation

uses
  MasterMind.ConsoleUtils, MasterMind.TestHelper, EnumHelper;

procedure TTestMasterMindConsoleUtils.TestTryStringToCodeWithInvalidCode;
var
  Code: TMasterMindCode;
begin
  CheckFalse(TryStringToCode('invalid', Code));
end;

procedure TTestMasterMindConsoleUtils.TestTryStringToCodeWithValidCode;
var
  Code: TMasterMindCode;
begin
  CheckTrue(TryStringToCode('robw', Code));
  TEnumHelper<TMasterMindCodeColor>.CheckArraysEqual(MakeCode([mmcRed, mmcOrange, mmcBrown, mmcWhite]), Code);
end;

initialization
  RegisterTest(TTestMasterMindConsoleUtils);

end.