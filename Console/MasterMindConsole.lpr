program MasterMindConsole;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  MasterMind.View.Console, MasterMind.API;

type
  TMasterMindConsole = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
  end;

procedure TMasterMindConsole.DoRun;
var
  View: IGameView;
begin
  View := TMasterMindConsoleView.Create;
  View.Start;
  Terminate;
end;

var
  Application: TMasterMindConsole;
begin
  Application := TMasterMindConsole.Create(nil);
  Application.Title:='MasterMind Console';
  Application.Run;
  Application.Free;
end.