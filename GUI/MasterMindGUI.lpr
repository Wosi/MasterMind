program MasterMindGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MasterMind.View.Form
  { you can add units after this };

{$R *.res}

var
  FormMasterMind: TFormMasterMind;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMasterMind, FormMasterMind);
  Application.Run;
end.