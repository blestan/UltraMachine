program TestMachine;

{$mode objfpc}{$H+}
{$$apptype console}


uses
  {$IFDEF UNIX}
  cmem,
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TestMain, xon, xtypes, UltraBuffers, UltraParser, UltraSockets,
UltraWorkers;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

