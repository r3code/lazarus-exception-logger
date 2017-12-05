program Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}  //{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF} //{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='ExceptionLogger Commponent demo';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

