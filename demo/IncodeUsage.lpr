program IncodeUsage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uTestIncode, UExceptionLogger, sysutils
  { you can add units after this };

{$R *.res}

var
  exceptionLogger: TExceptionLogger;
begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  exceptionLogger := TExceptionLogger.Create(Application);
  exceptionLogger.LogFileName:= 'bugreport.txt';
  Application.CreateForm(TfrmTestIncode, frmTestIncode);
  Application.Run;
end.

