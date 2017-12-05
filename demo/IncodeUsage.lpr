program IncodeUsage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uTestIncode, UExceptionLogger, sysutils, usysinfo, uappinfo;

{$R *.res}
{$I 'revision.inc'}


begin
  Application.Title:='IncodeUsage Exaple of ExceptionLogger';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  exceptionLogger := TExceptionLogger.Create(Application);
  exceptionLogger.LogFileName:= 'bugreport.txt';
  exceptionLogger.AddExtraInfo('code revision', RevisionStr);
  Application.CreateForm(TfrmTestIncode, frmTestIncode);
  Application.Run;
end.

