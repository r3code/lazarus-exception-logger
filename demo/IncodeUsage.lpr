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
{$I 'revision.inc'}

var
  exceptionLogger: TExceptionLogger;
begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  exceptionLogger := TExceptionLogger.Create(Application);
  exceptionLogger.LogFileName:= 'bugreport.txt';
  exceptionLogger.AddExtraInfo('code rev', RevisionStr);
  Application.CreateForm(TfrmTestIncode, frmTestIncode);
  Application.Run;
end.

