unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UExceptionLogger;

type

  { TSampleThread }

  TSampleThread = class(TThread)
    procedure Execute; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    btnMakeException: TButton;
    btnMakeThreadException: TButton;
    ExceptionLogger1: TExceptionLogger;
    procedure btnMakeExceptionClick(Sender: TObject);
    procedure btnMakeThreadExceptionClick(Sender: TObject);
  private
    { private declarations }
  public
    Thread: TSampleThread;
  end; 

var
  MainForm: TMainForm;

implementation

{ TSampleThread }

procedure TSampleThread.Execute;
begin
  try
    raise Exception.Create('Exception inside thread');

  except
    on E: Exception do
      MainForm.ExceptionLogger1.HandleException(Self, E);
  end;
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnMakeExceptionClick(Sender: TObject);
begin
  raise Exception.Create('Simple exception');
end;

procedure TMainForm.btnMakeThreadExceptionClick(Sender: TObject);
begin
  Thread := TSampleThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.

