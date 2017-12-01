unit uTestIncode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  EMyException = class(Exception);

   { TSampleThread }

  TSampleThread = class(TThread)
    procedure Execute; override;
  end;

  { TfrmTestIncode }

  TfrmTestIncode = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
    Thread: TSampleThread;
  end;

var
  frmTestIncode: TfrmTestIncode;

implementation

uses
  UExceptionLogger;

{ TSampleThread }

procedure TSampleThread.Execute;
var
  k: string;
begin
  try
    k := 'a';
    Sleep(1000);
    k := 'b';
    raise Exception.Create('Exception inside thread. K=' + K);

  except
    on E: Exception do
      UExceptionLogger.exceptionLogger.HandleException(Self, E);
  end;
end;

{$R *.lfm}

{ TfrmTestIncode }

procedure TfrmTestIncode.Button1Click(Sender: TObject);
begin
  raise EMyException.Create('Woo!');
end;

procedure TfrmTestIncode.Button2Click(Sender: TObject);
begin
  Thread := TSampleThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.

