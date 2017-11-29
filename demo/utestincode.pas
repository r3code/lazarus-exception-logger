unit uTestIncode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  EMyException = class(Exception);

  { TfrmTestIncode }

  TfrmTestIncode = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTestIncode: TfrmTestIncode;

implementation

{$R *.lfm}

{ TfrmTestIncode }

procedure TfrmTestIncode.Button1Click(Sender: TObject);
begin
  raise EMyException.Create('Woo!');
end;

end.

