unit wiademomain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  wia;

type

  { TForm1 }

  TForm1 = class(TForm)
    scanbutton: TButton;
    procedure scanbuttonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.scanbuttonClick(Sender: TObject);
var
  WIAScanner: TLocalWIAScanner;
begin
  WIAScanner:=TLocalWIAScanner.Create;
  try
    WIAScanner.Scan;
  except
    on E: Exception do
    begin
      ShowMessage('Error while scanning. Technical details: ' + E.Message);
      exit;
    end;
  end;
  WIAScanner.Free;
end;

end.

