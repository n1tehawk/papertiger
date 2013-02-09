unit tigercgimain;

{$i tigerserver.inc}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,
  tigerservercore;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure listRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure unsupportedRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.listRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>To do: add list scans stuff here.</p>');
  Handled:=true;
end;

procedure TFPWebModule1.unsupportedRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>Unsupported method.</p>' );
  Handled := true;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

