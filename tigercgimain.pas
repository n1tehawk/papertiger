unit tigercgimain;

{$i tigerserver.inc}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,
  tigerservercore;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure deletedocumentRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure listRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure scanRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure showdocumentRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure unsupportedRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure uploadimageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    FTigerCore: TTigerServerCore;
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.deletedocumentRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>todo: support method '+ARequest.QueryString+'.</p>' );
  Handled := true;
end;

procedure TFPWebModule1.listRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  {$IFDEF DEBUG}
  AResponse.Contents.Add('<p>papertiger build date: '+{$INCLUDE %DATE%}+' '+{$INCLUDE %TIME%}+'</p>'));
  {$ENDIF}
  AResponse.Contents.Add('<p>List of documents:</p>');
  try
    AResponse.Contents.Add(FTigerCore.ListDocuments(''));
  except
    on E: Exception do
    begin
      AResponse.Contents.Add('todo: debug: exception '+E.Message);
    end;
  end;
  Handled:=true;
end;

procedure TFPWebModule1.scanRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>todo: support method '+ARequest.QueryString+'.</p>' );
  Handled := true;
end;

procedure TFPWebModule1.showdocumentRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>todo: support method '+ARequest.QueryString+'.</p>' );
  Handled := true;
end;

procedure TFPWebModule1.unsupportedRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>Unsupported method.</p>' );
  Handled := true;
end;

procedure TFPWebModule1.uploadimageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Contents.Add('<p>todo: support method '+ARequest.QueryString+'.</p>' );
  Handled := true;
end;

constructor TFPWebModule1.Create;
begin
  FTigerCore:=TTigerServerCore.Create;
end;

destructor TFPWebModule1.Destroy;
begin
  FTigerCore.Free;
  inherited Destroy;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

