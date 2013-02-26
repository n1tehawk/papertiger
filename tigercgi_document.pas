unit tigercgi_document;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type

  { TFPWebdocument }

  TFPWebdocument = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FPWebdocument: TFPWebdocument;

implementation

{$R *.lfm}

{ TFPWebdocument }

procedure TFPWebdocument.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
// We don't define any actions but handle the request at the module level.
// This allows handling e.g.
// POST http://server/cgi-bin/tigercig/document/ //new doc
// GET http://server/cgi-bin/tigercig/document/304 //get document with id 304
begin
  // First make sure the URI is valid
  ARequest.PathInfo;

end;

initialization
  RegisterHTTPModule('TFPWebDocument', TFPWebdocument);
end.

