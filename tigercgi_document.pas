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
    procedure everythingAfterResponse(Sender: TObject; AResponse: TResponse);
    procedure everythingBeforeRequest(Sender: TObject; ARequest: TRequest);
    procedure everythingRequest(Sender: TObject; ARequest: TRequest;
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
// We don't define any actions but handle the request at the module level before any actions would be evaluated.
{
Handled URLs/methods:
DELETE http://server/cgi-bin/tigercgi/document/    //delete all docs?!?!
GET    http://server/cgi-bin/tigercgi/document/    //list of docs
POST   http://server/cgi-bin/tigercgi/document/    //let server create new doc, return documentid
DELETE http://server/cgi-bin/tigercgi/document/304 //remove document with id 304
GET    http://server/cgi-bin/tigercgi/document/304 //get document with id 304
PUT    http://server/cgi-bin/tigercgi/document/304 //edit doc with id 304
}
begin
  writeln('<p>document module</p>');
  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      {
      pathinfo apparently returns something like
      /document/304
      }

    end;
    'GET':
    begin

    end;
    'POST':
    begin

    end;
    'PUT':
    begin

    end;
  end;
  Handled:=true;
end;

procedure TFPWebdocument.everythingAfterResponse(Sender: TObject;
  AResponse: TResponse);
begin

end;

procedure TFPWebdocument.everythingBeforeRequest(Sender: TObject;
  ARequest: TRequest);
begin

end;

procedure TFPWebdocument.everythingRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  writeln('Sender classname: '+Sender.ClassName);
  Handled:=true;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/document/*
  RegisterHTTPModule('document', TFPWebdocument);
end.

