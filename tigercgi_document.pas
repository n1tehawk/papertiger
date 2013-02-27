unit tigercgi_document;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tigerutil, tigerservercore, strutils;

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
var
  DocumentID: integer;
  IsValidRequest: boolean;
  StrippedPath: string;
begin
  IsValidRequest:=false;
  {
  pathinfo apparently returns something like
  /document/304
  StrippedPath will remove trailing and leading /
  }
  StrippedPath:=copy(ARequest.PathInfo,2,Length(ARequest.PathInfo));
  if StrippedPath[Length(StrippedPath)]='/' then StrippedPath:=Copy(StrippedPath,1,Length(StrippedPath)-1);
  writeln('<p>document module</p>');
  writeln('<p>sender classname '+Sender.Classname+'</p>');
  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      case CountSubString(StrippedPath,'/') of
        0:
        begin
          IsValidRequest:=true;
          //todo: delete every document
          AResponse.Contents.Add('<p>todo delete all documents</p>');
        end;
        1:
        begin
          DocumentID:=StrToIntDef(Copy(StrippedPath,RPos('/',StrippedPath),Length(StrippedPath)),INVALIDiD);
          if DocumentID<>INVALIDID then IsValidRequest:=true;
          //todo: delete given document
          AResponse.Contents.Add('<p>todo delete document '+inttostr(documentid)+'</p>');
        end;
      end;
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
  if not(IsValidRequest) then
  begin
    AResponse.Code:=404;
    AResponse.CodeText:='Document not found.';
    AResponse.Contents.Add('<p>Document not found/invalid request</p>');
  end;
  Handled:=true;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/document/*
  RegisterHTTPModule('document', TFPWebdocument);
end.

