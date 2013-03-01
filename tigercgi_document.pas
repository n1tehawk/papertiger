unit tigercgi_document;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tigerutil, tigerservercore, strutils, fpjson;

type

  { TFPWebdocument }

  TFPWebdocument = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    FTigerCore: TTigerServerCore;
  public
    { public declarations }
  end;

var
  FPWebdocument: TFPWebdocument;

implementation

{$R *.lfm}

{ TFPWebdocument }

procedure TFPWebdocument.DataModuleCreate(Sender: TObject);
begin
  FTigerCore:=TTigerServerCore.Create;
end;

procedure TFPWebdocument.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

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
  DocumentArray: TJSONArray;
  DocumentID: integer;
  IsValidRequest: boolean;
  OutputJSON: TJSONObject;
  StrippedPath: string;
begin
  IsValidRequest:=false;
  {
  pathinfo apparently returns something like
  /document/304
  StrippedPath will remove trailing and leading /
  }
  StrippedPath:=copy(ARequest.PathInfo,2,Length(ARequest.PathInfo));
  if RightStr(StrippedPath,1)='/' then StrippedPath:=Copy(StrippedPath,1,Length(StrippedPath)-1);
  AResponse.Contents.Add('<p>todo: debug; document module</p>');
  AResponse.Contents.Add('<p>Got request method: '+ARequest.Method+'</p>');
  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      case WordCount(StrippedPath,['/']) of
        1: //http://server/cgi-bin/tigercgi/document/
        begin
          IsValidRequest:=true;
          //todo: delete every document
          AResponse.Contents.Add('<p>todo delete all documents</p>');
        end;
        2: //http://server/cgi-bin/tigercgi/document/304
        begin
          DocumentID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
          if DocumentID<>INVALIDID then
          begin
            IsValidRequest:=true;
            if FTigerCore.DeleteDocument(DocumentID)=false then
            begin
              AResponse.Code:=404;
              AResponse.CodeText:='Error deleting document.';
              AResponse.Contents.Add('<p>Error deleting document.</p>');
            end
            else
            begin
              //Success; 200 OK
            end;
          end;
        end;
      end;
    end;
    'GET':
    begin
      case WordCount(StrippedPath,['/']) of
        1: //http://server/cgi-bin/tigercgi/document/
        begin
          IsValidRequest:=true;
          DocumentArray := TJSONArray.Create();
          try
            FTigerCore.ListDocuments(INVALIDID, DocumentArray);
            AResponse.ContentType := 'application/json';
            AResponse.Contents.Add(DocumentArray.AsJSON);
          except
            on E: Exception do
            begin
              DocumentArray.Clear;
              DocumentArray.Add(TJSONSTring.Create('listRequest: exception ' + E.Message));
              AResponse.Contents.Insert(0, DocumentArray.AsJSON);
            end;
          end;
        end;
        2: //http://server/cgi-bin/tigercgi/document/304
        begin
          DocumentID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
          if DocumentID<>INVALIDID then IsValidRequest:=true;
          //todo: delete given document
          AResponse.Contents.Add('<p>todo get document '+inttostr(documentid)+'</p>');
        end;
      end;
    end;
    'POST':
    begin
      //http://server/cgi-bin/tigercgi/document/
      if WordCount(StrippedPath,['/'])=1 then
      begin
        IsValidRequest:=true;
        //todo: create new document, return id
        AResponse.Contents.Add('<p>todo post/create new document, return id</p>');
      end;
    end;
    'PUT':
    begin
      //http://server/cgi-bin/tigercgi/document/304
      if WordCount(StrippedPath,['/'])=2 then
      begin
        DocumentID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
        if DocumentID<>INVALIDID then
        begin
          IsValidRequest:=true;
          DocumentID:=FTigerCore.AddDocument('Document ' +
            FormatDateTime('yyyymmddhhnnss', Now));
          if DocumentID=INVALIDID then
          begin
            AResponse.Code:=404;
            AResponse.CodeText:='Error inserting new document.';
            AResponse.Contents.Add('<p>Error inserting new document.</p>');
          end
          else
          begin
            AResponse.ContentType := 'application/json';
            OutputJSON := TJSONObject.Create();
            try
              OutputJSON.Add('documentid',DocumentID);
              AResponse.Contents.Add(OutputJSON.AsJSON);
            finally
              OutputJSON.Free;
            end;
          end;
        end;
        //todo: modify given document
        AResponse.Contents.Add('<p>todo put/modify document '+inttostr(documentid)+'</p>');
      end;
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

