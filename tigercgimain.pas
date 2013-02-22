unit tigercgimain;

{ CGI server part of papertiger.

  Copyright (c) 2013 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$i tigerserver.inc}

interface

uses
  SysUtils, Classes, httpdefs, fpjson, jsonparser, fpHTTP, fpWeb,
  tigerutil, tigerservercore;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure deletedocumentRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure listRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure scanRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure serverinfoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure showdocumentRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure showimageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure unsupportedRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure uploadimageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    { private declarations }
    FTigerCore: TTigerServerCore;
  public
    { public declarations }
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  FTigerCore := TTigerServerCore.Create;
end;

procedure TFPWebModule1.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

procedure TFPWebModule1.deletedocumentRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>todo: support method ' + ARequest.QueryString + '.</p>');
  Handled := True;
end;

procedure TFPWebModule1.listRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  DocumentArray: TJSONArray;
begin
  AResponse.ContentType := 'application/json';
  DocumentArray := TJSONArray.Create();
  try
    FTigerCore.ListDocuments(INVALIDID, DocumentArray);
    AResponse.Contents.Add(DocumentArray.AsJSON);
  except
    on E: Exception do
    begin
      DocumentArray.Clear;
      DocumentArray.Add(TJSONSTring.Create('listRequest: exception ' + E.Message));
      AResponse.Contents.Insert(0, DocumentArray.AsJSON);
    end;
  end;
  Handled := True;
end;

procedure TFPWebModule1.scanRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
// Scans page and adds it to existing document or creates new document if none given
var
  DocumentID: integer;
  Message: string;
begin
  //todo implement existing document, look at showdocument
  //todo implement number of pages, language etc
  //todo: json this up
  try
    DocumentID := FTigerCore.ScanAndProcess;
    if DocumentID <> INVALIDID then
      AResponse.Contents.Add('<p>Scanning succeeded.</p>')
    else
      AResponse.Contents.Add('<p>Scanning failed; an error occurred.</p>');
  except
    Message := 'Scanning failed; an excecption occurred.';
    AResponse.Contents.Add('<p>' + Message + '</p>');
    TigerLog.WriteLog(etError, 'scanRequest ' + Message);
  end;
  Handled := True;
end;

procedure TFPWebModule1.serverinfoRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  OutputJSON: TJSONString;
begin
  AResponse.ContentType := 'application/json';
  OutputJSON := TJSONString.Create(FTigerCore.ServerInfo);
  try
    AResponse.Contents.Add(OutputJSON.AsJSON);
  finally
    OutputJSON.Free;
  end;
  Handled := True;
end;

procedure TFPWebModule1.showdocumentRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  //todo: do the same as for showdocument except show the pdf
  aresponse.contents.add('<p>todo debug this needs much work.</p>');
  handled := True;
end;

procedure TFPWebModule1.showimageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
// Show image given by post with json docid integer
var
  DocumentID, Sequence: integer;
  Query: TJSONObject;
  Success: boolean;
begin
  Success := False;
  try
    // for uniformity, we expect a POST+a generic json tag, though we could have used e.g. docid directly
    //todo: adapt so query in URL is also accepted (for gets)
    Query := TJSONParser.Create(ARequest.Content).Parse as TJSONObject;
    DocumentID := Query.Integers['documentid'];
    Sequence := Query.Integers['sequence']; //image order number
    Success := True;
  except
    TigerLog.WriteLog(etDebug, 'showDocumentRequest: error parsing document id.');
  end;

  if Success then
  begin
    //retrieve tiff and put in output stream
    AResponse.ContentStream := TMemoryStream.Create;
    try
      // Load tiff into content stream:
      if FTigerCore.GetImage(DocumentID, Sequence, AResponse.ContentStream) then
      begin
        // Indicate papertiger should be able to deal with this data:
        AResponse.ContentType := 'image/tiff; application=papertiger';
        AResponse.ContentLength:=AResponse.ContentStream.Size; //apparently doesn't happen automatically?
        AResponse.SendContent;
      end
      else
      begin
        // Not found? error message
        //todo: for all error messages, return 500 or something instead of 200 ok
        AResponse.Code:=404;
        AResponse.CodeText:='Error getting image file for document ID ' +
          IntToStr(DocumentID);
        AResponse.Contents.Add('<p>Error getting image file for document ID ' +
          IntToStr(DocumentID) + '</p>');
      end;
    finally
      AResponse.ContentStream.Free;
    end;
  end
  else
  begin
    // error message
    AResponse.Code:=404;
    AResponse.CodeText:='Error retrieving image for document ID ' +
      IntToStr(DocumentID);
    AResponse.Contents.Add('<p>Error retrieving image for document ID ' +
      IntToStr(DocumentID) + '</p>');
  end;
  Handled := True;
end;

procedure TFPWebModule1.unsupportedRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>Unsupported method.</p>');
  Handled := True;
end;

procedure TFPWebModule1.uploadimageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>todo: support method ' + ARequest.QueryString + '.</p>');
  Handled := True;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.
