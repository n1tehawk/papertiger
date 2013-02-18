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
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,
  tigerutil, tigerservercore;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure deletedocumentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure listRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure scanRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure serverinfoRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure showdocumentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure unsupportedRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
    procedure uploadimageRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
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

procedure TFPWebModule1.deletedocumentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>todo: support method ' + ARequest.QueryString + '.</p>');
  Handled := true;
end;

procedure TFPWebModule1.listRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  {$IFDEF DEBUG}
  AResponse.Contents.Add('<p>papertiger build date: ' +
{$INCLUDE %DATE%}
    +' ' +
{$INCLUDE %TIME%}
    +'</p>');
  {$ENDIF}
  AResponse.Contents.Add('<p>List of documents:</p>');
  try
    AResponse.Contents.Add(FTigerCore.ListDocuments(''));
  except
    on E: Exception do
    begin
      AResponse.Contents.Add('<p>todo: debug: exception ' + E.Message + '</p>');
    end;
  end;
  Handled := true;
end;

procedure TFPWebModule1.scanRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
var
  DocumentID: integer;
  Message: string;
begin
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
  Handled := true;
end;

procedure TFPWebModule1.serverinfoRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>' + StringReplace(FTigerCore.ServerInfo, LineEnding, #13 + #10, [rfReplaceAll]) + '</p>');
  Handled := true;
end;

procedure TFPWebModule1.showdocumentRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>todo: support method ' + ARequest.QueryString + '.</p>');
  Handled := true;
end;

procedure TFPWebModule1.unsupportedRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>Unsupported method.</p>');
  Handled := true;
end;

procedure TFPWebModule1.uploadimageRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Contents.Add('<p>todo: support method ' + ARequest.QueryString + '.</p>');
  Handled := true;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.










