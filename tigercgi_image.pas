unit tigercgi_image;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tigerutil, tigerservercore, strutils;

type

  { TFPWebimage }

  TFPWebimage = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FPWebimage: TFPWebimage;

implementation

{$R *.lfm}

{ TFPWebimage }

procedure TFPWebimage.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
// We don't define any actions but handle the request at the module level before any actions would be evaluated.
{
Handled URLs/methods:
DELETE http://server/cgi-bin/tigercgi/image/    //delete all images?!?!
GET    http://server/cgi-bin/tigercgi/image/    //list of images
POST   http://server/cgi-bin/tigercgi/image/    //let server create new image, return imageid
DELETE http://server/cgi-bin/tigercgi/image/304 //remove image with id 304
GET    http://server/cgi-bin/tigercgi/image/304 //get image with id 304
PUT    http://server/cgi-bin/tigercgi/image/304 //edit image with id 304
}
var
  ImageID: integer;
  IsValidRequest: boolean;
  StrippedPath: string;
begin
  IsValidRequest:=false;
  {
  pathinfo apparently returns something like
  /image/304
  StrippedPath will remove trailing and leading /
  }
  StrippedPath:=copy(ARequest.PathInfo,2,Length(ARequest.PathInfo));
  if RightStr(StrippedPath,1)='/' then StrippedPath:=Copy(StrippedPath,1,Length(StrippedPath)-1);
  AResponse.Contents.Add('<p>todo: debug; image module</p>');
  AResponse.Contents.Add('<p>Got request method: '+ARequest.Method+'</p>');
  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      case WordCount(StrippedPath,['/']) of
        1: //http://server/cgi-bin/tigercgi/image/
        begin
          IsValidRequest:=true;
          //todo: delete every image
          AResponse.Contents.Add('<p>todo delete all images</p>');
        end;
        2: //http://server/cgi-bin/tigercgi/image/304
        begin
          ImageID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
          if ImageID<>INVALIDID then IsValidRequest:=true;
          //todo: delete given image
          AResponse.Contents.Add('<p>todo delete image '+inttostr(ImageID)+'</p>');
        end;
      end;
    end;
    'GET':
    begin
      case WordCount(StrippedPath,['/']) of
        1: //http://server/cgi-bin/tigercgi/image/
        begin
          IsValidRequest:=true;
          //todo: get every image
          AResponse.Contents.Add('<p>todo get all images</p>');
        end;
        2: //http://server/cgi-bin/tigercgi/image/304
        begin
          ImageID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
          if ImageID<>INVALIDID then IsValidRequest:=true;
          //todo: delete given image
          AResponse.Contents.Add('<p>todo get image '+inttostr(ImageID)+'</p>');
        end;
      end;
    end;
    'POST':
    begin
      //http://server/cgi-bin/tigercgi/image/
      if WordCount(StrippedPath,['/'])=1 then
      begin
        IsValidRequest:=true;
        //todo: create new image, return id
        AResponse.Contents.Add('<p>todo post/create new image, return id</p>');
      end;
    end;
    'PUT':
    begin
      //http://server/cgi-bin/tigercgi/image/304
      if WordCount(StrippedPath,['/'])=2 then
      begin
        ImageID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
        if ImageID<>INVALIDID then IsValidRequest:=true;
        //todo: modify given image
        AResponse.Contents.Add('<p>todo put/modify image '+inttostr(ImageID)+'</p>');
      end;
    end;
  end;
  if not(IsValidRequest) then
  begin
    AResponse.Code:=404;
    AResponse.CodeText:='Image not found.';
    AResponse.Contents.Add('<p>Image not found/invalid request</p>');
  end;
  Handled:=true;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/image/*
  RegisterHTTPModule('image', TFPWebimage);
end.

