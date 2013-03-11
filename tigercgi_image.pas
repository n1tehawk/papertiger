unit tigercgi_image;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tigerutil, tigerservercore, strutils, fpjson, jsonparser;

type

  { TFPWebimage }

  TFPWebimage = class(TFPWebModule)
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
  FPWebimage: TFPWebimage;

implementation

{$R *.lfm}

{ TFPWebimage }

procedure TFPWebimage.DataModuleCreate(Sender: TObject);
begin
  FTigerCore:=TTigerServerCore.Create;
end;

procedure TFPWebimage.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

procedure TFPWebimage.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
// We don't define any actions but handle the request at the module level before any actions would be evaluated.
{
Handled URLs/methods:
DELETE http://server/cgi-bin/tigercgi/image/               //delete all images?!?!
GET    http://server/cgi-bin/tigercgi/image/               //list of images
GET    http://server/cgi-bin/tigercgi/image/304            // get specific image
POST   http://server/cgi-bin/tigercgi/image/?documentid=55 // let server scan new image, return imageid
POST   http://server/cgi-bin/tigercgi/image/?documentid=55 // with image posted as form data: upload image, return imageid
DELETE http://server/cgi-bin/tigercgi/image/304            //remove image with id 304
GET    http://server/cgi-bin/tigercgi/image/304            //get image with id 304
PUT    http://server/cgi-bin/tigercgi/image/304            //edit image with id 304
}
var
  DocumentID: integer;
  ImageID: integer;
  InputJSON: TJSONObject;
  IsValidRequest: boolean;
  OutputJSON: TJSONObject;
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
        1: //http://server/cgi-bin/tigercgi/image/ either get list of images or all images
        begin
          IsValidRequest:=true;
          //todo: get every image
          AResponse.Contents.Add('<p>todo get all images</p>');
        end;
        2: //http://server/cgi-bin/tigercgi/image/304 get specific image
        begin
          ImageID:=StrToIntDef(ExtractWord(2,StrippedPath,['/']), INVALIDID);
          if ImageID<>INVALIDID then
          begin
            IsValidRequest:=true;
            //retrieve tiff and put in output stream
            AResponse.ContentStream := TMemoryStream.Create;
            try
              // Load tiff into content stream:
              //todo: replace this with image id => then add a call getimageid from document input documentid, order output imageid
              if FTigerCore.GetImage(DocumentID, 1, AResponse.ContentStream) then
              begin
                // Indicate papertiger should be able to deal with this data:
                AResponse.ContentType := 'image/tiff; application=papertiger';
                AResponse.ContentLength:=AResponse.ContentStream.Size; //apparently doesn't happen automatically?
                AResponse.SendContent;
              end
              else
              begin
                ISValidRequest:=false; //ask follow up code to return 404 error
              end;
            finally
              AResponse.ContentStream.Free;
            end;
          end;
        end;
      end;
    end;
    'POST':
    begin
      {
      POST   http://server/cgi-bin/tigercgi/image/?documentid=55 // let server scan new image, return imageid
      POST   http://server/cgi-bin/tigercgi/image/?documentid=55 // with image posted as form data: upload image, return imageid
      }
      // Note we don't allow empty images to be created: either scan or upload image
      if WordCount(StrippedPath,['/'])=1 then
      begin
        // Check if user wants to add image/scan to existing document, by a query field or...
        DocumentID:=INVALIDID;
        if (ARequest.QueryFields.Values['documentid']<>'') then
        begin
          DocumentID:=StrToIntDef(ARequest.QueryFields.Values['documentid'],INVALIDID);
          if DocumentID<>INVALIDID then
          begin
            // Scan
            ImageID:=FTigerCore.ScanSinglePage(invalidid); //todo fix this with proper document id
            if ImageID<>INVALIDID then
              IsValidRequest:=true;
          end;
        end;
        if DocumentID<>INVALIDID then
        begin
          // Check for uploaded image file
          //todo: figure out how to get resolution: it is encoded in the TIFF file; edentify bla.tif =>20130218144142.tif: TIFF 2472x3262 @ 300x300dpi (209x276mm) 1 bit, 1 channel
          // see e.g. http://stackoverflow.com/questions/7861600/get-horizontal-resolution-from-tif-in-c/7862187#7862187
          if ARequest.Files.Count>0 then
          begin
            ImageID:=FTigerCore.AddImage(ARequest.Files[0].Stream,ARequest.Files[0].FileName,DocumentID,-1);
            if ImageID<>INVALIDID then
              IsValidRequest:=true;
          end;
        end;
      end;
      if IsValidRequest then
      begin
        AResponse.ContentType := 'application/json';
        OutputJSON := TJSONObject.Create();
        try
          OutputJSON.Add('imageid',ImageID);
          AResponse.Contents.Add(OutputJSON.AsJSON);
        finally
          OutputJSON.Free;
        end;
      end;
    end;
    'PUT':
    begin
      //http://server/cgi-bin/tigercgi/image/304 modify this image/replace with new data
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

