unit imagecleaner;

{ Image cleaning unit; to be used to straighten up/deskew, despeckle etc images
  so OCR is more accurate.

  Copyright (c) 2012 Reinier Olislagers

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
  Classes, SysUtils,
  processutils, tigerutil,
  magick_wand, ImageMagick, fpimage;

type

  { TImageCleaner }

  TImageCleaner = class(TObject)
  private
    FImageFile: string;
    FScanDevice: string;
    // Tests page layout by running a scan. Returns OCR recognition score
    function CheckRecognition(ImageFile: string): integer;
    // Returns degrees image needs to be turned to end right-side-up
    function DetectRotation: integer;
    // Rotates source image to destination image over specified number of degrees clockwise
    // Returns true if succesful
    function Rotate(Degrees: integer; SourceFile, DestinationFile: string): boolean;
  public
    // Reads image, performs OCR tests on it to figure out if it needs to be rotated.
    // Rotates image if needed
    // Returns number of degrees the image has been turned,
    // e.g. -90: image rotated counterclockwise 90 degrees
    function DetectApplyRotation: integer;
    // Input image
    property ImageFile: string write FImageFile;
    // Device to be used to scan with (used for orientation detection).
    // Same as the property in scan
    property ScanDevice: string read FScanDevice write FScanDevice;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

// Common constants etc:
{$i tigercommondefs.inc}

procedure ThrowWandException(wand: PMagickWand);
var
  description: PChar;
  severity: ExceptionType;
begin
  description := MagickGetException(wand, @severity);
  TigerLog.WriteLog(etError,Format('An error ocurred. Description: %s', [description]));
  description := MagickRelinquishMemory(description);
  Abort;
end;

function TImageCleaner.CheckRecognition(ImageFile: string): integer;
var
  Options: string;
  ScanDevicePart: string;
begin
  if FScanDevice = '' then
    ScanDevicePart := ''
  else
    ScanDevicePart := '--device-name=' + FScanDevice;

  //todo: remove deskew, crop; replace by unpaper/scantailor
  Options := ' "' + ImageFile + '" ' + ScanDevicePart + ' --mode=Lineart --resolution=300' +
    ' --swdeskew=yes --swcrop=yes --format=tiff';
  TigerLog.WriteLog(etDebug, 'Executing: ' + ScanCommand + Options);
  try
    if ExecuteCommand(ScanCommand + Options, false) = 0 then
    begin
      TigerLog.WriteLog(etDebug, 'TImageCleaner.DetectRotation: Orientation by scan succeeded.');
      Result := 10; //todo: fill in with actual score
    end
    else
    begin
      Result := -1;
      TigerLog.WriteLog(etError, 'TImageCleaner.DetectRotation: error occurred running command: ' + ScanCommand + Options);
    end;
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etError, 'TImageCleaner.DetectRotation: exception (' + E.Message + ') occurred running command: ' + ScanCommand + Options);
    end;
  end;

  // Now open the hocr file to check effectiveness

end;

{ TImageCleaner }
function TImageCleaner.DetectRotation: integer;
// For now, uses tesseract to do a couple of OCR runs as
// lineart, 300 dpi.
var
  DetectedRotation: integer;
  RotatedImage: string;
  Options: string;
  Rotation: integer;
  Score: integer;
  TopScore: integer;
begin
  //todo: first convert image to 300dpi, lineart if it isn't already
  Result := 0;

  Rotation:=0;
  Score:=CheckRecognition(FImageFile);
  if Score>TopScore then
  begin
    TopScore:=Score;
    DetectedRotation:=Rotation;
  end;
  Rotation:=90;
  while Rotation <= 270 do
  begin
    RotatedImage:=GetTempFileName('',inttostr(Rotation));
    Rotate(Rotation,FImageFile, RotatedImage);
    Score:=CheckRecognition(RotatedImage);
    if Score>TopScore then
    begin
      TopScore:=Score;
      DetectedRotation:=Rotation;
    end;
    Rotation:=Rotation + 90;
  end;
  result := DetectedRotation;
end;

constructor TImageCleaner.Create;
begin

end;

destructor TImageCleaner.Destroy;
begin
  inherited Destroy;
end;

function TImageCleaner.Rotate(Degrees: integer; SourceFile,
  DestinationFile: string): boolean;
var
  status: MagickBooleanType;
  wand: PMagickWand;
begin
  wand := NewMagickWand;
  try
    status := MagickReadImage(wand, PChar(SourceFile));
    if (status = MagickFalse) then ThrowWandException(wand);
    MagickRotateImage(wand,nil,Degrees);
    if (status = MagickFalse) then ThrowWandException(wand);
    status := MagickWriteImages(wand, PChar(DestinationFile), MagickTrue);
    if (status = MagickFalse) then ThrowWandException(wand);
    result := not(status=MagickFalse);
  finally
    wand := DestroyMagickWand(wand);
    MagickWandTerminus;
  end;
end;

function TImageCleaner.DetectApplyRotation: integer;
var
  Degrees:integer;
begin
  Degrees:=DetectRotation;
  try
    //todo: actual turning
  except
    Degrees:=0;
    //error message
  end;
  result:=Degrees;

end;

end.
