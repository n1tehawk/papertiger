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
  Classes, SysUtils;

type

  { TImageCleaner }

  TImageCleaner = class(TObject)
  private
    FImageFile: string;
    FScanDevice: string;
    // Returns degrees image needs to be turned to end right-side-up
    function DetectRotation: integer;
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
uses processutils, tigerutil;

// Common constants etc:
{$i tigercommondefs.inc}

{ TImageCleaner }

function TImageCleaner.DetectRotation: integer;
// For now, uses tesseract to do a couple of OCR runs as
// lineart, 300 dpi.
var
  Options: string;
  ScanDevicePart: string;
  ScanType: string;
begin
  //todo: first convert image to 300dpi, lineart if it isn't already
  Result := 0;
  if FScanDevice = '' then
    ScanDevicePart := ''
  else
    ScanDevicePart := '--device-name=' + FScanDevice;

  //todo: rotate in steps
  //todo: remove deskew, crop; replace by unpaper/scantailor
  Options := ' "' + FImageFile + '" ' + ScanDevicePart + ' --mode=Lineart --resolution=300' +
    ' --swdeskew=yes --swcrop=yes --format=tiff';
  TigerLog.WriteLog(etDebug, 'Executing: ' + ScanCommand + Options);
  try
    if ExecuteCommand(ScanCommand + Options, false) = 0 then
    begin
      TigerLog.WriteLog(etDebug, 'TImageCleaner.DetectRotation: Orientation by scan succeeded.');
      Result := true;
    end
    else
    begin
      TigerLog.WriteLog(etError, 'TImageCleaner.DetectRotation: error occurred running command: ' + ScanCommand + Options);
    end;
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etError, 'TImageCleaner.DetectRotation: exception (' + E.Message + ') occurred running command: ' + ScanCommand + Options);
    end;
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

constructor TImageCleaner.Create;
begin

end;

destructor TImageCleaner.Destroy;
begin
  inherited Destroy;
end;

end.
