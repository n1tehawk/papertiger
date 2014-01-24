unit imagecleaner;

{ Image cleaning unit; to be used to straighten up/deskew, despeckle etc images
  so OCR is more accurate.

  Copyright (c) 2012-2013 Reinier Olislagers

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

{todo: add preprocess code to CleanUpImage despeckle, deskew etc? ScanTailor?
Scantailor: more for letters/documents; unpaper more for books
scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}


{$i tigerserver.inc}
{$DEFINE USE_IMAGEMAGICK}
{.$DEFINE USE_EXACTIMAGE}

interface

uses
  Classes, SysUtils,
  processutils, strutils, tigerutil,
  ocr;

type

  { TImageCleaner }

  TImageCleaner = class(TObject)
  private
    FLanguage: string;
    // Tests page layout by running a scan.
    // Returns OCR recognition score (percentage: correct/total words) as well as the
    // approximate number of correctly-detected words found
    function CheckRecognition(ImageFile: string): integer;
    // Returns degrees image needs to be turned to end right-side-up
    function DetectRotation(Source: string): integer;
  public
    // Cleans up before scanning:
    // Converts image to black/white
    // Reads image, performs OCR tests on it to figure out if it needs to be rotated.
    // Rotates image if needed
    // Returns number of degrees the image has been turned,
    // e.g. 90: image rotated counterclockwise 90 degrees
    // Returns INVALIDID if function failed.
    function Clean(Source, Destination: string; AutoRotate: boolean): integer;
    // Rotates source image to destination image over specified number of degrees clockwise
    // Returns true if succesful
    function Rotate(Degrees: integer; SourceFile, DestinationFile: string): boolean;
    // Convert image to black and white TIFF image
    function ToBlackWhiteTIFF(SourceFile,DestinationFile: string): boolean;
    // Language to use for OCR, e.g. eng for English, nld for Dutch
    property Language: string read FLanguage write FLanguage;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

// Common constants etc:
{$i tigercommondefs.inc}

const
  {$IFDEF USE_EXACTIMAGE}
  ConvertCommand='econvert'; //exactimage's econvert utility
  {$ENDIF}
  {$IFDEF USE_IMAGEMAGICK}
  ConvertCommand='convert'; //Imagemagick's version
  {$ENDIF}
  NormalizeCommand='optimize2bw'; //exactimage's => black&white TIFF conversion tool

function TImageCleaner.CheckRecognition(ImageFile: string): integer;
{todo:  tesseract tries to output valid words in the selected language and
will often falsely detect numbers instead of gibberish when scanning rotated text.
Therefore remove all words containing only numbers before calculating statistics
}
const
  DetectLog = '/tmp/detectlog.txt';
var
  i, LinesRead: integer;
  Proc: TProcessEx;
  TempOCR: TOCR;
  ResList: TStringList;
  ResText: string;
begin
  result:=-1; //Negative recognition rate: fail by default
  TempOCR:=TOCR.Create;
  ResList:=TStringList.Create;
  try
    result:=0;
    TigerLog.WriteLog(etDebug,'CheckRecognition: going to call ocr for file '+ImageFile);
    TempOCR.ImageFile:=ImageFile;
    TempOCR.Language:=FLanguage;
    TempOCR.RecognizeText;

    // strip out all numbers - including gibberish misdetected as numbers
    ResText:=TempOCR.Text;
    for i:=length(ResText) downto 1 do
    begin
      if char(ResText[i]) in ['0'..'9'] then
        Delete(ResText,i,1);
    end;
    ResList.Text:=ResText;
    ResText:='';

    result:=WordCount((ResList.Text),StdWordDelims);
  finally
    TempOCR.Free;
    ResList.Free;
  end;
end;

{ TImageCleaner }
function TImageCleaner.DetectRotation(Source: string): integer;
const
  MinWords = 10; //Below this number, the image probably has no valid text
var
  DetectedRotation: integer=0;
  RotatedImage: string;
  Rotation: integer=0;
  Score: integer=0;
  TopScore: integer=0;
begin
  Result:=0;
  Rotation:=0;
  while Rotation <= 270 do
  begin
    if Rotation = 0 then
      RotatedImage:=Source
    else
    begin
      RotatedImage:=GetTempFileName('',inttostr(Rotation));
      Rotate(Rotation,Source,RotatedImage);
    end;
    Score:=CheckRecognition(RotatedImage);
    TigerLog.WriteLog(etDebug, 'File: '+RotatedImage+' rotation '+inttostr(Rotation)+' score '+inttostr(Score));
    {$IFNDEF DEBUG}
    DeleteFile(RotatedImage); //clean up
    {$ENDIF}

    if (Score>TopScore) and (Score>MinWords) then
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
  FLanguage:='eng'; //default to English; tesseract format
end;

destructor TImageCleaner.Destroy;
begin
  inherited Destroy;
end;

function TImageCleaner.ToBlackWhiteTIFF(SourceFile,DestinationFile: string): boolean;
var
  ErrorCode: integer;
  TempFile: string;
begin
  result:=false;
  if ExpandFileName(SourceFile)=ExpandFileName(DestinationFile) then
    TempFile:=GetTempFileName('','TIF')
  else
    TempFile:=DestinationFile;
  ErrorCode:=ExecuteCommand(NormalizeCommand+
    ' --denoise --dpi 300'+
    ' --input "'+SourceFile+'" --output "tiff:'+TempFile+'" ', false);
  if ErrorCode=0 then
  begin
    result:=true;
  end
  else
  begin
    TigerLog.WriteLog(etWarning,
      'ToBlackWhiteTIFF: got result code '+inttostr(ErrorCode)+
      ' when calling '+NormalizeCommand+' for image '+SourceFile);
  end;
  if (result) and (ExpandFileName(SourceFile)=ExpandFileName(DestinationFile)) then
  begin
    // Copy over original file as requested
    result:=FileCopy(TempFile,DestinationFile);
  end;
end;

function TImageCleaner.Rotate(Degrees: integer; SourceFile,
  DestinationFile: string): boolean;
// Rotates uses exactimage tools (econvert); imagemagick
// seems not to except certain file types and we need
// an exactimage dependency anyway for hocr2pdf
var
  ErrorCode: integer;
  Overwrite: boolean;
  TempFile: string;
begin
  result:=false;
  TigerLog.WriteLog(etDebug,
    'TImageCleaner.Rotate: going to rotate '+SourceFile+' to '+
    DestinationFile+' over '+inttostr(Degrees)+' degrees');

  Overwrite:=(ExpandFileName(SourceFile)=ExpandFileName(DestinationFile));
  if Overwrite then
    TempFile:=GetTempFileName('','TIFR')
  else
    TempFile:=DestinationFile;

  // We just let the tool do the 0 degree rotations, too.
  {$IFDEF USE_EXACTIMAGE}
  //todo: this just doesn't seem to rotate. Command line appears correct though.
  // perhaps bug in exactimage 0.8.8 which I ran.

  // Rotate; indicate output should be tiff format
  // Output appears to be CCIT fax T.6, but apparently tesseract 3.02.02
  // can now read that
  ErrorCode:=ExecuteCommand(ConvertCommand+
    ' --rotate "'+inttostr(Degrees)+'" '+
    ' --input "'+SourceFile+'" '+
    ' --output "tiff:'+TempFile+'" ', false);
  {$ENDIF}
  {$IFDEF USE_IMAGEMAGICK}
  // Rotate; indicate output should be tiff format
  // Output appears to be CCIT fax T.6, but apparently tesseract 3.02.02
  // can now read that
  ErrorCode:=ExecuteCommand(ConvertCommand+
    ' "'+SourceFile+'" '+
    ' -rotate '+inttostr(Degrees)+
    ' "'+TempFile+'" ', false);
  {$ENDIF}

  result:=(ErrorCode=0);
  if not(result) then
    TigerLog.WriteLog(etWarning,
      'TImageCleaner.Rotate: got result code '+inttostr(ErrorCode)+
      ' when calling '+ConvertCommand+' for rotation '+inttostr(Degrees))
  else
  if Overwrite then
  begin
    // Copy over original file as requested
    result:=FileCopy(TempFile,DestinationFile);
  end;
end;

function TImageCleaner.Clean(Source, Destination: string; AutoRotate: boolean): integer;
var
  TempImage: string;
  Degrees:integer=0;
begin
  Result:=INVALIDID;
  TempImage:=GetTempFileName('','BW');
  ToBlackWhiteTIFF(Source,TempImage);
  if AutoRotate then
  begin
    Degrees:=DetectRotation(TempImage);
    if Rotate(Degrees,TempImage,Destination) then
      result:=Degrees;
  end
  else
  begin
    FileCopy(TempImage,Destination);
    result:=0;
  end;
  {$IFNDEF DEBUG}
  DeleteFile(TempImage);
  {$ENDIF}
end;

end.
