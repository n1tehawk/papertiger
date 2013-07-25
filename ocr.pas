unit ocr;

{ Optical character recognitions functionality

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

{
This needs Tesseract version 3 or higher for hocr output.
//todo: test for that version

cuneiform alternative: cuneiform -l <language> -o hocr.html imagefile.bmp
}

{$i tigerserver.inc}


interface

uses
  Classes, SysUtils, tigerutil;

type
  // Orientation of scanned image versus the "right side up".
  // E.g. TurnedAntiClock means the scanned image should be rotated 90 degrees
  // clockwise to get the correct orientation
  Orientation=(orNormal,orUpsideDown,orTurnedClock,orTurnedAntiClock,orUnknown);

  { TOCR }

  TOCR = class(TObject)
  private
    FHOCRFile: string;
    FImageFile: string;
    FLanguage: string;
    FOrientation: Orientation;
    //todo: unicode??
    FText: string;
  public
    // Perform the actual OCR
    function RecognizeText: boolean;
    // Scan results file containing
    // hOCR (position and text in image)
    property HOCRFile: string read FHOCRFile;
    // Input image
    property ImageFile: string write FImageFile;
    // Language to use for OCR, e.g. eng for English, nld for Dutch
    property Language: string read FLanguage write FLanguage;
    // Rotation (if any) of scanned image versus reality
    property Orientation: Orientation read FOrientation write FOrientation;
    // Text recognized in image
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses processutils;

{ TOCR }

function TOCR.RecognizeText: boolean;
const
  OCRCommand='tesseract';
var
  Command: string;
  HOCRResult:integer;
  OutputFile:string;
  Results:TStringList;
  TessResult:integer;
begin
  result:=false;

  if not(FileExists(FImageFile)) then
  begin
    TigerLog.WriteLog(etError,'TOCR.RecognizeText: input image file '+FImageFile+' does not exist. Aborting.');
    exit;
  end;

  OutputFile:=GetTempFileName;
  FHOCRFile:='';
  FText:='';

  // tesseract sticks results in outputfile+.txt
  // Plain text detection
  Command:=OCRCommand+' "'+FImageFile+'" "'+OutputFile+'" -l '+FLanguage;
  TigerLog.WriteLog(etDebug,'TOCR.RecognizeText: going to run '+Command);
  TessResult:=ExecuteCommand(Command,false);
  if TessResult=0 then
  begin
    Results:=TStringList.Create;
    try
      // By now we should have scan.tif, scan.txt and scan.html
      Results.LoadFromFile(OutputFile+'.txt');
      FText:=Results.Text;
      // No more use for text output
      Sysutils.DeleteFile(OutputFile+'.txt');
    finally
      Results.Free;
    end;

    // Output position & word text in hocr format:
    Command:=OCRCommand+' "'+FImageFile+'" "'+OutputFile+'" -l '+FLanguage+' hocr';
    HOCRResult:=ExecuteCommand(Command,false);
    case HOCRResult of
      0:
      begin
        FHOCRFile:=OutputFile+'.html';
        TigerLog.WriteLog(etDebug,'Result: hocr done: '+FHOCRFile);
        result:=true;
      end;
      1:  //OCR error, need not be a problem
      begin
        FHOCRFile:=OutputFile+'.html';
        TigerLog.WriteLog(etDebug,'Result: hocr found no text for: '+FHOCRFile);
        result:=true;
      end;
      else
        begin
          TigerLog.WriteLog(etError,'RecognizeText: Error generating hocr. Result code: '+inttostr(HOCRResult)+LineEnding+
            'Command given was: '+Command);
        end;
    end;
  end
  else
  begin
    TigerLog.WriteLog(etError,'RecognizeText: Error performing tesseract OCR. Result code: '+inttostr(TessResult)+LineEnding+
      'Command given was: '+Command);
  end;
end;

constructor TOCR.Create;
begin
  inherited Create;
  FHOCRFile:='';
  FLanguage:='eng'; //default to English; tesseract format
  FOrientation:=orUnknown;
end;

destructor TOCR.Destroy;
begin
  inherited Destroy;
end;

end.

