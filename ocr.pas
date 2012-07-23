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

{$i tigerserver.inc}


interface

uses
  Classes, SysUtils;

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
    procedure RecognizeText;
    // File with hOCR: position and text in image
    property HOCRFile: string read FHOCRFile;
    // Input image
    property ImageFile: string write FImageFile;
    property Language: string read FLanguage write FLanguage;
    // Rotation (if any) of scanned image versus reality
    property Orientation: Orientation read FOrientation write FOrientation;
    // Text recognized in image
    //todo: add outputfile property?!?!!
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses processutils;

{ TOCR }

procedure TOCR.RecognizeText;
const
  OCRCommand='tesseract';
var
  OutputFile:string;
  Results:TStringList;
begin
  if FLanguage='' then FLanguage:='en'; //Default to English
  OutputFile:=GetTempFileName;
  FText:='';
  FHOCRFile:='';
  //todo: upside down detection? OCR 2x, higher detection rate is right orientation
  //=> we can use tesseract 3 -pam parameter perhaps?
  // tesseract sticks results in outputfile+.txt
  if ExecuteCommand(OCRCommand+' "'+FImageFile+'" "'+OutputFile+'" -l '+FLanguage,false)=0 then
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
    if ExecuteCommand(OCRCommand+' "'+FImageFile+'" "'+OutputFile+'" -l '+FLanguage+' hocr',false)=0 then
    begin
      FHOCRFile:=OutputFile+'.html';
      writeln('Result: hocr done: '+FHOCRFile);
    end
    else
    begin
      writeln('Error generating hocr.');
    end;
  end
  else
  begin
    writeln('Error performing tesseract OCR.');
  end;
end;

constructor TOCR.Create;
begin
  inherited Create;
  FHOCRFile:='';
  FOrientation:=orUnknown;
  FLanguage:='nld'; //Let's test with Dutch.
end;

destructor TOCR.Destroy;
begin
  inherited Destroy;
end;

end.

