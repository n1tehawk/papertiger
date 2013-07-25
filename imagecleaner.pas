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



{$i tigerserver.inc}
{$DEFINE USE_IMAGEMAGICK}
{.$DEFINE USE_EXACTIMAGE}

interface

uses
  Classes, SysUtils,
  processutils, tigerutil,
  ocr;

type

  { TImageCleaner }

  TImageCleaner = class(TObject)
  private
    FLanguage: string;
    // Tests page layout by running a scan.
    // Returns OCR recognition score (percentage: correct/total words) as well as the
    // approximate number of correctly-spelled words found
    function CheckRecognition(ImageFile: string; var CorrectWords: integer): integer;
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

function TImageCleaner.CheckRecognition(ImageFile: string; var CorrectWords: integer): integer;
const
  DetectLog = '/tmp/detectlog.txt';
var
  i, LinesRead: integer;
  ImageTextFile: string;
  Proc: TProcessEx;
  TempOCR: TOCR;
  ResList: TStringList;
  WordsTotal, WordsWrong: integer;
begin
  result:=-1; //Negative(!) recognition rate: fail by default
  TempOCR:=TOCR.Create;
  ResList:=TStringList.Create;
  try
    TigerLog.WriteLog(etDebug,'CheckRecognition: going to call ocr for file '+ImageFile);
    TempOCR.ImageFile:=ImageFile;
    TempOCR.Language:=FLanguage;
    TempOCR.RecognizeText;

    // Now run a spell check and open the result text file to check effectiveness
    ImageTextFile:=GetTempFileName('','OCT');
    ResList.Clear;
    ResList.Add(TempOCR.Text);
    ResList.SaveToFile(ImageTextFile);
    ResList.Clear;
    {$IFNDEF DEBUG}
    DeleteFile(ImageTextFile);
    {$ENDIF}

    Proc:=TProcessEx.Create(nil);
    try
      try
        Proc.CommandLine:=TextDetectCommand+' "'+ImageTextFile+'"';
        //todo: temporary solution pending lookup table
        // Convert tesseract style language codes to LANG variables
        case FLanguage of
          'eng': Proc.Environment.SetVar('LANG','en_US.UTF-8');
          'fra': Proc.Environment.SetVar('LANG','fr_FR.UTF-8');
          'nld': Proc.Environment.SetVar('LANG','nl_NL.UTF-8');
          else
          begin
            Proc.Environment.SetVar('LANG','en_US.UTF-8');
            TigerLog.WriteLog(etWarning,'TImageCleaner.CheckRecognition: Unknown detction language '+FLanguage+'; defaulting to English. Detection scores may be invalid.');
          end;
        end;
        Proc.Execute;
        if (Proc.ExitStatus=0) then
          result:=0
        else
          result:=-1;
      except
        result:=-1;
      end;
    finally
      Proc.Free;
    end;

    if result=0 then
    begin
      // hardcoded results in /tmp/detectlog.txt
      ResList.LoadFromFile(DetectLog);
      {$IFDEF DEBUG}
      SysUtils.RenameFile(DetectLog,DetectLog+'_'+ImageFile);
      {$ELSE}
      sysutils.DeleteFile(DetectLog);
      {$ENDIF}
      LinesRead:=0;
      if ResList.Count=0 then
      begin
        TigerLog.WriteLog(etError,'TImageCleaner.CheckRecognition: error running spell check recognition for image '+ImageFile+': empty file '+DetectLog);
        exit;
      end;

      for i:=0 to ResList.Count-1 do
      begin
        // Ignore comments starting with #
        if pos('#',trim(Reslist[i]))<>1 then
        begin
          case LinesRead of
          0:
            begin
              // Total wordcount
              WordsTotal:=strtointdef(Trim(ResList[i]),-1);
              LinesRead:=LinesRead+1;
            end;
          1:
            begin
              // Number of spelling errors/incorrectly spelled words
              try
                WordsWrong:=strtointdef(Trim(ResList[i]),0);
                CorrectWords:=WordsTotal-WordsWrong;
                if CorrectWords<0 then CorrectWords:=0;
                TigerLog.WriteLog(etDebug,'TImageCleaner.CheckRecognition: found '+
                  inttostr(WordsTotal)+' total words: '+
                  inttostr(CorrectWords)+' correct; '+
                  inttostr(WordsWrong)+' wrong.');
                Result:=Round(100*CorrectWords/WordsTotal);
              except
                // keep result at -1
              end;
              LinesRead:=LinesRead+1;
            end;
          else
            begin
              TigerLog.WriteLog(etWarning,'TImageCleaner.CheckRecognition: unknown detect line: '+ResList[i]);
            end;
          end;
        end;
      end;
    end
    else
    begin
      TigerLog.WriteLog(etError,'TImageCleaner.CheckRecognition: '+TextDetectCommand+' exited with non-zero result code.');
    end;
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
  CorrectWords: integer=0;
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
    Score:=CheckRecognition(RotatedImage,CorrectWords);
    TigerLog.WriteLog(etDebug, 'File: '+RotatedImage+' rotation '+inttostr(Rotation)+' score '+inttostr(Score)+' %; correct words: '+inttostr(CorrectWOrds));
    {$IFNDEF DEBUG}
    DeleteFile(RotatedImage); //clean up
    {$ENDIF}

    if (Score>TopScore) and (CorrectWords>MinWords) then
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
    DeleteFile(DestinationFile);
    result:=Sysutils.RenameFile(TempFile,DestinationFile);
  end;
end;

function TImageCleaner.Rotate(Degrees: integer; SourceFile,
  DestinationFile: string): boolean;
// Rotates uses exactimage tools (econvert); imagemagick
// seems not to except certain file types and we need
// an exactimage dependency anyway for hocr2pdf
var
  ErrorCode: integer;
  TempFile: string;
begin
  result:=false;
  TigerLog.WriteLog(etDebug,
    'TImageCleaner.Rotate: going to rotate '+SourceFile+' to '+
    DestinationFile+' over '+inttostr(Degrees)+' degrees');

  if ExpandFileName(SourceFile)=ExpandFileName(DestinationFile) then
    TempFile:=GetTempFileName('','TIFR')
  else
    TempFile:=DestinationFile;

  // We just let the tool do the 0 degree rotations, too.
  {$IFDEF USE_EXACTIMAGE}
  //todo: this just doesn't seem to rotate. Command line appears correct
  // perhaps bug in exactimage 0.8.8 which I ran.

  // Rotate; indicate output should be tiff format
  // Output appears to be CCIT fax T.6, but apparently tesseract 3.02.02
  // can now read that
  ErrorCode:=ExecuteCommand(ConvertCommand+
    ' --rotate '+inttostr(Degrees)+
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
  if ErrorCode=0 then
  begin
    result:=true;
  end
  else
  begin
    TigerLog.WriteLog(etWarning,
      'TImageCleaner.Rotate: got result code '+inttostr(ErrorCode)+
      ' when calling '+ConvertCommand+' for rotation '+inttostr(Degrees));
  end;
  if (result) and (ExpandFileName(SourceFile)=ExpandFileName(DestinationFile)) then
  begin
    // Copy over original file as requested
    DeleteFile(DestinationFile);
    result:=Sysutils.RenameFile(TempFile,DestinationFile);
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
    result:=0;
  end;
  {$IFNDEF DEBUG}
  DeleteFile(TempImage);
  {$ENDIF}
end;

end.
