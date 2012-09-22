program tigerserver;
{ Paper Tiger paper scanning/OCR/archiving solution

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
//todo:
- look at getting ocrfeeder (text mode) instead of the scan/ocr/pdf processes
http://git.gnome.org/browse/ocrfeeder
- run multiple scan engines, compare results. Differences should be marked for manual intervention
- get confidence output from tesseract (e.g. in the hocr)=>also assign manual intervention score
- run through dictionary (e.g. aspell -l en -c file.txt ...) and calculate confidence level there
- trigram analysis?!?!
- address (to, from), date detection in letters=>requires heuristics/processing text blocks (search term: data capture)
- if we can find out where logos are: extract embedded text using e.g. groundtruth
- recognize barcodes with zxing? exactimage tools?

=> if this is working, tell the guys at watchocr.com (they use cuneiform); perhaps they're interested
}

{$i tigerserver.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, scan, ocr, pdf, tigerdb, inifiles;

const
  // todo: create separate settings class
  SettingsFile = 'tigerserver.ini';

type

  { TTigerServer }

  TTigerServer = class(TCustomApplication)
  private
    FImageDirectory: string;
    //Directory where scanned images must be/are stored
    //todo: perhaps publish property?
    FPDFDirectory: string;
    //Directory where resulting PDFs must be stored
    FTigerDB: TTigerDB;
  protected
    procedure DoRun; override;
    // Main entry point into the program
    procedure ProcessImage(ImageFile: string; Resolution: integer);
    // Process existing (TIFF) image; should be named <image>.tif
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    procedure ScanAndProcess;
    // Scan a document and process it.
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTigerServer }

procedure TTigerServer.DoRun;
var
  ErrorMsg: String;
  Settings: TINIFile;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:s','help image: scan');
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FTigerDB:=TTigerDB.Create;
  if FileExists(SettingsFile) then
  begin
    Settings := TINIFile.Create(SettingsFile);
    try
      // When reading the settings, expand ~ to home directory etc
      FImageDirectory:=ExpandFileName(Settings.ReadString('General', 'ImageDirectory', '~/scans')); //Default to current directory
      FPDFDirectory:=ExpandFileName(Settings.ReadString('General', 'PDFDirectory', '~/pdfs')); //Default to current directory
    finally
      Settings.Free;
    end;
  end
  else
  begin
    // Set up defaults
    FImageDirectory:='';
  end;

  if FImageDirectory='' then FImageDirectory:=ExtractFilePath(SettingsFile);
  //Make sure there's a trailing / or \
  FImageDirectory:=IncludeLeadingPathDelimiter(FImageDirectory);

  if HasOption('i','image') then
  begin
    ProcessImage(GetOptionValue('i','image'),0);
  end;

  if HasOption('s','scan') then
  begin
    ScanAndProcess;
  end;

  if Assigned(FTigerDB) then
    FTigerDB.Free;
  // stop program loop
  Terminate;
end;

procedure TTigerServer.ProcessImage(ImageFile: string; Resolution: integer);
var
  HOCRFile: string;
  OCR: TOCR;
  PDF: TPDF;
begin
  {todo: add preprocess unit??! despeckle, deskew etc? ScanTailor?
  Scantailor: more for letters/documents; unpaper more for books
  scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
  unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}
  OCR:=TOCR.Create;
  try
    if ImageFile<>'' then
    begin
      OCR.ImageFile:=ImageFile;
      OCR.RecognizeText;
      HOCRFile:=OCR.HOCRFile;
      writeln('Got this text:');
      writeln(OCR.Text);
    end;
  finally
    OCR.Free;
  end;

  PDF:=TPDF.Create;
  try
    if ImageFile<>'' then
    begin
      // Only pass on overrides on resolution
      if Resolution>0 then
        PDF.ImageResolution:=Resolution;
      PDF.HOCRFile:=HOCRFile;
      PDF.ImageFile:=ImageFile;
      PDF.PDFFile:=IncludeTrailingPathDelimiter(FPDFDirectory)+ChangeFileExt(ExtractFileName(ImageFile),'.pdf');
      //todo: add metadata stuff to pdf unit
      //todo: add compression to pdf unit?
      PDF.CreatePDF;
      writeln('Got PDF:');
      writeln(PDF.PDFFile);
    end;

    // Add to database
    FTigerDB.InsertScan('test',ImageFile,'',Now);
  finally
    PDF.Free;
  end;
{
#adapted from http://ubuntuforums.org/showthread.php?t=1647350
in.info should contain (replace <>):
InfoKey: Author
InfoValue: <authorvalue>
InfoKey: Title
InfoValue: <title>
InfoKey: Creator
InfoValue: papertiger 20120722
# only when joining multiple pdf pages in single document:
# assumes individualpage1.pdf individualpage2.pdf etc
# could be done by pdftk as well..
pdfjoin --fitpaper --tidy --outfile "plainresult.pdf" "individualpage*.pdf"
#now remove individual pages files
#add info: - note: doc_data.txt probably generated by pdftk burst in original script which we don't use
#note: pdftk v1.44 and higher has update_info_utf8

pdftk "plainresult.pdf" update_info doc_data.txt output "someinfo.pdf"
pdftk "someinfo.pdf" update_info in.info output "scan.pdf"
#remove temp files: someinfo.pdf doc_data.txt in.info
rm -f "$1.ocr1.pdf" "$1.ocr2.pdf" doc_data.txt in.info
}

{look into compressing final result - lossless with:
qdf
http://qpdf.sourceforge.net/files/qpdf-manual.html
qdf --stream-data=compress input.pdf output.pdf
}

{ Look into pdftk
attach_files <filename> <filename> <...>
Can attach arbitrary files to PDF using PDF file attachment.
We could save some data here? If so, what?
}
end;

procedure TTigerServer.ScanAndProcess;
// Performs the document scan, and process result
var
  ImageFile: string;
  Resolution: integer;
  Scanner: TScanner;
begin
  // Try a 300dpi scan, probably best for normal sized letters on paper
  Resolution:=300;
  Scanner:=TScanner.Create;
  try
    Scanner.Resolution:=Resolution;
    Scanner.ColorType:=stLineArt;
    Scanner.FileName:=FImageDirectory+FormatDateTime('yyyymmddhhnnss', Now())+'.tif';
    Scanner.Scan;
    ImageFile:=Scanner.FileName;
    writeln('Image file: '+ImageFile);
    //todo: add teventlog logging support
  finally
    Scanner.Free;
  end;
  ProcessImage(ImageFile, Resolution);
end;

constructor TTigerServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTigerServer.Destroy;
begin
  inherited Destroy;
end;

procedure TTigerServer.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln('-i --image <image>');
  writeln(' Process image.');
  writeln('-s --scan: scan');
  writeln(' Scan document, process.');
end;

var
  Application: TTigerServer;
begin
  Application:=TTigerServer.Create(nil);
  Application.Run;
  Application.Free;
end.

