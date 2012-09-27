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
- use unpaper/scantailor for deskewing/despeckling instead of device/driver dependent sane functionality
- orientation detection (e.g. upside down) with new tesseract (or by doing OCR on all orientations, picking highing confidence level)
- look at ocropus 0.6 which apparently has tesseract as line recognizer
- look at getting ocrfeeder (text mode) instead of the scan/ocr/pdf processes
http://git.gnome.org/browse/ocrfeeder
- run multiple scan engines, compare results. Differences should be marked for manual intervention
- run multiple scan engines, output text output of all of them to PDF to improve searching on keywords
- get confidence output from tesseract (e.g. in the hocr)=>also assign manual intervention score
- run through dictionary (e.g. aspell -l en -c file.txt ...) and calculate confidence level there?
- trigram analysis?!?!
- address (to, from), date detection in letters=>requires heuristics/processing text blocks (search term: data capture)
- if we can find out where logos are: extract embedded text using e.g. groundtruth =>
  this would perhaps mean an additional colour scan of part of the image
- recognize barcodes with zxing? exactimage tools?

=> if this is working, tell the guys at watchocr.com (they use cuneiform); perhaps they're interested
}

{$i tigerserver.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, scan, ocr, pdf, tigerdb, inifiles, imagecleaner;

const
  // todo: create separate settings class
  SettingsFile = 'tigerserver.ini';

type

  { TTigerServer }

  TTigerServer = class(TCustomApplication)
  private
    FImageDirectory: string;
    // Directory where scanned images must be/are stored
    // Has trailing path delimiter.
    //todo: perhaps publish property?
    FLanguage: string;
    // (Tesseract) language code, used to set OCR lanague for document.
    // Default en (for English)
    FPages: integer;
    // Number of pages to scan/process at once
    // Use >1 for batch (e.g. multipage documents)
    //todo: think about multipage tiff
    FPDFDirectory: string;
    // Directory where resulting PDFs must be stored
    // Has trailing path delimiter.
    FTigerDB: TTigerDB;
  protected
    function CleanImage(const ImageFile: string): boolean;
    // Cleans up image (postprocessing): straightens them up, despeckles etc
    procedure DoRun; override;
    // Main entry point into the program; processes command line options etc
    function ProcessImages(const ImageFiles: TStringList; Resolution: integer): string;
    // Process (set of) existing (TIFF) image(s); should be named <image>.tif
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    // Returns resulting pdf file/path
    procedure ScanAndProcess;
    // Scan a document (with one or more pages) and process it.
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTigerServer }

function TTigerServer.CleanImage(const ImageFile: String): boolean;
var
  Cleaner: TImageCleaner;
begin
  result:=false;
  Cleaner:=TImageCleaner.Create;
  try
    //todo: write me
    //result;=true;
  finally
    Cleaner.Free;
  end;
end;

procedure TTigerServer.DoRun;
var
  ErrorMsg: String;
  Images:TStringList;
  PDF: string;
  Settings: TINIFile;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:l:p:s','help image: language: pages: scan');
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

  if FileExists(SettingsFile) then
  begin
    Settings := TINIFile.Create(SettingsFile);
    try
      // When reading the settings, expand ~ to home directory etc
      FImageDirectory:=IncludeTrailingPathDelimiter(ExpandFileName(Settings.ReadString('General', 'ImageDirectory', '~/scans'))); //Default to current directory
      FPDFDirectory:=IncludeTrailingPathDelimiter(ExpandFileName(Settings.ReadString('General', 'PDFDirectory', '~/pdfs'))); //Default to current directory
    finally
      Settings.Free;
    end;
  end
  else
  begin
    // Set up defaults
    FImageDirectory:='';
    FPDFDirectory:='';
  end;

  // Fallback to directory where .ini file is stored
  if FImageDirectory='' then FImageDirectory:=IncludeTrailingPathDelimiter(ExtractFilePath(SettingsFile));
  if FPDFDirectory='' then FPDFDirectory:=FImageDirectory;

  if HasOption('l','language') then
  begin
    FLanguage:=GetOptionValue('l','language');
  end;

  if HasOption('p','pages') then
  begin
    FPages:=strtoint(GetOptionValue('p','pages'));
  end;

  // Branching off into processing starts here
  if HasOption('i','image') then
  begin
    Images:=TStringList.Create;
    try
      //todo: add support for ; or , separated image names when pages>1
      Images.Add(ExpandFileName(GetOptionValue('i','image')));
      PDF:=ProcessImages(Images,0);
      //todo: update images so they are part of the pdf
      //what to do with images that already belonged to another pdf?
      if PDF<>'' then
        FTigerDB.InsertDocument('fixmeimages',PDF,'',Now())
      else
        writeln('Error creating PDF. Stopping.');
    finally
      Images.Free;
    end;
  end;

  if HasOption('s','scan') then
  begin
    ScanAndProcess;
  end;

  // stop program loop
  Terminate;
end;

function TTigerServer.ProcessImages(const ImageFiles: TStringList; Resolution: integer):string;
var
  HOCRFile: string;
  i: integer;
  OCR: TOCR;
  PDF: TPDF;
  Success:boolean;
begin
  {todo: add preprocess unit??! despeckle, deskew etc? ScanTailor?
  Scantailor: more for letters/documents; unpaper more for books
  scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
  unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}
  result:='';
  if not(ForceDirectories(FPDFDirectory)) then
    raise Exception.Create('PDF directory '+FPDFDirectory+' does not exist and cannot be created.');
  for i:=0 to ImageFiles.Count-1 do
  begin
    Success:=CleanImage(ImageFiles[i]);
    if Success then
    begin
      OCR:=TOCR.Create;
      try
        OCR.ImageFile:=ImageFiles[i];
        OCR.Language:=FLanguage;
        Success:=OCR.RecognizeText;
        HOCRFile:=OCR.HOCRFile;
        writeln('Got this text:');
        writeln(OCR.Text);
      finally
        OCR.Free;
      end;
    end;

    if Success then
    begin
      PDF:=TPDF.Create;
      try
        // Only pass on overrides on resolution
        if Resolution>0 then
          PDF.ImageResolution:=Resolution;
        PDF.HOCRFile:=HOCRFile;
        PDF.ImageFile:=ImageFiles[i];
        writeln('pdfdirectory: '+FPDFDirectory);
        PDF.PDFFile:=IncludeTrailingPathDelimiter(FPDFDirectory)+
          ChangeFileExt(ExtractFileName(ImageFiles[i]),'.pdf');
        //todo: add metadata stuff to pdf unit
        //todo: add compression to pdf unit?
        Success:=PDF.CreatePDF;
        writeln('Got PDF:');
        writeln(PDF.PDFFile);
        result:=PDF.PDFFile;
      finally
        PDF.Free;
      end;
      //todo: concatenate pdfs; we just add the last one for now
      //todo: update pdf name
    end;
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
  DocumentID: integer;
  i:integer;
  ImageFiles: TStringList;
  PDF:string;
  Resolution: integer;
  Scanner: TScanner;
  StartDate: TDateTime;
  StartDateString: string;
begin
  // Try a 300dpi scan, probably best for normal sized letters on paper
  Resolution:=300;
  if not(ForceDirectories(FImageDirectory)) then
    raise Exception.Create('Image directory '+FImageDirectory+' does not exist and cannot be created.');
  Scanner:=TScanner.Create;
  ImageFiles:=TStringList.Create;
  try
    Scanner.Resolution:=Resolution;
    Scanner.ColorType:=stLineArt;
    StartDate:=Now();
    StartDateString:=FormatDateTime('yyyymmddhhnnss', StartDate);

    writeln('Going to scan '+inttostr(FPages)+' pages; start date: '+StartDateString);
    for i:=0 to FPages-1 do
    begin
      if FPages=1 then
        Scanner.FileName:=FImageDirectory+StartDateString+'.tif'
      else
        Scanner.FileName:=FImageDirectory+StartDateString+'_'+format('%.4d',[i])+'.tif';
      Scanner.Scan;
      writeln('Image file: '+Scanner.FileName);
      ImageFiles.Add(Scanner.FileName);
      if (i<FPages-1) then
      begin
        // todo: rebuild using event procedure so this can be plugged in (via web interface etc)
        writeln('Once the scan is completed, please put in sheet '+inttostr(i)+' and press enter to continue.');
        readln;
      end;
    end;

    //todo: add teventlog logging support
    writeln('going to process images');
    PDF:=ProcessImages(ImageFiles, Resolution);
    DocumentID:=FTigerDB.InsertDocument(StartDateString,PDF,'',StartDate);
    if DocumentID=DBINVALIDID then
    begin
      writeln('Error: could not insert document/scan into database. Please try again.');
    end
    else
    begin
      for i:=0 to FPages-1 do
      begin
        // Add to database
        FTigerDB.InsertImage(DocumentID,ImageFiles[i],'');
      end;
    end;
  finally
    Scanner.Free;
    ImageFiles.Free;
  end;
end;

constructor TTigerServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLanguage:='eng'; //Tesseract notation
  //todo: debug
  writeln('Debug: setting language to Dutch for testing. Remove me in production code.');
  FLanguage:='nld'; //Let's test with Dutch.
  FPages:=1; //Assume single scan, not batch
  StopOnException:=True;
  FTigerDB:=TTigerDB.Create;
end;

destructor TTigerServer.Destroy;
begin
  FTigerDB.Free;
  inherited Destroy;
end;

procedure TTigerServer.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln('-i <image> --image=<image>');
  writeln(' Process image.');
  writeln('-l <lang> --language=<language>');
  writeln(' Language to be used for OCR.');
  writeln(' eng (English) by default. See the OCR documentation for ');
  writeln(' language codes (e.g. man tesseract)');
  writeln('-s --scan');
  writeln(' Scan document, process.');
  writeln('-p <n> --pages=<n>');
  writeln(' Specify number of pages for processing/scanning multi page docs.');
end;

var
  Application: TTigerServer;
begin
  Application:=TTigerServer.Create(nil);
  Application.Run;
  Application.Free;
end.

