unit tigerservercore;

{ Core functionality for the server part of papertiger.
  Shared by web/CGI, command line etc frontends.

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


interface

uses
  Classes, SysUtils,
  {$IFDEF DEBUG}
    {$IFDEF CGI}
      {$IFDEF RUNDEBUGGERWITHCGI}
  selfdebug, //only useful when running the CGI/web module code
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  tigerutil {put this first for logging support},
  tigerdb, tigersettings,
  scan, imagecleaner, ocr, pdf, fpjson;

const
  INVALIDID = DBINVALIDID; //Used to indicate document ID etc is invalid.

type

  { TTigerServerCore }

  TTigerServerCore = class(TObject)
  private
    FCurrentOCRLanguage: string; //effective language (from settings file, possibly overridden by e.g. command-line options)
    FDocumentID: integer; //database ID for currently handled scanned document
    FImageFiles: TStringList; //current image(s) being processed: result of scan or input for OCR
    FPages: integer;
    // Number of pages to scan/process at once
    // Use >1 for batch (e.g. multipage documents)
    //todo: think about multipage tiff
    FSettings: TTigerSettings;
    FTigerDB: TTigerDB;
  protected
  public
    // Language to be used for OCR. Will not be saved in settings
    property CurrentOCRLanguage: string read FCurrentOCRLanguage write FCurrentOCRLanguage;
    // Image files to be OCRed or files that result from scanning
    property Images: TStringList read FImageFiles;
    // Number of pages to scan in one scan run.
    property Pages: integer read FPages write FPages;
    // Cleans up image (postprocessing): straightens them up, despeckles etc. Returns true if succesful
    function CleanImage(const ImageFile: string): boolean;
    // Lists document specified by DocumentID or all documents (if DocumentID is INVALIDID)
    procedure ListDocuments(DocumentID: integer; var DocumentArray: TJSONArray);
    // Process (set of) existing (TIFF) image(s); should be named <image>.tif
    // Images are specified using the Images property
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    // Returns resulting pdf file (including path)
    function ProcessImages(DocumentName: string; Resolution: integer): string;
    // Scan a document (with one or more pages) and process it.
    // Returns document ID if succesful; <=0 if not.
    function ScanAndProcess: integer;
    // Returns server version, compile date, etc in one big string
    function ServerInfo: String;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTigerServerCore }

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}

function TTigerServerCore.CleanImage(const ImageFile: string): boolean;
var
  Cleaner: TImageCleaner;
begin
  Result := false;
  Cleaner := TImageCleaner.Create;
  try
    //todo: write me
    result:=true;
    TigerLog.WriteLog(etInfo, 'CleanImage: not yet implemented. File argument Passed: ' + ImageFile);
  finally
    Cleaner.Free;
  end;
end;

procedure TTigerServerCore.ListDocuments(DocumentID: integer; var DocumentArray: TJSONArray);
begin
  FTigerDB.ListDocuments(DocumentID,DocumentArray);
end;

function TTigerServerCore.ProcessImages(DocumentName: string; Resolution: integer): string;
var
  HOCRFile: string;
  i: integer;
  OCR: TOCR;
  PDF: TPDF;
  Success: boolean;
begin
  {todo: add preprocess unit??! despeckle, deskew etc? ScanTailor?
  Scantailor: more for letters/documents; unpaper more for books
  scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
  unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}
  Result := '';
  if not (ForceDirectories(FSettings.PDFDirectory)) then
    raise Exception.CreateFmt('PDF directory %s does not exist and cannot be created.',[FSettings.PDFDirectory]);
  if DocumentName='' then
    raise Exception.Create('ProcessImages: cannot accept empty DocumentName. Please fix the program code.');
  for i := 0 to FImageFiles.Count - 1 do
  begin
    Success := CleanImage(FImageFiles[i]);
    if Success then
    begin
      OCR := TOCR.Create;
      try
        OCR.ImageFile := FImageFiles[i];
        OCR.Language := FCurrentOCRLanguage;
        Success := OCR.RecognizeText;
        HOCRFile := OCR.HOCRFile;
        TigerLog.WriteLog(etDebug,'ProcessImages: Got this text:'+OCR.Text);
      finally
        OCR.Free;
      end;
    end;

    if Success then
    begin
      PDF := TPDF.Create;
      try
        // Only pass on overrides on resolution
        if Resolution > 0 then
          PDF.ImageResolution := Resolution;
        PDF.HOCRFile := HOCRFile;
        PDF.ImageFile := FImageFiles[i];
        TigerLog.WriteLog(etDebug,'pdfdirectory: ' + FSettings.PDFDirectory);
        PDF.PDFFile := IncludeTrailingPathDelimiter(FSettings.PDFDirectory) + ChangeFileExt(ExtractFileName(FImageFiles[i]), '.pdf');
        //todo: add metadata stuff to pdf unit
        //todo: add compression to pdf unit?
        Success := PDF.CreatePDF;
        TigerLog.WriteLog(etDebug,'Got PDF:'+PDF.PDFFile);
        Result := PDF.PDFFile;
      finally
        PDF.Free;
      end;
      //todo: concatenate pdfs; we just add the last one for now
      //todo: update pdf name
      // Insert result into database:
      if Result <> '' then
      begin
        {todo: don't use now but get timestamp from oldest image and use that as scandate??? Or leave like this as
         the scan command has actually been issued now}
        FDocumentID := FTigerDB.InsertDocument(DocumentName, Result, '', Now);
        // todo: next call db update or insert images here to make sure images assigned to proper document
      end
      else
        FDocumentID := DBINVALIDID; //invalidate any previously valid document ID
    end;
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

function TTigerServerCore.ScanAndProcess: integer;
  // Performs the document scan, and process result
var
  i: integer;
  Resolution: integer;
  Scanner: TScanner;
  StartDate: TDateTime;
  StartDateString: string;
begin
  Result := INVALIDID; //fail by default
  FDocumentID := DBINVALIDID; //Avoid processing old documents after failure

  // Try a 300dpi scan, probably best for normal sized letters on paper
  Resolution := 300;
  if not (ForceDirectories(FSettings.ImageDirectory)) then
    raise Exception.Create('Image directory ' + FSettings.ImageDirectory + ' does not exist and cannot be created.');
  Scanner := TScanner.Create;

  try
    Scanner.Resolution := Resolution;
    Scanner.ColorType := stLineArt;
    StartDate := Now();
    StartDateString := FormatDateTime('yyyymmddhhnnss', StartDate);

    TigerLog.WriteLog(etInfo, 'Going to scan ' + IntToStr(FPages) + ' pages; start date: ' + StartDateString);
    for i := 0 to FPages - 1 do
    begin
      if FPages = 1 then
        Scanner.FileName := FSettings.ImageDirectory + StartDateString + '.tif'
      else
        Scanner.FileName := FSettings.ImageDirectory + StartDateString + '_' + format('%.4d', [i]) + '.tif';
      if not (Scanner.Scan) then
        raise Exception.CreateFmt('TigerServerCore: an error occurred while scanning document %s', [Scanner.FileName]);
      TigerLog.WriteLog(etDebug, 'Image file: ' + Scanner.FileName);
      FImageFiles.Clear;
      FImageFiles.Add(Scanner.FileName);
      if (i < FPages - 1) then
      begin
        // todo: rebuild using event procedure so this can be plugged in (via web interface etc)
        // Ask for page after current page:
        //todo: do this with a callback!?!?!
        {$IFNDEF CGI}
        writeln('Once the scan is completed, please put in sheet ' + IntToStr(i + 2) + ' and press enter to continue.');
        readln;
        {$ELSE}
        //to do: implement this!
        TigerLog.WriteLog(etError, 'to do: please implement multipage scan support!');
        {$ENDIF}
      end;
    end;

    TigerLog.WriteLog(etDebug, 'going to process image(s): '+inttostr(FImageFiles.Count));
    ProcessImages(StartDateString, Resolution);
    if FDocumentID = DBINVALIDID then
    begin
      TigerLog.WriteLog(etError, 'ScanAndProcess: Error: could not insert document/scan into database. Please try again.');
    end
    else
    begin
      for i := 0 to FPages - 1 do
      begin
        // Add images to database
        FTigerDB.InsertImage(FDocumentID, FImageFiles[i], '');
      end;
      Result := FDocumentID;
    end;
  finally
    Scanner.Free;
  end;
end;

function TTigerServerCore.ServerInfo: string;
begin
  result:=
  'Papertiger ' + LineEnding + 'version: based on commit ' + RevisionStr + ' (' + versiondate + ')' + LineEnding + 'build date: ' +
{$INCLUDE %DATE%}
    +' ' +
{$INCLUDE %TIME%}
    +LineEnding + 'Compiled for CPU: ' + lowercase(
{$INCLUDE %FPCTARGETCPU%}
    ) + ' on ' + lowercase(
{$INCLUDE %FPCTARGETOS%}
    );
end;

constructor TTigerServerCore.Create;

begin
  inherited Create;
  {$IFDEF DEBUG}
  TigerLog.WriteLog(etDebug, 'Starting TTigerServerCore');
  {$ENDIF}
  FSettings := TTigerSettings.Create;
  FCurrentOCRLanguage := FSettings.Language; //read language from settings; can be overridden by command line optoin
  FImageFiles := TStringList.Create;
  FPages := 1; //Assume single scan, not batch
  FTigerDB := TTigerDB.Create;
end;

destructor TTigerServerCore.Destroy;
begin
  FImageFiles.Free;
  FTigerDB.Free;
  FSettings.Free;
  {$IFDEF DEBUG}
  TigerLog.WriteLog(etDebug, 'Stopping TTigerServerCore');
  {$ENDIF}
  inherited Destroy;
end;

end.
