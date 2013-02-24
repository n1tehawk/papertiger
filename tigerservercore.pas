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
  scan, imagecleaner, ocr, pdf, fpjson, dateutils;

// Common constants etc:
{$i tigercommondefs.inc}

const
  ISO8601FullDateFormat = tigerdb.ISO8601FullDateFormat;

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
    // Adds new, empty document (with name if specified), returns document ID
    function AddDocument(DocumentName: string=''): integer;
    // Language to be used for OCR. Will not be saved in settings
    property CurrentOCRLanguage: string read FCurrentOCRLanguage write FCurrentOCRLanguage;
    // Image files to be OCRed or files that result from scanning
    property Images: TStringList read FImageFiles;
    // Number of pages to scan in one scan run.
    property Pages: integer read FPages write FPages;
    // Cleans up image (postprocessing): straightens them up, despeckles etc. Returns true if succesful
    function CleanImage(const ImageFile: string): boolean;
    // Delete document and associated images from DB and filesystem
    function DeleteDocument(const DocumentID: integer): boolean;
    // Get image identified by documentID and image number/imageorder (starting with 1)
    function GetImage(DocumentID, ImageOrder: integer; const ImageStream: TStream): boolean;
    // Get PDF identified by DocumentID
    function GetPDF(DocumentID: integer; const ImageStream: TStream): boolean;
    // Lists document specified by DocumentID or all documents (if DocumentID is INVALIDID)
    procedure ListDocuments(DocumentID: integer; var DocumentsArray: TJSONArray);
    // Process (set of) existing (TIFF) image(s); should be named <image>.tif
    // Images are specified using the Images property
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    // Returns resulting pdf file (including path)
    function ProcessImages(DocumentID: integer; Resolution: integer): string;
    // Scans a single page and adds it to an existing document.
    // Returns result status
    function ScanSinglePage(DocumentID: integer): boolean;
    // Returns server version, compile date, etc in one big string
    function ServerInfo: String;
    // Tries to parse full ISO8601 UTC datetime; returns datetime (1,1,0,0,0) if invalid
    class function TryParseDate(DateString: string; out ParseDate: TDateTime): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTigerServerCore }

// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}

function TTigerServerCore.AddDocument(DocumentName: string=''): integer;
begin
  result:=INVALIDID;
  try
    result:=FTigerDB.InsertDocument(DocumentName,'','',LocalTimeToUniversal(Now));
  except
    on E: Exception do
    begin
      TigerLog.WriteLog('AddDocument: error adding new document. '+E.Message);
    end;
  end;
end;

function TTigerServerCore.CleanImage(const ImageFile: string): boolean;
// Cleans up image before OCR (despeckle etc)
var
  Cleaner: TImageCleaner;
begin
  Result := false;
  Cleaner := TImageCleaner.Create;
  try
    //todo: write me
    result:=true;
    TigerLog.WriteLog(etInfo, 'CleanImage: not yet implemented. File argument passed: ' + ImageFile);
  finally
    Cleaner.Free;
  end;
end;

function TTigerServerCore.DeleteDocument(const DocumentID: integer): boolean;
begin
  //todo: get all images, delete from fs
  //todo: get pdf, delete from fs
  //todo: delete document and images from db
end;

function TTigerServerCore.GetImage(DocumentID, ImageOrder: integer; const ImageStream: TStream): boolean;
var
  ImageFile: string;
  MemStream: TMemoryStream;
begin
  result:=false;
  if DocumentID<>INVALIDID then
  begin
    ImageFile:=FTigerDB.ImagePath(DocumentID,ImageOrder);
    if ImageFile<>'' then
    begin
      try
        // Cater for different ImageDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers with different mountpoints
        if not(fileexists(ImageFile)) then
        begin
          TigerLog.WriteLog(etWarning,'GetImage: cannot read image "'+ImageFile+'". Hack: trying again with mangled ImageDirectory');
          ImageFile:=FSettings.ImageDirectory+ExtractFileName(ImageFile);
        end;
        MemStream:=TMemoryStream.Create;
        try
          MemStream.LoadFromFile(ImageFile);
          ImageStream.CopyFrom(MemStream,MemStream.Size);
        finally
          MemStream.Free;
        end;
        result:=true;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError,'GetImage: error trying to read image file '+ImageFile+'. Exception:'+E.Message);
        end;
      end;
    end;
  end;
end;

function TTigerServerCore.GetPDF(DocumentID: integer; const ImageStream: TStream
  ): boolean;
var
  PDFFile: string;
  MemStream: TMemoryStream;
begin
  result:=false;
  if DocumentID<>INVALIDID then
  begin
    PDFFile:=FTigerDB.GetPDFPath(DocumentID);
    if PDFFile<>'' then
    begin
      try
        // Cater for different PDFDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers
        if not(fileexists(PDFFile)) then
        begin
          TigerLog.WriteLog(etWarning,'GetPDF: cannot read PDF '+PDFFile+'. Hack: trying again with mangled PDFDirectory');
          PDFFile:=FSettings.PDFDirectory+ExtractFileName(PDFFile);
        end;
        MemStream:=TMemoryStream.Create;
        try
          MemStream.LoadFromFile(PDFFile);
          ImageStream.CopyFrom(MemStream,MemStream.Size);
        finally
          MemStream.Free;
        end;
        result:=true;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError,'GetPDF: error trying to read PDF file '+PDFFile+'. Exception:'+E.Message);
        end;
      end;
    end;
  end;
end;

procedure TTigerServerCore.ListDocuments(DocumentID: integer; var DocumentsArray: TJSONArray);
begin
  FTigerDB.ListDocuments(DocumentID,DocumentsArray);
end;

function TTigerServerCore.ProcessImages(DocumentID: integer; Resolution: integer): string;
var
  HOCRFile: string;
  i: integer;
  OCR: TOCR;
  PDF: TPDF;
  Success: boolean;
begin
  {todo: add preprocess code to cleanimage despeckle, deskew etc? ScanTailor?
  Scantailor: more for letters/documents; unpaper more for books
  scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
  unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}
  Result := '';
  if not (ForceDirectories(FSettings.PDFDirectory)) then
    raise Exception.CreateFmt('PDF directory %s does not exist and cannot be created.',[FSettings.PDFDirectory]);
  if DocumentID=INVALIDID then
    raise Exception.Create('ProcessImages: document ID must be valid. Please fix the program code.');
  //todo: add image if not already in db?
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
        if Success then
        begin
          TigerLog.WriteLog(etDebug,'ProcessImages: Got PDF: '+PDF.PDFFile);
          FTigerDB.SetPDFPath(DocumentID,PDF.PDFFile);
          Result := PDF.PDFFile;
        end;
        //todo: update pdf name based on OCR?!?
      finally
        PDF.Free;
      end;
    end;
  end; //all images added
  //todo: concatenate pdfs; we just add the last one for now
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


function TTigerServerCore.ScanSinglePage(DocumentID: integer): boolean;
var
  i: integer;
  Resolution: integer;
  Scanner: TScanner;
  ImageOrder: integer;
  StartDate: TDateTime;
  StartDateString: string;
begin
  Result := false; //fail by default
  FDocumentID := DocumentID; //Avoid processing old documents after failure

  // Try a 300dpi scan, probably best for normal sized letters on paper
  Resolution := 300;
  if not (ForceDirectories(FSettings.ImageDirectory)) then
    raise Exception.Create('Image directory ' + FSettings.ImageDirectory + ' does not exist and cannot be created.');
  Scanner := TScanner.Create;

  try
    Scanner.Resolution := Resolution;
    Scanner.ColorType := stLineArt;
    StartDate := Now(); //local time
    StartDateString := FormatDateTime('yyyymmddhhnnss', StartDate);

    TigerLog.WriteLog(etInfo, 'Going to scan single page; start date: ' + StartDateString);

    ImageOrder:=FTigerDB.GetHighestImageOrder(DocumentID)+1; //insert image after existing images
    Scanner.FileName := FSettings.ImageDirectory + StartDateString + '_' + format('%.4d', [ImageOrder]) + TESSERACTTIFFEXTENSION;
    if not (Scanner.Scan) then
      raise Exception.CreateFmt('TigerServerCore: an error occurred while scanning document %s', [Scanner.FileName]);
    TigerLog.WriteLog(etDebug, 'Image file: ' + Scanner.FileName);
    FImageFiles.Clear;
    FImageFiles.Add(Scanner.FileName); //We need to fill this for processimages

    TigerLog.WriteLog(etDebug, 'going to process single image) '+Scanner.FileName);
    if FDocumentID = INVALIDID then
    begin
      TigerLog.WriteLog(etError, 'ScanAndProcess: Error: could not insert document/scan into database. Please try again.');
    end
    else
    begin
      // Add images to database
      FTigerDB.InsertImage(FDocumentID, ImageOrder, FImageFiles[0], '');
      Result := true;
    end;
  finally
    Scanner.Free;
  end;
end;

function TTigerServerCore.ServerInfo: String;
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

class function TTigerServerCore.TryParseDate(DateString: string; out ParseDate: TDateTime): boolean;
// Try to detect ISO 8601 formatted UTC datetime. Note: only full datetime
// Returns false if not a date.
begin
  result:=false;
  try
    // Scandatetime won't work, so do it the old-fashioned way
    //2013-02-21T09:47:42.467Z
    //0        1         2
    //123456789012345678901234
    if (copy(DateString, 5, 1) = '-') and (copy(DateString, 8, 1) = '-') and
      (copy(DateString, 11, 1) = 'T') and (copy(DateString, 14, 1) = ':') and
      (copy(DateString, 17, 1) = ':') and (copy(DateString, 20, 1) = '.') and
      (copy(DateString, 24, 1) = 'Z') then
    begin
      ParseDate := UniversalTimeToLocal(EncodeDateTime(
        StrToInt(copy(DateString, 1, 4)), StrToInt(copy(DateString, 6, 2)),
        StrToInt(copy(DateString, 9, 2)), StrToInt(copy(DateString, 12, 2)),
        StrToInt(copy(DateString, 15, 2)), StrToInt(copy(DateString, 18, 2)),
        StrToInt(copy(DateString, 21, 3))));
      result:=true;
    end;
  except
    //ignore
  end;
end;


constructor TTigerServerCore.Create;

begin
  inherited Create;
  {$IFDEF DEBUG}
  TigerLog.WriteLog(etDebug, 'Starting TTigerServerCore');
  TigerLog.WriteLog(etDebug, Self.ServerInfo);
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
