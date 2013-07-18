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
  scan, imagecleaner, ocr, pdf,
  fpjson, dateutils, md5;

// Common constants etc:
{$i tigercommondefs.inc}

const
  ISO8601FullDateFormat = tigerdb.ISO8601FullDateFormat;

type

  { TTigerServerCore }

  TTigerServerCore = class(TObject)
  private
    FCurrentOCRLanguage: string;
    //effective language (from settings file, possibly overridden by e.g. command-line options)
    FPages: integer;
    FScanDevice: string;
    // Number of pages to scan/process at once
    // Use >1 for batch (e.g. multipage documents)
    //todo: think about multipage tiff
    FSettings: TTigerSettings;
    FTigerDB: TTigerDB;
  protected
  public
    // Adds new, empty document (with name if specified), returns document ID
    function AddDocument(DocumentName: string = ''): integer;
    // Adds the tiff image to given documentID. Add at end of any existing images, unless ImageOrder>0. Returns image ID or INVALIDID when failed.
    function AddImage(ImageData: TStream; ImageName: string;
      DocumentID: integer; ImageOrder: integer): integer;
    // Adds the tiff image to given documentID. Add at end of any existing images, unless ImageOrder>0. Returns image ID or INVALIDID when failed.
    function AddImage(ImageFile: string; DocumentID: integer;
      ImageOrder: integer): integer;
    // Language to be used for OCR. Will not be saved in settings
    property CurrentOCRLanguage: string read FCurrentOCRLanguage
      write FCurrentOCRLanguage;
    // Number of pages to scan in one scan run.
    property Pages: integer read FPages write FPages;
    // Device to be used to scan with in sane notation; e.g. genesys:libusb:001:002
    // Specify e.g. net:192.168.0.4:genesys:libusb:001:002 for a sane network
    // scanner
    property ScanDevice: string read FScanDevice write FScanDevice;
    // Cleans up image (postprocessing): straightens them up, despeckles etc. Returns true if succesful
    function CleanUpImage(const ImageFile: string): boolean;
    // Delete document and associated images from DB and filesystem
    function DeleteDocument(const DocumentID: integer): boolean;
    // Delete all document and associated images from DB and filesystem
    function DeleteDocuments: boolean;
    // Get image identified by documentID and image number/imageorder (starting with 1)
    function GetImage(DocumentID, ImageOrder: integer;
      const ImageStream: TStream): boolean;
    // Get PDF identified by DocumentID
    function GetPDF(DocumentID: integer; const ImageStream: TStream): boolean;
    // Lists document specified by DocumentID or all documents (if DocumentID is INVALIDID)
    procedure ListDocuments(DocumentID: integer; var DocumentsArray: TJSONArray);
    // List images specified DocumentID or all images (if DocumentID is INVALIDID). Image path contains full path+file name.
    procedure ListImages(DocumentID: integer; var DocumentsArray: TJSONArray);
    // Process (set of) existing (TIFF) image(s); should be named <image>.tif
    // Images are specified using the Images property
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    // Returns resulting pdf file (including path) or empty if error
    function ProcessImages(DocumentID: integer; Resolution: integer): string;
    // Scans a single page and adds it to an existing document.
    // Returns image ID or INVALIDID if failure
    function ScanSinglePage(DocumentID: integer): integer;
    // Returns server version, compile date, etc in one big string
    function ServerInfo: string;
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

function TTigerServerCore.AddDocument(DocumentName: string = ''): integer;
begin
  Result := INVALIDID;
  try
    {$IF FPC_FULLVERSION>=20602}
    Result := FTigerDB.InsertDocument(DocumentName, '', '', LocalTimeToUniversal(Now));
    {$ELSE}
    {$WARNING This FPC version does not support UTC time conversion - times will be off!}
    Result := FTigerDB.InsertDocument(DocumentName, '', '', Now);
    {$ENDIF}
  except
    on E: Exception do
    begin
      TigerLog.WriteLog('AddDocument: error adding new document. ' + E.Message);
    end;
  end;
end;

function TTigerServerCore.AddImage(ImageData: TStream; ImageName: string;
  DocumentID: integer; ImageOrder: integer): integer;
var
  MemStream: TMemoryStream;
  ImagePath, ImageHash: string;
begin
  Result := INVALIDID;
  try
    // First get image into file system
    if not (assigned(ImageData)) then
    begin
      TigerLog.WriteLog(etError,'AddImage: no valid stream with image data.');
      exit;
    end;
    // Extract only filename part from image name and add to storage path
    ImagePath := FSettings.ImageDirectory + ExtractFileName(ImageName);
    MemStream := TMemoryStream.Create;
    try
      try
        ImageData.Position := 0;
        MemStream.CopyFrom(ImageData, ImageData.Size);
        MemStream.SaveToFile(ImagePath);
        MemStream.Position := 0;
        ImageHash := MD5Print(MD5Buffer(MemStream.Memory^, MemStream.Size));
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError,'AddImage: exception copying image file: '+E.Message);
        end;
      end;
    finally
      MemStream.Free;
    end;

    // Insert image reference into database
    Result := FTigerDB.InsertImage(DocumentID, ImageOrder, ImagePath, ImageHash);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog('AddImage: error adding new document. ' + E.Message);
    end;
  end;
end;

function TTigerServerCore.AddImage(ImageFile: string; DocumentID: integer;
  ImageOrder: integer): integer;
var
  ImageStream: TFileStream;
begin
  ImageStream := TFileStream.Create(ImageFile, fmOpenRead);
  try
    Result := AddImage(ImageStream, ExtractFileName(ImageFile), DocumentID, ImageOrder)
  finally
    ImageStream.Free;
  end;
end;

function TTigerServerCore.CleanUpImage(const ImageFile: string): boolean;
  // Cleans up image before OCR (despeckle etc)
var
  Cleaner: TImageCleaner;
begin
  Result := False;
  Cleaner := TImageCleaner.Create;
  try
    Cleaner.ScanDevice:=FScanDevice;
    Cleaner.ImageFile:=ImageFile;
    Cleaner.DetectApplyRotation;
    Result := True;
    TigerLog.WriteLog(etInfo, 'CleanImage: not yet completely implemented. File argument passed: ' +
      ImageFile);
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

function TTigerServerCore.DeleteDocuments: boolean;
begin
  //todo: for all docs,
  //todo: get all images, delete from fs
  //todo: get pdf, delete from fs
  //todo: delete document and images from db
end;

function TTigerServerCore.GetImage(DocumentID, ImageOrder: integer;
  const ImageStream: TStream): boolean;
var
  ImageFile: string;
  MemStream: TMemoryStream;
begin
  Result := False;
  if DocumentID <> INVALIDID then
  begin
    ImageFile := FTigerDB.ImagePath(DocumentID, ImageOrder);
    if ImageFile <> '' then
    begin
      try
        // Cater for different ImageDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers with different mountpoints
        if not (fileexists(ImageFile)) then
        begin
          TigerLog.WriteLog(etWarning, 'GetImage: cannot read image "' +
            ImageFile + '". Hack: trying again with mangled ImageDirectory');
          ImageFile := FSettings.ImageDirectory + ExtractFileName(ImageFile);
        end;
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(ImageFile);
          ImageStream.CopyFrom(MemStream, MemStream.Size);
        finally
          MemStream.Free;
        end;
        Result := True;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'GetImage: error trying to read image file ' +
            ImageFile + '. Exception:' + E.Message);
        end;
      end;
    end;
  end;
end;

function TTigerServerCore.GetPDF(DocumentID: integer;
  const ImageStream: TStream): boolean;
var
  PDFFile: string;
  MemStream: TMemoryStream;
begin
  Result := False;
  if DocumentID <> INVALIDID then
  begin
    PDFFile := FTigerDB.GetPDFPath(DocumentID);
    if PDFFile <> '' then
    begin
      try
        // Cater for different PDFDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers
        if not (fileexists(PDFFile)) then
        begin
          TigerLog.WriteLog(etWarning, 'GetPDF: cannot read PDF ' +
            PDFFile + '. Hack: trying again with mangled PDFDirectory');
          PDFFile := FSettings.PDFDirectory + ExtractFileName(PDFFile);
        end;
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(PDFFile);
          ImageStream.CopyFrom(MemStream, MemStream.Size);
        finally
          MemStream.Free;
        end;
        Result := True;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'GetPDF: error trying to read PDF file ' +
            PDFFile + '. Exception:' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TTigerServerCore.ListDocuments(DocumentID: integer;
  var DocumentsArray: TJSONArray);
begin
  FTigerDB.ListDocuments(DocumentID, DocumentsArray);
end;

procedure TTigerServerCore.ListImages(DocumentID: integer;
  var DocumentsArray: TJSONArray);
begin
  FTigerDB.ListImages(DocumentID, DocumentsArray);
end;

function TTigerServerCore.ProcessImages(DocumentID: integer;
  Resolution: integer): string;
var
  HOCRFile: string;
  i: integer;
  ImagesArray: TJSONArray;
  ImageFile: string;
  Message: string;
  OCR: TOCR;
  PDF: TPDF;
  Success: boolean;
begin
  {todo: add preprocess code to CleanUpImage despeckle, deskew etc? ScanTailor?
  Scantailor: more for letters/documents; unpaper more for books
  scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
  unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}
  Result := '';
  Success := False;
  if not (ForceDirectories(FSettings.PDFDirectory)) then
  begin
    Message := 'PDF directory %s does not exist and cannot be created.';
    TigerLog.WriteLog(etError, StringReplace(Message, '%s',
      FSettings.PDFDirectory, [rfReplaceAll]));
    exit;
    //raise Exception.CreateFmt(Message,[FSettings.PDFDirectory]); rather pass back empty value to indicate failure
  end;
  if DocumentID = INVALIDID then
  begin
    Message := 'ProcessImages: document ID must be valid. Please fix the program code.';
    TigerLog.WriteLog(etError, Message);
    exit;
    //raise Exception.Create(Message); rather pass back empty value to indicate failure
  end;

  // Get images belonging to document
  FTigerDB.ListImages(DocumentID, ImagesArray);
  for i := 0 to ImagesArray.Count - 1 do
  begin
    if (ImagesArray.Items[i].JSONType = jtObject) then
    begin
      // path contain full image path, no need to add FSettings.ImageDirectory
      ImageFile := (ImagesArray.Items[i] as TJSONObject).Elements['path'].AsString;
      Success := CleanUpImage(ImageFile, Resolution);
      if Success then
      begin
        OCR := TOCR.Create;
        try
          OCR.ImageFile := ImageFile;
          OCR.Language := FCurrentOCRLanguage;
          Success := OCR.RecognizeText;
          HOCRFile := OCR.HOCRFile;
          TigerLog.WriteLog(etDebug, 'ProcessImages: Got this text:' + OCR.Text);
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
          // todo: read tiff file and extract resolution ourselves, pass it on
          PDF.HOCRFile := HOCRFile;
          PDF.ImageFile := ImageFile;
          TigerLog.WriteLog(etDebug, 'pdfdirectory: ' + FSettings.PDFDirectory);
          PDF.PDFFile := IncludeTrailingPathDelimiter(FSettings.PDFDirectory) +
            ChangeFileExt(ExtractFileName(ImageFile), '.pdf');
          //todo: add metadata stuff to pdf unit
          //todo: add compression to pdf unit?
          Success := PDF.CreatePDF;
          if Success then
          begin
            TigerLog.WriteLog(etDebug, 'ProcessImages: Got PDF: ' + PDF.PDFFile);
            FTigerDB.SetPDFPath(DocumentID, PDF.PDFFile);
            Result := PDF.PDFFile;
          end;
          //todo: update pdf name based on OCR?!?
        finally
          PDF.Free;
        end;
      end;
    end
    else
    begin
      TigerLog.WriteLog(etDebug, 'ProcessImages: got invalid json array item from ListImages; item number '+inttostr(i));
    end;
  end; //all images added
  //todo: concatenate pdfs; we just add the last one for now
  if success=false then
    TigerLog.WriteLog(etDebug, 'ProcessImages failed.');
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


function TTigerServerCore.ScanSinglePage(DocumentID: integer): integer;
var
  i: integer;
  ImageOrder: integer;
  Message: string;
  Resolution: integer;
  Scanner: TScanner;
  StartDate: TDateTime;
  StartDateString: string;
begin
  Result := INVALIDID; //fail by default

  // Try a 300dpi scan, probably best for normal sized letters on paper
  Resolution := 300;
  if not (ForceDirectories(FSettings.ImageDirectory)) then
  begin
    Message := 'ScanSinglePage: Image directory %s does not exist and cannot be created.';
    TigerLog.WriteLog(etError, StringReplace(Message, '%s',
      FSettings.ImageDirectory, [rfReplaceAll]));
    exit;
    //raise Exception.CreateFmt('Image directory %s does not exist and cannot be created.', [FSettings.ImageDirectory]);
  end;

  Scanner := TScanner.Create;
  try
    Scanner.Resolution := Resolution;
    Scanner.ColorType := stLineArt;
    Scanner.ScanDevice := FScanDevice;
    StartDate := Now(); //local time
    StartDateString := FormatDateTime('yyyymmddhhnnss', StartDate);

    TigerLog.WriteLog(etInfo, 'ScanSinglePage: Going to scan single page; start date: ' +
      StartDateString);

    //Insert image after existing images:
    try
      ImageOrder := FTigerDB.GetHighestImageOrder(DocumentID) + 1;
    except
      TigerLog.WriteLog(etError,'ScanSinglePage: error getting next image order for document '+inttostr(DocumentID));
      exit;
    end;
    Scanner.FileName := FSettings.ImageDirectory + StartDateString +
      '_' + format('%.4d', [ImageOrder]) + TESSERACTTIFFEXTENSION;

    if not (Scanner.Scan) then
    begin
      message := 'TigerServerCore: an error occurred while scanning document %s';
      TigerLog.WriteLog(etError, StringReplace(Message, '%s',
        Scanner.FileName, [rfReplaceAll]));
      exit;
      //raise Exception.CreateFmt(Message, [Scanner.FileName]);
    end;
    TigerLog.WriteLog(etDebug, 'ScanSinglePage: going to process single image) ' + Scanner.FileName);
    Result := FTigerDB.InsertImage(DocumentID, ImageOrder, Scanner.FileName, '');
  finally
    Scanner.Free;
  end;
  if result=INVALIDID then
    TigerLog.WriteLog(etDebug,'ScanSinglePage: an error occurred.');
end;

function TTigerServerCore.ServerInfo: string;
begin
  Result :=
    'Papertiger ' + LineEnding + 'version: based on commit ' +
    RevisionStr + ' (' + versiondate + ')' + LineEnding + 'build date: ' +
{$INCLUDE %DATE%}
    +' ' +
{$INCLUDE %TIME%}
    +LineEnding + 'Compiled for CPU: ' + lowercase(
{$INCLUDE %FPCTARGETCPU%}
    ) + ' on ' + lowercase(
{$INCLUDE %FPCTARGETOS%}
    );
end;

class function TTigerServerCore.TryParseDate(DateString: string;
  out ParseDate: TDateTime): boolean;
  // Try to detect ISO 8601 formatted UTC datetime. Note: only full datetime
  // Returns false if not a date.
begin
  Result := False;
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
      {$IF FPC_FULLVERSION>=20602}
      ParseDate := UniversalTimeToLocal(EncodeDateTime(StrToInt(copy(DateString, 1, 4)),
        StrToInt(copy(DateString, 6, 2)), StrToInt(copy(DateString, 9, 2)),
        StrToInt(copy(DateString, 12, 2)), StrToInt(copy(DateString, 15, 2)),
        StrToInt(copy(DateString, 18, 2)), StrToInt(copy(DateString, 21, 3))));
      {$ELSE}
      {$WARNING This FPC version does not support UTC time conversion - times will be off!}
      ParseDate := EncodeDateTime(StrToInt(copy(DateString, 1, 4)),
        StrToInt(copy(DateString, 6, 2)), StrToInt(copy(DateString, 9, 2)),
        StrToInt(copy(DateString, 12, 2)), StrToInt(copy(DateString, 15, 2)),
        StrToInt(copy(DateString, 18, 2)), StrToInt(copy(DateString, 21, 3)));
      {$ENDIF}
      Result := True;
    end;
  except
    //ignore
  end;
end;


constructor TTigerServerCore.Create;

begin
  inherited Create;
  TigerLog.WriteLog(etDebug, 'TTigerServerCore: starting.');
  TigerLog.WriteLog(etDebug, Self.ServerInfo);
  FSettings := TTigerSettings.Create;
  FCurrentOCRLanguage := FSettings.Language;
  //read language from settings; can be overridden by command line optoin
  FPages := 1; //Assume single scan, not batch
  FTigerDB := TTigerDB.Create;
end;

destructor TTigerServerCore.Destroy;
begin
  TigerLog.WriteLog(etDebug, 'TTigerServerCore: stopping.');
  FTigerDB.Free;
  FSettings.Free;
  inherited Destroy;
end;

end.
