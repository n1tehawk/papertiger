unit tigerservercore;

{ Core functionality for the server part of papertiger.
  Shared by web/CGI, command line etc frontends.

  Copyright (c) 2012-2014 Reinier Olislagers

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
- use tesseract position detection since January 2014 trunk
- use tesseract searchable PDF output since January 2014 trunk=>probably needs leptonica 1.70 (r1009, 23 Jan 2014 Turned on pdfrenderer functionality that needs leptonica 1.70 )
- use unpaper/scantailor for cropping/deskewing/despeckling instead of device/driver dependent sane functionality
- look at getting ocrfeeder (text mode) instead of the scan/ocr/pdf processes
http://git.gnome.org/browse/ocrfeeder
- run multiple scan engines, compare results. Differences should be marked for manual intervention
- run multiple scan engines, output text output of all of them to PDF to improve searching on keywords
- get confidence output from tesseract (e.g. in the hocr)=>also assign manual intervention score
- trigram analysis?!?!
- address (to, from), date detection in letters=>requires heuristics/processing text blocks (search term: data capture)
- if we can find out where logos are: extract embedded text using e.g. groundtruth =>
  this would perhaps mean an additional colour scan of part of the image
- recognize barcodes with zxing? or rather exactimage tools: bardecode?

=> if this is working, tell the guys at watchocr.com (they use cuneiform); perhaps they're interested
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
  ScanType = scan.ScanType;

  { TTigerServerCore }

  TTigerServerCore = class(TObject)
  private
    FColorType: ScanType;
    // Effective language (from settings file, possibly overridden by e.g. command-line options)
    FCurrentOCRLanguage: string;
    // Any rotation specified by user; FUserSpecifiedRotation must be true, too
    FDesiredRotation: integer;
    // Number of pages to scan/process at once
    // Use >1 for batch (e.g. multipage documents)
    //todo: think about multipage tiff
    FPages: integer;
    FScanDevice: string;
    FSettings: TTigerSettings;
    // Enables FDesiredRotation
    FUserSpecifiedRotation: boolean;
    FTigerDB: TTigerDB;
    procedure SetDesiredRotation(AValue: integer);
  protected
  public
    // Adds new, empty document (with name if specified), returns document ID
    function AddDocument(DocumentName: string = ''): integer;
    // Adds the tiff image to given documentID.
    // Add at end of any existing images, unless ImageOrder>0.
    // Returns either image ID or INVALIDID (when failed).
    function AddImage(ImageData: TStream; ImageName: string; DocumentID: integer; ImageOrder: integer): integer;
    // Adds the tiff image to given documentID.
    // Rotates it first if the user asked for it, and checks for sane bug
    // Add at end of any existing images, unless ImageOrder>0.
    // Returns image ID, or INVALIDID when failed.
    function AddImage(ImageFile: string; DocumentID: integer; ImageOrder: integer): integer;
    // Whether to scan in black and white, gray or colo(u)r.
    property ColorType: ScanType read FColorType write FColorType;
    // Language to be used for OCR. Will not be saved in settings
    property CurrentOCRLanguage: string read FCurrentOCRLanguage write FCurrentOCRLanguage;
    // Number of pages to scan in one scan run.
    property Pages: integer read FPages write FPages;
    // Device to be used to scan with.
    // For sane driver: in sane notation; e.g. genesys:libusb:001:002
    // Specify e.g. net:192.168.0.4:genesys:libusb:001:002 for a sane network
    // scanner
    property ScanDevice: string read FScanDevice write FScanDevice;
    // Cleans up image (postprocessing): straightens them up, despeckles etc.
    // Returns true if succesful
    function CleanUpImage(const Source, Destination: string): boolean;
    // Delete document and associated images from DB
    // Deletes *all* documents if InvalidID specified as DocumentID
    // - if DeleteFromDisk, also delete from filesystem
    // Returns success or failure
    function DeleteDocument(const DocumentID: integer; DeleteFromDisk: boolean): boolean;
    // Deletes all documents
    // Internally calls DeleteDocument
    function DeleteDocuments(DeleteFromDisk: boolean): boolean;
    // Delete image(s) from DB
    // Deletes *all* images if InvalidID specified as ImageID
    // - if DeleteFromDisk, also delete from filesystem
    // Returns success or failure
    function DeleteImage(const ImageID: integer; DeleteFromDisk: boolean): boolean;
    // Deletes all images
    // Internally calls DeleteImage
    function DeleteImages(DeleteFromDisk: boolean): boolean;

    // Get image identified by documentID and image number/imageorder (starting with 1)
    function GetImage(DocumentID, ImageOrder: integer; const ImageStream: TStream): boolean;
    // Get image identified by image ID
    function GetImage(ImageID: integer; const ImageStream: TStream): boolean;
    // Get PDF identified by DocumentID
    function GetPDF(DocumentID: integer; const ImageStream: TStream): boolean;

    // Lists document specified by DocumentID or all documents (if DocumentID is INVALIDID)
    procedure ListDocuments(const DocumentID: integer; var DocumentsArray: TJSONArray);
    // List images specified DocumentID or all images (if DocumentID is INVALIDID).
    // Lists specified image for documentid if valid imageorder given; all images if ImageOrder=INVALIDID
    // Image path contains full path+file name.
    procedure ListImages(const DocumentID, ImageOrder: integer; var ImagesArray: TJSONArray);
    // Process (set of) existing (TIFF) image(s) for specified document
    // Specify resolution override to indicate image resolution to hocr2pdf
    // Specify 0 to leave alone and let hocr detect resolution or fallback to 300dpi
    // Returns resulting pdf file (including path) or empty if error
    function ProcessImages(DocumentID: integer; Resolution: integer): string;
    // Cleans up database by removing empty image/document records and
    // removing records with invalid files
    function PurgeDB: boolean;
    // Scans a single page and adds it to an existing document.
    // Returns image ID or INVALIDID if failure
    function ScanSinglePage(DocumentID: integer): integer;
    // Returns server version, compile date, etc in one big string
    function ServerInfo: string;
    // Tries to parse full ISO8601 UTC datetime; returns datetime (1,1,0,0,0) if invalid
    class function TryParseDate(DateString: string; out ParseDate: TDateTime): boolean;
    // Desired rotation of image in degrees, e.g.:
    // 90 means: rotate the image 90 degrees clockwise
    property DesiredRotation: integer read FDesiredRotation write SetDesiredRotation;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTigerServerCore }

 // Get revision from our source code repository:
 // If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}

procedure TTigerServerCore.SetDesiredRotation(AValue: integer);
begin
  FUserSpecifiedRotation := true;
  if FDesiredRotation = AValue then
    Exit;
  FDesiredRotation := FDesiredRotation mod 360; //Normalize...
  if FDesiredRotation < 0 then
    FDesiredRotation := 360 + FDesiredRotation; //...part 2
  FDesiredRotation := AValue;
end;

function TTigerServerCore.AddDocument(DocumentName: string = ''): integer;
  // DocumentName is a real name, not necessarily a file name. So no use checking for file existence.
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
      TigerLog.WriteLog(etError, 'AddDocument: error adding new document. ' + E.Message);
    end;
  end;
end;

function TTigerServerCore.AddImage(ImageData: TStream; ImageName: string; DocumentID: integer; ImageOrder: integer): integer;
const
  SaneBuggyText = 'Failed cupsGetDevices' + Chr($0A); //linefeed
var
  MemStream: TMemoryStream;
  Message: string;
  ImageFile, ImageHash: string;
begin
  Result := INVALIDID;
  try
    // Sanity checks:
    if not (assigned(ImageData)) then
    begin
      TigerLog.WriteLog(etError, 'AddImage: no valid stream with image data.');
      exit;
    end;
    if ImageData.Size = 0 then
    begin
      TigerLog.WriteLog(etError, 'AddImage: empty image data stream.');
      exit;
    end;
    if not (ForceDirectories(FSettings.ImageDirectory)) then
    begin
      Message := 'AddImage: Image directory %s does not exist and cannot be created.';
      TigerLog.WriteLog(etError, StringReplace(Message, '%s', FSettings.ImageDirectory, [rfReplaceAll]));
      exit;
      //raise Exception.CreateFmt('Image directory %s does not exist and cannot be created.', [FSettings.ImageDirectory]);
    end;

    // Get image into file system; don't overwrite existing files
    // Extract only filename part from image name and add to storage path
    ImageFile := ExpandFileName(FSettings.ImageDirectory + ExtractFileName(ImageName));

    // Copy image and fix sane bug if possible; then calculate image hash
    MemStream := TMemoryStream.Create;
    try
      try
        ImageData.Position := 0;
        MemStream.CopyFrom(ImageData, ImageData.Size);
        // Fix sane bug (use MemStream as we can write to it; not to ImageStream)
        if FindInStream(MemStream, 0, SaneBuggyText) = 0 then
        begin
          DeleteFromStream(MemStream, 0, length(SaneBuggyText));
          TigerLog.WriteLog(etDebug, 'TTigerServerCore.AddImage: fixed sane bug 313851 for file ' + ImageName);
        end;

        // Don't overwrite existing images:
        if (not (ImageData is TFileStream)) or ((ImageData is TFileStream) and
          (ExpandFileName((ImageData as TFileStream).FileName) <> ImageFile)) then
        begin
          MemStream.Position := 0;
          MemStream.SaveToFile(ImageFile);
        end;
        MemStream.Position := 0;
        ImageHash := MD5Print(MD5Buffer(MemStream.Memory^, MemStream.Size));
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'AddImage: exception copying image file: ' + E.Message);
        end;
      end;
    finally
      MemStream.Free;
    end;

    // Insert image reference into database
    if FileExists(ImageFile) then
      Result := FTigerDB.InsertImage(DocumentID, ImageOrder, ImageFile, ImageHash);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog('AddImage: error adding new document. ' + E.Message);
    end;
  end;
end;

function TTigerServerCore.AddImage(ImageFile: string; DocumentID: integer; ImageOrder: integer): integer;
var
  ImageStream: TFileStream;
begin
  Result := 0;
  if not (FileExists(ImageFile)) then
  begin
    TigerLog.WriteLog(etError, 'AddImage: ' + ImageFile + ' is no valid image file.');
    exit;
  end;

  try
    ImageStream := TFileStream.Create(ImageFile, fmOpenRead);
    try
      Result := AddImage(ImageStream, ExtractFileName(ImageFile), DocumentID, ImageOrder)
    except
      on E: Exception do
      begin
        TigerLog.WriteLog(etError, 'AddImage: error adding new image file ' + ImageFile + '. ' + E.Message);
      end;
    end;
  finally
    ImageStream.Free;
  end;
end;

function TTigerServerCore.CleanUpImage(const Source, Destination: string): boolean;
  // Cleans up image before OCR (despeckle etc)
var
  Cleaner: TImageCleaner;
begin
  Result := false;
  if not (FileExists(Source)) then
  begin
    TigerLog.WriteLog(etError, 'CleanUpImage: ' + Source + ' is no valid image file.');
    exit;
  end;

  Cleaner := TImageCleaner.Create;
  try
    Cleaner.Language := FCurrentOCRLanguage;
    // If user wanted to, rotate and overwrite existing image
    if (FUserSpecifiedRotation) then
    begin
      if (FDesiredRotation <> 0) then
        Cleaner.Rotate(FDesiredRotation, Source, Source);
      Cleaner.Clean(Source, Destination, false); //result in destination
    end
    else
    begin
      Cleaner.Clean(Source, Destination, true); //result in destination
    end;
    Result := true;
  finally
    Cleaner.Free;
  end;
end;

function TTigerServerCore.DeleteDocument(const DocumentID: integer; DeleteFromDisk: boolean): boolean;
var
  Document: TJSONObject;
  DocumentsArray: TJSONArray;
  ImagesArray: TJSONArray;
  Image: TJSONObject;
  DocCount, ImCount: integer;
begin
  Result := false;

  //Get any images belonging to document, delete from fs/db
  ImagesArray := TJSONArray.Create;
  ListImages(DocumentID, InvalidID, ImagesArray);
  // Empty (list of) images is no problem here; just don't delete them
  if ImagesArray.Count > 0 then
  begin
    // Check for empty object=>empty recordset
    Image := TJSONObject(ImagesArray.Items[0]);
    if Image.JSONType = jtObject then
    begin
      for ImCount := 0 to ImagesArray.Count - 1 do
      begin
        Image := (ImagesArray[ImCount] as TJSONObject);
        // Delete image
        // image.items[3]: path database column
        if DeleteFromDisk and (FileExists(Image.Items[3].AsString)) then
          DeleteFile(Image.Items[3].AsString);
        if not (FTigerDB.DeleteImageRecord(Image.Items[0].AsInteger)) then
          TigerLog.WriteLog(etError, 'DeleteDocument: could not delete image ID ' +
            IntToStr(Image.Items[0].AsInteger) + ' belonging to document ID ' + IntToStr(DocumentID));
      end;
    end;
  end;

  //Delete document PDF file and document record from db
  DocumentsArray := TJSONArray.Create;
  ListDocuments(DocumentID, DocumentsArray);
  // Check for empty array
  if DocumentsArray.Count < 1 then
  begin
    TigerLog.WriteLog(etError, 'DeleteDocument: empty DocumentsArray for document ID ' + IntToStr(DocumentID));
    exit;
  end;

  // Check for empty object=>empty recordset
  Document := TJSONObject(DocumentsArray.Items[0]);
  if Document.JSONType <> jtObject then
  begin
    TigerLog.WriteLog(etError, 'DeleteDocument: invalid document object for ID ' + IntToStr(DocumentID));
    exit;
  end;

  // If INVALIDID specified, we're looking at multiple docs so run a loop.
  for DocCount := 0 to DocumentsArray.Count - 1 do
  begin
    Document := (DocumentsArray[DocCount] as TJSONObject);
    // Delete document from disk & db
    // item 2=pdfpath
    if DeleteFromDisk and (FileExists(Document.Items[2].AsString)) then
      DeleteFile(Document.Items[2].AsString);
    Result := FTigerDB.DeleteDocumentRecord(Document.Items[0].AsInteger);
    if not (Result) then
    begin
      TigerLog.WriteLog(etError, 'DeleteDocument: could not delete document ID ' + IntToStr(DocumentID));
    end;
  end;
end;

function TTigerServerCore.DeleteDocuments(DeleteFromDisk: boolean): boolean;
begin
  Result := DeleteDocument(INVALIDID, DeleteFromDisk);
end;

function TTigerServerCore.DeleteImage(const ImageID: integer; DeleteFromDisk: boolean): boolean;
var
  ImagesArray: TJSONArray;
  Image: TJSONObject;
  ImCount: integer;
begin
  Result := false;

  //Get any images belonging to document, delete from fs/db
  ImagesArray := TJSONArray.Create;
  ListImages(InvalidID, InvalidID, ImagesArray);
  // Empty (list of) images is no problem here; just don't delete them
  if ImagesArray.Count > 0 then
  begin
    // Check for empty object=>empty recordset
    Image := TJSONObject(ImagesArray.Items[0]);
    if Image.JSONType = jtObject then
    begin
      for ImCount := 0 to ImagesArray.Count - 1 do
      begin
        Image := (ImagesArray[ImCount] as TJSONObject);
        // Delete image if it matches the user's wishes
        if (ImageID = InvalidID) or (ImageID = Image.Items[0].AsInteger) then
        begin
          // image.items[3]: path database column
          if DeleteFromDisk and (FileExists(Image.Items[3].AsString)) then
            DeleteFile(Image.Items[3].AsString);
          if not (FTigerDB.DeleteImageRecord(Image.Items[0].AsInteger)) then
            TigerLog.WriteLog(etError, 'DeleteDocument: could not delete image ID ' + Image.Items[0].AsString);
        end;
      end;
    end;
  end;
end;

function TTigerServerCore.DeleteImages(DeleteFromDisk: boolean): boolean;
begin
  Result := DeleteImage(INVALIDID, DeleteFromDisk);
end;

function TTigerServerCore.GetImage(DocumentID, ImageOrder: integer; const ImageStream: TStream): boolean;
var
  ImageFile: string;
  MemStream: TMemoryStream;
begin
  Result := false;
  if DocumentID <> INVALIDID then
  begin
    ImageFile := FTigerDB.GetImagePath(DocumentID, ImageOrder);
    if ImageFile <> '' then
    begin
      try
        // Cater for different ImageDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers with different mountpoints
        if not (fileexists(ImageFile)) then
        begin
          TigerLog.WriteLog(etWarning, 'GetImage: cannot read image "' + ImageFile +
            '". Hack: trying again with mangled ImageDirectory');
          ImageFile := FSettings.ImageDirectory + ExtractFileName(ImageFile);
        end;
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(ImageFile);
          ImageStream.CopyFrom(MemStream, MemStream.Size);
        finally
          MemStream.Free;
        end;
        Result := true;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'GetImage: error trying to read image file ' + ImageFile + '. Exception:' + E.Message);
        end;
      end;
    end;
  end;
end;

function TTigerServerCore.GetImage(ImageID: integer; const ImageStream: TStream
  ): boolean;
var
  ImageFile: string;
  MemStream: TMemoryStream;
begin
  Result := false;
  if ImageID <> INVALIDID then
  begin
    ImageFile := FTigerDB.GetImagePath(ImageID);
    if ImageFile <> '' then
    begin
      try
        // Cater for different ImageDirectory setting on this server.
        // Although this is a bit of a hack, it allows testing from different servers with different mountpoints
        if not (fileexists(ImageFile)) then
        begin
          TigerLog.WriteLog(etWarning, 'GetImage: cannot read image "' + ImageFile +
            '". Hack: trying again with mangled ImageDirectory');
          ImageFile := FSettings.ImageDirectory + ExtractFileName(ImageFile);
        end;
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(ImageFile);
          ImageStream.CopyFrom(MemStream, MemStream.Size);
        finally
          MemStream.Free;
        end;
        Result := true;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'GetImage: error trying to read image file ' + ImageFile + '. Exception:' + E.Message);
        end;
      end;
    end;
  end;
end;

function TTigerServerCore.GetPDF(DocumentID: integer; const ImageStream: TStream): boolean;
var
  PDFFile: string;
  MemStream: TMemoryStream;
begin
  Result := false;
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
          TigerLog.WriteLog(etWarning, 'GetPDF: cannot read PDF ' + PDFFile + '. Hack: trying again with mangled PDFDirectory');
          PDFFile := FSettings.PDFDirectory + ExtractFileName(PDFFile);
        end;
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(PDFFile);
          ImageStream.CopyFrom(MemStream, MemStream.Size);
        finally
          MemStream.Free;
        end;
        Result := true;
      except
        on E: Exception do
        begin
          TigerLog.WriteLog(etError, 'GetPDF: error trying to read PDF file ' + PDFFile + '. Exception:' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TTigerServerCore.ListDocuments(const DocumentID: integer; var DocumentsArray: TJSONArray);
begin
  FTigerDB.ListDocuments(DocumentID, DocumentsArray);
end;

procedure TTigerServerCore.ListImages(const DocumentID, ImageOrder: integer; var ImagesArray: TJSONArray);
begin
  FTigerDB.ListImages(DocumentID, ImageOrder, ImagesArray);
end;

function TTigerServerCore.ProcessImages(DocumentID: integer; Resolution: integer): string;
var
  CleanImage: string;
  HOCRFile: string;
  i: integer;
  ImagesArray: TJSONArray;
  ImageFile: string;
  Message: string;
  OCR: TOCR;
  PDF: TPDF;
  Success: boolean;
begin
  Result := '';
  Success := false;
  if not (ForceDirectories(FSettings.PDFDirectory)) then
  begin
    Message := 'PDF directory %s does not exist and cannot be created.';
    TigerLog.WriteLog(etError, StringReplace(Message, '%s', FSettings.PDFDirectory, [rfReplaceAll]));
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

  // todo: perhaps only continue if NEEDSOCR=1 for this document?

  // Get images belonging to document
  FTigerDB.ListImages(DocumentID, InvalidID, ImagesArray);
  for i := 0 to ImagesArray.Count - 1 do
  begin
    if (ImagesArray.Items[i].JSONType = jtObject) then
    begin
      // path contain full image path, no need to add FSettings.ImageDirectory
      ImageFile := (ImagesArray.Items[i] as TJSONObject).Elements['path'].AsString;
      CleanImage := GetTempFileName('', 'TIFC');
      // Clean up image, copy into temporary file
      Success := CleanUpImage(ImageFile, CleanImage);

      if Success then
      begin
        OCR := TOCR.Create;
        try
          OCR.ImageFile := CleanImage;
          OCR.Language := FCurrentOCRLanguage;
          Success := OCR.RecognizeText;
          HOCRFile := OCR.HOCRFile;
          TigerLog.WriteLog(etDebug, 'ProcessImages: Got file '+HOCRFile+' with this text:' + OCR.Text);
        finally
          OCR.Free;
        end;
        {$IFNDEF DEBUG}
        DeleteFile(CleanImage);
        {$ENDIF}
      end;

      if Success then
      begin
        PDF := TPDF.Create;
        try
          // Only pass on overrides on resolution
          if Resolution > 0 then
            PDF.ImageResolution := Resolution;
          // todo: read tiff file and extract resolution ourselves, pass it on
          if not(FileExists(HOCRFile)) then
            raise Exception.CreateFmt('OCR file %s does not exist',[HOCRFile]);
          PDF.HOCRFile := HOCRFile;
          PDF.ImageFile := ImageFile; // The original, unaltered image file
          TigerLog.WriteLog(etDebug, 'pdfdirectory: ' + FSettings.PDFDirectory);
          PDF.PDFFile := IncludeTrailingPathDelimiter(FSettings.PDFDirectory) + ChangeFileExt(
            ExtractFileName(ImageFile), '.pdf');
          //todo: add metadata stuff to pdf unit
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
      TigerLog.WriteLog(etDebug, 'ProcessImages: got invalid json array item from ListImages; item number ' + IntToStr(i));
    end;
  end; //all images added
       //todo: concatenate pdfs; we just add the last one for now
  if Success then
    FTigerDB.SetOCRDone(DocumentID)
  else
    TigerLog.WriteLog(etDebug, 'ProcessImages failed.');
end;

function TTigerServerCore.PurgeDB: boolean;
begin
  Result := FTigerDB.Purge;
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
  Clean: TImageCleaner;
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
    TigerLog.WriteLog(etError, StringReplace(Message, '%s', FSettings.ImageDirectory, [rfReplaceAll]));
    exit;
    //raise Exception.CreateFmt('Image directory %s does not exist and cannot be created.', [FSettings.ImageDirectory]);
  end;

  Scanner := TScanner.Create;
  try
    Scanner.Resolution := Resolution;
    Scanner.ColorType := FColorType;
    Scanner.ScanDevice := FScanDevice;
    StartDate := Now(); //local time
    StartDateString := FormatDateTime('yyyymmddhhnnss', StartDate);

    TigerLog.WriteLog(etInfo, 'ScanSinglePage: Going to scan single page; start date: ' + StartDateString);

    //Insert image after existing images:
    try
      ImageOrder := FTigerDB.GetHighestImageOrder(DocumentID) + 1;
    except
      TigerLog.WriteLog(etError, 'ScanSinglePage: error getting next image order for document ' + IntToStr(DocumentID));
      exit;
    end;
    Scanner.FileName := FSettings.ImageDirectory + StartDateString + '_' + format('%.4d', [ImageOrder]) + TESSERACTTIFFEXTENSION;

    if not (Scanner.Scan) then
    begin
      message := 'TigerServerCore: an error occurred while scanning document %s';
      TigerLog.WriteLog(etError, StringReplace(Message, '%s', Scanner.FileName, [rfReplaceAll]));
      exit;
      //raise Exception.CreateFmt(Message, [Scanner.FileName]);
    end;

    // Rotate image first if user had requested it
    if FDesiredRotation <> 0 then
    begin
      Clean := TImageCleaner.Create;
      try
        if not (Clean.Rotate(FDesiredRotation, Scanner.FileName, Scanner.FileName)) then
        begin
          Result := INVALIDID;
          TigerLog.WriteLog(etError, 'TTigerServerCore.AddImage: error rotating image ' + Scanner.FileName + '. Aborting.');
          exit;
        end;
      finally
        Clean.Free;
      end;
    end;

    TigerLog.WriteLog(etDebug, 'ScanSinglePage: going to process single image) ' + Scanner.FileName);
    Result := FTigerDB.InsertImage(DocumentID, ImageOrder, Scanner.FileName, '');
  finally
    Scanner.Free;
  end;
  if Result = INVALIDID then
    TigerLog.WriteLog(etDebug, 'ScanSinglePage: an error occurred.');
end;

function TTigerServerCore.ServerInfo: string;
begin
  Result :=
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
  Result := false;
  try
    // Scandatetime won't work, so do it the old-fashioned way
    //2013-02-21T09:47:42.467Z
    //0        1         2
    //123456789012345678901234
    if (copy(DateString, 5, 1) = '-') and (copy(DateString, 8, 1) = '-') and (copy(DateString, 11, 1) = 'T') and
      (copy(DateString, 14, 1) = ':') and (copy(DateString, 17, 1) = ':') and (copy(DateString, 20, 1) = '.') and
      (copy(DateString, 24, 1) = 'Z') then
    begin
      {$IF FPC_FULLVERSION>=20602}
      ParseDate := UniversalTimeToLocal(EncodeDateTime(StrToInt(copy(DateString, 1, 4)), StrToInt(copy(DateString, 6, 2)),
        StrToInt(copy(DateString, 9, 2)), StrToInt(copy(DateString, 12, 2)), StrToInt(copy(DateString, 15, 2)),
        StrToInt(copy(DateString, 18, 2)), StrToInt(copy(DateString, 21, 3))));
      {$ELSE}
      {$WARNING This FPC version does not support UTC time conversion - times will be off!}
      ParseDate := EncodeDateTime(StrToInt(copy(DateString, 1, 4)), StrToInt(copy(DateString, 6, 2)),
        StrToInt(copy(DateString, 9, 2)), StrToInt(copy(DateString, 12, 2)), StrToInt(copy(DateString, 15, 2)),
        StrToInt(copy(DateString, 18, 2)), StrToInt(copy(DateString, 21, 3)));
      {$ENDIF}
      Result := true;
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
  FColorType := stLineArt; //todo: save to settings/restore from settings as text string
  //read language from settings; can be overridden by command line option
  FCurrentOCRLanguage := FSettings.Language;
  FDesiredRotation := 0;
  FUserSpecifiedRotation := false;
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
