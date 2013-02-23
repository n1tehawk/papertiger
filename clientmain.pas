unit clientmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids,
  StdCtrls, tigersettings, LJGridUtils, FPJSON, jsonparser, httpclient, imageformunit,
  fpreadtiff {adds TIFF format read support to TImage}, lclintf,
  magick_wand, ImageMagick {for conversion from TIFF formats unsupported by FPC to regular bitmaps},
  IntfGraphics, FPimage, LazUTF8;
//todo: think about splitting up data access layer so you can e.g. build a CLI client

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    UploadImageButton: TButton;
    NumberPagesControl: TEdit;
    Label1: TLabel;
    ShowImageButton: TButton;
    ScanButton: TButton;
    RefreshDocumentsButton: TButton;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuQuit: TMenuItem;
    DocumentsGrid: TStringGrid;
    ShowPDFButton: TButton;
    DeleteButton: TButton;
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure RefreshDocumentsButtonClick(Sender: TObject);
    procedure ScanButtonClick(Sender: TObject);
    procedure ShowImageButtonClick(Sender: TObject);
    procedure ShowPDFButtonClick(Sender: TObject);
    procedure UploadImageButtonClick(Sender: TObject);
  private
    { private declarations }
    FCGIURL: string; //Base cgi URL used for connecting
    // Asks the server to add a new document and returns the document ID
    function AddDocument: integer;
    // Refresh list of documents in grid
    procedure RefreshDocuments;
    // Shows pdf for relevant document
    procedure ShowPDF(DocumentID: integer);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
{$i tigercommondefs.inc}
// Get revision from our source code repository:
// If you have a file not found error for revision.inc, please make sure you compile hgversion.pas before compiling this project.
{$i revision.inc}


procedure LoadMagickBitmap(ImageMemoryPtr: Pointer; ImageSize: integer; Bmp: TBitmap);
// Let imagemagick convert an image and return a bitmap.
// Adapted from code from theo on the Lazarus forum.
var
  status: MagickBooleanType;
  wand: PMagickWand;
  img: Pimage;
  pack: PPixelPacket;
  limg: TLazIntfImage;
  i, j, wi, he: integer;
  colo: TFPColor;
  description: PChar;
  severity: ExceptionType;
begin
  wand := NewMagickWand;
  try
    status := MagickReadImageBlob(wand, ImageMemoryPtr, ImageSize);

    if (status = MagickFalse) then
    begin
      description := MagickGetException(wand, @severity);
      raise Exception.Create(Format('LoadMagickBitmap: an error ocurred. Description: %s',
        [description]));
      description := MagickRelinquishMemory(description);
    end;
    img := GetImageFromMagickWand(wand);
    he := MagickGetImageHeight(wand);
    wi := MagickGetImageWidth(wand);
    limg := TLazIntfImage.Create(0, 0);
    limg.DataDescription := GetDescriptionFromDevice(0, wi, he);
    pack := GetAuthenticPixels(img, 0, 0, wi, he, nil);
    for j := 0 to he - 1 do
      for i := 0 to wi - 1 do
      begin
        colo.red := pack^.red;
        colo.green := pack^.green;
        colo.blue := pack^.blue;
        colo.alpha := pack^.opacity;
        limg.Colors[i, j] := colo;
        Inc(pack);
      end;
    Bmp.LoadFromIntfImage(limg);
  finally
    limg.Free;
    wand := DestroyMagickWand(wand);
  end;
end;

{ TForm1 }

procedure TForm1.mnuAboutClick(Sender: TObject);
var
  Message: string;
  ReturnJSON: TJSONObject;
  Success:boolean;
begin
  Success:=false;
  ReturnJSON:=TJSONObject.Create;
  try
    Success:=(HttpRequestWithData(ReturnJSON,FCGIURL+'serverinfo',rmPost).Code=200);
    if Success then
    try
      Message:=ReturnJSON.Strings['serverinfo'];
    except
      on E: Exception do
      begin
        Success:=false;
        Message:='Error getting server info. Technical details: exception '+E.Message;
      end;
    end;
    Message:='Papertiger client' + LineEnding + 'version: based on commit ' + RevisionStr + ' (' + versiondate + ')' + LineEnding + 'build date: ' +
  {$INCLUDE %DATE%}
      +' ' +
  {$INCLUDE %TIME%}
      +LineEnding + 'Compiled for CPU: ' + lowercase(
  {$INCLUDE %FPCTARGETCPU%}
      ) + ' on ' + lowercase(
  {$INCLUDE %FPCTARGETOS%}
      ) +LineEnding+
       'Uses ImageMagick software.'+LineEnding+
       LineEnding+
       'Papertiger server: '+Message;
    ShowMessage(Message);
  finally
    ReturnJSON.Free;
  end;
end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RefreshDocumentsButtonClick(Sender: TObject);
begin
  RefreshDocuments;
end;

function TForm1.AddDocument: integer;
var
  CurrentPage: integer;
  RequestResult: THttpResult;
  CommunicationJSON: TJSONObject;
begin
  result:=INVALIDID;
  CommunicationJSON:=TJSONObject.Create;
  try
    try
      RequestResult:=HttpRequest(FCGIURL+'adddocument',CommunicationJSON,rmGet);
      if RequestResult.Code<>200 then
      begin
        showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
        exit;
      end;
      result:=CommunicationJSON.Integers['documentid'];
    except
      on E: Exception do
      begin
        showmessage('Error interpreting response from server. Technical details: '+E.Message);
        exit;
      end;
    end;
  finally
    CommunicationJSON.Free;
  end;
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
var
  CurrentPage: integer;
  DocumentID: integer; //New document ID returned by the server
  NumberPages: integer; //Number of pages user requested for sca
  RequestResult: THttpResult;
  CommunicationJSON: TJSONObject;
begin
  NumberPages:=StrToIntDef(NumberPagesControl.Text,1);

  DocumentID:=AddDocument;

  for CurrentPage:=1 to NumberPages do
  begin
    if CurrentPage>1 then
    begin
      ShowMessage('Please put page '+inttostr(CurrentPage)+' in the scanner.');
    end;

    CommunicationJSON:=TJSONObject.Create;
    try
      Screen.Cursor:=crHourglass;
      CommunicationJSON.Add('documentid',DocumentID); //pass newly created document
      try
        RequestResult:=HTTPRequestWithData(CommunicationJSON,FCGIURL+'scan',rmPost);
        if RequestResult.Code<>200 then
        begin
          Screen.Cursor:=crDefault;
          showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
          exit;
        end;
      except
        on E: Exception do
        begin
          Screen.Cursor:=crDefault;
          showmessage('Error interpreting response from server. Technical details: '+E.Message);
          exit;
        end;
      end;
    finally
      Screen.Cursor:=crDefault;
      {
      rather mem leaks than this getting runtime error 210 etc.
      FreeAndNil(CommunicationJSON);
      or
      // The JSON could have been changed by the httprequest code, so
      if assigned(CommunicationJSON) and (CommunicationJSON.JSONType=jtObject) then
        CommunicationJSON.Free;
      }
    end;
  end; //all pages scanned now

  CommunicationJSON:=TJSONObject.Create();
  try
    CommunicationJSON.Add('documentid',DocumentID);
    RequestResult:=HTTPRequestWithData(CommunicationJSON,FCGIURL+'processdocument',rmPost);
    if RequestResult.Code<>200 then
    begin
      showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
      exit;
    end;
  except
    on E: Exception do
    begin
      showmessage('Error interpreting response from server. Technical details: '+E.Message);
      exit;
    end;
  end;
  //todo: investigate leak

  //When succesful, add docs to list
  RefreshDocuments;
  ShowPDF(DocumentID);
  ShowMessage('Scan complete.');
end;

procedure TForm1.ShowImageButtonClick(Sender: TObject);
var
  DocumentID, Sequence: integer;
  RequestResult: THTTPResult;
  TIFFStream: TMemoryStream;
  VData: TJSONObject;
begin
  // Check for selected document
  if DocumentsGrid.Row<1 then
  begin
    ShowMessage('No document selected. Please select a document in the grid first.');
    exit;
  end;
  DocumentID:=StrToInt(DocumentsGrid.Cells[0,DocumentsGrid.Row]);

  VData:=TJSONObject.Create;
  TIFFStream:=TMemoryStream.Create;
  try
    Sequence:=1; //todo: add support for multi tiff images, e.g. using next/previous button & capturing errors
    VData.Add('documentid',DocumentID);
    VData.Add('sequence',Sequence); //image order number
    //post a request to show the image
    RequestResult:=HttpRequestWithData(VData,FCGIURL+'showimage',TIFFStream,rmPost);
    if RequestResult.Code<>200 then
    begin
      showmessage('Error getting image from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
      exit;
    end;
    imageform.Hide;
    if TIFFStream.Size=0 then
    begin
      ShowMessage('Got an empty image from server.');
      exit;
    end
    else
    begin
      TIFFStream.Position:=0;
      try
        // Use ImageMagick to convert to a viewable bitmap; FPC tiff routines don't support black & white tiff
        LoadMagickBitmap(TIFFStream.Memory, TIFFStream.Size, imageform.ScanImage.Picture.Bitmap);
        //imageform.ScanImage.Picture.LoadFromStreamWithFileExt(TIFFStream,'.tiff');
        ImageForm.Show;
      except
        on E: Exception do
        begin
          showmessage('Error showing image'+LineEnding+
          'Technical details: '+E.Message);
        end;
      end;
    end;
  finally
    VData.Free;
    TIFFStream.Free;
  end;
end;

procedure TForm1.ShowPDFButtonClick(Sender: TObject);
var
  DocumentID: integer;
begin
  // Check for selected document
  if DocumentsGrid.Row<1 then
  begin
    ShowMessage('No document selected. Please select a document in the grid first.');
    exit;
  end;
  DocumentID:=StrToInt(DocumentsGrid.Cells[0,DocumentsGrid.Row]);

  ShowPDF(DocumentID);
end;

procedure TForm1.UploadImageButtonClick(Sender: TObject);
var
  DocumentID: integer;
  ImageFile: string;
begin
  if DocumentsGrid.Row<1 then
  begin
    // Create new document if user wants to
    if (MessageDlg('Create document?',
      'No document selected. Create a new document for this image?',
      mtConfirmation,[mbOK,mbCancel],0,mbOK)=mrCancel) then exit;
    DocumentID:=AddDocument;
  end
  else
  begin
    DocumentID:=StrToInt(DocumentsGrid.Cells[0,DocumentsGrid.Row]);
  end;


  OpenDialog1.Execute;
  ImageFile:=OpenDialog1.FileName;
  if ImageFile<>'' then
  begin
    //todo: upload the image file along with the document ID
  end;
end;

procedure TForm1.RefreshDocuments;
var
  RequestResult: THTTPResult;
  VData: TJSONArray;
begin
  VData:=TJSONArray.Create; //needs to be assigned for HTTPRequest
  try
    ClearGrid(DocumentsGrid);
    RequestResult:=HttpRequest(FCGIURL+'list',VData,rmGet);
    if RequestResult.Code<>200 then
    begin
      showmessage('Error getting document list from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
      exit;
    end;
    LoadJSON(DocumentsGrid,VData,false,false,false);
  finally
    VData.Free;
  end;
end;

procedure TForm1.ShowPDF(DocumentID: integer);
var
  RequestResult: THTTPResult;
  PDFFile: string;
  PDFStream: TMemoryStream;
  VData: TJSONObject;
begin
  VData:=TJSONObject.Create;
  PDFStream:=TMemoryStream.Create;
  try
    VData.Add('documentid',DocumentID);
    //post a request to get the PDF
    RequestResult:=HttpRequestWithData(VData,FCGIURL+'showdocument',PDFStream,rmPost);
    if RequestResult.Code<>200 then
    begin
      showmessage('Error getting PDF from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
      exit;
    end;
    imageform.Hide;
    PDFStream.Position:=0;
    try
      PDFFile:=ChangeFileExt(sysutils.GetTempFileName('','tpdf'), '.pdf');
      PDFStream.SaveToFile(PDFFile);
      OpenDocument(PDFFile);
    except
      on E: Exception do
      begin
        showmessage('Error showing PDF'+LineEnding+
        'Technical details: '+E.Message);
      end;
    end;
  finally
    VData.Free;
    PDFStream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Settings: TTigerSettings;
begin
  Settings:=TTigerSettings.Create('tigerclient.ini');
  try
    FCGIURL:=Settings.CGIURL;
  finally
    Settings.Free;
  end;
end;

procedure TForm1.DeleteButtonClick(Sender: TObject);
var
  CommunicationJSON: TJSONObject;
  DocumentID: integer;
  DocumentPrompt: string;
  ImageFile: string;
begin
  if DocumentsGrid.Row<1 then
  begin
    ShowMessage('Please select the document you want to delete first.');
  end
  else
  begin
    DocumentID:=StrToInt(DocumentsGrid.Cells[0,DocumentsGrid.Row]);
  end;

  // Create new document if user wants to
  if DocumentsGrid.Cells[1,DocumentsGrid.Row]='' then
    DocumentPrompt:='ID '+ inttostr(DocumentID)+'?'
  else
    DocumentPrompt:='"'+DocumentsGrid.Cells[1,DocumentsGrid.Row]+'"?';

  if (MessageDlg('Delete document?',
    'Are you sure you want to delete document '+DocumentPrompt,
    mtConfirmation,[mbOK,mbCancel],0,mbCancel)=mrCancel) then exit;

  CommunicationJSON:=TJSONObject.Create;
  try
    Screen.Cursor:=crHourglass;
    CommunicationJSON.Add('documentid',DocumentID); //indicate what document we want to delete
    try
      RequestResult:=HTTPRequest(FCGIURL+'deletedocument',CommunicationJSON,rmGet);
      if RequestResult.Code<>200 then
      begin
        Screen.Cursor:=crDefault;
        showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
        exit;
      end;
    except
      on E: Exception do
      begin
        Screen.Cursor:=crDefault;
        showmessage('Error interpreting response from server. Technical details: '+E.Message);
        exit;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
    CommunicationJSON.Free;
  end;
end;

end.

