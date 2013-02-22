unit clientmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids,
  StdCtrls, tigersettings, LJGridUtils, FPJSON, jsonparser, httpclient, imageformunit,
  fpreadtiff {adds TIFF format read support to TImage}, lclintf,
  magick_wand, ImageMagick {for conversion from TIFF formats unsupported by FPC to regular bitmaps};
//todo: think about splitting up data access layer so you can e.g. build a CLI client

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure RefreshDocumentsButtonClick(Sender: TObject);
    procedure ScanButtonClick(Sender: TObject);
    procedure ShowImageButtonClick(Sender: TObject);
    procedure ShowPDFButtonClick(Sender: TObject);
  private
    { private declarations }
    FCGIURL: string; //Base cgi URL used for connecting
    // Refresh list of documents in grid
    procedure RefreshDocuments;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
{$i tigercommondefs.inc}

{ TForm1 }

procedure TForm1.mnuAboutClick(Sender: TObject);
var
  Success:boolean;
  VersionInfo: string;
  ReturnJSON: TJSONObject;
begin
  Success:=false;
  ReturnJSON:=TJSONObject.Create;
  try
    Success:=(HttpRequestWithData(ReturnJSON,FCGIURL+'serverinfo',rmPost).Code=200);
    if Success then
    try
      VersionInfo:=ReturnJSON.Strings['serverinfo'];
    except
      on E: Exception do
      begin
        Success:=false;
        VersionInfo:=' Technical details: exception '+E.Message;
      end;
    end;
    if Success then
    begin
      ShowMessage('Papertiger client version 0.1'+LineEnding+
       'Papertiger server: '+VersionInfo);
    end
    else
    begin
      ShowMessage('Papertiger client version 0.1'+LineEnding+
       'Papertiger server: error retrieving server information. '+VersionInfo);
    end;
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

procedure TForm1.ScanButtonClick(Sender: TObject);
var
  CurrentPage: integer;
  DocumentID: integer; //New document ID returned by the server
  NumberPages: integer; //Number of pages user requested for sca
  RequestResult: THttpResult;
  CommunicationJSON: TJSONObject;
begin
  DocumentID:=INVALIDID;
  NumberPages:=StrToIntDef(NumberPagesControl.Text,1);
  CommunicationJSON:=TJSONObject.Create;
  try
    try
      RequestResult:=HttpRequest(FCGIURL+'adddocument',CommunicationJSON,rmGet);
      if RequestResult.Code<>200 then
      begin
        showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
        exit;
      end;
      DocumentID:=CommunicationJSON.Integers['documentid'];
    except
      on E: Exception do
      begin
        showmessage('Error parsing addocument response from server. Technical details: '+E.Message);
      end;
    end;
  finally
    CommunicationJSON.Free;
  end;

  for CurrentPage:=1 to NumberPages do
  begin
    if CurrentPage>1 then
    begin
      ShowMessage('Please put page '+inttostr(CurrentPage)+' in the scanner.');
    end;
    CommunicationJSON:=TJSONObject.Create;
    try
      try
        CommunicationJSON.Add('documentid',DocumentID); //pass newly created document
        RequestResult:=HTTPRequestWithData(CommunicationJSON,FCGIURL+'scan',rmPost);
        if RequestResult.Code<>200 then
        begin
          showmessage('Error from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
          exit;
        end;
      except
        on E: Exception do
        begin
          showmessage('Error parsing scan response from server. Technical details: '+E.Message);
        end;
      end;
    finally
      CommunicationJSON.Free;
    end;
  end;


  //When succesful, add docs to list
  RefreshDocuments;
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
    TIFFStream.Position:=0;
    //todo: fix tiff only supporting 8 and 16 bits samples=>imagemagick. What do we have? 1 bit?
    try
      imageform.ScanImage.Picture.LoadFromStreamWithFileExt(TIFFStream,'.tiff');
      ImageForm.Show;
    except
      on E: Exception do
      begin
        showmessage('Error showing image'+LineEnding+
        'Technical details: '+E.Message);
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
  RequestResult: THTTPResult;
  PDFFile: string;
  PDFStream: TMemoryStream;
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

end.

