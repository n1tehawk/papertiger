unit clientmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids,
  StdCtrls, tigersettings, LJGridUtils, FPJSON, jsonparser, httpclient, imageformunit,
  fpreadtiff {adds TIFF format read support to TImage};
//todo: think about splitting up data access layer so you can e.g. build a CLI client

type

  { TForm1 }

  TForm1 = class(TForm)
    ShowImageButton: TButton;
    ScanButton: TButton;
    RefreshDocumentsButton: TButton;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuQuit: TMenuItem;
    DocumentsGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure RefreshDocumentsButtonClick(Sender: TObject);
    procedure ScanButtonClick(Sender: TObject);
    procedure ShowImageButtonClick(Sender: TObject);
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
  VersionInfoJSON: TJSONString;
begin
  Success:=false;
  VersionInfoJSON:=TJSONString.Create('');
  try
    Success:=(HttpRequestWithData(VersionInfoJSON,FCGIURL+'serverinfo',rmPost).Code=200);
    if Success then
    try
      if Assigned(VersionInfoJSON) then
      begin
        VersionInfo:=VersionInfoJSON.AsString;
      end;
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
    VersionInfoJSON.Free;
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
begin
  //RequestResult:=HttpRequest(FCGIURL+'scan',
  //todo: finish

  //When succesful, add docs to list
  RefreshDocuments;
end;

procedure TForm1.ShowImageButtonClick(Sender: TObject);
var
  DocumentID: integer;
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
  DocumentID:=StrToIntDef(DocumentsGrid.Cells[0,DocumentsGrid.Row],;

  VData:=TJSONObject.Create;
  TIFFStream:=TMemoryStream.Create;
  try
    VData.Add('documentid',DocumentID);
    //post a request to show the image
    RequestResult:=HttpRequestWithData(VData,FCGIURL+'showimage',rmPost);
    if RequestResult.Code<>200 then
    begin
      showmessage('Error getting image from server. HTTP result code: '+inttostr(RequestResult.Code)+'/'+RequestResult.Text);
      exit;
    end;
    imageform.Hide;
    imageform.ScanImage.Picture.LoadFromStreamWithFileExt(TIFFStream,'.tiff');
    ImageForm.Show;
  finally
    VData.Free;
    TIFFStream.Free;
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

