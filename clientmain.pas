unit clientmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  tigersettings, LJGridUtils, FPJSON, jsonparser, httpclient;
//todo: think about splitting up data access layer so you can e.g. build a CLI client

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuQuit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
  private
    { private declarations }
    FCGIURL: string; //Base cgi URL used for connecting
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.mnuAboutClick(Sender: TObject);
var
  Success:boolean;
  VersionInfo: string;
  VersionInfoJSON: TJSONString;
begin
  Success:=false;
  try
    Success:=(HttpRequest(FCGIURL+'serverinfo',VersionInfoJSON,rmGet).Code=200);
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
    //VersionInfoJSON.Free;
  end;
end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  Close;
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

