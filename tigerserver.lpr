program tigerserver;
{ Paper Tiger paper scanning/OCR/archiving solution

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
  Classes, SysUtils, CustApp, tigerservercore;

type

  { TTigerServer }

  TTigerServer = class(TCustomApplication)
  private
    FTigerCore: TTigerServerCore;
  protected
    procedure DoRun; override;
    // Main entry point into the program; processes command line options etc
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTigerServer }


procedure TTigerServer.DoRun;
var
  ErrorMsg: String;
  PDF: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:l:p:s','help image: language: list pages: scan');
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters; show help if no params given
  if (ParamCount=0) or (HasOption('h','help')) then
  begin
    writeln(FTigerCore.ServerInfo);
    writeln('');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('list') then
  begin
    writeln('Existing documents on server:');
    writeln(FTigerCore.ListDocuments(''));
    Terminate;
    exit;
  end;

  if HasOption('l','language') then
  begin
    FTigerCore.CurrentOCRLanguage:=GetOptionValue('l','language');
  end;

  if HasOption('p','pages') then
  begin
    FTigerCore.Pages:=strtoint(GetOptionValue('p','pages'));
  end;

  // Branching off into processing starts here
  if HasOption('i','image') then
  begin
    //todo: add support for ; or , separated image names when pages>1
    FTigerCore.Images.Clear;
    FTigerCore.Images.Add(ExpandFileName(GetOptionValue('i','image')));
    PDF:=FTigerCore.ProcessImages('Document'+FormatDateTime('yyyymmddhhnnss', Now),0);
    if PDF<>'' then
      writeln('Error creating PDF. Stopping.');
  end;

  if HasOption('s','scan') then
  begin
    FTigerCore.ScanAndProcess;
  end;

  // stop program loop
  Terminate;
end;

constructor TTigerServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FTigerCore:=TTigerServerCore.Create;
end;

destructor TTigerServer.Destroy;
begin
  FTigerCore.Free;
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
  writeln('--list');
  writeln(' list already scanned documents');
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

