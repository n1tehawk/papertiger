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


{$i tigerserver.inc}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  fpjson,
  jsonparser,
  tigerservercore;

type

  { TTigerServer }

  TTigerServer = class(TCustomApplication)
  private
    FTigerCore: TTigerServerCore;
    procedure ListDocuments;
  protected
    procedure DoRun; override;
    // Main entry point into the program; processes command line options etc
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TTigerServer }

  procedure TTigerServer.ListDocuments;
  var
    Cell: string;
    DateCell: TDateTime;
    Document: TJSONObject;
    DocumentsArray: TJSONArray;
    i, Col: integer;
  begin
    writeln('Existing documents on server:');
    DocumentsArray := TJSONArray.Create;
    FTigerCore.ListDocuments(INVALIDID, DocumentsArray);
  {$IFDEF DEBUG}
    // Extra troubleshooting; useful in client/server environment
    if DocumentsArray.JSONType <> jtArray then
      Exception.CreateFmt('ListDocuments error: Got "%s", expected "TJSONArray".',
        [DocumentsArray.ClassName]);
  {$ENDIF DEBUG}

    // Check for empty array
    if DocumentsArray.Count < 1 then
    begin
      writeln('*** no documents available ***');
      exit;
    end;

    // Check for empty object=>empty recordset
    Document := TJSONObject(DocumentsArray.Items[0]);
    if Document.JSONType <> jtObject then
    begin
      writeln('*** no documents available ***');
      exit;
    end;

    for i := 0 to DocumentsArray.Count - 1 do
    begin
      Document := (DocumentsArray[i] as TJSONObject);
      if i = 0 then // column headers
      begin
        for Col := 0 to Document.Count - 1 do
        begin
          Write(Document.Names[Col] + ';');
        end;
        writeln();
      end;
      for Col := 0 to Document.Count - 1 do
      begin
        //todo: for date, we get a number instead of a date. fix this
        try
          Cell := Document.Items[Col].AsString;
        except
          Cell := '[INVALID]';
        end;
        case Document.Items[Col].JSONType of
          jtUnknown: Write('[UNKNOWN];');
          jtNumber: Write(Cell + ';');
          jtString:
          begin
            if FTigerCore.TryParseDate(Cell, DateCell) then
              Write(DateTimeToStr(DateCell))
            else
              Write(Cell + ';');
          end;
          jtBoolean: Write(Cell + ';');
          jtNull: Write(Cell + ';');
          jtArray: Write('[ARRAY];');
          jtObject: Write('[OBJECT];');
        end;
      end;
    end;
    writeln();
  end;

  procedure TTigerServer.DoRun;
  var
    DocumentID: integer;
    i: integer;
    ErrorMsg: string;
    PDF: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('d:hi:l:p:r:sv', 'blackwhite color colour device: gray grayscale help image: language: lineart list pages: rotate: scan version');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters; show help if no params given
    if (ParamCount = 0) or (HasOption('h', 'help')) then
    begin
      writeln(FTigerCore.ServerInfo);
      writeln('');
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('d','device') then
    begin
      FTigerCore.ScanDevice:=GetOptionValue('d','device');
    end
    else
    begin
      FTigerCore.ScanDevice:='';
    end;

    if HasOption('color') or HasOption('colour') then
    begin
      FTigerCore.ColorType:=ScanType.stColor;
    end;

    if HasOption('gray') or HasOption('grayscale') then
    begin
      FTigerCore.ColorType:=ScanType.stGray;
    end;

    // Put black and white last so it trumps the other settings
    if HasOption('blackwhite') or HasOption('lineart') then
    begin
      FTigerCore.ColorType:=ScanType.stLineArt;
    end;

    if HasOption('list') then
    begin
      ListDocuments;
      Terminate;
      exit;
    end;

    if HasOption('v', 'version') then
    begin
      writeln(FTigerCore.ServerInfo);
      Terminate;
      Exit;
    end;


    if HasOption('l', 'language') then
    begin
      FTigerCore.CurrentOCRLanguage := GetOptionValue('l', 'language');
    end;

    if HasOption('p', 'pages') then
    begin
      FTigerCore.Pages := StrToIntDef(GetOptionValue('p', 'pages'),1);
    end;

    if HasOption('r', 'rotate') then
    begin
      FTigerCore.DesiredRotation:=StrToIntDef(GetOptionValue('r','rotate'),0);
    end;

    // Branching off into processing starts here
    if HasOption('i', 'image') then
    begin
      //todo: add support for ; or , separated image names when pages>1
      DocumentID := FTigerCore.AddDocument('Document ' + FormatDateTime('yyyymmddhhnnss', Now));
      if DocumentID <> INVALIDID then
      begin
        if FTigerCore.AddImage(ExpandFileName(GetOptionValue('i', 'image')), DocumentID, 0) <> INVALIDID then
        begin
          PDF := FTigerCore.ProcessImages(DocumentID, 0);
          if PDF = '' then
            writeln('Error creating PDF. Stopping.')
          else
            writeln('Finished adding image.');
        end
        else
        begin
          writeln('Error adding image. Stopping.');
        end;
      end
      else
      begin
        writeln('Error getting document ID. Stopping.');
      end;
    end;

    if HasOption('s', 'scan') then
    begin
      DocumentID := INVALIDID;
      PDF := '';
      try
        DocumentID := FTigerCore.AddDocument('Document ' + FormatDateTime('yyyymmddhhnnss', Now));
        if DocumentID <> INVALIDID then
        begin
          for i := 1 to FTigerCore.Pages do
          begin
            FTigerCore.ScanSinglePage(DocumentID);
            if (FTigerCore.Pages > 1) and (i < FTigerCore.Pages) then
            begin
              writeln('Please put page ' + IntToStr(i + 1) + ' in the scanner and press enter to continue.');
              readln;
            end;
          end;
          PDF := FTigerCore.ProcessImages(DocumentID, 0);
        end;
      except
        on E: Exception do
        begin
          writeln('Exception: ' + E.Message);
        end;
      end;
      if PDF = '' then
        writeln('Error while scanning')
      else
        writeln('Scanning complete.');
    end;

    // stop program loop
    Terminate;
  end;

  constructor TTigerServer.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := true;



    FTigerCore := TTigerServerCore.Create;
  end;

  destructor TTigerServer.Destroy;
  begin
    FTigerCore.Free;
    inherited Destroy;
  end;

  procedure TTigerServer.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -h');
    writeln('--blackwhite, --lineart');
    writeln(' Scan/PDF in black & white. Useful for text only documents such as');
    writeln(' letters.');
    writeln('--color, --colour');
    writeln(' Scan/PDF in color (text detection internally still in black/white');
    writeln(' for better performance). Useful for photos, graphics etc.');
    writeln('-d <device> --device=<device>');
    writeln(' Scanning device (use sane notation) - empty to select default.');
    writeln('--gray, --grayscale');
    writeln(' Scan/PDF in grayscale (text detection internally still in black/white');
    writeln(' for better performance)');
    writeln('-i <image> --image=<image>');
    writeln(' Process image.');
    writeln('-l <lang> --language=<language>');
    writeln(' Language to be used for OCR.');
    writeln(' eng (English) by default. See the OCR documentation for ');
    writeln(' language codes (e.g. man tesseract)');
    writeln('--list');
    writeln(' list already scanned documents');
    writeln('-r <d> --rotate=<d>');
    writeln(' rotate image or scan d degrees clockwise before processing');
    writeln('-s --scan');
    writeln(' Scan document, process.');
    writeln('-p <n> --pages=<n>');
    writeln(' Specify number of pages for processing/scanning multi page docs.');
    writeln('-v --version');
    writeln(' Show version information and exit.');
  end;

var
  Application: TTigerServer;
begin
  Application := TTigerServer.Create(nil);
  Application.Run;
  Application.Free;
end.
