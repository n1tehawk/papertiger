unit pdf;

{ PDF generation functionality

  Copyright (c) 2012 Reinier Olislagers

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
  Classes, SysUtils;

type

  { TPDF }

  TPDF = class(TObject)
  private
    FHOCRFile: string;
    FImageFile: string;
    FPDFFile: string;

  public
    // File with hOCR: position and text in image
    property HOCRFile: string write FHOCRFile;
    // Input image
    property ImageFile: string write FImageFile;
    property PDFFile: string read FPDFFile write FPDFFile;
    procedure CreatePDF;
    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses processutils;

{ TPDF }

procedure TPDF.CreatePDF;
// Create searchable PDF using exactimage (using -s for aligning text better)
const
  Command='hocr2pdf';
var
  Options:string;
begin
  if FPDFFile='' then
    FPDFFile:=ChangeFileExt(FImageFile,'.pdf');

  Options:=' -i "'+FImageFile+'" -o "'+FPDFFile+'" --sloppy-text';
  writeln('PDF generation: running '+Command+Options);
  if ExecuteCommand(Command+Options,false)=0 then
  begin
    writeln('PDF succeeded.');
  end
  else
  begin
    writeln('Error running command.');
  end;
  //todo: deal with temp files somewhere. complicated because some are needed by other processes. Best to add them to an overarchiing object with list?
end;

constructor TPDF.Create;
begin
  inherited Create;
  FImageFile:='';
  FHOCRFile:='';
end;

destructor TPDF.Destroy;
begin
  inherited Destroy;
end;

end.

