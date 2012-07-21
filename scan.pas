unit scan;
{ Paper scanning functionality

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
//todo: add support for pascalsane/using libsane instead of wrapping sane command line

type

  { TScanner }

  TScanner = class(TObject)
  private
    FFileName: string;
    FScanDevice: string;
  public
    property FileName: string read FFileName write FFileName;
    property ScanDevice: string read FScanDevice write FScanDevice;
    procedure Scan;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

{ TScanner }

procedure TScanner.Scan;
begin
  //todo: call runprocess scanimage... etc
end;

constructor TScanner.Create;
begin
  inherited Create;
end;

destructor TScanner.Destroy;
begin
  inherited Destroy;
end;

end.

