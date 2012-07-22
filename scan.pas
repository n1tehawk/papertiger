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
  Classes, SysUtils, process;
//todo: add support for pascalsane/using libsane instead of wrapping sane command line?

type

  { TScanner }

  TScanner = class(TObject)
  private
    FFileName: string;
    FScanDevice: string;
    procedure RunCommand(Command: string);
  public
    // File where scanned image should be or has been stored
    //todo: how to deal with existing files?
    property FileName: string read FFileName write FFileName;
    // Device to be used to scan with; e.g. genesys:libusb:001:002
    property ScanDevice: string read FScanDevice write FScanDevice;
    // Interrogate scanner software for a list of installed devices
    procedure ShowDevices(DeviceList: TStringList);
    // Scan paper to image
    procedure Scan;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

uses processutils;

const
  //todo: fix hardcoded path; perhaps also write FPC wrapper instead of bash scanwrap.sh
  ScanCommand='/opt/tigerserver/scanwrap.sh';

{ TScanner }

procedure TScanner.RunCommand(Command: string);

begin

end;

procedure TScanner.ShowDevices(DeviceList: TStringList);
const
  ScanListCommand='scanimage';
var
  Output: string;
begin
  //todo:
  //scanimage --list-devices
  {
  --list-devices         show available scanner devices
  --formatted-device-list=FORMAT similar to -L, but the FORMAT of the output
                             can be specified: %d (device name), %v (vendor),
                             %m (model), %t (type), %i (index number), and
                             %n (newline)
  }
  if ExecuteCommand(ScanListCommand + ' --list-devices',Output,false)=0 then
  begin
    writeln('Result:');
    writeln(Output);
  end
  else
  begin
    writeln('Error running command.');
  end;
end;

procedure TScanner.Scan;
var
  Options: string;
begin
  //todo: call runprocess scanimage... etc
  //Example call:
  //scanimage --device-name=genesys:libusb:001:002 --mode=Color --swdeskew=yes --swcrop=yes --format=tiff > /tmp/testscan.tiff
  // color= argument is device dependent
  if FScanDevice='' then
    Options:=' "'+FFileName+'"  --mode=Color --resolution=300 --swdeskew=yes --swcrop=yes --format=tiff'
  else
    Options:=' "'+FFileName+'" --device-name='+FScanDevice+' --mode=Color --resolution=300 --swdeskew=yes --swcrop=yes --format=tiff';
  writeln('Executing:');
  writeln(ScanCommand+Options);
  if ExecuteCommand(ScanCommand+Options,false)=0 then
  begin
    writeln('Scan succeeded.');
  end
  else
  begin
    writeln('Error running command.');
  end;
end;

constructor TScanner.Create;
begin
  inherited Create;
  FFileName:=Sysutils.GetTempFileName(GetTempDir(false),'SCN');
  //todo: check whether sane works by --version ??
end;

destructor TScanner.Destroy;
begin
  inherited Destroy;
end;

end.

