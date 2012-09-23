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
  Classes, SysUtils, inifiles;
//todo: add support for pascalsane/using libsane instead of wrapping sane command line?

const
  // todo: create separate settings class
  SettingsFile = 'tigerserver.ini';

type
  // Colour/color, grayscale and lineart/black & white scan modes
  ScanType=(stColor,stGray,stLineArt);

  { TScanner }

  TScanner = class(TObject)
  private
    FFileName: string;
    FResolution: integer;
    FScanDevice: string;
    FColorType: ScanType;
  public
    property ColorType: ScanType read FColorType write FColorType;
    // Black & white, grayscale or colour scan?
    property FileName: string read FFileName write FFileName;
    // File where scanned image should be or has been stored
    //todo: how to deal with existing files?
    property Resolution: integer read FResolution write FResolution;
    // Scan resolution in DPI
    property ScanDevice: string read FScanDevice write FScanDevice;
    // Device to be used to scan with; e.g. genesys:libusb:001:002
    // Specify e.g. net:192.168.0.4:genesys:libusb:001:002 for a sane network
    // scanner
    procedure ShowDevices(var DeviceList: TStringList);
    // Interrogate scanner software for a list of installed devices
    procedure Scan;
    // Scan paper to image
    constructor Create;
    destructor Destroy; override;
  end;
implementation

uses processutils;

const
  //todo: perhaps write FPC wrapper instead of bash scanwrap.sh
  ScanCommand='./scanwrap.sh';

{ TScanner }

procedure TScanner.ShowDevices(var DeviceList: TStringList);
const
  ScanListCommand='scanimage';
var
  Output: string='';
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
    DeviceList.Text:=Output;
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
  ScanDevicePart: string;
  ScanType: string;
begin
  //todo: call runprocess scanimage... etc
  {Example call:
  scanimage --device-name=genesys:libusb:001:002 --mode=Color --swdeskew=yes --swcrop=yes --format=tiff > /tmp/testscan.tiff
  swdeskew, swcrop, color arguments are device dependent
  Network:
  scanimage --device-name=net:192.168.0.1:genesys:libusb:001:002
  and the arguments are the same
  }
  { Alternative: while scanadf has a file output option, you can't specify file format:
  scanadf --depth=8 --resolution=300 --mode=Gray --start-count=1 --end-count=1 --output-file=/tmp/scan.tiff
  }
  //todo: device-specific parameters -> get from scanimage --help?
  //Compress TIFF?? => no, not really useful, wil be compressed in PDF
  case FColorType of
    stLineArt: ScanType:='Lineart';
    stGray: ScanType:='Gray';
    stColor: ScanType:='Color';
    else ScanType:='Lineart';
  end;
  if FScanDevice='' then
    ScanDevicePart:=''
  else
    ScanDevicePart:='--device-name='+FScanDevice;

  Options:=' "'+FFileName+'" '+ScanDevicePart+' --mode='+ScanType+' --resolution='+inttostr(FResolution)+' --format=tiff';
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
var
  Settings: TINIFile;
begin
  inherited Create;
  FColorType:=stLineArt; //Lineart is suitable for OCR for black & white docments?!?
  //todo: we would like to scan graphics in B&W or even colour, then text as lineart. This would probably require fiddling with the hOCR output to get the text areas etc???
  // Tesseract requires a tif extension
  FFileName:=Sysutils.GetTempFileName(GetTempDir(false),'SCN')+'.tif';
  FResolution:=300;
  //todo: check whether sane works by --version ??

  if FileExists(SettingsFile) then
  begin
    Settings := TINIFile.Create(SettingsFile);
    try
      FScanDevice:=Settings.ReadString('Sane', 'DeviceName', '');
    finally
      Settings.Free;
    end;
  end
end;

destructor TScanner.Destroy;
begin
  inherited Destroy;
end;

end.

