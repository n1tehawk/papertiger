unit wia;

{ WIA image scanning module

  Copyright (c) 2014 Reinier Olislagers

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


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WIA_1_0_TLB,
  forms, controls, dialogs {todo: debug: temporarily added for troubleshooting output};

type

  { TLocalWIAScanner }

  TLocalWIAScanner = class(TObject)
  private
    FDevMgr: DeviceManager;
  public
    procedure Scan;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

{ TLocalWIAScanner }

procedure TLocalWIAScanner.Scan;
// Adapted from
// http://stackoverflow.com/questions/721948/delphi-twain-issue-help
var
  Found:boolean;
  DevNo:integer;
  Scanner: Device;
  Picture: IItem;
  Image: OleVariant;
  InVar: OLEVariant;
  StringVar: OleVariant;
  OutVar: OLEVariant;
  AImage: IImageFile;
  ReturnString: widestring;
begin
  try
    // List of devices is a 1 based array
    showmessage('number of devices: '+inttostr(FDevMgr.DeviceInfos.Count));
    Found:=false;
    for DevNo:=1 to FDevMgr.DeviceInfos.Count do
    begin
      InVar:=DevNo;
      // Only check scanners (ignore cameras etc)
      StringVar:='Type';
      OutVar:=FDevMgr.DeviceInfos[@InVar].Properties[@StringVar].get_Value;
      // Apparently need to force result to 4 bytes to compare to constant:
      if word(OutVar)=ScannerDeviceType then
      begin
        StringVar:='Name';
        if FDevMgr.DeviceInfos[@InVar].Properties.Exists(StringVar) then
        begin
          ReturnString:=FDevMgr.DeviceInfos[@InVar].Properties[@StringVar].get_Value;
          showmessage('Device: '+utf8encode(ReturnString));
          Found:=true;
          break;
        end
        else
        begin
          ShowMessage('Name property does not exist. Aborting.');
          exit;
        end;
      end
      else
      begin
        showmessage('found a device but it is not a scanner');
        OutVar:=FDevMgr.DeviceInfos[@InVar].Properties[@StringVar].get_Value;
        showmessage('got device type '+utf8encode(OutVar));
      end;
    end;

    if not(Found) then
    begin
      ShowMessage('No compatible scanner found. Aborting.');
      exit;
    end;

    //=========< code works until here - will need to fix stuff below >==============
    // Connect to detected scanner
    InVar:=DevNo;
    Scanner:=FDevMgr.DeviceInfos.Item[@DevNo].Connect;
    // to do: figure out which command scans
    //reference: wia item property constants - grayscale deskew etc
    //http://msdn.microsoft.com/en-us/library/ms630196%28v=VS.85%29
    Picture := Scanner.ExecuteCommand(Scanner.Commands.Item[1].CommandID);
    { todo: add scan selection dialog:
    lDialog.ShowAcquireImage(WIA_TLB.ScannerDeviceType,WIA_TLB.GrayscaleIntent,WIA_TLB.MinimizeSize,
           jpegFormat,false,false,false);
    }
    //Transfer as JPG if scanner supports it. todo: change to bmp or tiff
    Image := Picture.Transfer(Picture.Formats.Item[1]);
    { todo: image may need to be converted to bmp/tiff
    http://msdn.microsoft.com/en-us/library/ms630826%28v=VS.85%29.aspx#SharedSample002
    }
    //Save the image
    {todo: replace with in memory manipulation
    IImageFile has a property FileData with provides access to the binary image data, via IVector.BinaryData
    via vector binarydata - an array of bytes
    http://msdn.microsoft.com/en-us/library/windows/desktop/ms630518%28v=vs.85%29.aspx
    }
    AImage := IImageFile(Image);
    AImage.SaveFile('wiaimage.' + AImage.FileExtension);
  except
    //for now
    //to do: debug
    on E: Exception do
    begin
      writeln(stderr,'Error scanning locally: '+E.Message);
    end;
  end;


end;

constructor TLocalWIAScanner.Create;
begin
  FDevMgr:=CoDeviceManager.Create;
end;

destructor TLocalWIAScanner.Destroy;
begin
  //FDevMgr._Release; //perhaps?
  inherited Destroy;
end;

end.

