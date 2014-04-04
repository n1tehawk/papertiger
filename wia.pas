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
  Classes, SysUtils, WIA_1_0_TLB;

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
  Scanner: Device;
  Picture: IItem;
  Image: OleVariant;
  AImage: IImageFile;
begin
  try
    // Figure out first scanner(?)
    Scanner:=FDevMgr.DeviceInfos.Item[POleVariant(1)].Connect;
    // figure out which command scans
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

