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
    FFile: string;
    FFilePart: string;
  public
    // Scans image into tiff file; returns success status
    function Scan: boolean;
    // Scanned image file
    property FileName: string read FFile;
    // Filename part (without extension) for files that are scanned
    property FilePart: string read FFilePart write FFilePart;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

{ TLocalWIAScanner }

const
  wiaCommandTakePicture: widestring = '{AF933CAC-ACAD-11D2-A093-00C04F72DC3C}'; //CommandID for Take Picture. Causes a WIA device to acquire an image.
//  http://msdn.microsoft.com/en-us/library/windows/desktop/ms630829%28v=vs.85%29.aspx
  wiaConvertFilterID: widestring = '{42A6E907-1D2F-4b38-AC50-31ADBE2AB3C2}';
  wiaFormatBMP: widestring = '{B96B3CAB-0728-11D3-9D7B-0000F81EF32E}';
  wiaFormatPNG: widestring = '{B96B3CAF-0728-11D3-9D7B-0000F81EF32E}';
  wiaFormatGIF: widestring = '{B96B3CB0-0728-11D3-9D7B-0000F81EF32E}';
  wiaFormatJPEG: widestring = '{B96B3CAE-0728-11D3-9D7B-0000F81EF32E}';
  wiaFormatTIFF: widestring = '{B96B3CB1-0728-11D3-9D7B-0000F81EF32E}';
  wiaCurrentIntent: widestring = '6146';
  WIA_INTENT_IMAGE_TYPE_COLOR=&00000001;
  WIA_INTENT_IMAGE_TYPE_GRAYSCALE=&00000002;
  WIA_INTENT_IMAGE_TYPE_TEXT=&00000004;
  WIA_INTENT_MINIMIZE_SIZE=&00010000;
  WIA_INTENT_MAXIMIZE_QUALITY=&00020000;
  WIA_INTENT_BEST_PREVIEW=&00040000;
  WIA_HORIZONTAL_SCAN_RESOLUTION_DPI: widestring = '6147';
  WIA_VERTICAL_SCAN_RESOLUTION_DPI: widestring = '6148';
  WIA_HORIZONTAL_SCAN_START_PIXEL: widestring = '6149';
  WIA_VERTICAL_SCAN_START_PIXEL: widestring = '6150';
  WIA_HORIZONTAL_SCAN_SIZE_PIXELS: widestring = '6151'; //scan size in pixels
  WIA_VERTICAL_SCAN_SIZE_PIXELS: widestring = '6152'; //scan size in pixels
  WIA_SCAN_BRIGHTNESS_PERCENTS: widestring = '6154';
  WIA_SCAN_CONTRAST_PERCENTS: widestring = '6155';
  WIA_SCAN_ORIENTATION: widestring = '6156'; //Specifies the current orientation of the document to be scanned.
  WIA_SCAN_ROTATION: widestring = '6157'; //Indicates WIA should rotate the scanned image


function TLocalWIAScanner.Scan: boolean;
{ Adapted from: see bottom of function }
const
  Resolution=300; //dpi
var
  Found:boolean;
  i:integer;
  DevNo:integer;
  CommonDial: ICommonDialog;
  Scanner: Device;
  Picture: IItem;
  TheFilterInfos: IFilterInfos;
  Image: OleVariant;
  TheImageFile: ImageFile;
  ImageProcess: IImageProcess;
  InVar: OleVariant; //temp var used for passing values
  InVar2: OleVariant;
  ScannerItem: IItem;
  StringVar: OleVariant;
  OutVar: OLEVariant; //temp var used for passing values
  AImage: IImageFile;
  ReturnString: widestring;

  procedure SetScannerProperty(PropertyName, PropertyValue: OLEVariant);
  begin
    ScannerItem.Properties[@PropertyName].Set_Value(@PropertyValue);
  end;

begin
  //todo: allow specifying specific scanner device name
  FFile:='';
  try
    // List of devices is a 1 based array
    //showmessage('number of devices: '+inttostr(FDevMgr.DeviceInfos.Count));
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
          //showmessage('Device: '+utf8encode(ReturnString));
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
        showmessage('Found a device but it is not a scanner');
        OutVar:=FDevMgr.DeviceInfos[@InVar].Properties[@StringVar].get_Value;
        showmessage('got device type '+utf8encode(OutVar));
      end;
    end;

    if not(Found) then
    begin
      ShowMessage('No compatible scanner found. Aborting.');
      exit;
    end;

    // Connect to detected scanner
    InVar:=DevNo;
    Scanner:=FDevMgr.DeviceInfos.Item[@InVar].Connect;

    InVar:=1;
    ScannerItem:=Scanner.Items[Invar];

    // Set up scanner for OCR oriented tasks: black & white
    Invar:=WIA_HORIZONTAL_SCAN_RESOLUTION_DPI;
    InVar2:=Resolution;
    SetScannerProperty(Invar,Invar2);
    Invar:=WIA_VERTICAL_SCAN_RESOLUTION_DPI;
    InVar2:=Resolution;
    SetScannerProperty(Invar,Invar2);
    InVar:=wiaCurrentIntent; //what type of image we want to sacn
    //todo: this won't work gives invalid parameter!!
    //InVar2:=(WIA_INTENT_IMAGE_TYPE_TEXT or WIA_INTENT_MAXIMIZE_QUALITY); // black & white; good quality for given resolution
    InVar2:=WIA_INTENT_IMAGE_TYPE_TEXT;
    SetScannerProperty(Invar,Invar2);
    CommonDial:=CoCommonDialog.Create; //todo: free when done? release when done?
    //todo: seems to scan only part of the entire surface
    OutVar:=CommonDial.ShowTransfer(ScannerItem, wiaFormatTIFF, false);
    {
    to do: perhaps replace with Item.Transfer which does not show the GUI
    however, visual confirmation is nice, too
    }
    if OutVar=null then
    begin
      // User cancelled
      exit(false);
    end
    else
    begin
      TheImageFile:=ImageFile(OutVar); //is this the right translation?
      // While we may have requested a certain format, some scanners/drivers only implement e.g.
      // bmp format, so the file format is not certain at this point.
      // Therefore, convert if needed - adapted from
      // http://msdn.microsoft.com/en-us/library/ms630826%28v=VS.85%29.aspx#SharedSample002
      if TheImageFile.FormatID<>wiaFormatTIFF then
      begin
        ImageProcess:=CoImageProcess.Create;
        i:=ImageProcess.Filters.Count;
        ImageProcess.Filters.Add(wiaConvertFilterID,i); //the i is just guessing
        StringVar:='FormatID';
        Invar:=1;
        InVar2:=wiaFormatTiff;
        ImageProcess.Filters[InVar].Properties[@StringVar].Set_Value(@InVar2);
        TheImageFile:=ImageProcess.Apply(TheImageFile);
      end;
      // note: there may be a file already present which shouldn't be overwritten...
      FFile:= IncludeTrailingPathDelimiter(GetTempDir(false))+FFilePart+'.'+TheImageFile.FileExtension;
      if FileExists(FFile) then
      begin
        DeleteFile(FFile);
        sleep(20); //give filesystem time to process
      end;
      TheImageFile.SaveFile(FFile);
      {todo: replace with in memory manipulation
      IImageFile has a property FileData with provides access to the binary image data, via IVector.BinaryData
      via vector binarydata - an array of bytes
      http://msdn.microsoft.com/en-us/library/windows/desktop/ms630518%28v=vs.85%29.aspx
      }
    end;
  except
    //for now
    on E: Exception do
    begin
      ShowMessage('Error scanning: '+E.Message);
    end;
  end;
{
http://stackoverflow.com/questions/721948/delphi-twain-issue-help
and
c# code:
private void ScanDoc()
  {
    CommonDialogClass commonDialogClass = new CommonDialogClass();
    Device scannerDevice = commonDialogClass.ShowSelectDevice(WiaDeviceType.ScannerDeviceType, false, false);
    if (scannerDevice != null)
    {
      Item scannnerItem = scannerDevice.Items[1];
      AdjustScannerSettings(scannnerItem, (int)nudRes.Value, 0, 0, (int)nudWidth.Value, (int)nudHeight.Value, 0, 0, cmbCMIndex);
      object scanResult = commonDialogClass.ShowTransfer(scannnerItem, WIA.FormatID.wiaFormatTIFF, false);
      if (scanResult != null)
      {
        ImageFile image = (ImageFile)scanResult;
        string fileName = "";

        var files = Directory.GetFiles(txtPath.Text, "*.tiff");

        try
        {
            string f = ((files.Max(p1 => Int32.Parse(Path.GetFileNameWithoutExtension(p1)))) + 1).ToString();
            fileName = txtPath.Text + "\\" + f + ".tiff";
        }
        catch (Exception ex)
        {
            fileName = txtPath.Text + "\\" + "1.tiff";
        }
        SaveImageToTiff(image, fileName);
        picScan.ImageLocation = fileName;
      }
    }
  }
}
end;

constructor TLocalWIAScanner.Create;
begin
  FDevMgr:=CoDeviceManager.Create;
  FFilePart:='scan';
end;

destructor TLocalWIAScanner.Destroy;
begin
  //FDevMgr._Release; //perhaps?
  inherited Destroy;
end;

end.

