unit tigersettings;

{ Settings management for paper tiger.

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

interface

uses
  Classes, SysUtils, IniFiles;

const
  SettingsFile = 'tigerserver.ini'; //Default settings filename

type

  { TTigerSettings }

  TTigerSettings = class(TObject)
  private
    FCGIURL: string;
    FImageDirectory: string;
    FLanguage: string;
    FPDFDirectory: string;
    FScanDevice: string;
    FSettings: TINIFile;
    FSettingsFileName: string; //name part only of the required settings file
  public
    property CGIURL: string read FCGIURL write FCGIURL; //Client config: the URL that points to the tiger server
    property ImageDirectory: string read FImageDirectory write FImageDirectory;
    // Directory where scanned images must be/are stored; Has trailing path delimiter.
    property Language: string read FLanguage write FLanguage;
    //Language used for text recognition. Use Tesseract notation. Default English.
    property PDFDirectory: string read FPDFDirectory write FPDFDirectory;
    // Directory where resulting PDFs must be stored; Has trailing path delimiter.
    property ScanDevice: string read FScanDevice write FScanDevice; //Device to be used for scanning (in SANE notation)
    constructor Create;
    constructor Create(SettingsFileName: string);
    destructor Destroy; override;
  end;

implementation

{ TTigerSettings }

constructor TTigerSettings.Create;
begin
  //todo: handle config storage directory /etc on linux etc
  // default settings file unless called with overridden constructor
  if FSettingsFileName = '' then
    FSettingsFileName := SettingsFile;
  FSettings := TINIFile.Create(FSettingsFileName);
  // Default for Apache localhost:
  FCGIURL := 'http://127.0.0.1/cgi-bin/tigercgi/';
  FImageDirectory := '';
  FLanguage := 'eng'; //Default to English
  FPDFDirectory := '';
  FScanDevice := ''; //todo: find if there is some SANE default device name
  try
    FCGIURL := FSettings.ReadString('General', 'CGIURL', FCGIURL);

    // When reading the settings, expand ~ to home directory etc
    FImageDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FSettings.ReadString('General', 'ImageDirectory', '~/scans')));
    //Default to current directory
    FLanguage := FSettings.ReadString('General', 'Language', FLanguage);
    FPDFDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FSettings.ReadString('General', 'PDFDirectory', '~/pdfs')));
    //Default to current directory
    FScanDevice := FSettings.ReadString('Sane', 'DeviceName', '')
  except
    // ignore errors
  end;
  // Fallback to directory where .ini file is stored
  if FImageDirectory = '' then
    FImageDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(FSettingsFileName));
  if FLanguage = '' then
    FLanguage := 'eng';
  if FPDFDirectory = '' then
    FPDFDirectory := FImageDirectory;
end;

constructor TTigerSettings.Create(SettingsFileName: string);
begin
  FSettingsFileName := SettingsFileName;
  Create;
end;

destructor TTigerSettings.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

end.
