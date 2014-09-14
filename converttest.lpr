program converttest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Sysutils,Classes,tigerutil;

const
  InputFile='scan_24bpp.bmp';
  OutputFile='scan_result.tiff';
begin
  if FileExists(outputfile) then
    DeleteFile(outputfile);
  tigerutil.ConvertTIFFCCITT4(InputFile,OutputFile);
end.

