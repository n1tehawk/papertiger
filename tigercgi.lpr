program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigercgimain, tigerservercore, tigerdb, scan, ocr;

begin
  Application.Initialize;
  Application.Run;
end.

