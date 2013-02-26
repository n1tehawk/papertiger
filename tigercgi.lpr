program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigercgimain, tigerservercore, tigerdb, scan, ocr, tigercgi_document;

begin
  Application.Initialize;
  Application.Run;
end.

