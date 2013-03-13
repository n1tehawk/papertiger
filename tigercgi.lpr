program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigerutil, tigercgimain, tigercgi_document, tigercgi_image, tigerservercore, tigerdb, scan, ocr;

begin
  Application.Initialize;
  Application.AllowDefaultModule:=false; //do not redirect unknown urls to document module handler
  if Application.EventLog=nil then ;  //initialize event log
  Application.Run;
end.

