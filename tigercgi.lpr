program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigercgimain, tigerservercore, tigerdb, scan, ocr, tigercgi_document;

begin
  Application.Initialize;
  Application.AllowDefaultModule:=true; //redirect unknown urls to document module handler //todo: how to set default module
  Application.DefaultModuleName:='document'; //suppose this is it?
  Application.Run;
end.

