program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigerutil, tigercgimain, tigercgi_document, tigercgi_image, tigerservercore, tigerdb, scan, ocr;

begin
  Application.Initialize;
  Application.AllowDefaultModule:=true; //redirect unknown urls to document module handler //todo: how to set default module
  {$IF FPC_FULLVERSION>=20701}
  Application.DefaultModuleName:='document'; //suppose this is it?
  {$ENDIF}
  if Application.EventLog=nil then ;  //initialize event log
  Application.Run;
end.

