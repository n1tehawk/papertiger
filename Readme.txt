Paper Tiger:

The Paper Tiger code has a liberal MIT license. It uses various other open-source programs.

Planned architecture/functionality:
- scanning documents using sane into TIFF documents
- OCR/text recognition using Tesseract
- storage of documents as PDF file (image file+OCR text) e.g. on a Samba share
- index of documents+notes+full text in Firebird database
- GUI viewer/scanner controls written in Lazarus+FreePascal
- server component written in FreePascal, so no X Windows required

Further possible refinements:
- support for other databases (sqlite, PostgreSQL, MS SQL Server)
- using image cleanup tools such as unpaper
- using WIA etc on Windows to support Windows scanners


Installation instructions:
- prerequisites: have sane installed:
  aptitude install sane
- copy server program e.g. to /opt/tigerserver/
- copy scanwrap.sh to server directory
- go to the server directory, run:
  chmod u+rx tigerserver
  chmod u+rx scanwrap.sh
  