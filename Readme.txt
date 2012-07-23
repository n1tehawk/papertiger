Paper Tiger:
scanning, text recognition and archiving of paper documents... with GUI clients but from the command line if necessary 

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

Architecture and development principles
- use other people's work if possible - the Unix way...
- if possible, build using modules: e.g. allow use of multiple OCR engines etc
- store OCR text in the PDF as well as the tiff to enable external tools to work with the PDFs, use the PDFs in other applications etc.
- save all OCR text in database or file (e.g. a Lucene index) in order to allow fast search across all documents
- this means synchronizing PDF text with the full text archive may be required
- develop towards a single point of control: tigerserver, which may speak multiple protocols, e.g. via plugins
- however, use standard methods of storing data (e.g. full text search components), normalized database schema in order to allow programs/tools that don't speak the protocols mentioned above to get data easily
- these 2 principles clash; the code will need to stabilize until it is wise to directly try to access e.g. the databsse. Even then, breaking changes cannot and will not be avoided (if e.g. cleanness of design would be compromised)


Installation instructions:
- prerequisites: have sane installed and configured for your scanner. E.g.:
  aptitude install sane-utils
- prerequisites: have tesseract installed and configured. E.g.:
  aptitude install tesseract-ocr tesseract-ocr-eng #for English language support
  Note: we need version 3 because of hOCR support needed for getting searchable PDFs.
- prerequisites: have exactimage installed (for hocr2pdf), e.g.:
  aptitude install exactimage
- todo: check if specifying hocr works, too - we seem to get language problems?!?
  Tesseract must/can then be configured to output hocr, e.g.:
  cat >> /usr/local/share/tessdata/configs/hocr << "EOF_DOCUMENT"
  tessedit_create_hocr 1
  EOF_DOCUMENT
- copy server program e.g. to /opt/tigerserver/
- copy scanwrap.sh to server directory
- go to the server directory, run:
  chmod u+rx tigerserver
  chmod u+rx scanwrap.sh

  
Preliminary notes for building Tesseract 3 on Debian aqueeze
sources:
http://ubuntuforums.org/showthread.php?t=1647350
aptitude install build-essential leptonica libleptonica-dev libpng-dev libjpeg-dev libtiff-dev zlib1g-dev

# as root:
cd ~
wget https://tesseract-ocr.googlecode.com/files/tesseract-3.01.tar.gz
tar -zxvf tesseract-3.01.tar.gz
cd tesseract-3.01
./runautoconf
./configure
make
checkinstall (follow the prompts and type "y" to create documentation directory. Enter a brief description then press enter twice)
ldconfig

#language/training data, e.g. for Dutch and English:
#todo: check dir
cd /usr/local/share/tessdata
wget https://tesseract-ocr.googlecode.com/files/nld.traineddata.gz
gunzip nld.traineddata.gz
wget https://tesseract-ocr.googlecode.com/files/tesseract-ocr-3.01.eng.tar.gz
gunzip tesseract-ocr-3.01.eng.tar.gz
