Paper Tiger
===========
Scanning, text recognition and archiving of paper documents... with GUI clients but from the command line if necessary 

The Paper Tiger code has a liberal MIT license. It uses various other open-source programs.

Functionality
=============
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
- write .deb for Debian servers

Architecture and development principles
- use other people's work if possible - the Unix way...
- if possible, build using modules: e.g. allow use of multiple OCR engines etc
- store OCR text in the PDF, and store the image tiff to enable external tools to work with the PDFs, use the PDFs in other applications etc.
- save all OCR text in database or file (e.g. a Lucene index) in order to allow fast search across all documents
- this means synchronizing PDF text with the full text archive may be required
- develop towards a single point of control: tigerserver, which may speak multiple protocols, e.g. via plugins
- however, use standard methods of storing data (e.g. full text search components), normalized database schema in order to allow programs/tools that don't speak the protocols mentioned above to get data easily
- these 2 principles clash; the code will need to stabilize until it is wise to directly try to access e.g. the databsse. Even then, breaking changes will not be avoided if e.g. cleanness of design would be compromised


Installation instructions
=========================
- prerequisites: Linux machine. Windows support may come later.
- prerequisites: have sane installed and configured for your scanner. E.g.:
  aptitude install sane-utils
- prerequisites: have tesseract installed and configured. E.g.:
  aptitude install tesseract-ocr tesseract-ocr-eng #for English language support
  Note: we need version 3 because of hOCR support needed for getting searchable PDFs.
- prerequisites: have exactimage installed (for hocr2pdf), e.g.:
  aptitude install exactimage
- prerequirisites: have scantailor installed (for aligning/cleaning up the tiff images before OCR)
  see installation notes below
- prerequisites if using the cgi server/client server solution: apache2 or another HTTP server that supports cgi
  aptitude install apache2
	copy tigercgi to cgi directory (e.g. /usr/lib/cgi-bin). 
	Make sure the user Apache runs under may read and execute the file (e.g. chmod ugo+rx tigercgi)
- Tesseract must/can then be configured to output hocr, e.g.:
  check you have this file present (adjust config directory to your situation):
  cat /usr/local/share/tessdata/configs/hocr
  If not (again, adjust config file location to your situation):
  cat >> /usr/local/share/tessdata/configs/hocr << "EOF_DOCUMENT"
  tessedit_create_hocr 1
  EOF_DOCUMENT
- copy hocrwrap.sh to server directory (e.g. /opt/tigerserver/)
- copy tigerserver to server directory
- copy scanwrap.sh to server directory
- go to the server directory and make files executable, e.g. (replace directory with your own if necessary):
  cd /opt/tigerserver/
  chmod u+rx hocrwrap.sh
  chmod u+rx scanwrap.sh
  chmod u+rx tigerserver
- copy tigerserver.ini.template to tigerserver.ini and edit settings to match your environment

Test by running ./tigerserver --help

Building Tesseract 3
====================
If tesseract 3 is not available for your platform, you will need to build it.  
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

Building scantailor from source
===============================
Scantailor is being developed; we use the scantailor enhanced fork.
http://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
Build instructions:
https://sourceforge.net/apps/mediawiki/scantailor/index.php?title=Building_from_source_code_on_Linux_and_Mac_OS_X

Notes for Debian below.

# compilers and dependencies
aptitude install build-essential cmake libqt4-dev libjpeg-dev zlib1g-dev libpng-dev libtiff-dev libboost-dev libxrender-dev 

Get source from
cd ~
wget http://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/scantailor-enhanced-20120914-pre.tar.bz2/download
mv download scantailor-enhanced-20120914-pre.tar.bz2
tar xvjf scantailor-enhanced-20120914-pre.tar.bz2
rm scantailor-enhanced-20120914-pre.tar.bz2
cd scantailor-enhanced-20120914-pre
cmake .
=> gives problems with boost?!?

Miscellaneous notes
===================
Getting PDF viewers to open a certain page:
Adobe Acrobat Reader
http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_open_parameters.pdf
acrobat.exe /A "page=<pageNo>"
could also use "nameddest=<named destination>"

SumatraPDF
https://code.google.com/p/sumatrapdf/wiki/CommandLineArguments
sumatrapdf -reuse-instance -page <pageNo>
Scrolls the first indicated file to the indicated page.
Tells an already open SumatraPDF to load the indicated files. If there are several running instances, behavior is undefined.
