// Common code for tiger server/client/cgi in implementation section
const
  INVALIDID=-1; //Used to return invalid primary key ids for (db) objects where integers are used
  TESSERACTTIFFEXTENSION='.tif'; //Extension for .tiff files. Needs to be this for tesseract support.
  //todo: perhaps write FPC wrapper instead of bash scanwrap.sh
  ScanCommand = './scanwrap.sh'; //for command line use
  TextDetectCommand = './textdetect.sh'; //for counting all words and the number of incorrectly spelled words

