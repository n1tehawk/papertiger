#!/bin/bash
# scanwrap.sh
# Scan using scanimage with output file specified as first parameter

# Pass on all options to scanimage except the first
OUTPUTFILE="$1"
# get rid of first parameter, the output filename
shift
# call sane with the rest of the parameters:
# workaround for issue
# http://arch.debian.org/tracker/?func=detail&atid=410366&aid=313851&group_id=30186
scanimage "$@" | grep -v "Failed cupsGetDevices" > $OUTPUTFILE 
