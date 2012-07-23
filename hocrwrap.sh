#!/bin/bash
# hocrwrap.sh
# Create pdf file with hocr with hocr file specified as first parameter

# Pass on all options to hocr2pdf except the first
HOCRFILE="$1"
# get rid of first parameter
shift
hocr2pdf "$@" < $HOCRFILE