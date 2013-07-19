#!/bin/bash
# Detects whether text in a file is actually proper text or gibberish
# created by OCR.
# Useful when detecting page orientation

# Input: argument 1: text file to process
# Output: /tmp/detectlog.txt

# Could use another spell library instead of hunspell, too
echo "# Processing file " $1 > /tmp/detectlog.txt
echo "# Number of words in file:" >> /tmp/detectlog.txt
cat "$1" | wc -w >> /tmp/detectlog.txt
echo "# Number of incorrectly spelled words from spell check: " >> /tmp/detectlog.txt
cat "$1" | hunspell -l -i utf-8 | wc -w >> /tmp/detectlog.txt
