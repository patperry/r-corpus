#!/usr/bin/env bash

set -e

FILE="pg18.txt"
FILE_MD5="126eca27b879c078a189ba0783186330"

if [ ! -f $FILE ];
then
    echo "Downloading raw data file '$FILE' from gutenberg.org"
    curl 'http://www.gutenberg.org/cache/epub/18/pg18.txt' -o ${FILE}.download
    mv ${FILE}.download ${FILE}
fi

echo "Checking raw data file '${FILE}'"
md5sum -c - <<< "${FILE_MD5} ${FILE}"
