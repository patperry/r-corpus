#!/usr/bin/env bash

set -e

FILE="imm6010.zip"
FILE_MD5="ea6216f43d27188ea2b5bfadf068ff37"

if [ ! -f $FILE ];
then
    echo "Downloading raw data file '$FILE' from www2.imm.dtu.dk"
    curl 'http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/6010/zip/imm6010.zip' -o ${FILE}.download
    mv ${FILE}.download ${FILE}
fi

if [ ! -d AFINN ];
then
    unzip ${FILE}
fi
