#!/bin/ksh
# da_compare_netcdf.ksh
# Purpose: Compare netcdf files

. da_test_defaults.ksh

if [[ $# != 2 ]]; then
   echo "Arguments file1 file2"
   exit 1
fi

FILE1=$1
FILE2=$2

if [[ ! -f $FILE1 ]]; then
   echo "File $FILE1 does not exist"
   exit 1
fi

if [[ ! -f $FILE2 ]]; then
   echo "File $FILE2 does not exist"
   exit 1
fi

if $FULL; then
   diffwrf_netcdf $FILE1 $FILE2
fi

TMPFILE=/tmp/${USER}.da_compare_netcdf.temp
diffwrf_netcdf $FILE1 $FILE2 | tail +5 > $TMPFILE

MIN_DIGITS=0

if [[ -s $TMPFILE ]]; then
   MIN_DIGITS=100
   { while read A A A A A B A; do
      if [[ $B -lt $MIN_DIGITS ]]; then
         MIN_DIGITS=$B
      fi
   done } < $TMPFILE
else
   export MIN_DIGITS=0
fi
rm -rf $TMPFILE fort.88 fort.98

if [[ $MIN_DIGITS -gt 0 && $MIN_DIGITS -lt $DIGITS ]]; then
   echo $FILE1 $FILE2 differ at $MIN_DIGITS dp
   exit $MIN_DIGITS
else 
   exit 0
fi
