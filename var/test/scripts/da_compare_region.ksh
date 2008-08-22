#!/bin/ksh
# da_compare_region.ksh
# Purpose: Compare files between regions

DIR1=$1
DIR2=$2

if [[ ! -d $DIR1 ]]; then
   echo "Directory $DIR1 does not exist"
   exit 1
fi

if [[ ! -d $DIR2 ]]; then
   echo "Directory $DIR2 does not exist"
   exit 1
fi

for DIR in $DIR1/*; do
   SUITE=$(basename $DIR)
   if [[ -d $DIR1/$SUITE/run && -d $DIR2/$SUITE/run ]]; then
      da_compare_suite.ksh $DIR1/$SUITE $DIR2/$SUITE
   fi
done
