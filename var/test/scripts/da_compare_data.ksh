#!/bin/ksh
# da_compare_data.ksh
# Purpose: Compare top level data directories

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
   REG=$(basename $DIR)
   if [[ -d $DIR1/$REG && -d $DIR2/$REG ]]; then
      da_compare_region.ksh $DIR1/$REG $DIR2/$REG
   fi
done
