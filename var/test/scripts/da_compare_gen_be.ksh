#!/bin/ksh
# da_compare_gen_be.ksh
# Purpose: Compare files between gen_be runs

. da_test_defaults.ksh

if [[ $# != 2 ]]; then
   echo "Arguments dir1 dir2"
   exit 1
fi

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

# Text files

FILE[1]=gen_be_diags.log
FILE[2]=gen_be_dias_read.log

COUNT=1

while [[ $COUNT -le ${#FILE[@]} ]]; do
      FILE1=${FILE[$COUNT]}
      FILE2=${FILE[$COUNT]}

   if [[ -f $DIR1/$FILE1 && -f $DIR2/$FILE2 ]]; then
      # Can't use -q option as missing from braindead Aix
      diff $DIR1/$FILE1 $DIR2/$FILE2 > /dev/null 2>&1
      if [[ $? != 0 ]] && $FULL; then
         diff $DIR1/$FILE1 $DIR2/$FILE2
      fi
   fi 
   let COUNT=$COUNT+1
done

# binary files

BINARY_FILE[1]=working/be.dat

COUNT=1

while [[ $COUNT -le ${#BINARY_FILE[@]} ]]; do
      FILE1=${BINARY_FILE[$COUNT]}
      FILE2=${BINARY_FILE[$COUNT]}
   if [[ -f $DIR1/$FILE1 && -f $DIR2/$FILE2 ]]; then
      cmp $DIR1/$FILE1 $DIR2/$FILE2
   fi 
   let COUNT=$COUNT+1
done
