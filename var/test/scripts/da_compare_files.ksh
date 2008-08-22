#!/bin/ksh
# da_compare_files.ksh

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

COUNT=1

DIFFER=0

if [[ ! -z $TEXT_FILES ]]; then
   for FILE in $TEXT_FILES; do
      if [[ -f $DIR1/$FILE && -f $DIR2/$FILE ]]; then
         # Can't use -q option as missing from braindead Aix
         diff $DIR1/$FILE $DIR2/$FILE >/dev/null 2>&1
         if [[ $? != 0 ]] then
            let DIFFER=$DIFFER+1
            echo "$DIR1/$FILE $DIR2/$FILE differ"
            if $FULL; then
               diff $DIR1/$FILE $DIR2/$FILE
            fi
         fi
      fi 
      let COUNT=$COUNT+1
   done
fi

COUNT=1

if [[ ! -z $NETCDF_FILES ]]; then
   for FILE in $NETCDF_FILES; do
      if [[ -f $DIR1/$FILE && -f $DIR2/$FILE ]]; then
         da_compare_netcdf.ksh $DIR1/$FILE $DIR2/$FILE
         if [[ $? != 0 ]]; then
            let DIFFER=$DIFFER+1
         fi
      fi 
      let COUNT=$COUNT+1
   done
fi

exit $DIFFER

