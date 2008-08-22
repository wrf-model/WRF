#!/bin/ksh
# da_compare_suite.ksh
# Purpose: Compare files between suite runs

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

for DIR in $DIR1/run/2*; do
   DATE=$(basename $DIR)
   if [[ -d $DIR1/run/$DATE/wrfvar && -d $DIR2/run/$DATE/wrfvar ]]; then
      da_compare_wrfvar.ksh $DIR1/run/$DATE/wrfvar $DIR2/run/$DATE/wrfvar
   fi
   if [[ -d $DIR1/run/$DATE/wps && -d $DIR2/run/$DATE/wps ]]; then
      da_compare_wps.ksh $DIR1/run/$DATE/wps $DIR2/run/$DATE/wps
   fi
   if [[ -d $DIR1/run/$DATE/wrf && -d $DIR2/run/$DATE/wrf ]]; then
      da_compare_wrf.ksh $DIR1/run/$DATE/wrf $DIR2/run/$DATE/wrf
   fi
   if [[ -d $DIR1/run/$DATE/real && -d $DIR2/run/$DATE/real ]]; then
      da_compare_real.ksh $DIR1/run/$DATE/real $DIR2/run/$DATE/real
   fi
done
