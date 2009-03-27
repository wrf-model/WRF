#!/bin/ksh

# Called from da_transform_vtoy_adj inside WRFVAR. The script is only 
# temporary until we can couple the models through memory rather than files
#
# VAR provides files af00 to af06
#
# WRF produces gradient file wrfvar_input_d${DOMAIN}_2000-01-25_00:00:00 which
# is renamed gr00 for VAR

# Preserve wrfvar namelist.input

arg1=$1

WORK_DIR=`pwd`
cd $WORK_DIR/ad

if [[ $NUM_PROCS -eq 1 ]]; then
   ./wrfplus.exe > wrf_ad.out 2>wrf_ad.error
else
   if [[ $arg1 == "pre" ]]; then
      mv -f ../namelist.input ../namelist_wrfvar.input
      cp -f namelist.input ../.
      if [[ $NL_MULTI_INC == 2 ]]; then
         ln -fs $DA_FIRST_GUESS $WORK_DIR/wrfinput_d01
      fi
   fi
   if [[ $arg1 == "post" ]]; then
      mv ../namelist.output .
      mv -f ../namelist_wrfvar.input ../namelist.input
      if [[ $NL_MULTI_INC == 2 ]]; then
         ln -fs $WORK_DIR/wrfinput_d01-thin $WORK_DIR/wrfinput_d01
      fi
   fi
fi

