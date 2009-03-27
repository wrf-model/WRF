#!/bin/ksh

# Called from da_minimisation inside WRFVAR. The script is only temporary 
# until we can couple the models through memory rather than files

# VAR produces tl00 used as input wrfinput_d${DOMAIN}

# WRF produces 3 timestep files for T+1 to T+3 hardwired to
# 2000-01-25 by the namelist file, renamed to tl01 to tl03 for
# use by VAR

# Preserve wrfvar namelist.input

arg1=$1

WORK_DIR=`pwd`
cd $WORK_DIR/tl

if [[ $NUM_PROCS -eq 1 ]]; then
   ./wrfplus.exe > wrf_tl.out 2>wrf_tl.error
else
   if [[ $arg1 == "pre" ]]; then
      mv -f ../namelist.input ../namelist_wrfvar.input
      cp -f namelist.input ../.
      ln -fs $WORK_DIR/tl01 $WORK_DIR/wrfinput_d01
   fi
   if [[ $arg1 == "post" ]]; then
      mv ../namelist.output .
      mv -f ../namelist_wrfvar.input ../namelist.input
      ln -fs $DA_FIRST_GUESS $WORK_DIR/wrfinput_d01
      if [[ $NL_MULTI_INC == 2 ]]; then
         ln -fs $WORK_DIR/wrfinput_d01-thin $WORK_DIR/wrfinput_d01
      fi
   fi
fi
