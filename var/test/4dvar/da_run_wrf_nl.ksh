#!/bin/ksh

# Called from da_solve inside WRFVAR. The script is only temporary until we
# can couple the models through memory rather than files
#
# When called the first time, wrfvar_output does not exist, so use the
# existing wrfvar_input as the initial file
#
# The T+0 input to VAR is wrfinput_d${DOMAIN} described above.
#
# The other input files for VAR are the T+1,T+2,T+3 dumps

arg1=$1

WORK_DIR=`pwd`
cd $WORK_DIR/nl

export G95_UNIT_ENDIAN_98=BIG

if [[ $NUM_PROCS == 1 ]]; then
   mv ../namelist.output ../namelist_wrfvar.output
   ./wrf.exe > wrf_nl.out 2>wrf_nl.error
else
   if [[ $arg1 == "pre" ]]; then
      mv ../namelist.output ../namelist_wrfvar.output
      mv -f ../namelist.input ../namelist_wrfvar.input
      cp -f namelist.input ../.
   fi
   if [[ $arg1 == "post" ]]; then
      mv ../namelist.output .
      mv -f ../namelist_wrfvar.input ../namelist.input
   fi
fi

exit 0


