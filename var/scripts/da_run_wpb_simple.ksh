#!/bin/ksh
#########################################################################
# Script: da_run_wpb_simple.ksh
#
# Purpose: Provide an ensemble of WRF lateral boundary condition (wrfbdy)
# files.
#
# Description:
# 1. Requires wrfinput and wrfbdy files already created 
# 2. Run WRF-Var in "randomcv" mode (produces ensemble of perturbed
#    wrfinput_d01 files.
# 3. Loop over 1. and 2. for each time of tendency in wrfbdy file out to
#    forecast length (e.g. 3hourly tendency update in a 72hr forecast).
# 4. Run perturb_wrf_bc to provide perturbed wrfbdy files.
#
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wpb}
export WORK_DIR=$RUN_DIR/working

#------------------------------------------------------------------------------------------

#Save these, as they are overwritten locally:
export DATE_SAVE=$DATE
export END_DATE_SAVE=$END_DATE
export FCST_RANGE_SAVE=$FCST_RANGE
export NL_RUN_HOURS_SAVE=$NL_RUN_HOURS
export RC_DIR_SAVE=$RC_DIR
export NL_ANALYSIS_TYPE_SAVE=$NL_ANALYSIS_TYPE
export RUN_DIR_SAVE=$RUN_DIR

let MEM=1
let JOB=1

while [[ $MEM -le $NUM_MEMBERS ]]; do 
   echo "Producing perturbed wrfbdy files for ensemble member $MEM"
   $SCRIPTS_DIR/da_perturb_wrf_bc.ksh > ${RUN_DIR}/da_perturb_wrf_bc.${MEM}.out 2>&1 &

   let MEM=$MEM+1
   let JOB=$JOB+1

   if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
   sleep 1
done

export END_DATE=$END_DATE_SAVE
export FCST_RANGE=$FCST_RANGE_SAVE
export NL_RUN_HOURS=$NL_RUN_HOURS_SAVE
export NL_ANALYSIS_TYPE=$NL_ANALYSIS_TYPE_SAVE
export RUN_DIR=$RUN_DIR_SAVE

exit 0

