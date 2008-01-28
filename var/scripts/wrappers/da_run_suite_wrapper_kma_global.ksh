#!/bin/ksh
#########################################################################
# Author:  Syed RH Rizvi    conatct email: rizvi@ucar.edu
# Script: WRAPPER SCRIPT
#
# Script: WRAPPER SCRIPT
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#
# Here are a few examples of environment variables you may want to
# change in da_run_suite_wrapper.ksh:
# 1) "export RUN_WPS=true" runs the WPS).
# 2) "export REL_DIR=$HOME/trunk" points to directory containing all code
#    with components subdirectories e.g. $REL_DIR/wrfvar for WRFVAR code.
# 3) "export START_DATE=2003010100" begins experiment at 00 UTC 1 Jan 2003.
# Full list of environment variables and their default values are
# in da_run_suite.ksh.
#########################################################################

export NUM_PROCS=16
export WALLCLOCK=60
export PROJECT=64000420
export ID=${ID:-trunk_xlf_bluesky}
export EXPT=run_kma_T213_${NUM_PROCS}   

export REL_DIR=/mmm/users/rizvi/code
export DAT_DIR=/ptmp/rizvi/data    

export WRFVAR_DIR=$REL_DIR/wrfvar      

export CLEAN=false
export NL_VAR4D=false


#########################################################################

#Decide which stages to run (run if true):
export RUN_RESTORE_DATA_NCEP=false
export RUN_RESTORE_DATA_RTOBS=false
export RUN_WPS=false
export RUN_REAL=false
export RUN_OBSPROC=false
export RUN_WRFVAR=true
export RUN_UPDATE_BC=false
export RUN_WRF=false



# Fiddle with options here

#export NL_MAX_VERT_VAR1=${MAX_VERT_VAR1:-90.0}
#export NL_MAX_VERT_VAR2=${MAX_VERT_VAR2:-90.0}
#export NL_MAX_VERT_VAR3=${MAX_VERT_VAR3:-90.0}
#export NL_MAX_VERT_VAR4=${MAX_VERT_VAR4:-90.0}
#export NL_MAX_VERT_VAR5=${MAX_VERT_VAR5:-90.0}

export NL_NTMAX=${NL_NTMAX:-100}
export NL_TRACE_USE=true
export NL_TRACE_UNIT=0
#export NL_PRINT_DETAIL_XB=true

# Defines test

export REGION=kma_t213
export INITIAL_DATE=2007020100
export   FINAL_DATE=2007020100
export NL_GLOBAL=true
export NL_E_WE=429
export NL_E_SN=216
export NL_DX=80000
export NL_DY=80000
export NL_E_VERT=41
export NL_FG_FORMAT=3
export NL_DYN_OPT=2

#export NL_USE_AMSUAOBS=true
#export NL_USE_RAD=true
#export NL_USE_FILTERED_RAD=false
#export NL_RTMINIT_NSENSOR=2
#export NL_RTMINIT_PLATFORM=1,1
#export NL_RTMINIT_SATID=15,16
#export NL_RTMINIT_SENSOR=3,3
#export NL_READ_BIASCOEF=true
#export NL_BIASCORR=true
#export NL_WRITE_IV=false
#export NL_WRITE_IV_RAD_ASCII=false
#export NL_WRITE_OA_RAD_ASCII=false
#export NL_WRITE_FILTERED_RAD=false


export EXP_DIR=$DAT_DIR/$REGION/$EXPT
export RUN_DIR=$EXP_DIR
rm -rf $RUN_DIR
export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

