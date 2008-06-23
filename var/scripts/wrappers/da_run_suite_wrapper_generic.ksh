#!/bin/ksh
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#
# Description:
#
# Here are a few examples of environment variables you may want to 
# change in da_run_suite_wrapper.ksh:
#
# 1) "export RUN_WRFVAR=true" runs WRFVAR.
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export INITIAL_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see a more complete list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable) then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################
#set echo 

#Decide which stages to run (run if true):
export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
export RUN_WPS=true
export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
export RUN_WRF=true

#Experiment details:
#export CLEAN=${CLEAN:-true}

#Time info:
export INITIAL_DATE=2007010100
export FINAL_DATE=2007013100
export CYCLE_PERIOD=12
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=24
export LONG_FCST_RANGE_2=24
export LONG_FCST_RANGE_3=24
export LONG_FCST_RANGE_4=24


#Directories:
export NUM_PROCS=4
export REGION=con200
export REL_DIR=/karri/users/xinzhang/support
#export DAT_DIR=$REL_DIR/TEST
export WRFVAR_DIR=$REL_DIR/WRFDA
export WRF_DIR=$REL_DIR/WRFV3
export RUN_DIR=$REL_DIR/$REGION/run_cpu${NUM_PROCS}

export DAT_DIR=$REL_DIR
export REG_DIR=$DAT_DIR/$REGION

export OB_DIR=${DAT_DIR}/ob
export RC_DIR=${DAT_DIR}/rc
export BE_DIR=${DAT_DIR}/be

#From WPS (namelist.wps):
export GEOG_DATA_RES='10m'
export WPS_GEOG_DIR=$REL_DIR/WPS_GEOG

#WRF:

#WRF-Var:
#export NL_CHECK_MAX_IV=.false.

export RUN_CMD="mpiexec -n $NUM_PROCS"
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh
exit 0

