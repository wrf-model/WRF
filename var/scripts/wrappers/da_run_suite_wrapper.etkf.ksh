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
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
export RUN_WPS=true
#export RUN_REAL=true
#export RUN_WPB=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_ETKF=true
#export RUN_UPDATE_BC=true
#export RUN_WRF=true
#export RUN_ENSMEAN=true
#export FILE_TYPE=wrfout

#Experiment details:
#export NUM_JOBS=4
#export NUM_MEMBERS=30
#export LBC_FREQ=12
#export NL_HISTORY_INTERVAL=720
#export CYCLE_NUMBER=8
export INITIAL_DATE=2006100100
export FINAL_DATE=2006100100
#export EXPT=exp05
#export CYCLING=true

#ETKF:
#export NACCUMT1=5
#export NSTARTACCUM1=14

#Directories (smoke):
export REL_DIR=/smoke/dmbarker/code/trunk
export DAT_DIR=/smoke/dmbarker/data
export WRFVAR_DIR=$REL_DIR/wrfvar
export WRF_BC_DIR=$REL_DIR/WRF_BC

#From WPS (namelist.wps):
export RUN_GEOGRID=false

#WRF:

#WRF-Var:
#export NL_CHECK_MAX_IV=.false.

export RUN_CMD=" "
$WRFVAR_DIR/var/scripts/da_run_suite.ksh

exit 0

