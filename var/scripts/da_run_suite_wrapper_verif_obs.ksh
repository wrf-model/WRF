#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper_verif_obs.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite_verif_obs.ksh 
# Author:    Syed RH Rizvi,    NCAR/MMM   
#########################################################################

#Experiment details:
export REGION=AUG2007
export EXPT=CV5_verify        
export CYCLE_PERIOD=12

#Scheduling:

export PROJECT=64000510       # DATC GAUs.
export QUEUE=debug    #  debug #  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
export NUM_PROCS=8 
#export SUBMIT=LSF
export SUBMIT=none
export WALLCLOCK=10       

export NUM_PROCS=1
export RUN_CMD="mpirun -np $NUM_PROCS "
#Time info:

#WRF-Var Directory:   
export WRFVAR_DIR=/karri/users/guo/CODE/VAR/WRFDA           
export BUILD_DIR=$WRFVAR_DIR/var/da

export START_INIT=2007082100
export LAST_INIT=2007082200
export VERIFY_HOUR=24      # 0 for analysis 

# forecast time, not initial time:
export INITIAL_DATE=`$BUILD_DIR/da_advance_time.exe $START_INIT $VERIFY_HOUR`
export FINAL_DATE=`$BUILD_DIR/da_advance_time.exe $LAST_INIT $VERIFY_HOUR`
echo "  ==> FCST INITIAL_DATE= $INITIAL_DATE  FINAL_DATE= $FINAL_DATE"

#DATA Directories:
export DAT_DIR=/karri3/guo/Verif_V3
export FILTERED_OBS_DIR=$DAT_DIR                   
export VERIFICATION_FILE_STRING='wrfout'
export FC_DIR=$DAT_DIR/fc/exp1

export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/${EXPT}_H${VERIFY_HOUR}

export DA_BACK_ERRORS=/karri1/guo/CWB/BES/200708/be.dat
export RC_DIR=$DAT_DIR
export OB_DIR=$DAT_DIR

# Namelist options
export NL_E_WE=222
export NL_E_SN=128
export MAP_PROJ=lambert
export REF_LAT=17.0655
export REF_LON=118.5891
export TRUELAT1=10.0
export TRUELAT2=40.0
export STAND_LON=120.0
export NL_DX=45000
export NL_DY=45000
export NL_E_VERT=45

export NL_SFC_ASSI_OPTIONS=1

#Continuous job

export SCRIPTS_DIR=$WRFVAR_DIR/var/scripts
export SCRIPT=${SCRIPTS_DIR}/da_run_suite_verif_obs.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

