#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper_amps2.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          for AMPS domain 2 (20km).

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
#
# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
# 
#########################################################################

export RUN_WPS=true
export RUN_REAL=true
#export RUN_OBSPROC=true
export RUN_WRF=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
#export CYCLING=true
#export FIRST=true

#Experiment details:
export REGION=amps2
export EXPT=noda
export CYCLE_PERIOD=12
#export CYCLING=true
#export FIRST=false
export CLEAN=true

#Scheduling:
export PROJECT=25000026        # JNT GAUs (1200/month).
#export PROJECT=48500053       # JNT GAUs (1200/month).
#export PROJECT=48503001       # DATC GAUs.
export QUEUE=premium
#export QUEUE=regular
#export QUEUE=share
export NUM_PROCS=64
export SUBMIT=LSF
export WALLCLOCK=180

#Time info:
export INITIAL_DATE=2006101612
export FINAL_DATE=2006102800
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=24
export LONG_FCST_TIME_2=12
export LONG_FCST_RANGE_2=24
export NL_HISTORY_INTERVAL=720

#Directories:
export REL_DIR=/rap/datc/code
export DAT_DIR=/rap/datc/data
export FG_TYPE=GFS
export GRIB_DIR=$DAT_DIR/$FG_TYPE
export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC         # Which version?
#export WRFVAR_DIR=$REL_DIR/wrfvar_r2522           # r2522.
export WRFVAR_DIR=/mmm/users/dmbarker/code/trunk/wrfvar_afwa_2.2 # AFWA branch.
export WPS_DIR=$REL_DIR/wps_r237                  # r237 AFWA release.
export WRF_BC_DIR=$REL_DIR/WRF_BC                 # Which version?
export WRF_DIR=$REL_DIR/wrf_r2234                 #
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/ob

#WPS (namelist.wps):
export RUN_GEOGRID=false
export NL_E_WE=331
export NL_E_SN=313 
export MAP_PROJ=polar
export REF_LAT=-51.0329
export REF_LON=49.8218
export REF_X=1
export REF_Y=1
export TRUELAT1=-71.0
export TRUELAT2=-91.0
export STAND_LON=180.0
export NL_DX=20000
export NL_DY=20000

#WRF real (not already covered above):
export NL_P_TOP_REQUESTED=5000.
export NL_TIME_STEP=90
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.993, 0.980, 0.966, 0.950, "\
                                      " 0.933, 0.913, 0.892, 0.869, 0.844, "\
                                      " 0.816, 0.786, 0.753, 0.718, 0.680, "\
                                      " 0.639, 0.596, 0.550, 0.501, 0.451, "\
                                      " 0.398, 0.345, 0.290, 0.236, 0.188, "\
                                      " 0.145, 0.108, 0.075, 0.046, 0.021, 0.000 "}
export NL_E_VERT=31
export NL_MP_PHYSICS=4
export NL_RADT=10
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_BASE_TEMP=268.
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=4
export NL_MP_ZERO_OUT=0

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

