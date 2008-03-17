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
# 1) "export RUN_WRFVAR=true" runs WRFVAR).
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export INITIAL_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################
set echo 

#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WPS=true
export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
#export RUN_WRF=true

#Experiment details:
#export NUM_PROCS=8
export RUN_CMD=" "
#export DUMMY=${DUMMY:-true}
export REGION=asr
export EXPT=noda
#export CLEAN=true
#export CYCLING=${CYCLING:-true}
#export CYCLE_PERIOD=6
#export FIRST=false

#Time info:
export INITIAL_DATE=2006100100
export FINAL_DATE=2006100100

#Directories:
#snowdrift:
export REL_DIR=/data1/$USER/code/trunk
export DAT_DIR=/data3/$USER/data
export WRF_BC_DIR=/data1/dmbarker/code/WRF_BC
export WRFVAR_DIR=$REL_DIR/wrfvar

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export NL_E_WE=450
export NL_E_SN=450 
export MAP_PROJ=polar
export REF_LAT=90.0
export REF_LON=0.0 # 180 in AMPS
export TRUELAT1=71.0
export TRUELAT2=91.0
export STAND_LON=0.0 # 180 in AMPS
export NL_DX=20000 # 60000 for AMPS.
export NL_DY=20000 # 60000 for AMPS.

#WRF:
export NL_TIME_STEP=90 #240s for AMPS.
export NL_E_VERT=31
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.993, 0.980, 0.966, 0.950, "\ 
                                      " 0.933, 0.913, 0.892, 0.869, 0.844, "\
                                      " 0.816, 0.786, 0.753, 0.718, 0.680, "\
                                      " 0.639, 0.596, 0.550, 0.501, 0.451, "\
                                      " 0.398, 0.345, 0.290, 0.236, 0.188, "\
                                      " 0.145, 0.108, 0.075, 0.046, 0.021, 0.000 "}

export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=8 # 4 for AMPS
export NL_RADT=10
export NL_CU_PHYSICS=3 # AMPS = 1
export NL_RA_SW_PHYSICS=2 # AMPS = 1
export NL_SF_SFCLAY_PHYSICS=1
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_BL_PBL_PHYSICS=1
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_BASE_TEMP=268.0
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=4

#WRF-Var:
#export NL_CHECK_MAX_IV=.false.

$WRFVAR_DIR/var/scripts/da_run_suite.ksh

exit 0

