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
#set echo 

#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WPS=true
#export RUN_REAL=true
export RUN_WPB=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_ETKF=true
#export RUN_UPDATE_BC=true
#export RUN_WRF=true
#export RUN_ENSMEAN=true
#export FILE_TYPE=wrfout

#Experiment details:
#export DUMMY=${DUMMY:-true}
export REGION=t46_45km_hybrid
export MEMBER=$1
export EXPT=randomcv$MEMBER
#export FCYCLE_HOUR=00
export CLEAN=${CLEAN:-false}
#export CYCLING=${CYCLING:-true}
#export CYCLE_NUMBER=1
#export NUM_JOBS=4
export NUM_MEMBERS=12

#Time info:
export INITIAL_DATE=2006100100
export FINAL_DATE=2006100100
#export LBC_FREQ=03
#export CYCLE_PERIOD=6
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=24
export LONG_FCST_RANGE_2=24
export LONG_FCST_RANGE_3=24
export LONG_FCST_RANGE_4=24

#Directories:
export REL_DIR=/smoke/dmbarker/code/trunk
export DAT_DIR=/smoke/dmbarker/data
export WPS_DIR=$REL_DIR/wps_r237
export WRF_DIR=$REL_DIR/wrf_r2234
export WRFVAR_DIR=$REL_DIR/wrfvar_be

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export DEBUG_LEVEL_UNGRIB=20
export NL_E_WE=54 # 162
export NL_E_SN=71 # 212
export GEOG_DATA_RES=5m
export REF_LAT=37.74
export REF_LON=129.51
export TRUELAT1=60.0
export TRUELAT2=30.0
export STAND_LON=129.51
export NL_DX=45000 # 15000
export NL_DY=45000 # 15000

#WRF:
export NL_HISTORY_INTERVAL=720   # Every 12 hours to save disk space
export NL_INPUTOUT_INTERVAL=360  # Every 6 hours to save disk space
#export NL_INPUTOUT_END_H=6 
export NL_TIME_STEP=240 # 90
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.995, 0.992, 0.983, 0.975, "\ 
                                      " 0.961, 0.949, 0.932, 0.917, 0.897, "\
                                      " 0.878, 0.855, 0.832, 0.806, 0.778, "\
                                      " 0.749, 0.718, 0.687, 0.654, 0.623, "\
                                      " 0.590, 0.559, 0.526, 0.495, 0.462, "\
                                      " 0.431, 0.398, 0.367, 0.334, 0.304, "\
                                      " 0.272, 0.244, 0.213, 0.187, 0.158, "\
                                      " 0.134, 0.107, 0.085, 0.060, 0.040, "\
                                      " 0.018, 0.000 "}
export NL_E_VERT=42
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_MP_ZERO_OUT=0
export NL_CU_PHYSICS=1
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_PD_MOIST=.true.
export NL_RADT=30
export NL_TIME_STEP_SOUND=0  # Registry.EM default is set to 0, whereas Registry.wrefvar is set to 4.
export NL_FEEDBACK=0 
#dampcoef  = 0.01,  # It is set to 0 in Registry.EM and Registry.wrfvar
#specified = .true. # It is set to "false" in Registry.EM!
#export NL_INPUTOUT_INTERVAL=60
export NL_INPUTOUT_BEGIN_H=6   #our previous setting: 3      # Output input format start.
export NL_INPUTOUT_END_H=6     #our previous setting: 9       # Output input format end.

#WRF-Var:
export NL_ANALYSIS_TYPE=randomcv
#export NL_CHECK_MAX_IV=.false.
#export NL_ANALYSIS_TYPE=QC-OBS
#export NL_WRITE_FILTERED_OBS=.true.                 # It is "false" in AFWA's namelist.
#export DA_BACK_ERRORS=/rap/datc/data/t46/be        # There are two be.dat files, be careful!

#ETKF:
#export NACCUMT1=5
#export NSTARTACCUM1=14

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

