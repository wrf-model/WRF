#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          specific to KMA Project setting.
#
#  Important to note:
#  * LBC frequency is not the same for all run hours as listed:
#     - LBC_FREQ=12 # For 00/12Z KMA runs
#     - LBC_FREQ=6  # For 06/18Z KMA runs
#   The user may run this script separately for above listed run hours
#   separately to accommodate different settings of LBC frequencies. 
#
#########################################################################

#Decide which stages to run (run if true):
export RUN_WPS=false
export RUN_REAL=false  
export RUN_WRFVAR=true  
export RUN_UPDATE_BC=false
export RUN_WRF=false 

#Experiment details:
export REGION=t8
export RUN_UNGRIB_METGRID_AFWA=true  
export METGRID_TABLE_TYPE=AFWA
export FG_TYPE=GFS
export EXPT=noda        
export CYCLE_PERIOD=12
export CYCLING=false
#export FIRST=false
export CLEAN=true 

#Scheduling:
export PROJECT=64000510       # DATC GAUs.
export QUEUE=debug    #  debug #  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
#export QUEUE=premium    # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
export NUM_PROCS=8 
export SUBMIT=LSF
export WALLCLOCK=10       
export PREV_JOBNAME=${EXPT}_job0
export JOBNAME=${EXPT}_job1
export SUBMIT_OPTIONS1="#BSUB -w \"done(${PREV_JOBNAME})\""
export SUBMIT_OPTIONS1=" "

#Time info:

export INITIAL_DATE=2007081500
export FINAL_DATE=2007081500 
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=48 
export LONG_FCST_TIME_2=12
export LONG_FCST_RANGE_2=48
export LBC_FREQ=3  # For 06/18Z KMA runs

#Directories:
export DAT_DIR=/ptmp/rizvi/data

export REL_DIR=/ptmp/rizvi                   
export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC         # Which version?
export WRFVAR_DIR=$REL_DIR/trunk           
#export WPS_DIR=$REL_DIR/wps_r237               # r237 AFWA release.
#export WPS_DIR=/blhome/demirtas/code/wps_r237   # r237 AFWA release.
export WPS_DIR=/mmm/users/rizvi/code/wps_r237               # r237 AFWA release.
export WRF_BC_DIR=/rap/datc/code/WRF_BC                 # Which version?
export WRF_DIR=$REL_DIR/wrf_dfi_ndown
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export RC_DIR=$REG_DIR/rc
export RUN_DIR=$EXP_DIR
export OB_DIR=$REG_DIR/ob
export GRIB_DIR=$DAT_DIR/$FG_TYPE

#WPS (namelist.wps):
export MAP_PROJ=mercator
export RUN_GEOGRID=false 
export NL_E_WE=361
export NL_E_SN=325
export REF_LAT=20.0
export REF_LON=-75.0
export TRUELAT1=20.0
export TRUELAT2=20.0
export STAND_LON=-75.00
export GEOG_DATA_RES=5m
export NL_DX=15000
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_TIME_STEP=60
#export NL_TIME_STEP=240 #15km: 90
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.00000,0.99730,0.99157,0.98506,0.97772, "\
                                      " 0.96943,0.96010,0.94962,0.93789,0.92478, "\
                                      " 0.91020,0.89403,0.87618,0.85657,0.83514, "\
                                      " 0.81183,0.78664,0.75960,0.73078,0.70027, "\
    " 0.66825,0.63491,0.60048,0.56527,0.52957,0.49370,0.45802,0.42281,0.38842, "\
    " 0.35510,0.32311,0.29265,0.26386,0.23686,0.21172,0.18845,0.16706,0.14749, "\
    " 0.12968,0.11355,0.09901,0.08593,0.07423,0.06378,0.05447,0.04621,0.03888, "\
    " 0.03240,0.02668,0.02163,0.01718,0.01328,0.00984,0.00682,0.00418,0.00186, "\
    " 0.00000 "}
export NL_E_VERT=57
export NL_P_TOP_REQUESTED=1000
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0

#WRF (not already covered above):
export NL_HISTORY_INTERVAL=720      # Every 12 hours to save disk space
export NL_INPUTOUT_INTERVAL=360     # Every 6 hours to save disk space
export NL_INPUTOUT_BEGIN_H=6        # our previous setting: 3       # Output input format start.
export NL_INPUTOUT_END_H=6   #6     # our previous setting: 9       # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6    # AFWA'S original is 6,     (recommend switch to 0, Dave 31 May 2007)

export NL_FEEDBACK=0            # Not in 2234?
export NL_ADJUST_HEIGHTS=true   # Not in 2234?
#export NL_NIO_TASKS_PER_GROUP=0 # Not in 2234?
#export NL_NIO_GROUPS=1          # Not in 2234?

#AFWA's latest changes (as of June 2007):

export NL_MP_ZERO_OUT=0
export NL_PD_MOIST=.true.

#OBSPROC (not covered above):
#export TS0=265.   # (AFWA's original is 290.) Since OBSPROC is updated to use base_temp.

#WRF-Var:
#export NL_WRITE_FILTERED_OBS=true.# ("false" in AFWA's and Registry.wrfvar.)
#export NL_WRITE_INCREMENTS=.false.
export NL_USE_SSMIRETRIEVALOBS=true
#export NL_VAR_SCALING4=0.01  # Meral how about "NL_VAR_SCALING4=1.0 for V2.2 BEs."?
#export NL_CALCULATE_CG_COST_FN=.true.
export NL_USE_GPSREFOBS=false
export NL_USE_AIRSRETOBS=false
export NL_LAT_STATS_OPTION=.false.

## Meral added the following to match DATC setting with AFWA's t46 settings:

#export NL_USE_PROFILEROBS=false   # (AFWA's original)
export NL_USE_QSCATOBS=false      # (AFWA's original)
#export NL_USE_AIRSRETOBS=false    # (AFWA's original)
#export NL_USE_OBS_ERRFAC=true     # (AFWA's original) ### DALE to check with AFWA.

#Continuous job

export SCRIPTS_DIR=$WRFVAR_DIR/var/scripts
export SCRIPT=${SCRIPTS_DIR}/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

