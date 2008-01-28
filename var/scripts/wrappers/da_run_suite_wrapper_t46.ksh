#!/bin/ksh 

#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          specific to AFWA Project t46 setting.
#
#  Important to note:
#  * RUN_UNGRIB_METGRID_AFWA should be set to "true" to run AFWA Project
#    specific WPS script.
#
#  * METGRID_TABLE_TYPE should be set to "AFWA" in upper cases to make
#    "metgrid" code to use the AFWA specific METGRID_TABLE.
#
#########################################################################

set echo 

#Decide which stages to run (run if true):
export RUN_WPS=true
#export RUN_REAL=true
#export RUN_WRF=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true

#Experiment details:
export REGION=t46
export EXPT=noda.july
export CYCLE_PERIOD=6
#export CYCLING=true
export FIRST=false

#Scheduling:
export PROJECT=48500053       # JNT GAUs (1200/month).
#export PROJECT=48503001      # DATC GAUs.
export QUEUE=share  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
export NUM_PROCS=1  #64 is for WRF and WRF-VAR # 1 is for WPS, REAL, UPDATE_BC and OBS_PROC
export SUBMIT=LSF
export WALLCLOCK=10

#Time info:
export INITIAL_DATE=2007070300
export FINAL_DATE=2007070300
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=06  #24 #6
export LONG_FCST_TIME_2=06
export LONG_FCST_RANGE_2=12  #48 #72
export LONG_FCST_TIME_3=12
export LONG_FCST_RANGE_3=06  #24 #6
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_4=12  #48 #72
export LBC_FREQ=3

#Directories:
export SCRIPTS_DIR=/rap/datc/demirtas/experiments/afwa/t46/da_scripts
export REL_DIR=/rap/datc/demirtas/code            # ibm
export DAT_DIR=/ptmp/demirtas/data                # ibm

export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC         # Which version? New trunk version.
export WRFVAR_DIR=$REL_DIR/wrfvar_r2522           # r2522.       
export WPS_DIR=$REL_DIR/wps_r237                  # r237 AFWA release.
export WRF_BC_DIR=$REL_DIR/WRF_BC                 # Which version? Web-release version.
export WRF_DIR=$REL_DIR/wrf_r2234                 # r2234 AFWA release.
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/ob

#WPS (namelist.wps):
export RUN_UNGRIB_METGRID_AFWA=true
export METGRID_TABLE_TYPE=AFWA    # Note that it should be in upper cases.
export RUN_GEOGRID=false
export DEBUG_LEVEL=20
export NL_E_WE=162
export NL_E_SN=212
#export NL_E_WE=54 # 15km: 162
#export NL_E_SN=71 # 15km: 212
export REF_LAT=37.74
export REF_LON=129.51
export TRUELAT1=60.0
export TRUELAT2=30.0
export STAND_LON=129.51
export GEOG_DATA_RES=5m
export NL_DX=15000
#export NL_DX=45000 #15km: 15000
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_TIME_STEP=90
#export NL_TIME_STEP=240 #15km: 90
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
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0


#WRF (not already covered above):
export NL_INPUTOUT_BEGIN_H=6   #our previous setting: 3       # Output input format start.
export NL_INPUTOUT_END_H=6     #our previous setting: 9       # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6  # AFWA'S original is 6,     (recommend switch to 0, Dave 31 May 2007)

export NL_FEEDBACK=0            # Not in 2234? 
export NL_ADJUST_HEIGHTS=true   # Not in 2234?
#export NL_NIO_TASKS_PER_GROUP=0 # Not in 2234?
#export NL_NIO_GROUPS=1          # Not in 2234?

#AFWA's latest changes (as of June 2007):

export NL_MP_ZERO_OUT=0    # Check with Dale
export NL_PD_MOIST=.true.  # Check with Dale

#export NL_HISTORY_INTERVAL=720   # Every 12 hours to save disk space # Check with Dale

#OBSPROC (not covered above):
#export TS0=265.   # (AFWA's original is 290.) Since OBSPROC is updated to use base_temp.

#WRF-Var:
#export NL_WRITE_FILTERED_OBS=.true.   # ("false" in AFWA's and Registry.wrfvar.)
#export NL_WRITE_INCREMENTS=.false.
#export NL_USE_SSMIRETRIEVALOBS=.false.
#export NL_VAR_SCALING4=0.01           # How about "NL_VAR_SCALING4=1.0 for V2.2 BEs."?
#export NL_CALCULATE_CG_COST_FN=.true.
#export NL_USE_GPSREFOBS=.true.
#export NL_USE_AIRSRETOBS=.false.

## Meral added the following to macth DATC's settings with AFWA's t46 settings:

#export NL_USE_PROFILEROBS=false   # (AFWA's original)
#export NL_USE_QSCATOBS=false      # (AFWA's original)
#export NL_USE_AMSUAOBS=true       # (AFWA's original) 
#export NL_USE_AIRSRETOBS=false    # (AFWA's original)
#export NL_USE_OBS_ERRFAC=true     # (AFWA's original) ### DALE to check with AFWA.
#export NL_RTM_OPTION=2            # (AFWA's original) It is for CRTM.

#Continuous job 
#export CONTJOB=n
export SCRIPT=${SCRIPTS_DIR}/da_run_suite.ksh
${SCRIPTS_DIR}/da_run_job.ksh


exit 0

