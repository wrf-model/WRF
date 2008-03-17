#!/bin/ksh
#########################################################################

#Experiment details:
export REGION=jme
export EXPT=test
export CYCLE_NUMBER=1
#export CYCLING=true
#export FIRST=false

#Scheduling:
export PROJECT=25000026        # JNT GAUs (1200/month).
export QUEUE=share
export SUBMIT=LSF
export WALLCLOCK=60
#export FCST_RANGE=0

#Time info:
export DATE=2006100312

#Directories:
export REL_DIR=/mmm/users/dmbarker/code/trunk
export DAT_DIR=/rap/datc/data
export WRFVAR_DIR=$REL_DIR/wrfvar
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/ob
export RUN_DIR=$EXP_DIR/run/$DATE/etkf

#WPS (namelist.wps):
#export RUN_GEOGRID=false
export NL_E_WE=122
export NL_E_SN=110
export GEOG_DATA_RES=5m
export REF_LAT=33.00
export REF_LON=137.0
export STAND_LON=130.0
export NL_DX=45000
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_TIME_STEP=240
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

#Following physics options taken from member 1:
export NL_MP_PHYSICS=1
export NL_RADT=45
export NL_W_DAMPING=1
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=4
export NL_MP_ZERO_OUT=0

#WRF (not already covered above):
export NL_INPUTOUT_BEGIN_H=6   #our previous setting: 3       # Output input format start.
export NL_INPUTOUT_END_H=6     #our previous setting: 9       # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6  # AFWA'S original is 6,     (recommend switch to 0, Dave 31 May 2007)

#OBSPROC (not covered above):

#WRF-Var:

#ETKF:
export NUM_MEMBERS=20
export FILE_TYPE=wrfout_d01
export NV=7
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'QVAPOR'"}

# Submit script:
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_etkf.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

