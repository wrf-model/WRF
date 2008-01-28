#!/bin/ksh 
#-auex
#============================================================;
# Purpose:  wrapper for running 
#           $WRFVAR_DIR/scripts/da_verif_obs_plot.ksh
#============================================================;
# Author: Syed RH Rizvi          MMM/NCAR
# Date  : 10/09/2007
#
#============================================================;

export WRFVAR_DIR=/ptmp/rizvi/trunk
export REG_DIR=/ptmp/rizvi/data/amps1
export NUM_EXPT=3
export EXP_DIRS="${REG_DIR}/CY1 ${REG_DIR}/CY2 ${REG_DIR}/CY3"
export EXP_NAMES="CY1 CY2 CY3"
export EXP_LEGENDS='(/"CY1", "CY2", "CY3"/)'
export DESIRED_LEVELS='(/"850","500", "200"/)'
export DESIRED_SCORES='(/"RMSE","BIAS", "ABIAS"/)'
export EXP_LINES_COLORS='(/"blue","green", "orange"/)'
export START_DATE=2006100112
export END_DATE=2006100312

export RUN_DIR=/ptmp/rizvi/verif_obs     

export INTERVAL=12
export Verify_Date_Range="01 - 28 October 2006 (${INTERVAL} hour Cycle)"

export NUM_OBS_TYPE=4
export OBS_TYPES="synop sound airep geoamv"

export PLOT_WKS=pdf
export GET_OMBOMA_PLOTS=false
export CLEAN=false
export FILE_PATH_STRING="wrfvar/working/gts_omb_oma"

#run script
$WRFVAR_DIR/scripts/da_verif_obs_plot.ksh

exit

