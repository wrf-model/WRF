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
export NCARG_ROOT=/karri/local/ncl-4.2.0.a034

export WRFVAR_DIR=/karri/users/guo/CODE/VAR/WRFDA/var
export BUILD_DIR=$WRFVAR_DIR/da

export REG_DIR=/karri3/guo/Verif_V3/AUG2007
export NUM_EXPT=3
export EXP_DIRS="${REG_DIR}/CV5_verify_H24 ${REG_DIR}/CV5_verify_H24a ${REG_DIR}/CV5_verify_H24b"
export EXP_NAMES="CV5_24 CV5_24a CV5_24b"
export EXP_LEGENDS='(/"CV524","CV524A","CV524B"/)'
export DESIRED_LEVELS='(/"850","500", "200"/)'
export DESIRED_SCORES='(/"RMSE","BIAS", "ABIAS"/)'
export EXP_LINES_COLORS='(/"blue", "red", "black"/)'
export START_DATE=2007082200
export END_DATE=2007082300

export RUN_DIR=$REG_DIR     

export INTERVAL=12
export Verify_Date_Range="22 - 23 August 2007 (${INTERVAL} hour Cycle)"

export NUM_OBS_TYPE=2
export OBS_TYPES="synop sound"

export PLOT_WKS=pdf
export GET_OMBOMA_PLOTS=false
export CLEAN=false
export FILE_PATH_STRING="wrfvar/gts_omb_oma"

#run script
export SCRIPTS_DIR=$WRFVAR_DIR/scripts
$SCRIPTS_DIR/da_verif_obs_plot.ksh

exit

