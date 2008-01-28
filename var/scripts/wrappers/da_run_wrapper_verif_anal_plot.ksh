#!/bin/ksh 
#-auex
#============================================================;
# Purpose:  wrapper for running 
#           $WRFVAR_DIR/scripts/da_verif_anal_plot.ksh
#============================================================;
# Author: Syed RH Rizvi          MMM/NCAR
# Date  : 10/14/2007
#
#============================================================;
export WRFVAR_DIR=/ptmp/rizvi/trunk
export REG_DIR=/ptmp/rizvi/data/t46     
export RUN_DIR=/ptmp/rizvi/t46_verif

export NUM_EXPT=2
export EXP_DIRS="${REG_DIR}/noda ${REG_DIR}/full_cycling_rad"
export CONTROL_EXP_DIR='/ptmp/rizvi/data/t46/noda'
export EXP_NAMES="noda full_cycling_rad"
export VERIFICATION_FILE_STRING='wrfout'                   
export EXP_LEGENDS='(/"NODA","CY_RAD"/)'     
export DESIRED_LEVELS='850 500 200'
export DESIRED_SCORES='(/"RMSE","BIAS", "ABIAS"/)'
export EXP_LINES_COLORS='(/"blue","green", "orange"/)'
export VERIFY_HOUR=36
export INTERVAL=12

export START_DATE=2007070212
export END_DATE=2007071412 
export VERIFY_ITS_OWN_ANALYSIS=true

export Verify_Date_Range="1-14 July 2007 (${INTERVAL} hour Cycle)"      

export PLOT_WKS=pdf
export CLEAN=false

#run script
$WRFVAR_DIR/scripts/da_verif_anal_plot.ksh

exit

