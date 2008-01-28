#!/bin/ksh 
#########################################################################
# Script: runplot_psot_wrapper.ksh
#
# Purpose:  A wrapper script to run/plot pseudo single observation test
#           in CAA1 domain
#          
# By Hui Shao, NCAR DATC 08/27/2007
#########################################################################
#Option 1- run PSOT; 2- plot PSOT
runplot_psot=2 

#Experiment
export REGION=caa1
export EXPT=psot
export NUM_PROCS=2
export QUEUE=share
export WALLCLOCK=10

#Directory
export REL_DIR=/rap/datc/huishao/code/trunk_blueice
export WRFVAR_DIR=$REL_DIR/wrfvar_r2522_caa	
export DAT_DIR=/ptmp/huishao/data
export RC_DIR=/rap/datc/huishao/data/caa1/rc
export OB_DIR=/rap/datc/huishao/data/caa1/ob
export BE_DIR=/rap/datc/huishao/data/caa1/be
export SCRIPTS_DIR=$WRFVAR_DIR/scripts

#Time info
export INITIAL_DATE=2006120100
export FINAL_DATE=2006120100

#Domain information
export NL_E_WE=222
export NL_E_SN=128
export NL_DX=45000
export NL_DY=45000
export NL_P_TOP_REQUESTED=3000.  
export NL_E_VERT=45
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.995, 0.988, 0.98 , 0.97 , "\
                                      " 0.96 , 0.945, 0.93 , 0.91 , 0.89 , "\
                                      " 0.87 , 0.85 , 0.82 , 0.79 , 0.76 , "\
                                      " 0.73 , 0.69 , 0.65 , 0.61 , 0.57 , "\
                                      " 0.53 , 0.49 , 0.45 , 0.41 , 0.37 , "\
                                      " 0.34 , 0.31 , 0.28 , 0.26 , 0.24 , "\
                                      " 0.22 , 0.20 , 0.18 , 0.16 , 0.14 , "\
                                      " 0.12 , 0.10 , 0.082, 0.066, 0.052, "\
                                      " 0.040, 0.030, 0.020, 0.010, 0.000  "}

#WRFVAR namelist (e.g., len_scaling, var_scaling)

#Single observation(s) (X,Y and Z are grid/level indice of the obs locations)  
export PSEUDO_VAR_SIZE=5 
#export PSEUDO_VAR_LIST="u u t t q" #default
#export PSEUDO_VAL_LIST="1.0 1.0 1.0 1.0 0.001" #default
#export PSEUDO_ERR_LIST="1.0 1.0 1.0 1.0 0.001" #default
export PSEUDO_X_LIST="112 112 112 112 112"
export PSEUDO_Y_LIST="64 64 64 64 64"
export PSEUDO_Z_LIST="12 29 12 21 12"

if (( $runplot_psot == 1 ));then
 $SCRIPTS_DIR/da_run_psot.ksh
else
 $SCRIPTS_DIR/da_plot_psot.ksh
fi

exit 0
