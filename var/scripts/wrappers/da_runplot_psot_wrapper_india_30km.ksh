#!/bin/ksh -aeux
#########################################################################
# Script: runplot_psot_wrapper.ksh
#
# Purpose:  A wrapper script to run/plot pseudo single observation test
#           in indian 30 Km domain
#          
# By Syed RH Rizvi   MMM/NCAR  09/20/2007
#########################################################################
#Option 1- run PSOT; 2- plot PSOT
runplot_psot=1 

#  Submit options
export NUM_PROCS=8
export QUEUE=share
export WALLCLOCK=10
export PROJECT=64000420

#Experiment
export REGION=india_30km
export EXPT=psot

#Directory
export REL_DIR=/ptmp/rizvi/updated_trunk_witout_cv_options_hum
export WRFVAR_DIR=$REL_DIR/trunk     
export DAT_DIR=/mmm/mmmtmp/rizvi/data


export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${REG_DIR}/${EXPT}
export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export BE_DIR=${BE_DIR:-$REG_DIR/be}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}
export SCRIPTS_DIR=$WRFVAR_DIR/scripts

#Time info
export INITIAL_DATE=2005080500

#Domain information
export NL_E_WE=151
export NL_E_SN=151
export NL_E_VERT=51

export NL_DX=30000
export NL_DY=30000
export NL_P_TOP_REQUESTED=1000.

export PSEUDO_VAR_SIZE=5
export PSEUDO_VAR_LIST="u u t t q"
export PSEUDO_VAL_LIST="1.0 1.0 1.0 1.0 0.001"
export PSEUDO_ERR_LIST="1.0 1.0 1.0 1.0 0.001"
export PSEUDO_X_LIST="75 75 75 75 75"
export PSEUDO_Y_LIST="75 75 75 75 75"
export PSEUDO_Z_LIST="12 32 12 22 12"


export NL_ETA_LEVELS=${NL_ETA_LEVELS:-"1.000, 0.994, 0.986, 0.978, 0.968, 0.957, 0.945, \
                                       0.931, 0.915, 0.897, 0.876, 0.854, 0.829, 0.802, \
                                       0.772, 0.740, 0.705, 0.668, 0.629, 0.588, 0.550, \
                                       0.513, 0.478, 0.445, 0.413, 0.383, 0.355, 0.328, \
                                       0.303, 0.279, 0.256, 0.234, 0.214, 0.195, 0.177, \
                                       0.160, 0.144, 0.128, 0.114, 0.101, 0.088, 0.076, \
                                       0.065, 0.055, 0.045, 0.036, 0.028, 0.020, 0.012, \
                                       0.0056, 0.000"}

if (( $runplot_psot == 1 ));then
 $SCRIPTS_DIR/da_run_psot.ksh
else
 $SCRIPTS_DIR/da_plot_psot.ksh
fi

exit 0
