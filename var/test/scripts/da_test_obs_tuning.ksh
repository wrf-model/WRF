#!/bin/ksh

. ./setup.ksh
. ${CASE}/setup.ksh

. da_test_defaults.ksh

export REG_DIR=$PWD

TUNING_DIR=$PWD/${MACHINE}_tuning
rm -rf $TUNING_DIR; mkdir -p $TUNING_DIR; cd $TUNING_DIR

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export WRFVAR_DIR=${WRFVAR_DIR:-~/code/${ID}/wrfvar}

export UNPERT_ID=${MACHINE}_${COMPILER}_opt
export PERT_ID=${MACHINE}_${COMPILER}_debug

export Y_DIR=$REG_DIR/${UNPERT_ID}_wrfvar_${NUM_PROCS}
export YP_DIR=$REG_DIR/${PERT_ID}_wrfvar_verbose_${NUM_PROCS}

export RUN_DIR=$TUNING_DIR/desroziers
mkdir -p $RUN_DIR
$WRFVAR_DIR/scripts/da_tune_obs_desroziers.ksh > $RUN_DIR/index.html 2>&1

export EXP_DIR=$Y_DIR
export RUN_DIR=$TUNING_DIR/hollingsworth
mkdir -p $RUN_DIR
$WRFVAR_DIR/scripts/da_tune_obs_hollingsworth.ksh > $RUN_DIR/index.html 2>&1
