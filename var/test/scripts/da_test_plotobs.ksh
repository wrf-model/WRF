#!/bin/ksh

# da_test_plotobs.ksh

. da_test_defaults.ksh

. ./setup.ksh
. $CASE/setup.ksh

export EXPT=${EXPT:-${ID}_${TEST}_${NUM_PROCS}}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$PWD/$EXPT/run
export RC_DIR=$REG_DIR/$CASE/rc

mkdir -p $EXP_DIR/plotobs
ncl $WRFVAR_DIR/graphics/ncl/plotobs.ncl > $EXP_DIR/plotobs/plotobs.log 2>&1
