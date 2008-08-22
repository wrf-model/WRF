#!/bin/ksh

export NUM_PROCS=1

. ./setup.ksh

export EXPT=${EXPT:-expt}
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export RUN=${RUN:-${ID}_etkf}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT

. $EXP_DIR/setup.ksh

export SCRIPT=$WRFVAR_DIR/scripts/da_run_etkf.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh
