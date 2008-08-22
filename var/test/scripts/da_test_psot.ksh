#!/bin/ksh

# da_test_psot.ksh

. ./setup.ksh

export EXPT=${EXPT:-expt}
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT

export PSEUDO_VAR_SIZE=1
export PSEUDO_VAR_LIST="u"
export PSEUDO_VAL_LIST=1.0
export PSEUDO_ERR_LIST=1.0
export PSEUDO_X_LIST=10
export PSEUDO_Y_LIST=10
export PSEUDO_Z_LIST=10

. $EXP_DIR/setup.ksh

export RUN=${MACHINE}_${COMPILER}_${TYPE}_psot_${NUM_PROCS}

export SCRIPT=$WRFVAR_DIR/scripts/da_run_psot.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh
