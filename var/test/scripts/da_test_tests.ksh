#!/bin/ksh

# da_test_tests.ksh

. ./setup.ksh

export EXPT=${EXPT:-expt}
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export NL_NTMAX=${NL_NTMAX:-2}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export NL_TEST_WRFVAR=true
export NL_TEST_TRANSFORMS=true
export NL_TEST_STATISTICS=true
export NL_TEST_DM_EXACT=false # does not work

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT

. $EXP_DIR/setup.ksh

export RUN=${MACHINE}_${COMPILER}_${TYPE}_tests_${NUM_PROCS}

export SCRIPT=$WRFVAR_DIR/scripts/da_run_wrfvar.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh
