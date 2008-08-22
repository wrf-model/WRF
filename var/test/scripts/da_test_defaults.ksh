#!/bin/ksh

# da_test_defaults.ksh

export FULL=${FULL:-false}
export RELEASE=${RELEASE:-trunk}
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export WRFVAR_DIR=${WRFVAR_DIR:-$HOME/code/$RELEASE/$ID/wrfvar}
export WRF_DIR=${WRF_DIR:-$HOME/code/$RELEASE/$ID/wrf}
export DIGITS=${DIGITS:-10}
export TEST=${TEST:-wrfvar}
