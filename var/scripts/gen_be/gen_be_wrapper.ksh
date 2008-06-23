#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:

export RUN_GEN_BE_STAGE0=true
export RUN_GEN_BE_STAGE1=true
export RUN_GEN_BE_STAGE2=true
export RUN_GEN_BE_STAGE2A=true
export RUN_GEN_BE_STAGE3=true
export RUN_GEN_BE_STAGE4=true
export RUN_GEN_BE_DIAGS=true
export RUN_GEN_BE_DIAGS_READ=true
export RUN_GEN_BE_MULTICOV=true

export WRFVAR_DIR=/karri/users/xinzhang/support/WRFDA

#t46 45km:
export START_DATE=2007010200
export END_DATE=2007013100
export NUM_LEVELS=28
export REGION=con200
export EXPT=expt
export BIN_TYPE=5
export EXP_DIR=/karri/users/xinzhang/support/$REGION/$EXPT

#Example of changes required for "be_method=ENS":
#export BE_METHOD=ENS
#export NE=56
#export FCST_RANGE=12

#[2] Run gen_be:
${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh

