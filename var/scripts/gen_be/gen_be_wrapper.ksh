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

export WRFVAR_DIR=/ptmp/xinzhang/blueice_ibm_opt/WRFDA

export START_DATE=2008020612  # the first perturbation valid date
export END_DATE=2008020700    # the last perturbation valid date
export NUM_LEVELS=40          # e_vert - 1
export BIN_TYPE=5
export FC_DIR=/mmm/mmmtmp/xinzhang/gen_be   # where wrf forecasts are
export RUN_DIR=/mmm/mmmtmp/xinzhang/gen_be${BIN_TYPE}
export DOMAIN=01
export FCST_RANGE1=24
export FCST_RANGE2=12
export INTERVAL=12
export STRIDE=1

#Example of changes required for "be_method=ENS":
#export BE_METHOD=ENS
#export NE=56
#export FCST_RANGE=12

#[2] Run gen_be:
${WRFVAR_DIR}/var/scripts/gen_be/gen_be.ksh

