#!/bin/ksh

# Define for your run

export REL_DIR=/smoke/dmbarker/code/latest
export WRFVAR_DIR=$REL_DIR/wrfvar
export DAT_DIR=/smoke/dmbarker/data

# Fiddle with options here
export DAT_DIR=/smoke/dmbarker/data/con200/xwang/test

#export EXPT=xwang
export NL_CHECK_MAX_IV=.false.
export DATE=2003010112
export BE_METHOD=ENS
export NL_ALPHACV_METHOD=2
export NL_ENSDIM_ALPHA=56
#export NL_JE_FACTOR=1.0
#export NL_ALPHA_STD_DEV=100.0
export EXPT=wrfvar.singlet.alpha${NL_ENSDIM_ALPHA}.ep${NL_ALPHACV_METHOD}.${BE_METHOD}

export DA_OBSERVATIONS=${DAT_DIR}/obs_gts.3dvar.2003010112.singlet
export DA_BACK_ERRORS=${DAT_DIR}/gen_be.${BE_METHOD}.dat
export DA_FIRST_GUESS=${DAT_DIR}/wrf_3dvar_input_d01_2003-01-01_12:00:00
export EP_DIR=/smoke/dmbarker/data/con200/xwang/2003010112/ep${NL_ALPHACV_METHOD}

#export NL_USE_BUOYOBS=T
#export NL_USE_PROFILEROBS=T
#export NL_USE_GPSPWOBS=T
#export NL_USE_GPSREFOBS=T
#export NL_USE_QSCATOBS=T
#export NL_CALCULATE_CG_COST_FN=T

export NL_E_WE=45
export NL_E_SN=45
export NL_DX=200000
export NL_DY=200000
export NL_E_VERT=28

export HOSTS=$HOME/hosts
export HOSTS=`eval echo $HOSTS`

$WRFVAR_DIR/var/scripts/da_run_job.ksh

