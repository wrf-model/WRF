#!/bin/ksh

#########################################################################

export NUM_PROCS=8
export PROJECT=64000420
export WALLCLOCK=60


export ID=${ID:-trunk_xlf_bluesky}

export EXPT=run_${NUM_PROCS}_kma_2007020100          

export REL_DIR=/mmm/users/rizvi/code
export DAT_DIR=/ptmp/rizvi/data    

export WRFVAR_DIR=$REL_DIR/wrfvar      

export CLEAN=false
export NL_VAR4D=false


#########################################################################

#Decide which stages to run (run if true):
export RUN_RESTORE_DATA_NCEP=false
export RUN_RESTORE_DATA_RTOBS=false
export RUN_WPS=false
export RUN_REAL=false
export RUN_OBSPROC=false
export RUN_WRFVAR=true
export RUN_UPDATE_BC=false
export RUN_WRF=false



# Fiddle with options here

export NL_NTMAX=${NL_NTMAX:-100}
export NL_TRACE_USE=true
export NL_TRACE_UNIT=0
#export NL_PRINT_DETAIL_XB=true

# Defines test

export REGION=kma_t213
export INITIAL_DATE=2007020100
export   FINAL_DATE=2007020100
export NL_GLOBAL=true
export NL_E_WE=429
export NL_E_SN=216
export NL_DX=80000
export NL_DY=80000
export NL_E_VERT=41
export NL_FG_FORMAT=3
export NL_DYN_OPT=2

export EXP_DIR=$DAT_DIR/$REGION/$EXPT
export RUN_DIR=$EXP_DIR
rm -rf $RUN_DIR
export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

