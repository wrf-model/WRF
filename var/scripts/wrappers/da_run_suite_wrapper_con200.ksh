#!/bin/ksh 
#-----------------------------------------------------

export RUN_WRFVAR=true            
export RUN_UPDATE_BC=false        
export SINGLE_OBS=false        
export SUBMIT=none
export NUM_PROCS=1 
if [[ $NUM_PROCS -gt 1 ]]; then
   export RUN_CMD="mpirun -np $NUM_PROCS"
else
   export RUN_CMD=""
fi
export CLEAN=false

# Define experiment name

export REGION=con200         

# Define directories for source code and input data 

export REL_DIR=/karri/users/xinzhang/support
export WRFVAR_DIR=$REL_DIR/WRFDA
export DAT_DIR=$REL_DIR
export RUN_DIR=$REL_DIR/$REGION/run_cpu${NUM_PROCS}
export FC_DIR=${RUN_DIR}/fc
export REG_DIR=$DAT_DIR/$REGION
export OB_DIR=${DAT_DIR}/ob
export RC_DIR=${DAT_DIR}/rc
export BE_DIR=${DAT_DIR}/be


# Define experiment time period and dimension

export INITIAL_DATE=2007010200    
export   FINAL_DATE=2007010200    

export DATE=$INITIAL_DATE         


export NL_E_WE=45
export NL_E_SN=45
export NL_DX=200000
export NL_DY=200000
export NL_E_VERT=28


# Define experiment options

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh

export NL_EPS=0.01
export NL_NTMAX=200
export NL_CALCULATE_CG_COST_FN=true
#export NL_USE_SOUNDOBS=true      
#export NL_USE_SYNOPOBS=false     
#export NL_USE_METAROBS=false     
#export NL_USE_PILOTOBS=false     
#export NL_USE_SHIPSOBS=false     
#export NL_USE_BUOYOBS=false     
#export NL_USE_GEOAMVOBS=false     
#export NL_USE_AIREPOBS=false     
#export NL_USE_GPSPWOBS=false     
#export NL_USE_SATEMOBS=false     


if $SINGLE_OBS ; then
##----------------------------------------------
##   For Single Obs test  starts
export runplot_psot=2  # 1 -run psot  2- plot psot
export RUN_DIR=$REL_DIR/$REGION/run_psot_cpu${NUM_PROCS}
export FC_DIR=${RUN_DIR}/fc
# 
export NL_NUM_PSEUDO=1
export NL_PSEUDO_VAR="t"         #  Can be    "u   v    t    p     q"
export NL_PSEUDO_X=23.0
export NL_PSEUDO_Y=23.0
export NL_PSEUDO_Z=14.0
export NL_PSEUDO_ERR=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
export NL_PSEUDO_VAL=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
export NL_CHECK_RH=0                                                   
##----------------------------------------------
export PSEUDO_VAR_SIZE=1
export PSEUDO_VAR_LIST="t"         #  Can be    "u   v    t    p      q"
export PSEUDO_VAL_LIST="1.0"       #  Should be "1.0 1.0 1.0  1.0  0.001"
export PSEUDO_ERR_LIST="1.0"       #  Should be "1.0 1.0 1.0  1.0  0.001"
export PSEUDO_X_LIST="23"          #  Middle of the domain
export PSEUDO_Y_LIST="23"          #  Middle of the domain
export PSEUDO_Z_LIST="14"          #  Middle of the domain
##
if (( $runplot_psot == 1 ));then
   export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
else
   export SCRIPT=$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
   export EXP_DIR=${RUN_DIR}
   $WRFVAR_DIR/var/scripts/da_plot_psot.ksh
   exit 0
fi
#export NL_LEN_SCALING1=0.5    
#export NL_LEN_SCALING2=0.5    
#export NL_LEN_SCALING3=0.5    
#export NL_LEN_SCALING4=0.5    
#export NL_LEN_SCALING5=0.5    

#export NL_LEN_SCALING1=0.25    
#export NL_LEN_SCALING2=0.25    
#export NL_LEN_SCALING3=0.25    
#export NL_LEN_SCALING4=0.25    
#export NL_LEN_SCALING5=0.25    

fi
##----------------------------------------------
##   For Single Obs test  ends
##----------------------------------------------

$WRFVAR_DIR/var/scripts/da_run_job.ksh
exit 0
