#!/bin/ksh -aeux
#-----------------------------------------------------
# Following command is for Linux ( MMM big machine)
#-----------------------------------------------------
# mpd &
# mpdboot -n 9 -f $HOME/hosts --ifhn=master
# export SUBMIT=none
#
#-----------------------------------------------------
export PROJECT=64000510     # DATC GAUs.
export WALLCLOCK=5 

export QUEUE=share  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC
export NUM_PROCS=1 #64 is for WRF and WRF-VAR # 1 is for WPS, REAL, UPDATE_BC and OBS_PROC

# Define directories for source codei and input data 

export REL_DIR=/mmm/users/rizvi/code
export WRFVAR_DIR=$REL_DIR/WRFV3_20080324.5
export DAT_DIR=/ptmp/rizvi/WRFV3-testdata         

# Define experiment name                             

export REGION=con200         
#export RUN_DIR=/ptmp/rizvi/data/$REGION/run_with_noopt_cpu${NUM_PROCS}
export RUN_DIR=/ptmp/rizvi/data/$REGION/run_cpu${NUM_PROCS}
export FC_DIR=${RUN_DIR}/fc

export INITIAL_DATE=2007010200    
export   FINAL_DATE=2007010200    

export DATE=$INITIAL_DATE         

export REG_DIR=$DAT_DIR/$REGION

export OB_DIR=${DAT_DIR}/ob
export RC_DIR=${DAT_DIR}/rc
export BE_DIR=${DAT_DIR}/be

export NL_E_WE=45
export NL_E_SN=45
export NL_DX=200000
export NL_DY=200000
export NL_E_VERT=28


export RUN_WRFVAR=true            
export RUN_UPDATE_BC=false        
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh

#export NL_PRINT_DETAIL_GRAD=true 
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
#export NL_NTMAX=2
export NL_CALCULATE_CG_COST_FN=true


##----------------------------------------------
##   For Single Obs test  starts
#export runplot_psot=2  # 1 -run psot  2- plot psot
#export RUN_DIR=/ptmp/rizvi/data/$REGION/run_psot_cpu${NUM_PROCS}
#export FC_DIR=${RUN_DIR}/fc
# 
##----------------------------------------------
#export NL_NUM_PSEUDO=1
#export NL_PSEUDO_VAR="u"         #  Can be    "u   u    t    t      q"
#export NL_PSEUDO_X=23.0
#export NL_PSEUDO_Y=23.0
#export NL_PSEUDO_Z=14.0
#export NL_PSEUDO_ERR=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
#export NL_PSEUDO_VAL=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
#export NL_CHECK_RH=0                                                   
##----------------------------------------------
#export PSEUDO_VAR_SIZE=1
#export PSEUDO_VAR_LIST="u"         #  Can be    "u   u    t    t      q"
#export PSEUDO_VAL_LIST="1.0"       #  Should be "1.0 1.0 1.0  1.0  0.001"
#export PSEUDO_ERR_LIST="1.0"       #  Should be "1.0 1.0 1.0  1.0  0.001"
#export PSEUDO_X_LIST="23"          #  Middle of the domain
#export PSEUDO_Y_LIST="23"          #  Middle of the domain
#export PSEUDO_Z_LIST="14"          #  Middle of the domain
##
#if (( $runplot_psot == 1 ));then
#export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
#else
#export SCRIPT=$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
#export EXP_DIR=${RUN_DIR}
#$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
#exit 0
#fi
##----------------------------------------------
##   For Single Obs test  ends
##----------------------------------------------

$WRFVAR_DIR/var/scripts/da_run_job.ksh
exit 0
