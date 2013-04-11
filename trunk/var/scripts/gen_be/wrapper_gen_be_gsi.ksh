#! /bin/ksh -aeux
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Author : Syed RH Rizvi, MMM/ESSL/NCAR,  Date:04/15/2009
#
# Purpose: Calculates WRF-ARW background error statistics for GSI
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:
export WRFVAR_DIR=/mmm/users/rizvi/code/trunk_mbe
export SCRIPTS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/scripts
export GRAPHICS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/graphics/ncl
#export NUM_PROCS=32
export NUM_PROCS=1
export WALL_CLOCK=30
export PROJECT=48503002  
export QUEUE=regular

export CLEAN=false   

export NUM_WE=44     # 1 point less than stagger points  
export NUM_SN=44     # 1 point less than stagger points 
export NUM_LEVELS=27 # 1 point less than stagger points

export LESS_LEVELS_FROM_TOP=0 # Exclude levels from top for moisture statistics
export LAT_BINS_IN_DEG=5.0    # Lat bins (in deg) for BE stats
export DEBUG=0      


export REGION=con200 
export DAT_DIR=/ptmp/rizvi/data
export REG_DIR=$DAT_DIR/$REGION
export EXPT=run_gsi_be_lat_bin_size_${LAT_BINS_IN_DEG}_lnps
        
export RUN_DIR=$REG_DIR/$EXPT
rm -rf $RUN_DIR

export FC_DIR=$REG_DIR/novar/fc


export RUN_GEN_BE_GSI_STAGE0=true
export RUN_GEN_BE_GSI_STAGE1=true
export RUN_GEN_BE_GSI_STAGE2=true


export START_DATE=2007070200    # the first perturbation valid date
export   END_DATE=2007073112    # the last perturbation valid date

export INTERVAL=12

${SCRIPTS_DIR}/gen_be/gen_be_gsi.ksh

