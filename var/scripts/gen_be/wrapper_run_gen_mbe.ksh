#! /bin/ksh 
#-----------------------------------------------------------------------
# Script gen_mbe_wrapper.ksh
#
# Purpose: Generate Multi-variate BE for WRFDA
# Author : Syed RH Rizvi,   MMM/NESL/NCAR  02/15/2010
#           
#-----------------------------------------------------------------------
# Specify Experiment details
#-----------------------------------------------------------------------
export REGION=con200  
export RESOLUTION_KM=200.
export NUM_WE=44        # 1 point less than WE stagger points  
export NUM_SN=44        # 1 point less than SN stagger points  
export NUM_LEVELS=27    # 1 point less than bottom_top stagger points 
export START_DATE=2007070200    # the first perturbation valid date
export   END_DATE=2007073112    # the last perturbation valid date
export INTERVAL=12
export CLEAN=true 
export DAT_DIR=/ptmp/rizvi/data
export REG_DIR=$DAT_DIR/${REGION}
export FC_DIR=${REG_DIR}/novar/fc
export NL_CV_OPTIONS=6                          
export EXPT=run_gen_mbe
export EXP_DIR=$REG_DIR/$EXPT

#rm -rf $EXP_DIR

#-----------------------------------------------------------------------
# Specify Job specifications for NCAR IBM
#-----------------------------------------------------------------------
export LOCAL=false
export NUM_JOBS=1000
export NUM_PROCS=1
export QUEUE=regular 
export WALL_CLOCK=5
export PROJECT=64000510
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Specify Desired directories  
#-----------------------------------------------------------------------
export WRFVAR_DIR=/mmm/users/rizvi/code/trunk_mbe
export SCRIPTS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/scripts
export GRAPHICS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/graphics/ncl
#-----------------------------------------------------------------------
# Commentout the string blow if you do not want ro run    
#-----------------------------------------------------------------------
export RUN_GEN_BE_STAGE0=true 
export RUN_GEN_BE_STAGE1=true 
export RUN_GEN_BE_STAGE2=true 
export RUN_GEN_BE_STAGE3=true 
export RUN_GEN_BE_STAGE4=true 
export RUN_GEN_BE_DIAGS=true 
export RUN_GEN_BE_DIAGS_READ=true 
export RUN_GEN_BE_MULTICOV=true 
export RUN_GEN_BE_MULTICOV_CONTRIB=true 
export RUN_GEN_BE_HISTOG=true 

# Run gen_mbe:
${SCRIPTS_DIR}/gen_be/gen_mbe.ksh

