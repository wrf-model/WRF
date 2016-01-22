#! /bin/ksh -aeux
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
# Purpose: Calculates WRF-ARW background error statistics for GSI
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:
export WRFVAR_DIR=/Volumes/sugar2/hclin/code/WRF/pgi/trunk_aod_be
export SCRIPTS_DIR=${WRFVAR_DIR}/var/scripts
export GRAPHICS_DIR=${WRFVAR_DIR}/var/graphics/ncl

export CLEAN=false

export RUN_GEN_BE_GSI_STAGE0=true
export RUN_GEN_BE_GSI_STAGE1=true
export RUN_GEN_BE_GSI_STAGE2=true

export LESS_LEVELS_FROM_TOP=0 # Exclude levels from top for moisture statistics
export LAT_BINS_IN_DEG=5.0    # Lat bins (in deg) for BE stats
export DEBUG=0      

export START_DATE=2008070200    # the first perturbation valid date
export END_DATE=2008070212      # the last perturbation valid date
export NUM_WE=245     # 1 point less than stagger points  
export NUM_SN=163     # 1 point less than stagger points 
export NUM_LEVELS=40  # 1 point less than stagger points
export FC_DIR=/Volumes/sugar2/hclin/data/tmp_afwa/gen_be/fcsts
export RUN_DIR=/Volumes/sugar2/hclin/data/tmp_afwa/gen_be/run_gsi_be_lat_bin_size_${LAT_BINS_IN_DEG}_lnps

export PROCESS_AERO=true  # True if you're generating BE stats for aerosols
export AEROS_TO_PROCESS=" 'BC1','BC2','OC1','OC2','SEAS_1','SEAS_2','SEAS_3','SEAS_4','DUST_1','DUST_2','DUST_3','DUST_4','DUST_5','sulf','P25' "  # List of aerosols to process

export DOMAIN=01
export BE_METHOD=NMC
export FCST_RANGE1=24 # Longer range forecast time
export FCST_RANGE2=12 # Shorter range time
export INTERVAL=12

${SCRIPTS_DIR}/gen_be/gen_be_gsi.ksh

