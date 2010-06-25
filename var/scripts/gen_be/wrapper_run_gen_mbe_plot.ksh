#! /bin/ksh -aeux
#-----------------------------------------------------------------------
# Script gen_mbe_plot_wrapper.ksh
#
# Purpose: Plot multivariate background error (MBE) for WRFDA  
# Author : Syed RH Rizvi,   MMM/NESL/NCAR  02/15/2010
#
#-----------------------------------------------------------------------

export WRFVAR_DIR=/mmm/users/rizvi/code/trunk_mbe
export SCRIPTS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/scripts
export GRAPHICS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/graphics/ncl

export GRAPHIC_WORKS=pdf

export REGION=t8_45km
export RESOLUTION_KM=45.
export BE_DIR=/ptmp/rizvi/data/t8_45km/run_gen_mbe/gen_be5
export NUM_WE=139       # 1 point less than WE stagger points
export NUM_SN=93        # 1 point less than SN stagger points
export NUM_LEVELS=56    # 1 point less than bottom_top stagger points

cd $BE_DIR
#
# 1, First five eigenvectors:
#----------------------------
ncl ${GRAPHICS_DIR}/gen_be/gen_be_global_evecs.ncl 
#
# 2, First five eigenvaluess:
#----------------------------
ncl ${GRAPHICS_DIR}/gen_be/gen_be_global_evals.ncl 
#
# 3, Lengthscales:
#----------------------------
ncl ${GRAPHICS_DIR}/gen_be/gen_be_lengthscales.ncl 
#
# 4, Display contribution of balanced fields 
#---------------------------------------------------------
ncl ${GRAPHICS_DIR}/gen_be/gen_mbe_contrib.ncl
#
#
