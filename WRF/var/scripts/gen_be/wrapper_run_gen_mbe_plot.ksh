#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_mbe_plot_wrapper.ksh
#
# Purpose: Plot multivariate background error (MBE) for WRFDA  
# Author : Syed RH Rizvi,   MMM/NESL/NCAR  02/15/2010
#
#-----------------------------------------------------------------------

export WRFVAR_DIR=/kukui/users/xinzhang/code/WRFDA
export SCRIPTS_DIR=${WRFVAR_DIR}/var/scripts
export GRAPHICS_DIR=${WRFVAR_DIR}/var/graphics/ncl

export GRAPHIC_WORKS=pdf

export REGION=nari
export RESOLUTION_KM=35.
export BE_DIR=/kukui/users/xinzhang/code/tmp/GENBE/cv6/gen_be5
export NUM_WE=74       # 1 point less than WE stagger points
export NUM_SN=69        # 1 point less than SN stagger points
export NUM_LEVELS=27    # 1 point less than bottom_top stagger points

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
