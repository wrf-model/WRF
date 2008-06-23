#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_plot_wrapper.ksh
#
# Purpose: Plot the background error statistics (BES) for WRF-Var
# 
#   After finished the BES, this script gives the BES plots
#   by using ncl shell under var/graphics/ncl/gen_be.
#
#                             Y.-R. Guo  06/16/2008
#-----------------------------------------------------------------------
export NCARG_ROOT=/karri/local/ncl-4.2.0.a034

#[1] Define job by overriding default environment variables:

#export RUN_GEN_BE_DIAGS=true
#export RUN_GEN_BE_DIAGS_READ=true
#export RUN_GEN_BE_MULTICOV=true

export WRFVAR_DIR=/karri/users/xinzhang/support/WRFDA
export BUILD_DIR=$WRFVAR_DIR/var/da
export GEN_BE_PLOT=${WRFVAR_DIR}/var/graphics/ncl/gen_be

export GRAPHIC_WORKS=x11

#CWB 45km:

export NUM_WE=44 # 1 point less than stagger points  45
export NUM_SN=44 # 1 point less than stagger points  45
export NUM_LEVELS=27 # 1 point less than stagger points 28
export REGION=con200
export EXPT=expt
export DAT_DIR=/karri/users/xinzhang/support
export BE_DIR=$DAT_DIR/$REGION/$EXPT/gen_be5/working
export BE_NROW=$NUM_LEVELS

#[2] Plot gen_be
#
# 1, First five eigenvectors:
#----------------------------
ncl ${GEN_BE_PLOT}/gen_be_global_evecs.ncl 
#
# 2, First five eigenvaluess:
#----------------------------
ncl ${GEN_BE_PLOT}/gen_be_global_evals.ncl 
#
# 3, Lengthscales:
#----------------------------
ncl ${GEN_BE_PLOT}/gen_be_lengthscales.ncl 
#
# 4, Correlation: chi_b.chi ans t_b.t
#-----------------------------------
ncl ${GEN_BE_PLOT}/gen_be_corr_z.ncl  
#
# 5, Correlation yz coross-section: <chi_b.chi> ans <t_b.t>
#---------------------------------------------------------
ncl ${GEN_BE_PLOT}/gen_be_corr_yz.ncl 
#
# 6, Correlation:  <ps_b.ps>
#--------------------------
ncl ${GEN_BE_PLOT}/gen_be_corr_ps.ncl 
#
