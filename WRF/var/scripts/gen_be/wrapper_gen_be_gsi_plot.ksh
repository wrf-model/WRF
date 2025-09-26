#! /bin/ksh -aeux
#-----------------------------------------------------------------------
# Script wrapper_gen_be_gsi_plot.ksh
# ***********************************************
# Author: Syed RH Rizvi
#         UCAR/NCAR/ESSL/MMM/DAG Date: 08/12/2009
#
# Purpose: Wrapper for ploting background error statistics for GSI
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:
export SCRIPTS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/scripts
export GRAPHICS_DIR=/mmm/users/rizvi/code/WRFDA_scripts/var/graphics/ncl
export GRAPHIC_WORKS=pdf


export REGION=con200 
export EXPT=run_gsi_be_lat_bin_size_1.0_lnps
export RESOLUTION=200.0  # Resolution in km
export NUM_WE=44         # 1 point less than stagger points  
export NUM_SN=44         # 1 point less than stagger points 
export NUM_LEVELS=27     # 1 point less than stagger points

export PLOT_CORRELATION=true   # Possible if you have access to RUN_DIR
export BY_LEVELS=False         # If "True" plots diagnostics by sigma levels

export DAT_DIR=/ptmp/rizvi/data
export REG_DIR=$DAT_DIR/$REGION
export PLOT_DIR=$REG_DIR/$EXPT
export BE_DIR=$REG_DIR/$EXPT
export BE_FILE_NAME=wrf-arw-gsi_be

        
#
#---------------------------------------------------------
# Plot Variance, Horizontal & Vertical scalelengths
#---------------------------------------------------------
ncl ${GRAPHICS_DIR}/gen_be/plot_gsi_be.ncl
#

#---------------------------------------------------------
# Plot Correlation:  <ps_b.ps>, <t_b.t> & <chi_b.chi>
#---------------------------------------------------------
#
if $PLOT_CORRELATION  ; then
ncl ${GRAPHICS_DIR}/gen_be/gsi_correlation.ncl
fi

