#!/bin/ksh
#########################################################################
# Author:  Syed RH Rizvi    conatct email: rizvi@ucar.edu
# Script: WRAPPER SCRIPT
#
# Script: WRAPPER SCRIPT 
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#
# Here are a few examples of environment variables you may want to 
# change in da_run_suite_wrapper.ksh:
# 1) "export RUN_WPS=true" runs the WPS).
# 2) "export REL_DIR=$HOME/trunk" points to directory containing all code 
#    with components subdirectories e.g. $REL_DIR/wrfvar for WRFVAR code.
# 3) "export START_DATE=2003010100" begins experiment at 00 UTC 1 Jan 2003.
# Full list of environment variables and their default values are
# in da_run_suite.ksh.
#########################################################################
#[1] Decide Batch Job Specifications first
#########################################################################
export NUM_PROCS=16 
export WALLCLOCK=15
export PROJECT=64000420
#-----------------------------------------------------------------------
#  Here Fix the Dates
#-----------------------------------------------------------------------
export INITIAL_DATE=2007020800
export FINAL_DATE=2007020800
#-----------------------------------------------------------------------
# Here the choice for sequence
#-----------------------------------------------------------------------
export DUMMY=false
#-----------------------------------------------------------------------
export RUN_RESTORE_DATA_NCEP=false
export RUN_RESTORE_DATA_RTOBS=false
export RUN_WPS=false
export RUN_REAL=false
#-----------------------------------------------------------------------
export RUN_OBSPROC=false
export RUN_WRFVAR=true 
export RUN_UPDATE_BC=false
#-----------------------------------------------------------------------
export RUN_WRF=false
export CYCLING=false
export FIRST=false
export EXPT=run_regional_kma_cpu_${NUM_PROCS}
#########################################################################
# [2] Override default environment variables here:
#########################################################################
# Disks, directories:
export DAT_DIR=/ptmp/rizvi/data
export REGION=regional_kma
#-----------------------------------------------------------------------
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export REL_DIR=/mmm/users/rizvi/code
export WRF_BC_DIR=${WRF_BC_DIR:-$REL_DIR/WRF_BC_v2.1}
#-----------------------------------------------------------------------
#export WPS_INPUT_DIR=${WPS_INPUT_DIR:-/ptmp/rizvi/ncep}
#-----------------------------------------------------------------------
# [3] ############     Job details:        ############################
#-------------------------------------------------------------------------
export LBC_FREQ=${LBC_FREQ:-6}                         # Interval (hrs) to update boundary
export LONG_FCST_TIME_1=${LONG_FCST_TIME_1:-00}        # Forecast starting time 1
export LONG_FCST_TIME_3=${LONG_FCST_TIME_3:-24}        # Forecast starting time 3
export LONG_FCST_RANGE_1=${LONG_FCST_RANGE_1:-24}      # Forecast range for hours 00
export LONG_FCST_RANGE_3=${LONG_FCST_RANGE_3:-24}      # Forecast range for hours 12
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}                # Interval (hrs) between two separate model run
#-------------------------------------------------------------------------
#####  Resolution and Domain for WPS
#-------------------------------------------------------------------------
export MAP_PROJ=${MAP_PROJ:-lambert}                   # Map Projection
export REF_LAT=${REF_LAT:-38.0}                        # Centre Latitude
export REF_LON=${REF_LON:-126.0}                        # Centre Longitude
export TRUELAT1=${TRUELAT1:-30.0}                       # True LAtitude 1
export TRUELAT2=${TRUELAT2:-60.0}                      # True LAtitude 2
export STAND_LON=${STAND_LON:-126.0}                    # Reference Longitude
#-------------------------------------------------------------------------
# Domains control options in WRF
#-------------------------------------------------------------------------
export NL_E_WE=${NL_E_WE:-191}                         # Number of grid points in X direction (E_W)
export NL_E_SN=${NL_E_SN:-171}                         # Number of grid points in Y direction (N_S)
export NL_E_VERT=${NL_E_VERT:-31}                      # Number of vertical eta levels 
export NL_DX=${NL_DX:-30000}                           # Grid distance in Km in X-direction
export NL_DY=${NL_DY:-30000}                           # Grid distance in Km in Y-direction
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}         # Smoothing option
#-------------------------------------------------------------------------
# Vertical levels definitions for real.exe
#-------------------------------------------------------------------------
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-"1.000, 0.993, 0.980, 0.966, 0.950, \
                                       0.933, 0.913, 0.892, 0.869, 0.844, \
                                       0.816, 0.786, 0.753, 0.718, 0.680, \
                                       0.639, 0.596, 0.550, 0.501, 0.451, \
                                       0.398, 0.345, 0.290, 0.236, 0.188, \
                                       0.145, 0.108, 0.075, 0.046, 0.021, \
                                       0.000"}

export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27} # No. of vertical pressure levels in Metgrid files (FNL/GFS)
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}     # The TOP pressure levels values in hPa 
#------------------------------------------------------------------------
# Few Time control options in WRF
#------------------------------------------------------------------------
export NL_INPUT_FROM_FILE=${NL_INPUT_FROM_FILE:-true}     # for domain it should point mother id
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-720}    # (in minutes)
export NL_FRAMES_PER_OUTFILE=1                            # number of output files
export NL_TIME_STEP=${NL_TIME_STEP:-60}                   # Timestep(s), dt~4-6*dx(km) recommended).
#export NL_RESTART_INTERVAL=5000
#------------------------------------------------------------------------
#####  PHYSICS Options in WRF
#
# Needs to be confirmed export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-2}         #
#export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-2} #(1=Thermal diffusion, 2=Noah LSM).
#------------------------------------------------------------------------
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-2}                  # 2 --Lin et al., 3-- WSM3-simple ice scheme
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-0}                #
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}            # 1 -- RRTM 
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}            # 1 -- Dudhia scheme, 2--- Goddard SW
export NL_RADT=${NL_RADT:-10}                             # Interval in minutes to call radiation scheme
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}    # 1 -- Monin-Obukov, 2-- Monin-Obukon(Janjic eta)
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1}  #(1=Thermal diffusion, 2=Noah LSM, 3--RUC )
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1}          #(1=YSU, 2=Mellor-Yamada-Janjic).
export NL_BLDT=${NL_BLDT:-0}                              # Interval in minutes to call PBL scheme
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-2}                  #(1=Kain-Fritsch, 2=Betts-Miller-Janjic,3=Grell-Deveneyi)
export NL_CUDT=${NL_CUDT:-5}                              #Interval in minutes to call cumulus parameterization
#export NL_SST_UPDATE=1
#------------------------------------------------------------------------
# Here are dynamics options
#------------------------------------------------------------------------
export NL_DYN_OPT=${NL_DYN_OPT:-2}                        # Eulerian-mass
export NL_W_DAMPING=${NL_W_DAMPING:-0}                    # 
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}                      # Diffusion co-efficient
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.01}                   # Damping co-efficient
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-4}        # Short time step for sound-wave
#------------------------------------------------------------------------
# Here are Bdy_control options 
#------------------------------------------------------------------------
export NL_SPECIFIED=.TRUE.
#------------------------------------------------------------------------
# Uncomment after you've run SI once for this domain!
# export RUN_GRID_GEN=${RUN_GRID_GEN:-false}
# export VERTICAL_LEVELS - do this directly in da_run_wrfsi.ksh.
#------------------------------------------------------------------------
#    Here are Namelist options for OBS_PROC
#------------------------------------------------------------------------
export PTOP_PA=${PTOP_PA:-1000}     # The TOP pressure levels values in hPa 
#------------------------------------------------------------------------
#    Here are options for WRF_VAR control
#------------------------------------------------------------------------
#export NL_PRINT_DETAIL_GRAD=.true.
export NL_NUM_FGAT_TIME=1
export WINDOW_START=-3
export WINDOW_END=3
#-----------------------------------------------------------------------
#  Here is the namelist flags to control different observations
#-----------------------------------------------------------------------
#export NL_USE_SYNOPOBS=${NL_USE_SYNOPOBS:-FLASE}
#export NL_USE_SHIPSOBS=${NL_USE_SHIPSOBS:-FLASE}
#export NL_USE_METAROBS=${NL_USE_METAROBS:-FLASE}
#export NL_USE_SOUNDOBS=${NL_USE_SOUNDOBS:-FALSE}
#export NL_USE_PILOTOBS=${NL_USE_PILOTOBS:-FLASE}
#export NL_USE_AIREPOBS=${NL_USE_AIREPOBS:-FLASE}
#export NL_USE_BUOYOBS=${NL_USE_BUOYOBS:-FLASE}
#export NL_USE_PROFILEROBS=${NL_USE_PROFILEROBS:-FLASE}
#-------------------------------------------------------------
export NL_USE_GEOAMVOBS=${NL_USE_GEOAMVOBS:-FLASE}
export NL_USE_POLARAMVOBS=${NL_USE_POLARAMVOBS:-FLASE}
export NL_USE_SATEMOBS=${NL_USE_SATEMOBS:-FLASE}
#export NL_USE_SSMIRETRIEVALOBS=${NL_USE_SSMIRETRIEVALOBS:-FALSE}
#export NL_USE_SSMITBOBS=${NL_USE_SSMITBOBS:-FLASE}
#export NL_USE_SSMT1OBS=${NL_USE_SSMT1OBS:-FLASE}
#export NL_USE_SSMT2OBS=${NL_USE_SSMT2OBS:-FLASE}
#export NL_USE_QSCATOBS=${NL_USE_QSCATOBS:-FLASE}
#-------------------------------------------------------------
#export NL_USE_GPSPWOBS=${NL_USE_GPSPWOBS:-FLASE}
#export NL_USE_GPSREFOBS=${NL_USE_GPSREFOBS:-FLASE}
#-------------------------------------------------------------
#export NL_USE_RADAROBS=${NL_USE_RADAROBS:-TRUE}
#export NL_USE_RADAR_RV=${NL_USE_RADAR_RV:-TRUE}
#export NL_USE_RADAR_RF=${NL_USE_RADAR_RF:-TRUE}
#-------------------------------------------------------------
#export NL_USE_HIRS2OBS=${NL_USE_HIRS2OBS:-TRUE}
#export NL_USE_HIRS3OBS=${NL_USE_HIRS3OBS:-TRUE}
#export NL_USE_MSUOBS=${NL_USE_MSUOBS:-TRUE}
#export NL_USE_AMSUAOBS=${NL_USE_AMSUAOBS:-TRUE}
#export NL_USE_AMSUBOBS=${NL_USE_AMSUBOBS:-TRUE}
#export NL_USE_AIRSOBS=${NL_USE_AIRSOBS:-TRUE}
#export NL_USE_AIRESRETOBS=${NL_USE_AIRESRETOBS:-TRUE}
#export NL_USE_EOS_AMSUAOBS=${NL_USE_EOS_AMSUAOBS:-TRUE}
#export NL_USE_EOS_RADOBS=${NL_USE_EOS_RADOBS:-TRUE}
#export NL_USE_HSBOBS=${NL_USE_HSBOBS:-TRUE}
#export NL_RTMINIT_NSENSOR=2
#export NL_RTMINIT_PLATFORM=1,1
#export NL_RTMINIT_SATID=15,16
#export NL_RTMINIT_SENSOR=3,3
#export NL_LQC_RAD=TRUE
#export NL_RTM_OPTION=1
#-----------------------------------------------------------------------
#export NL_CHECK_MAX_IV=FALSE
#export NL_LREAD_BIASCOEF=TRUE
#export NL_LBIASCORR=TRUE
#export NL_LWRITE_IV_RAD_ASCII=FALSE
#export NL_LWRITE_OA_RAD_ASCII=FALSE
##export NL_LWRITE_OA_RAD_ASCII=TRUE
#export NL_LWRITE_FILTERED_RAD=FALSE
#-----------------------------------------------------------------------
export NL_USE_HTML=true
export NL_TRACE_USE=true
export NL_TRACE_ALL_PES=true
export NL_TRACE_UNIT=0
#export NL_PRINT_DETAIL_OBS=true
# [4] ############   Final Confirmation    ############################
#-------------------------------------------------------------------------
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0
