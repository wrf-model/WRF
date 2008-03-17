#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper_hurricane12.ksh, Zhiquan Liu
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#          for Hurricane 12km domain run
#
# Description:
#
# Here are a few examples of environment variables you may want to 
# change in da_run_suite_wrapper.ksh:
#
# 1) "export RUN_WRFVAR=true" runs WRFVAR).
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export INITIAL_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################
#set echo 

# 1 Decide which stages to run (run if true):
#--------------------------------------------
  # 1.0 retrieval data
  #-----------------------
#export RUN_RESTORE_DATA_GRIB=true    # retrieval NCAR FNL grid analysis
#export RUN_RESTORE_DATA_RTOBS=true   # retrieval Jim Bresch's GTS archive
  
  # 1.1 run WPS/REAL/WRF
  #------------------------
#export RUN_WPS=true
#export RUN_REAL=true
export RUN_WRF=true

  # 1.2 run WRF-Var components
  #----------------------------
#export RUN_OBSPROC=true
export RUN_WRFVAR=true
export RUN_UPDATE_BC=true

# 2 Experiment details:
#-----------------------------
#export DUMMY=${DUMMY:-true}
#export FCYCLE_HOUR=00
export CLEAN=false
#export CYCLING=true  # true: use wrf_3dvar_input as input
#export FIRST=false   # if the first case, no wrf_3dvar_input file available

# 3 Job detail
#------------------------
export NUM_PROCS=32
export QUEUE=regular
export PROJECT=64000420 # 25000026
export WALLCLOCK=360

# 4 Time info:
#-------------------------------
export INITIAL_DATE=2005082600
export FINAL_DATE=2005082600
export LBC_FREQ=06
export CYCLE_PERIOD=06
export OBS_FREQ=06
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=120
export LONG_FCST_RANGE_2=6
export LONG_FCST_RANGE_3=6
export LONG_FCST_RANGE_4=6

# 5 Directories:
#------------------------------------
export USERNAME=liuz

  # 5.1 code dir
  #-------------------
export REL_DIR=/mmm/users/${USERNAME}/code_vista # TOP dir of code
export WRFVAR_DIR=$REL_DIR/trunk                 # WRF-Var dir
export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC        # OBSPROC dir
export WRF_BC_DIR=$WRFVAR_DIR/build              # da_update_bc dir

  # 5.2 experiments output dir
  #------------------------
export DAT_DIR=/ptmp/${USERNAME}/exps            # TOP dir of data
export RTOBS_DIR=${DAT_DIR}/rtobs                # global data
export GRIB_DIR=${DAT_DIR}/fnl                   # fnl grib
export REGION=katrina12                          # domain dir
export EXPT=bluevista_trunk_crtm_amsua_$NUM_PROCS     # experiment dir under REGION
export OB_DIR=${DAT_DIR}/${REGION}/ob            # WRF-Var obs input dir
export RC_DIR=${DAT_DIR}/${REGION}/rc      # WPS/REAL output dir
export FC_DIR=${DAT_DIR}/${REGION}/$EXPT/fc      # WRF model output

# 6 Namelsits
#--------------------------
  # 6.1 for WPS (namelist.wps):
  #-----------------------------
export RUN_GEOGRID=false
export DEBUG_LEVEL_UNGRIB=200
export MAP_PROJ=mercator
export NL_E_WE=460
export NL_E_SN=351
export REF_LAT=30.0
export REF_LON=-85.0
export TRUELAT1=30.0
export TRUELAT2=30.0
export STAND_LON=-85.0
export NL_DX=12000
export NL_DY=12000
export GEOG_DATA_RES=2m

  # 6.2 for REAL/WRF:
  #------------------------
# time_control
export NL_INPUTOUT_INTERVAL=360

# domains
export NL_TIME_STEP=60 # time step for integration in integer seconds (recommended 6*dx in km for a typical case)
export NL_E_VERT=51
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.994, 0.986, 0.978, 0.968, 0.957, 0.945, "\
                                      " 0.931, 0.915, 0.897, 0.876, 0.854, 0.829, 0.802, "\
                                      " 0.772, 0.740, 0.705, 0.668, 0.629, 0.588, 0.550, "\
                                      " 0.513, 0.478, 0.445, 0.413, 0.383, 0.355, 0.328, "\
                                      " 0.303, 0.279, 0.256, 0.234, 0.214, 0.195, 0.177, "\
                                      " 0.160, 0.144, 0.128, 0.114, 0.101, 0.088, 0.076, "\
                                      " 0.065, 0.055, 0.045, 0.036, 0.028, 0.020, 0.012, "\
                                      " 0.0056, 0.000 "}
export NL_SMOOTH_OPTION=0      # for nesting
export NL_FEEDBACK=0           # for nesting
#export NL_NUM_METGRID_LEVELS=27  # number of vertical levels in the incoming data: 
                               # type nudump h to find out (WPS data only)
export NL_P_TOP_REQUESTED=1000 # p_top to use in the model

# physics
export NL_MP_PHYSICS=3         # micro-physics: WSM 3-class simple ice scheme
export NL_MP_ZERO_OUT=0        # Qv is >= 0, all other moist arrays are set to zero 
                               # if they fall below a critical value
export NL_CU_PHYSICS=1         # cumulus option: Kain-Fritsch (new Eta) scheme
export NL_CUDT=5               # minutes between cumulus physics calls
export NL_SFCLAY_PHYSICS=1     # surface-layer option: Monin-Obukhov scheme
export NL_SF_SURFACE_PHYSICS=1 # land-surface: thermal diffusion scheme
export NL_NUM_SOIL_LAYERS=5    # thermal diffusion scheme for temp only 
export NL_BL_PBL_PHYSICS=1     # boundary-layer option: YSU scheme
export NL_BLDT=0               # minutes between boundary-layer physics calls
export NL_RA_LW_PHYSICS=1      # longwave radiation: rrtm scheme
export NL_RA_SW_PHYSICS=1      # shortwave radiation: Dudhia scheme
export NL_RADT=15              # minutes between radiation physics calls. 
                               # Recommend 1 minute per km of dx (e.g. 10 for 10 km grid)
# dynamics
export NL_DYN_OPT=2            # dynamical core: advanced research WRF core (Eulerian mass)
export NL_PD_MOIST=true        # positive define advection of moisture; set to .true. to turn it on
export NL_W_DAMPING=1          # vertical velocity damping flag (for operational use)
export NL_DIFF_OPT=1           # turbulence and mixing: evaluates 2nd order diffusion term 
                               # on coordinate surfaces. uses kvdif for vertical diff unless 
                               # PBL option is used. may be used with km_opt = 1 and 4. 
                               # (= 1, recommended for real-data case)
export NL_KM_OPT=4             # eddy coefficient:horizontal Smagorinsky first order closure 
                               # (recommended for real-data case)
export NL_DAMP_OPT=0           # upper level damping flag (may now be used in real-data runs)
                               # without damping
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0    # number of sound steps per time-step 
                               # (if using a time_step much larger than 6*dx (in km), 
                               # increase number of sound steps). = 0: the value computed automatically

   # 6.3 namelist for OBS_PROC:
   #----------------------------
export PTOP_PA=$NL_P_TOP_REQUESTED

   # 6.3 namelist for WRF-Var:
   #----------------------------
# tracing
#-----------------------------------
export NL_TRACE_USE=.TRUE.
export NL_TRACE_UNIT=0
export NL_TRACE_ALL_PES=true
export NL_TRACE_CSV=true

# for tuning poupose
#export NL_OMB_ADD_NOISE=true
#export NL_PUT_RAND_SEED=true
#export NL_SEED_ARRAY1=2005082600
#export NL_SEED_ARRAY2=2005082600

export NL_PRINT_DETAIL=999
export NL_PRINT_DETAIL_GRAD=true
export NL_ANALYSIS_TYPE=3DVAR # QC-OBS, 3DVAR, VERIFY, RANDOMCV
export NL_CHECK_MAX_IV=true
export NL_NTMAX=100
export DA_BACK_ERRORS=$HOME/be/be.cv_5_12km
export NL_VAR_SCALING4=0.01
export NL_LEN_SCALING1=1.0
export NL_LEN_SCALING2=1.0
export NL_LEN_SCALING3=1.0
export NL_LEN_SCALING4=1.0
export NL_LEN_SCALING5=1.0

# non-radiance obs usage
#export NL_USE_SOUNDOBS=false
#export NL_USE_SYNOPOBS=false
#export NL_USE_SHIPSOBS=false
#export NL_USE_METAROBS=false
#export NL_USE_PILOTOBS=false
#export NL_USE_AIREPOBS=false
#export NL_USE_BUOYOBS=false
#export NL_USE_PROFILEROBS=false
#export NL_USE_GPSPWOBS=false
#export NL_USE_GPSREFOBS=false
#export NL_USE_QSCATOBS=false

# satellite retrieval obs usage
export NL_USE_GEOAMVOBS=false
export NL_USE_POLARAMVOBS=false
export NL_USE_SATEMOBS=false
export NL_USE_AIRSRETOBS=false
export NL_USE_SSMIRETRIEVALOBS=false

# radiance obs usage
export NL_USE_AMSUAOBS=true
#export NL_USE_AMSUBOBS=true
#export NL_USE_HIRS3OBS=true
#export NL_USE_AIRSOBS=true
#export NL_USE_EOS_AMSUAOBS=true

# radiances assimilation control
export NL_RTM_OPTION=1    # 1:rttov; 2:crtm
#export NL_PRINT_DETAIL_RADIANCE=true
export NL_NUM_FGAT_TIME=1
export NL_RTMINIT_NSENSOR=2
export NL_RTMINIT_PLATFORM=1,1
export NL_RTMINIT_SATID=15,16
export NL_RTMINIT_SENSOR=3,3
export NL_QC_RAD=true
export NL_READ_BIASCOEF=true
#export NL_BIASCORR=true
export NL_BIASPREP=true
export NL_WRITE_IV_RAD_ASCII=.TRUE.
#export NL_WRITE_OA_RAD_ASCII=.TRUE.
#export NL_THINNING=true
#export NL_THINNING_MESH=120.0,120.0,120.0,120.0,120.0,120.0
export DA_RTTOV_COEFFS=/homebv/wrfhelp/external/rttov/rtcoef_rttov7
#export DA_CRTM_COEFFS=/homebv/wrfhelp/external/crtm/crtm_coefs
export WINDOW_START=-2
export WINDOW_END=2

# 7 run dir
#---------------------------------------
export EXP_DIR=$DAT_DIR/$REGION/$EXPT
#export RUN_DIR=$EXP_DIR

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0

