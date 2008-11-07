#!/bin/ksh
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
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
set echo 

#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_ENS_EP=true
#export RUN_WRFVAR=true
#export NL_NTMAX=500
#export NL_ANALYSIS_TYPE=randomcv
#export NL_PUT_RAND_SEED=true
export NUM_MEMBERS=10
#export NUM_JOBS=4
#export DATE=2006100100
#export NL_SEED_ARRAY1=$DATE
#export NL_SEED_ARRAY2=$MEMBER
#export RUNID=randomcv.$MEMBER
#export RUN_UPDATE_BC=true
#export RUN_WRF=true
export RUN_ENSMEAN=true
export NV=7  #JME fields meaned and perturbed in ETKF.
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'QVAPOR'"} # JME fields

#Experiment details:
export REGION=t64a
export EXPT=test
#export CLEAN=true
export CYCLING=true
export CYCLE_PERIOD=12
export CYCLE_NUMBER=1
export FILE_TYPE=wrfout

export NUM_PROCS=1
#export NUM_PROCS=64
#export LSF_PTILE=32
export LSF_PTILE=1
export QUEUE=regular
export QUEUE=share
export SUBMIT="none"
export RUN_CMD=" "
export WALLCLOCK=60
export LSF_EXCLUSIVE=" "
export PROJECT=48503001
#export PROJECT=64000510

#Time info:
export INITIAL_DATE=2006102712
export FINAL_DATE=2006102712
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=36 #72
export LONG_FCST_RANGE_1=6 #72
export LONG_FCST_TIME_2=06
export LONG_FCST_RANGE_2=6 #48
export LONG_FCST_TIME_3=12
export LONG_FCST_RANGE_3=36 #72
export LONG_FCST_RANGE_3=6 #72
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_4=6 #48

#Directories:
export        FG_TYPE=fnl
export        GRIB_DIR=/rap/datc/data/$FG_TYPE
export        DAT_DIR=/mmm/users/xinzhang/hybrid
export        REG_DIR=$DAT_DIR/$REGION
export        EXP_DIR=$REG_DIR/$EXPT
export        RUN_DIR=$EXP_DIR/run
export         RC_DIR=$DAT_DIR/$REGION/rc
export         FC_DIR=$DAT_DIR/$REGION/$EXPT/fc
export         OB_DIR=$DAT_DIR/$REGION/ob
export         BE_DIR=$DAT_DIR/$REGION/be

export        REL_DIR=/mmm/users/xinzhang/hybrid
export        WPS_DIR=$REL_DIR/wps_3.0
export        WRF_DIR=$HOME/code/trunk/blueice_ibm_opt/WRFV3
export        WRFVAR_DIR=$REL_DIR/wrf           # Latest code.
export    SCRIPTS_DIR=${WRFVAR_DIR}/var/scripts
#export     WRF_BC_DIR=$REL_DIR/WRF_BC                 

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export NL_E_WE=122
export NL_E_SN=110
export MAP_PROJ=lambert
export REF_LAT=33.00002
export REF_LON=137.0
export TRUELAT1=30.0
export TRUELAT2=60.0
export STAND_LON=130.0
export NL_DX=45000
export NL_DY=45000

#WRF:
export NL_TIME_STEP=240  # 15 mins should be plenty for 240km resolution.
#export NL_HISTORY_INTERVAL=360 # wrfout files every 6 hours.

export NL_E_VERT=42
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.995, 0.992, 0.983, 0.975, "\ 
                                      " 0.961, 0.949, 0.932, 0.917, 0.897, "\
                                      " 0.878, 0.855, 0.832, 0.806, 0.778, "\
                                      " 0.749, 0.718, 0.687, 0.654, 0.623, "\
                                      " 0.590, 0.559, 0.526, 0.495, 0.462, "\
                                      " 0.431, 0.398, 0.367, 0.334, 0.304, "\
                                      " 0.272, 0.244, 0.213, 0.187, 0.158, "\
                                      " 0.134, 0.107, 0.085, 0.060, 0.040, 0.018, 0.000 "}
export NL_P_TOP_REQUESTED=5000
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=1
#?export NL_RADT=10
export NL_SF_SFCLAY_PHYSICS=1
export NL_SF_SURFACE_PHYSICS=1
export NL_NUM_SOIL_LAYERS=5
export NL_BL_PBL_PHYSICS=1
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
#?export NL_BASE_TEMP=268.0
#?export NL_DAMPCOEF=0.01
#?export NL_TIME_STEP_SOUND=4

#WRF-Var:
#export NL_ANALYSIS_TYPE=QC-OBS
export NL_CHECK_MAX_IV=true
export NL_CALCULATE_CG_COST_FN=true
export NL_WRITE_FILTERED_OBS=true
#export NL_WRITE_INCREMENTS=false
export NL_USE_GPSREFOBS=true
export NL_USE_AIRSRETOBS=false 
export NL_VAR_SCALING4=1.0 # For Gen_be version 2.1 and higher
#export NL_VAR_SCALING4=0.01 # For Gen_be before version 2.1

### Radiance related setup ###
export NL_CV_OPTIONS_HUM=1
#export NL_USE_OBS_ERRFAC=true    
#export NL_NTMAX=0

export NL_USE_AMSUAOBS=false
export NL_USE_AMSUBOBS=false
export NL_USE_MHSOBS=false
export NL_USE_AIRSOBS=false
export NL_RTM_OPTION=2        # 1:rttov; 2:crtm
export  NL_RTMINIT_NSENSOR=1  # amsua, amsub, mhs
export NL_RTMINIT_PLATFORM=9  #,1,1,1,1,1,1,1
export    NL_RTMINIT_SATID=2  #,15,16,18,15,16,17,18
export   NL_RTMINIT_SENSOR=11 #,3,3,3,4,4,4,15
#export   NL_RAD_MONITORING=1
export NL_QC_RAD=true
export NL_ONLY_SEA_RAD=true
export NL_READ_BIASCOEF=false
export NL_BIASCORR=false
export NL_BIASPREP=false
export NL_WRITE_IV_RAD_ASCII=true
export NL_WRITE_OA_RAD_ASCII=true
export DA_RTTOV_COEFFS=~wrfhelp/external/rttov/rtcoef_rttov7
#export DA_RTTOV_COEFFS=~wrfhelp/external/rttov/rtcoef_rttov8
export DA_CRTM_COEFFS=~wrfhelp/external/crtm/CRTM_08_27_07/crtm_coeffs #new version CRTM coef
export NL_THINNING=true
export NL_THINNING_MESH=120.0
export WINDOW_START=-3 # use +-2h time window for statistics
export WINDOW_END=3

### AIRS related setup ###
export NL_USE_CRTM_KMATRIX=false
export NL_WRITE_JACOBIAN=false
export NL_CRTM_CLOUD=false
export NL_USE_AIRS_MMR=true
export NL_AIRS_WARMEST_FOV=true

### CRTM related setup ###
export NL_CRTM_ATMOSPHERE=4 # 0=INVALID_MODEL, 1=TROPICAL, 2=MIDLATITUDE_SUMMER
                            # 3=MIDLATITUDE_WINTER, 4=SUBARCTIC_SUMMER
			    # 5=SUBARCTIC_WINTER, 6=US_STANDARD_ATMOSPHERE

### VarBC related setup ###
export NL_USE_VARBC=false
export NL_FREEZE_VARBC=false
export NL_VARBC_FACTOR=10.0
export DA_VARBC_IN=${REL_DIR}/VARBC.in_2006102100

# Offline VarBC
#export NL_MAX_VERT_VAR1=0.0
#export NL_MAX_VERT_VAR2=0.0
#export NL_MAX_VERT_VAR3=0.0
#export NL_MAX_VERT_VAR4=0.0
#export NL_MAX_VERT_VAR5=0.0

### Desroziers & Ivanov Observation Error Tuning ###
#export NL_OMB_ADD_NOISE=true
#export NL_PUT_RAND_SEED=true

### Preconditioning setup ###
#export NL_PRECONDITION_CG=false
#export NL_MAX_EXT_ITS=3

### Testing and debugging ###
#export NL_TEST_TRANSFORMS=true
#export NL_PRINT_DETAIL_GRAD=true

### Model error ###
#export NL_USE_MODEL_ERROR=false
#export NL_ALPHACV_METHOD=1   # 1=Perturbations in control variable (vp) space
 export NL_ALPHACV_METHOD=2 # 2=Perturbations in model (xa) space (u, v, t, q, ps).
export NL_ALPHA_CORR_SCALE=500.0
export NL_ALPHA_STD_DEV=0.34626
export NL_ALPHA_STD_DEV=0.14160
export NL_JE_FACTOR=1.0
#optionally define: alpha_truncation (global)
#                   alpha_corr_type  (1=exp, 2=soar, 3=gaussian) (used in power spectrum)
#                   alpha_corr_scale (alpha_rf_lengthscale(:) = 1000.0 * alpha_corr_scale)
#                   alpha_std_dev 

#Continuous job

export SCRIPT=$SCRIPTS_DIR/da_run_suite.ksh

$SCRIPTS_DIR/da_run_job.ksh

exit 0

