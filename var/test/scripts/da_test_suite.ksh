#!/bin/ksh

# da_test_suite.ksh

. da_test_defaults.ksh

if [[ $# != 1 ]]; then
   echo "Arguments test"
   echo "One of : wrfvar wrfvar_tests wrfvar_verbose wrfvar_rttov"
   echo "ideal wrf wrf_real wrf_ideal quick"
   echo "em_real_1 em_real_2 em_real_3 em_real_4 em_real_5"
   exit 1
fi

TEST=$1

. ./setup.ksh
. $CASE/setup.ksh

export REG_DIR=$PWD
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export RUN=${RUN:-run}
export REL_DIR=${REL_DIR:-$HOME/code/$RELEASE/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

if [[ $TYPE == serial ]]; then
   export NUM_PROCS=1
fi

MAX_PROCS=${MAX_PROCS:-$NUM_PROCS}

if [[ $NUM_PROCS -gt $MAX_PROCS ]]; then
   export NUM_PROCS=$MAX_PROCS
fi

if [[ $TYPE == openmp ]]; then
   export OMP_NUM_THREADS=$NUM_PROCS
   export RUN_CMD=" "
fi

export EXPT=${ID}_${TEST}_${NUM_PROCS}
export EXP_DIR=$PWD/$EXPT

if [[ $TEST == wrfvar ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
elif [[ $TEST == wrfvar_rttov ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
   export RTM_OPTION=1 # RTTOV
elif [[ $TEST == wrfvar_tests ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
   export NL_TEST_WRFVAR=true
   export NL_TEST_TRANSFORMS=true
   export NL_TEST_STATISTICS=true
elif [[ $TEST == wrfvar_verbose ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
   export NL_NTMAX=${NL_NTMAX:-2}
   export NL_TRACE_USE_DULL=true
   export NL_TRACE_USE_FREQUENT=true
   export NL_TRACE_ALL_PES=true
   export NL_PRINT_DETAIL_AIREP=true
   export NL_PRINT_DETAIL_RADAR=true
   export NL_PRINT_DETAIL_RAD=true
   export NL_PRINT_DETAIL_XA=true
   export NL_PRINT_DETAIL_XB=true
   export NL_PRINT_DETAIL_OBS=false
   export NL_PRINT_DETAIL_F_OBS=true
   export NL_PRINT_DETAIL_MAP=true
   export NL_PRINT_DETAIL_RAD=true
   export NL_PRINT_DETAIL_GRAD=true
   export NL_PRINT_DETAIL_REGRESSION=true
   export NL_PRINT_DETAIL_SPECTRAL=true
   export NL_PRINT_DETAIL_TESTING=true
   export NL_PRINT_DETAIL_PARALLEL=true
   export NL_PRINT_DETAIL_BE=true
   export NL_PRINT_DETAIL_TIMING=true
   export NL_WRITE_QCW=true
   export NL_WRITE_QRN=true
   export NL_WRITE_QCI=true
   export NL_WRITE_QSN=true
   export NL_WRITE_QGR=true
   export NL_WRITE_FILTERED_OBS=true
   export NL_WRITE_FILTERED_RAD=true
   export NL_WRITE_INCREMENTS=true
   export NL_WRITE_IV_RAD_ASCII=true
   export NL_WRITE_OA_RAD_ASCII=true
   export NL_BIASPREP=true
   export NL_OMB_ADD_NOISE=true
   # stop hitting bug at line 247 of da_write_iv_rad_ascii where rttov coefficient
   # used in crtm run. Zhiquan has fix in local code.
   export NL_WRITE_PROFILE=false
elif [[ $TEST == wrf ]]; then
   export RUN_WRF=true
elif [[ $TEST == wrf_ideal ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
elif [[ $TEST == wrf_real ]]; then
   export RUN_WRF=true
   export RUN_REAL=true
elif [[ $TEST == quick ]]; then
   export RUN_WPS=true
   export RUN_REAL=true
   export RUN_OBSPROC=true
   export RUN_WRFVAR=true
   export RUN_UPDATE_BC=true
   export RUN_WRF=true
   export NL_NTMAX=${NL_NTMAX:-2}
elif [[ $TEST == em_real_1 ]]; then
   export RUN_REAL=true
   export RUN_WRF=true
   export REAL_INPUT_DIR=$PWD/$CASE/rc
   export NL_MP_PHYSICS=3
   export NL_RA_LW_PHYSICS=1
   export NL_RA_SW_PHYSICS=1
   export NL_SF_SFCLAY_PHYSICS=1
   export NL_SF_SURFACE_PHYSICS=1
   export NL_BL_PBL_PHYSICS=1
   export NL_CU_PHYSICS=1
   export NL_NUM_SOIL_LAYERS=5
   export NL_NESTED=F
   export NL_TIME_STEP_SOUND=4
   export NL_MP_ZERO_OUT=0
elif [[ $TEST == em_real_2 ]]; then
   export RUN_REAL=true
   export RUN_WRF=true
   export REAL_INPUT_DIR=$PWD/$CASE/rc
   export NL_MP_PHYSICS=4
   export NL_RA_LW_PHYSICS=1
   export NL_RA_SW_PHYSICS=2
   export NL_SF_SFCLAY_PHYSICS=2
   export NL_SF_SURFACE_PHYSICS=2
   export NL_BL_PBL_PHYSICS=2
   export NL_CU_PHYSICS=2
   export NL_NUM_SOIL_LAYERS=4
   export NL_NESTED=F
   export NL_TIME_STEP_SOUND=4
   export NL_MP_ZERO_OUT=0
elif [[ $TEST == em_real_3 ]]; then
   export RUN_REAL=true
   export RUN_WRF=true
   export REAL_INPUT_DIR=$PWD/$CASE/rc
   export NL_MP_PHYSICS=5
   export NL_RA_LW_PHYSICS=1
   export NL_RA_SW_PHYSICS=2
   export NL_SF_SFCLAY_PHYSICS=2
   export NL_SF_SURFACE_PHYSICS=3
   export NL_BL_PBL_PHYSICS=2
   export NL_CU_PHYSICS=3
   export NL_NUM_SOIL_LAYERS=6
   export NL_NESTED=F
   export NL_TIME_STEP_SOUND=4
   export NL_MP_ZERO_OUT=0
elif [[ $TEST == em_real_4 ]]; then
   export RUN_REAL=true
   export RUN_WRF=true
   export REAL_INPUT_DIR=$PWD/$CASE/rc
   export NL_MP_PHYSICS=5
   export NL_RA_LW_PHYSICS=1
   export NL_RA_SW_PHYSICS=2
   export NL_SF_SFCLAY_PHYSICS=2
   export NL_SF_SURFACE_PHYSICS=3
   export NL_BL_PBL_PHYSICS=2
   export NL_CU_PHYSICS=3
   export NL_NUM_SOIL_LAYERS=6
   export NL_NESTED=F
   export NL_TIME_STEP_SOUND=4
   export NL_MP_ZERO_OUT=0
elif [[ $TEST == em_real_5 ]]; then
   export RUN_REAL=true
   export RUN_WRF=true
   export REAL_INPUT_DIR=$PWD/$CASE/rc
   export NL_MP_PHYSICS=4
   export NL_RA_LW_PHYSICS=3
   export NL_RA_SW_PHYSICS=3
   export NL_SF_SFCLAY_PHYSICS=2
   export NL_SF_SURFACE_PHYSICS=2
   export NL_BL_PBL_PHYSICS=2
   export NL_CU_PHYSICS=2
   export NL_NUM_SOIL_LAYERS=4
   export NL_NESTED=F
   export NL_TIME_STEP_SOUND=4
   export NL_MP_ZERO_OUT=0
   export NL_UCMCALL=1
   export NL_LEVSIZ=59
   export NL_PAERLEV=29
   export NL_CAM_ABS_FREQ_S=21600
   export NL_CAM_ABS_DIM1=4
   export NL_CAM_ABS_DIM2=28
elif [[ $TEST == em_b_wave_1 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_b_wave
elif [[ $TEST == em_b_wave_2 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_b_wave
   export NL_NON_HYDROSTATIC=T
elif [[ $TEST == em_b_wave_3 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_b_wave
   export NL_MP_PHYSICS=2
elif [[ $TEST == em_quarter_ss_1 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_quarter_ss
   export NL_RUN_MINUTES=2
   export NL_HISTORY_INTERVAL=2
   export NL_RESTART_INTERVAL=120
   export NL_TIME_STEP=12
   export NL_E_WE=42   
   export NL_E_SN=82   
   export NL_DX=2000
   export NL_DY=2000
   export NL_ZTOP=20000
   export NL_I_PARENT_START=0
   export NL_J_PARENT_START=0
   export NL_PARENT_GRID_RATIO=1
   export NL_PARENT_TIME_STEP_RATIO=1
   export NL_MP_ZERO_OUT=0
   export NL_DIFF_OPT=2
   export NL_KM_OPT=2
   export NL_DAMP_OPT=1
   export NL_MP_PHYSICS=1
   export NL_ZDAMP=5000
   export NL_DAMPCOEF=0.003
   export NL_KHDIF=500
   export NL_KVDIF=500
   export NL_MIX_FULL_FIELDS=T
   export NL_TIME_STEP_SOUND=6
   export NL_PD_MOIST=F
   export NL_PD_SCALAR=F
   export NL_PD_CHEM=F
   export NL_PD_TKE=F,F,F
   export NL_PERIODIC_X=F
   export NL_PERIODIC_Y=F
   export NL_OPEN_XS=T
   export NL_OPEN_XE=T
   export NL_OPEN_YS=T
   export NL_OPEN_YE=T
   export NL_SYMMETRIC_XS=F
   export NL_SYMMETRIC_XE=F
   export NL_SYMMETRIC_YS=F
   export NL_SYMMETRIC_YE=F
elif [[ $TEST == em_quarter_ss_2 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_quarter_ss
   export NL_RUN_MINUTES=2
   export NL_HISTORY_INTERVAL=2
   export NL_RESTART_INTERVAL=120
   export NL_TIME_STEP=12
   export NL_E_WE=42 
   export NL_E_SN=82 
   export NL_DX=2000
   export NL_DY=2000
   export NL_ZTOP=20000
   export NL_I_PARENT_START=0
   export NL_J_PARENT_START=0
   export NL_PARENT_GRID_RATIO=1
   export NL_PARENT_TIME_STEP_RATIO=1
   export NL_MP_ZERO_OUT=0
   export NL_DIFF_OPT=2
   export NL_KM_OPT=2
   export NL_DAMP_OPT=1
   export NL_MP_PHYSICS=1
   export NL_ZDAMP=5000
   export NL_DAMPCOEF=0.003
   export NL_KHDIF=500
   export NL_KVDIF=500
   export NL_MIX_FULL_FIELDS=T
   export NL_TIME_STEP_SOUND=6
   export NL_PD_MOIST=F
   export NL_PD_SCALAR=F
   export NL_PD_CHEM=F
   export NL_PD_TKE=F
   export NL_PERIODIC_X=T
   export NL_PERIODIC_Y=T
   export NL_OPEN_XS=F
   export NL_OPEN_XE=F
   export NL_OPEN_YS=F
   export NL_OPEN_YE=F
   export NL_SYMMETRIC_XS=F
   export NL_SYMMETRIC_XE=F
   export NL_SYMMETRIC_YS=F
   export NL_SYMMETRIC_YE=F
elif [[ $TEST == em_quarter_ss_3 ]]; then
   export RUN_WRF=true
   export RUN_IDEAL=${RUN_IDEAL:-true}
   export SCENARIO=em_quarter_ss
   export NL_RUN_MINUTES=2
   export NL_HISTORY_INTERVAL=2
   export NL_RESTART_INTERVAL=120
   export NL_TIME_STEP=12
   export NL_E_WE=42
   export NL_E_SN=82
   export NL_DX=2000
   export NL_DY=2000
   export NL_ZTOP=20000
   export NL_I_PARENT_START=0
   export NL_J_PARENT_START=0
   export NL_PARENT_GRID_RATIO=1
   export NL_PARENT_TIME_STEP_RATIO=1
   export NL_MP_ZERO_OUT=0
   export NL_DIFF_OPT=2
   export NL_KM_OPT=3
   export NL_DAMP_OPT=1
   export NL_MP_PHYSICS=2
   export NL_ZDAMP=5000
   export NL_DAMPCOEF=0.003
   export NL_KHDIF=500
   export NL_KVDIF=500
   export NL_MIX_FULL_FIELDS=T
   export NL_TIME_STEP_SOUND=6
   export NL_PD_MOIST=F
   export NL_PD_SCALAR=F
   export NL_PD_CHEM=F
   export NL_PD_TKE=F
   export NL_PERIODIC_X=T
   export NL_PERIODIC_Y=T
   export NL_OPEN_XS=F
   export NL_OPEN_XE=F
   export NL_OPEN_YS=F
   export NL_OPEN_YE=F
   export NL_SYMMETRIC_XS=F
   export NL_SYMMETRIC_XE=F
   export NL_SYMMETRIC_YS=F
   export NL_SYMMETRIC_YE=F
   export NL_NON_HYDROSTATIC=F
else
   echo "Unknown test $TEST"
   exit 1
fi

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh

