#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_set_defaults.ksh
#
# Purpose: This scripts sets the environment variables used within the 
# entire scripts system to values corresponding to a "standard case".
# The standard case currently used is the con200 application.
# The namelist parameters specified here is that sub-set of the entire
# range of parameters for all namelists that we have found necessary
# to change through experience of the applications tested so far. As
# new applications are tests, additional environment valiables may
# be added. 

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

# Decide which stages to run (run if true):

export RUN_ENS_EP=${RUN_ENS_EP:-false}
export RUN_ENSMEAN=${RUN_ENSMEAN:-false}
export RUN_ETKF=${RUN_ETKF:-false}
export RUN_IDEAL=${RUN_IDEAL:-false}
export RUN_NDOWN=${RUN_NDOWN:-false}
export RUN_NUP=${RUN_NUP:-false}
export RUN_OBSPROC=${RUN_OBSPROC:-false}
export RUN_REAL=${RUN_REAL:-false}
export RUN_RESTORE_DATA_GRIB=${RUN_RESTORE_DATA_GRIB:-false}
export RUN_RESTORE_DATA_RTOBS=${RUN_RESTORE_DATA_RTOBS:-false}
export RUN_UPDATE_BC=${RUN_UPDATE_BC:-false}
export RUN_WPB=${RUN_WPB:-false}
export RUN_WPS=${RUN_WPS:-false}
export RUN_WRF=${RUN_WRF:-false}
export RUN_WRFVAR=${RUN_WRFVAR:-false}

# Experiment details:
export DUMMY=${DUMMY:-false}
export RELEASE=${RELEASE:-trunk}
export REGION=${REGION:-con200}
export DOMAINS=${DOMAINS:-01}                            # Domain names.
export EXPT=${EXPT:-expt}                             # Experiment name.
export ID=${ID:-test}
export CLEAN=${CLEAN:-false}
export DOUBLE=${DOUBLE:-false}
export CYCLING=${CYCLING:-false}                       # Cold start (false), cycle (true).
export CHECK_SVNVERSION=${CHECK_SVNVERSION:-true}
# Combination of cold start and cycling runs for AFWA projects: cold start (for 00,12) cycling (for 06,18)
export FG_TYPE=${FG_TYPE:-GFS}

# Scheduling:
export SUBMIT=${SUBMIT:-LSF}
export PROJECT=${PROJECT:-48500053}
export QUEUE=${QUEUE:-regular}
export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
export PREV_JOBID=${PREV_JOBID:-test}
export HOSTS=${HOSTS:-${HOME}/hosts}

export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors
export MAX_PROCS=${MAX_PROCS:-$NUM_PROCS}              # Maximum number of possible processors

if [[ $NUM_PROCS -gt $MAX_PROCS ]]; then
   export NUM_PROCS=$MAX_PROCS
fi

export RUN=${RUN:-${ID}_${NUM_PROCS}}
export JOBNAME=${JOBNAME:-${RELEASE}_${REGION}_${EXPT}_${RUN}}

# Cannot put - options inside default substitution
if [[ $SUBMIT == "LoadLeveller" ]]; then
   export SUBMIT_OPTIONS1='# @ job_type         = parallel'
   export SUBMIT_OPTIONS2='# @ environment      = COPY_ALL'
   export SUBMIT_OPTIONS3='# @ notification     = never'
   export SUBMIT_OPTIONS4='# @ network.MPI      = css0,shared,ip'
   export SUBMIT_OPTIONS5='# @ checkpoint       = no'
   export SUBMIT_OPTIONS6='# @ class            = share'
   export SUBMIT_OPTIONS7='# @ node_usage       = shared'
   export RUN_CMD_DEFAULT=" " # space important
   export LL_PTILE=${LL_PTILE:-1}
elif [[ $SUBMIT == "LSF" ]]; then 
   # Use SMT on an 8 processor node
   export LSF_PTILE=${LSF_PTILE:-16}
   export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-10} # minutes
   export SUBMIT_OPTIONS1="#BSUB -R span[ptile=$LSF_PTILE]"
   export SUBMIT_WAIT_FLAG="-K"
   export RUN_CMD_DEFAULT="mpirun.lsf"
elif [[ $SUBMIT == "PBS" ]]; then 
   export RUN_CMD_DEFAULT="aprun -m exclusive -N$TEMP -n$NUM_PROCS"
elif [[ $SUBMIT == none ]]; then
   if [[ -f $HOSTS ]]; then
      export RUN_CMD_DEFAULT="mpirun -machinefile $HOSTS -np $NUM_PROCS"
   else
      export RUN_CMD_DEFAULT="mpirun -np $NUM_PROCS"
   fi
fi

export RUN_CMD=${RUN_CMD:-$RUN_CMD_DEFAULT}

# Directories:
export REL_DIR=${REL_DIR:-$HOME/code/$RELEASE}    # Directory containing codes.
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}  # WRF-Var code directory.
export BUILD_DIR=${BUILD_DIR:-$WRFVAR_DIR/var/da}     # WRF-Var executable location.
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}           # WPS directory.
export WPB_DIR=${WPB_DIR:-$REL_DIR/wpb}           # Perturbed LBC dir.
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}           # WRF directory.
export WRFNL_DIR=${WRFNL_DIR:-$REL_DIR/wrfnl}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus} # WRF TL/ADJ directory.
export DAT_DIR=${DAT_DIR:-$HOME/data}             # Top-level data directory.
export MSS_GRIB_DIR=${MSS_GRIB_DIR:-mss:/DSS/DS083.2/data/grib1}
export GRIB_DIR=${GRIB_DIR:-$DAT_DIR/${FG_TYPE}}         # GRIB input data dir.
export MSS_RTOBS_DIR=${MSS_RTOBS_DIR:-mss:/BRESCH/RT/DATA}
export RTOBS_DIR=${RTOBS_DIR:-$DAT_DIR/rtobs}     # Real-time observation directory.
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}       # Region-specific data dir.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}         # Experiment-specific data dir.
export OB_DIR=${OB_DIR:-$EXP_DIR/ob}              # Observation data dir.
export RC_DIR=${RC_DIR:-$EXP_DIR/rc}              # Reconfiguration directory
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}              # Forecast directory
export ETKF_DIR=${ETKF_DIR:-$FC_DIR/etkf}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
export SCRIPT=${SCRIPT:-$SCRIPTS_DIR/da_run_wrfvar.ksh}
export VTABLE_DIR=${VTABLE_DIR:-$WPS_DIR/ungrib/Variable_Tables}

# Time info:
export DATE=${DATE:-2003010100}                   # Current date.
export INITIAL_DATE=${INITIAL_DATE:-2003010100}   # Start date of test period
export FINAL_DATE=${FINAL_DATE:-2003012800}       # Final date of test period.
export NL_NUM_FGAT_TIME=${NL_NUM_FGAT_TIME:-1}
export LBC_FREQ=${LBC_FREQ:-06}
export LBC_FREQ_SS=`echo $LBC_FREQ \* 3600 |bc -l`
export NL_INTERVAL_SECONDS=$LBC_FREQ_SS
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}                # Assimilation frequency.
export FCST_RANGE=${FCST_RANGE:-$CYCLE_PERIOD}
export NL_RUN_HOURS=${NL_RUN_HOURS:-$FCST_RANGE}
export CYCLE_NUMBER=${CYCLE_NUMBER:-0}
export OBS_FREQ=${OBS_FREQ:-12}
export WINDOW_START=${WINDOW_START:-0}                 # Start ob window difference (hrs).
export WINDOW_END=${WINDOW_END:-0}                     # End ob window difference (hrs).
export LONG_FCST_TIME_1=${LONG_FCST_TIME_1:-99}
export LONG_FCST_TIME_2=${LONG_FCST_TIME_2:-99}
export LONG_FCST_TIME_3=${LONG_FCST_TIME_3:-99}
export LONG_FCST_TIME_4=${LONG_FCST_TIME_4:-99}
export LONG_FCST_RANGE_1=${LONG_FCST_RANGE_1:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_2=${LONG_FCST_RANGE_2:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_3=${LONG_FCST_RANGE_3:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_4=${LONG_FCST_RANGE_4:-$CYCLE_PERIOD}

# Diagnostics:
export OK='<FONT COLOR="green">'
export ERR='<FONT COLOR="red">'
export END='</FONT>'

# WPS:
export RUN_GEOGRID=${RUN_GEOGRID:-true}                 # Run GEOGRID, or not.
export RUN_UNGRIB_METGRID_AFWA=${RUN_UNGRIB_METGRID_AFWA:-false}
export RUN_UNGRIB_METGRID_KMA=${RUN_UNGRIB_METGRID_KMA:-false}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}
export WPS_GEOG_DIR=${WPS_GEOG_DIR:-~wrfhelp/WPS_GEOG} 
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export MAP_PROJ=${MAP_PROJ:-lambert}
export REF_LAT=${REF_LAT:-40.0}
export REF_LON=${REF_LON:--98.0}
export TRUELAT1=${TRUELAT1:-30.0}
export TRUELAT2=${TRUELAT2:-60.0}
export STAND_LON=${STAND_LON:--98.0}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}
export GEOG_DATA_RES=${GEOG_DATA_RES:-30s}
export VTABLE_TYPE=${VTABLE_TYPE:-GFS}
export METGRID_TABLE_TYPE=${METGRID_TABLE_TYPE:-ARW}
export CONSTANTS1=${CONSTANTS1:-*}
export CONSTANTS2=${CONSTANTS2:-*}
export NL_DEBUG_LEVEL=${NL_DEBUG_LEVEL:-0}

# WRF real (not already covered above):
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export NL_FRAMES_PER_OUTFILE=${NL_FRAMES_PER_OUTFILE:-1}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-720}          # (minutes)
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                      " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                      " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                      " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                      " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000"}
export NL_E_VERT=${NL_E_VERT:-28}                   #
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           #
export NL_RADT=$(( NL_DX/1000 )) # 1 minute per km of dx
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_NUM_SOIL_LAYERS=${NL_NUM_SOIL_LAYERS:-5}
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_CUDT=${NL_CUDT:-5}           #(1=, 2=,3=).
export NL_W_DAMPING=${NL_W_DAMPING:-0}            #
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             #
export NL_KM_OPT=${NL_KM_OPT:-1}               #
export NL_BASE_TEMP=${NL_BASE_TEMP:-290.0}               #
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-6}    #
export NL_SPECIFIED=${NL_SPECIFIED:-true}          #

# WRF (not already covered above):
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-true}
export NL_INPUT_FROM_FILE=${NL_INPUT_FROM_FILE:-true}
export NL_INPUT_OUTNAME=${NL_INPUT_OUTNAME:-'wrfinput_d<domain>_<date>'}
export NL_INPUTOUT_INTERVAL=${NL_INPUTOUT_INTERVAL:-360}
export NL_INPUTOUT_BEGIN_H=${NL_INPUTOUT_BEGIN_H:-$CYCLE_PERIOD} # Output input format start.
export NL_INPUTOUT_END_H=${NL_INPUTOUT_END_H:-$FCST_RANGE}       # Output input format end.
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}

# OBSPROC (not covered above):
export MAX_OB_RANGE=${MAX_OB_RANGE:-2}             # Maximum difference O, B (hours)
export MAX_NUMBER_OF_OBS=${MAX_NUMBER_OF_OBS:-70000}
export THINING_SATOB=${THINING_SATOB:-false}
export THINING_SSMI=${THINING_SSMI:-false}
export THINING_QSCAT=${THINING_QSCAT:-false}
export PS0=${PS0:-100000.0}
export TS0=${TS0:-300.0}
export TLP=${TLP:-50.0}
export P_TOP_REQUESTED=${P_TOP_REQUESTED:-1000.0}
export NL_USE_FOR=${NL_USE_FOR:-3DVAR}
export NL_NUM_SLOTS_PAST=${NL_NUM_SLOTS_PAST:-3}
export NL_NUM_SLOTS_AHEAD=${NL_NUM_SLOTS_AHEAD:-3}

# WRF-Var (not covered above):
export NL_ANALYSIS_TYPE=${NL_ANALYSIS_TYPE:-"3D-VAR"}  # Analysis type.
export NL_MULTI_INC=${NL_MULTI_INC:-0}  # Analysis type.
export NL_VAR4D=${NL_VAR4D:-false}
export NL_GLOBAL=${NL_GLOBAL:-false}
export BE_DIR=${BE_DIR:-$REG_DIR/be}                   # Background error covariance directory.
export BIASCORR_DIR=${BIASCORR_DIR:-$WRFVAR_DIR/var/run/biascorr}
export OBS_TUNING_DIR=${OBS_TUNING_DIR:-$WRFVAR_DIR/var/run/obs_tuning}
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # background errors.
export NL_OB_FORMAT=${NL_OB_FORMAT:-2}   # Observation format: 1=BUFR, 2=ASCII "little_r"
export NL_LATS_STATS_OPTION_FALSE=${NL_LATS_STATS_OPTION_FALSE:-false}
export NL_CV_OPTIONS_HUM=${NL_CV_OPTIONS_HUM:-1} # Humidity control variable.
export NL_CHECK_MAX_IV=${NL_CHECK_MAX_IV:-true} # QC on O-B differences.
export NL_NTMAX=${NL_NTMAX:-100}         # Maximum number of inner loop iterations.
export NL_CHECK_RH=${NL_CHECK_RH:-2}     # RH bounds check.
export NL_JCDFI_USE=${NL_JCDFI_USE:-false} # Turn off JcDF option
export NL_JCDFI_IO=${NL_JCDFI_IO:-false} # Turn off JcDF IO
export OUTER_LOOP=${OUTER_ITER:-1}       # OUter loop number
export NL_PUT_RAND_SEED=${NL_PUT_RAND_SEED:-false} # Set to true if want to specify seeds explicitly.
export NL_SEED_ARRAY1=${NL_SEED_ARRAY1:-$DATE} # Random seed.
export NL_SEED_ARRAY2=${NL_SEED_ARRAY2:-$DATE} # Random seed.

# From Update_BC:
export PHASE=${PHASE:-false}     # Indicate which phase update_bc is.
export NL_LOW_BDY_ONLY=${NL_LOW_BDY_ONLY:-false}
export NL_UPDATE_LSM=${NL_UPDATE_LSM:-false}

# Ensemble parameters:
export NUM_MEMBERS=${NUM_MEMBERS:-0}                   # Number of ensemble members.
export NUM_JOBS=${NUM_JOBS:-0}                         # Number of parallel jobs to run.
export MEM=${MEM:-1}                                   # Ensemble member.
export FILE_TYPE=${FILE_TYPE:-wrfinput}                # ETKF input file-type.
export NV=${NV:-15}                                    # Number of ETKF variables.
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'TSLB'", "'TSK'", \
                "'QCLOUD'", "'QRAIN'", "'QVAPOR'", "'U10'", "'V10'", "'T2'", "'Q2'"} # ETKF variable names
export NACCUMT1=${NACCUMT1:-1}                         # ETKF parameter.
export NACCUMT2=${NACCUMT2:--1}                        # ETKF parameter.
export NSTARTACCUM1=${NSTARTACCUM1:-1}                 # ETKF parameter.
export NSTARTACCUM2=${NSTARTACCUM2:-1}                 # ETKF parameter.
export CYCLE_NUMBER=${CYCLE_NUMBER:-0}                 # ETKF parameter.
export TAINFLATINPUT=${TAINFLATINPUT:-1.0}             # ETKF parameter.
export RHOINPUT=${RHOINPUT:-1.0}                       # ETKF parameter.

#PSOT:
if [[ $DOMAINS == 01 ]]; then
   # This logic only works for a single domain
   let HALF_E_WE=$NL_E_WE/2                               # Center of w-e domain
   let HALF_E_SN=$NL_E_SN/2                               # Center of s-n domain
   export PSEUDO_X_LIST=${PSEUDO_X_LIST:-"$HALF_E_WE $HALF_E_WE $HALF_E_WE $HALF_E_WE $HALF_E_WE"}
                                                       # Grid indice for lon.
   export PSEUDO_Y_LIST=${PSEUDO_Y_LIST:-"$HALF_E_SN $HALF_E_SN $HALF_E_SN $HALF_E_SN $HALF_E_SN"}
fi
export PSEUDO_VAR_SIZE=${PSEUDO_VAR_SIZE:-5}           # Number of tests
export PSEUDO_VAR_LIST=${PSEUDO_VAR_LIST:-"u u t t q"} # Variables for each of the PSOTs
export PSEUDO_VAL_LIST=${PSEUDO_VAL_LIST:-"1.0 1.0 1.0 1.0 0.001"}
                                                       # Obs. values
export PSEUDO_ERR_LIST=${PSEUDO_ERR_LIST:-"1.0 1.0 1.0 1.0 0.001"}
                                                       # Sigma_b (O-B) values
                                                       # Grid indice for lat.
export PSEUDO_Z_LIST=${PSEUDO_Z_LIST:-"7 19  7 13  7"}
                                                       # level indice for half mass Eta levels
                                                       # Level  7: eta=0.86
                                                       # Level 19: eta=0.273
                                                       # Level 13: eta=0.507

#Alpha control variable parameters:
export NL_ENSDIM_ALPHA=${NL_ENSDIM_ALPHA:-$NUM_MEMBERS} # Number of ensemble members for alpha.
export NL_ALPHACV_METHOD=${NL_ALPHACV_METHOD:-2}       # 1=vp space, 2=xa' space perts.
export NL_ALPHA_TRUNCATION=${NL_ALPHA_TRUNCATION:-0}   # Spectral truncation (global only).
export NL_ALPHA_CORR_TYPE=${NL_ALPHA_CORR_TYPE:-3}     # 1=Exponential, 2=SOAR, 3=Gaussian.
export NL_ALPHA_CORR_SCALE=${NL_ALPHA_CORR_SCALE:-1500} # Localization lengthscale (km)
export NL_ALPHA_STD_DEV=${NL_ALPHA_STD_DEV:-1.0}       # Alpha standard deviation.
export NL_JB_FACTOR=${NL_JB_FACTOR:-1.0}               # Cost function multiplicative factor.
export NL_JE_FACTOR=${NL_JE_FACTOR:-1.0}               # Cost function multiplicative factor.
