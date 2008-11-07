#! /bin/ksh
#-----------------------------------------------------------------------
# Script da_run_ens_ep.ksh
#
# Purpose: To calculate ensemble perturbations for use in WRF-Var to
# represent flow-dependent forecast errors.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#export DATE=2006100106
#export FCST_RANGE=6
#export NUM_MEMBERS=12
#export WRFVAR_DIR=/mmm/users/dmbarker/code/trunk/wrf
#export DAT_DIR=/mmm/users/dmbarker/data
#export REGION=amps1_240km
#export EXPT=test1

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#Define environment variables:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

export EP_DIR=${EP_DIR:-${FC_DIR}/${DATE}/ep}
if [[ ! -d $EP_DIR ]]; then mkdir -p ${EP_DIR}; fi

export RUN_DIR=${RUN_DIR:-${EXP_DIR}/run/${DATE}/ep}
export WORK_DIR=${RUN_DIR}/working
if [[ ! -d $WORK_DIR ]]; then mkdir -p ${WORK_DIR}; fi
cd $WORK_DIR

export BEGIN_CPU=$(date)
echo "Beginning CPU time: ${BEGIN_CPU}"

export YYYY=$(echo $DATE | cut -c1-4)
export   MM=$(echo $DATE | cut -c5-6)
export   DD=$(echo $DATE | cut -c7-8)
export   HH=$(echo $DATE | cut -c9-10)
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00

export PREV_DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE -$FCST_RANGE)
export DIRECTORY=${FC_DIR}/${PREV_DATE}
export FILENAME=wrfout_d01_${FILE_DATE}

#Run:
if [[ NL_ALPHACV_METHOD == 1 ]]; then
   echo "gen_be_ep1: Calculating CV space (vp) perturbations at time " $DATE
   ln -sf ${BUILD_DIR}/gen_be_ep1.exe .
   ./gen_be_ep1.exe $DATE $NUM_MEMBERS $DIRECTORY $FILENAME > gen_be_ep1.out 2>&1
else
   echo "gen_be_ep2: Calculating model space (xa) perturbations at time " $DATE
   ln -sf ${BUILD_DIR}/gen_be_ep2.exe .
   ./gen_be_ep2.exe $DATE $NUM_MEMBERS $DIRECTORY $FILENAME > gen_be_ep2.out 2>&1
fi

#Tidy:
rm -rf tmp* 2> /dev/null
mv ${WORK_DIR}/* $EP_DIR

export END_CPU=$(date)
echo "Ending CPU time: ${END_CPU}"

exit 0

