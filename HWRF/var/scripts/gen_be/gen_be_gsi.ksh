#-----------------------------------------------------------------------
#! /bin/ksh 
#-----------------------------------------------------------------------
#
# Purpose: Creating WRF-ARW BE statistics for GSI                        
# 
# Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
#
#-----------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from WRF-ARW forecasts.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 2: Run Regional BE code to generate desired statistics for GSI
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi

mkdir -p $WORK_DIR
if [[ ! -d $STAGE0_GSI_DIR ]]; then mkdir -p $STAGE0_GSI_DIR; fi
cd $WORK_DIR

if [[ -e be_for_aero.nl ]]; then rm -f be_for_aero.nl; fi
if $PROCESS_AERO ; then
   cat > be_for_aero.nl << EOF
&be_for_aero_nl
process_aero = ${PROCESS_AERO},
aeros_to_process = $AEROS_TO_PROCESS
/
EOF
fi

#------------------------------------------------------------------------
# Run GSI Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo 
echo $(date) "Start"

if $RUN_GEN_BE_GSI_STAGE0; then
   echo "---------------------------------------------------------------"
   echo "Run GSI Stage 0: Calculate ensemble perturbations from model forecasts."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   $SCRIPTS_DIR/gen_be/gen_be_stage0_gsi.ksh
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 0 for WRF failed with error" $RC
      echo "stage 0" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
#------------------------------------------------------------------------

if $RUN_GEN_BE_GSI_STAGE1; then
if [[ ! -d $STAGE1_GSI_DIR ]]; then mkdir -p $STAGE1_GSI_DIR; fi
   echo "---------------------------------------------------------------"
   echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage1_gsi.exe .

   cat > gen_be_stage1_gsi_nl.nl << EOF
&gen_be_stage1_gsi_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    stage0_gsi_dir = '${STAGE0_GSI_DIR}',
    stage1_gsi_dir = '${STAGE1_GSI_DIR}',
 /
EOF

   ./gen_be_stage1_gsi.exe > gen_be_stage1_gsi.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 1 failed with error" $RC
      echo "stage 1" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run GSI Stage 2: Calculate final BE statistics for GSI
#------------------------------------------------------------------------

if $RUN_GEN_BE_GSI_STAGE2; then

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2_gsi.exe .
#   ln -sf /mmm/users/rizvi/code/Regional_Be/gen_be_stage2_gsi.exe .

   cat > gen_be_stage2_gsi_nl.nl << EOF
&gen_be_stage2_gsi_nl
    stage1_gsi_dir = '${STAGE1_GSI_DIR}',
    nx = ${NUM_WE},
    ny = ${NUM_SN},
    nz = ${NUM_LEVELS},
    lat_bins_in_deg  = ${LAT_BINS_IN_DEG},
    less_levels_from_top  = ${LESS_LEVELS_FROM_TOP},
    debug  = ${DEBUG} /
EOF

    #export JJ=gen_be_stage2_gsi
    #bsub -a poe -R "span[ptile=16]" -n ${NUM_PROCS} -o ${JJ}.out -e ${JJ}.err -J ${JJ} -q ${QUEUE} -W ${WALL_CLOCK} -P ${PROJECT} mpirun.lsf gen_be_stage2_gsi.exe
  
   #GSI_BE_FILE=${WORK_DIR}/wrf-arw-gsi_be            
   #while [[ ! -s ${GSI_BE_FILE} ]] ; do
   #echo "Waiting for file: " ${GSI_BE_FILE}
   #sleep 60
   #done

   if [[ $NUM_PROCS == 1 ]]; then
      ./gen_be_stage2_gsi.exe > gen_be_stage2_gsi.log 2>&1
   else
      mpirun -np $NUM_PROCS ./gen_be_stage2_gsi.exe > gen_be_stage2_gsi.log 2>&1
   fi

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 GSI failed with error" $RC
      echo "stage 2 GSI " > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

cp $WORK_DIR/wrf-arw-gsi_be* $RUN_DIR
cp $WORK_DIR/*.dat   $RUN_DIR
cp $WORK_DIR/fort.*  $RUN_DIR

if $CLEAN; then cd $RUN_DIR ; rm -rf $WORK_DIR $STAGE0_GSI_DIR $STAGE1_GSI_DIR ; fi

echo
echo $(date) "Finished"

touch $RUN_DIR/SUCCESS

exit 0
