#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage4_regional.ksh
#
# Purpose: To calculate correlation lengthscales for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

echo "---------------------------------------------------------------"
if ${USE_RFi}; then
   echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)." 
else
   echo "Run Stage 4: Calculate horizontal covariances (" $WAVELET_NBAND$WAVELET_NAME$WAVELET_FILT_LEN "-wavelet variances)."
fi
echo "---------------------------------------------------------------"

export BEGIN_CPU=$(date)
echo "Beginning CPU time: ${BEGIN_CPU}"

export TMP_DIR=${WORK_DIR}/gen_be_stage4_regional
if ${USE_RFi}; then
   export TMP_DIR=${TMP_DIR}.${STRIDE}
fi

if [[ ! -d $TMP_DIR ]]; then mkdir $TMP_DIR 2> /dev/null; fi

for VARIABLE in $CONTROL_VARIABLES; do

   # Check data exists:
   if [[ ! -d ${WORK_DIR}/$VARIABLE ]]; then
      echo "Input data directory ${WORK_DIR}/$VARIABLE is missing. Exiting"
      exit 1
   fi

   if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ]]; then
      let MAX_VINDEX=1
      export PRINT_WAVELETS=.true.
   else
      let MAX_VINDEX=$NUM_LEVELS
      export PRINT_WAVELETS=.false.
   fi

   let VINDEX=1
   let JOB=1

   while [[ $VINDEX -le $MAX_VINDEX ]]; do
      export TMP_DIR1=${TMP_DIR}/dir.${VARIABLE}${VINDEX}
      mkdir ${TMP_DIR1} 2> /dev/null
      cd ${TMP_DIR1}

      ln -sf ${BUILD_DIR}/gen_be_stage4_regional.exe .

      # Create namelist:
      cat > gen_be_stage4_regional_nl.nl << EOF
&gen_be_stage4_regional_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    ne = ${NE},
    k = ${VINDEX},
    stride = ${STRIDE},
    nbins = ${NBINS},
    ibin = ${IBIN},
    run_dir = '${WORK_DIR}',
    print_wavelets = ${PRINT_WAVELETS},
    do_normalize = ${DO_NORMALIZE},
    lf = ${WAVELET_FILT_LEN},
    namw = '${WAVELET_NAME}',
    nb = ${WAVELET_NBAND},
    use_rf = ${USE_RF},
    allow_missing_dates = ${ALLOW_MISSING_DATES}, /
EOF
 
      if $LOCAL; then
         if $SMPAR; then
            echo "Submitting job for variable $VARIABLE and vertical index $VINDEX using SMPAR"
            export OMP_NUM_THREADS=32
            JJ=gen_be_stage4_regional_${VARIABLE}_${VINDEX}
            # "-K" means "wait for job to complete":
            bsub -e ${JJ}.err -J $JJ -K -n 1 -o ${JJ}.out -P 64000510 -q debug -R "span[ptile=${OMP_NUM_THREADS}]" -W 6:00 ./gen_be_stage4_regional.exe
         else
            echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on local machine"
            ./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1 &
         fi
      else
         export MACHINE=${MACHINES[$JOB]}
         echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on $MACHINE"
         (rsh -n $MACHINE "cd $TMP_DIR1; ./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1") &

         sleep 2 # Create small gap between submissions to avoid overwriting output.
      fi

      let JOB=$JOB+1
      let VINDEX=$VINDEX+1

      if [[ $JOB -gt $NUM_JOBS || $VINDEX -gt $MAX_VINDEX ]]; then
         wait # Wait for current jobs to finish.
         let JOB=1
      fi
   done  # End loop over VINDEX.

   if ${USE_RFi};then
      # Collect files together: 
      let VINDEX=1
      cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}

      if [[ $MAX_VINDEX -gt 1 ]]; then
         let VINDEX=2
         while [[ $VINDEX -le $MAX_VINDEX ]]; do
            cat ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* >> ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}
            let VINDEX=$VINDEX+1
         done
      fi
   fi

done     # End loop over VARIABLE.

export END_CPU=$(date)
echo "Ending CPU time: ${END_CPU}"

exit 0

