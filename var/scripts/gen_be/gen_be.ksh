#!/bin/ksh
#-----------------------------------------------------------------------
# Purpose : Create BE statistics from input perturbation files.
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 2: Calculate regression coefficients.
# Run Stage 2a: Calculate control variable fields.
# Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
# Run Stage 4: Calculate horizontal covariances.
# Finally, gather data together into a single BE file.
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
if [[ ! -d $STAGE0_DIR ]]; then mkdir -p $STAGE0_DIR; fi

mkdir -p $WORK_DIR
cd $WORK_DIR

# List of control variables:
   if [[ $NL_CV_OPTIONS == 7 ]]; then
      if $GLOBAL; then
         echo "ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR "
         echo ""
         echo "Can not run for global with CV_OPTIONS=7"
         exit 1
      fi
      for SV in fullflds u v t rh ps; do mkdir -p $SV; done
   else
      for SV in fullflds psi chi t rh ps; do mkdir -p $SV; done
   fi

for CV in $CONTROL_VARIABLES; do mkdir -p $CV; done

#------------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo 
echo $(date) "Start"
echo "WRFVAR_DIR is" $WRFVAR_DIR $(svnversion $WRFVAR_DIR)
echo "RUN_DIR is" $RUN_DIR

if $RUN_GEN_BE_STAGE0; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 0: Calculate ensemble perturbations from model forecasts."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   $SCRIPTS_DIR/gen_be/gen_be_stage0_wrf.ksh
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

if $RUN_GEN_BE_STAGE1; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage1.exe .

   cat > gen_be_stage1_nl.nl << EOF
&gen_be_stage1_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    cv_options = ${NL_CV_OPTIONS},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT},
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    dat_dir = '${STAGE0_DIR}',
    allow_missing_dates = ${ALLOW_MISSING_DATES}, /
EOF

   ./gen_be_stage1.exe > gen_be_stage1.log 2>&1
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
#  Run Stage 2: Calculate regression coefficients.
#------------------------------------------------------------------------

#  Do not run Stage 2 and 2a for CV_OPTIONS = 7
if [[ $NL_CV_OPTIONS == 7 ]]; then
   echo "---------------------------------------------------------------"
   echo "Not running Stage 2 for CV_OPTIONS=7"
   echo "---------------------------------------------------------------"
fi

if $RUN_GEN_BE_STAGE2; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2.exe .

   cat > gen_be_stage2_nl.nl << EOF
&gen_be_stage2_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    ne = ${NE},
    testing_eofs = ${TESTING_EOFS},
    allow_missing_dates = ${ALLOW_MISSING_DATES}, /
EOF

   ./gen_be_stage2.exe > gen_be_stage2.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 failed with error" $RC
      echo "stage 2" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 2a: Calculate control variable fields.
#------------------------------------------------------------------------
if [[ $NL_CV_OPTIONS == 7 ]]; then
   echo "---------------------------------------------------------------"
   echo "Not running Stage 2a for CV_OPTIONS=7"
   echo "---------------------------------------------------------------"
elif [[ $NL_CV_OPTIONS == 6 ]]; then
   echo "---------------------------------------------------------------"
   echo "Not running Stage 2a for CV_OPTIONS=6"
   echo "---------------------------------------------------------------"
fi

if $RUN_GEN_BE_STAGE2A; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 2a: Calculate control variable fields."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2a.exe .

   cat > gen_be_stage2a_nl.nl << EOF
&gen_be_stage2a_nl
    start_date = '${START_DATE}',
    cv_options = ${NL_CV_OPTIONS},
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    ne = ${NE},
    num_passes = ${NUM_PASSES},
    rf_scale = ${RF_SCALE},
    allow_missing_dates = ${ALLOW_MISSING_DATES}, /
EOF

   ./gen_be_stage2a.exe > gen_be_stage2a.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2a failed with error" $RC
      echo "stage 2a" > $RUN_DIR/FAIL
      exit 1
   fi

   rm -rf ${DELETE_DIRS} 2> /dev/null
   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE3; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 3: Read 3D control variable fields, and calculate vertical covariances."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage3.exe .

   for CV in $CONTROL_VARIABLES; do
      cat > gen_be_stage3_nl.nl << EOF
&gen_be_stage3_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${CV}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT},
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    testing_eofs = ${TESTING_EOFS},
    use_global_eofs = ${USE_GLOBAL_EOFS},
    data_on_levels = ${DATA_ON_LEVELS},
    allow_missing_dates = ${ALLOW_MISSING_DATES}, /
EOF

      ./gen_be_stage3.exe > gen_be_stage3.${CV}.log 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 3 for $CV failed with error" $RC
         echo "stage 3" > $RUN_DIR/FAIL
         exit 1
      fi
   done

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi

#------------------------------------------------------------------------
#  Run Stage 4: Calculate horizontal covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE4; then

   if $GLOBAL; then    
      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (global power spectra)."
      echo "---------------------------------------------------------------"

      export BEGIN_CPU=$(date)
      echo "Beginning CPU time: ${BEGIN_CPU}"

      ${SCRIPTS_DIR}/gen_be/gen_be_stage4_global.ksh > gen_be_stage4_global.log 2>&1

   else
      echo "---------------------------------------------------------------"
      if ${USE_RFi}; then
         echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)."
      else
         echo "Run Stage 4: Calculate horizontal covariances (" $WAVELET_NBAND$WAVELET_NAME$WAVELET_FILT_LEN "-wavelet variances)."
      fi
      echo "---------------------------------------------------------------"

      export BEGIN_CPU=$(date)
      echo "Beginning CPU time: ${BEGIN_CPU}"

      ${SCRIPTS_DIR}/gen_be/gen_be_stage4_regional.ksh > gen_be_stage4_regional.log 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 4 failed with error" $RC
         echo "stage 4" > $RUN_DIR/FAIL
         exit 1
      fi
   fi 

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Finally, gather data together into a single BE file:
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS; then
   ln -sf ${BUILD_DIR}/gen_be_diags.exe .

   cat > gen_be_diags_nl.nl << EOF
&gen_be_diags_nl
   uh_method = '${UH_METHOD}',
   n_smth_sl = ${N_SMTH_SL},
   use_rf = ${USE_RF},
   do_normalize = ${DO_NORMALIZE},
   cv_options = ${NL_CV_OPTIONS}, /
EOF

   ./gen_be_diags.exe > gen_be_diags.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags failed with error" $RC
      echo "gen_be_diags" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Read BE file to check data packed correctly, and write plot diagnostics.
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS_READ; then
   cat > gen_be_diags_nl.nl << EOF
&gen_be_diags_nl
   uh_method = '${UH_METHOD}',
   cv_options = ${NL_CV_OPTIONS}, /
EOF

   ln -sf ${BUILD_DIR}/gen_be_diags_read.exe .
   ./gen_be_diags_read.exe > gen_be_diags_read.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags_read failed with error" $RC
      echo "gen_be_diags_read" > $RUN_DIR/FAIL
      exit 1
   fi
fi

#------------------------------------------------------------------------
#  Calculate multivariate regression diagnostics:
#------------------------------------------------------------------------

if $RUN_GEN_BE_MULTICOV; then
   # Calculate chi diagnostics:
   export VARIABLE1=chi_u
   export VARIABLE2=chi

   $SCRIPTS_DIR/gen_be/gen_be_cov3d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov3d (chi) failed with error" $RC
      echo "gen_be_cov3d (chi)" > $RUN_DIR/FAIL
      exit 1
   fi

   # Calculate T diagnostics:
   export VARIABLE1=t_u
   export VARIABLE2=t

   $SCRIPTS_DIR/gen_be/gen_be_cov3d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov3d (T) failed with error" $RC
      echo "gen_be_cov3d (T)" > $RUN_DIR/FAIL
      exit 1
   fi

   # Calculate ps diagnostics:
   export VARIABLE1=ps_u
   export VARIABLE2=ps

   $SCRIPTS_DIR/gen_be/gen_be_cov2d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov2d failed with error" $RC
      echo gen_be_cov2d > $RUN_DIR/FAIL
      exit 1
   fi
fi

# Preserve the interesting log files
cp $WORK_DIR/*log $RUN_DIR
cp $STAGE0_DIR/*log $RUN_DIR
cp $WORK_DIR/be.dat $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

echo
echo $(date) "Finished"

touch $RUN_DIR/SUCCESS

exit 0
