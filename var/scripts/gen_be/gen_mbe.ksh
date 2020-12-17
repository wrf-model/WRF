#!/bin/ksh
#-----------------------------------------------------------------------
# Purpose : Create BE statistics from input perturbation files.
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 2: Calculate regression coefficients.
# Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
# Run Stage 4: Calculate horizontal covariances.
# Finally, gather data together into a single BE file.
#
#-----------------------------------------------------------------------
#  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
#           & Monika Krysta, (CTBTO, Vienna, Austria)
#
#  Note: Please acknowledge author/institute in work that uses this code.
#------------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
export GRAPHICS_DIR=${GRAPHICS_DIR:-$WRFVAR_DIR/var/graphics/ncl}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir -p $STAGE0_DIR; fi

mkdir -p $WORK_DIR
cd $WORK_DIR

# List of control variables:
for SV in fullflds psi chi t rh ps; do mkdir -p $SV; done

for CV in $CONTROL_VARIABLES; do mkdir -p $CV; done

#------------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo 
echo $(date) "Start"
echo "WRFVAR_DIR is" $WRFVAR_DIR $(svnversion $WRFVAR_DIR)

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
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT},
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    dat_dir = '${STAGE0_DIR}' /
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

if $RUN_GEN_BE_STAGE2; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients and control variables"
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_mbe_stage2.exe .

   cat > gen_be_stage2_nl.nl << EOF
&gen_be_stage2_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    ne = ${NE},
    cv_options   = ${NL_CV_OPTIONS},
    testing_eofs = ${TESTING_EOFS},
    num_passes = ${NUM_PASSES},
    rf_scale = ${RF_SCALE} /
EOF

   ./gen_mbe_stage2.exe > gen_mbe_stage2.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 failed with error" $RC
      echo "stage 2" > $RUN_DIR/FAIL
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
    data_on_levels = ${DATA_ON_LEVELS} /
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
      echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)."
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
   cv_options = ${NL_CV_OPTIONS},
   uh_method = '${UH_METHOD}',
   use_rf = ${USE_RF},
   n_smth_sl = ${N_SMTH_SL}, /
EOF

   ./gen_be_diags.exe > gen_be_diags.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags failed with error" $RC
      echo "gen_be_diags" > $RUN_DIR/FAIL
      exit 1
   fi

#rizvi fi

#------------------------------------------------------------------------
#  Read BE file to check data packed correctly, and write plot diagnostics.
#------------------------------------------------------------------------
   cat > gen_be_diags_nl.nl << EOF
&gen_be_diags_nl
   cv_options = ${NL_CV_OPTIONS},
   uh_method = '${UH_METHOD}',
   n_smth_sl = ${N_SMTH_SL}, /
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

      echo "Computing Multi-Covariance " 
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

#------------------------------------------------------------------------
#  Calculate histograms:
#------------------------------------------------------------------------

if $RUN_GEN_BE_HISTOG; then

    echo "Computing Histogram for MBE" 
    # Histogram psi
    export VARIABLE=psi

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram chi
    export VARIABLE=chi

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram chi_u
    export VARIABLE=chi_u

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram t
    export VARIABLE=t

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram t_u
    export VARIABLE=t_u

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram rh
    export VARIABLE=rh

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi

    # Histogram rh_u
    export VARIABLE=rh_u

    $SCRIPTS_DIR/gen_be/gen_be_hist.ksh
    RC=$?
    if [[ $RC != 0 ]]; then
	echo "gen_be_hist failed with error" $RC
	echo "gen_be_hist" > $RUN_DIR/FAIL

    fi
fi

#------------------------------------------------------------------------
#  Calculate contribution to multivariate regression diagnostics:
#------------------------------------------------------------------------

if $RUN_GEN_BE_MULTICOV_CONTRIB; then
    echo "Computing Contributions for different variables for MBE"
   # Calculate psi_chi diagnostics:
   export VARIABLE1=psi
   export VARIABLE2=chi

   echo "---------------------------------------------------------------"
   echo "1. Balanced contribution (from psi) to full chi"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_bin3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_bin3d_contrib (psi_chi) failed with error" $RC
       echo "gen_be_cov3d3d_bin3d_contrib (psi_chi)" > $RUN_DIR/FAIL
       exit 1
   fi

   # Calculate psi_t diagnostics:
   export VARIABLE1=psi
   export VARIABLE2=t

   echo "---------------------------------------------------------------"
   echo "2. Balanced contribution (from psi) to full t"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_contrib (psi_t) failed with error" $RC
       echo "gen_be_cov3d3d_contrib (psi_t)" > $RUN_DIR/FAIL
       exit 1
   fi


   # Calculate psi_chi diagnostics:
   export VARIABLE1=psi
   export VARIABLE2=rh

   echo "---------------------------------------------------------------"
   echo "3. Balanced contribution (from psi) to full rh"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_contrib (psi_rh) failed with error" $RC
       echo "gen_be_cov3d3d_contrib (psi_rh)" > $RUN_DIR/FAIL
       exit 1
   fi

######  Calculate psi_chi diagnostics:
   export VARIABLE1=chi_u
   export VARIABLE2=t

   echo "---------------------------------------------------------------"
   echo "4. Balanced contribution (from chi_u) to full t"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_contrib (chi_u_t) failed with error" $RC
       echo "gen_be_cov3d3d_contrib (chi_u_t)" > $RUN_DIR/FAIL
       exit 1
   fi

######  Calculate psi_chi diagnostics:
   export VARIABLE1=chi_u
   export VARIABLE2=rh

   echo "---------------------------------------------------------------"
   echo "5. Balanced contribution (from chi_u) to full rh"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_contrib (chi_u_rh) failed with error" $RC
       echo "gen_be_cov3d3d_contrib (chi_u_rh)" > $RUN_DIR/FAIL
       exit 1
   fi

######  Calculate psi_chi diagnostics:
   export VARIABLE1=t_u
   export VARIABLE2=rh

   echo "---------------------------------------------------------------"
   echo "6. Balanced contribution (from t_u) to full rh"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d3d_contrib (t_u_rh) failed with error" $RC
       echo "gen_be_cov3d3d_contrib (t_u_rh)" > $RUN_DIR/FAIL
       exit 1
   fi

######  Calculate psi_ps diagnostics:
   export VARIABLE1=psi
   export VARIABLE2=ps

   echo "---------------------------------------------------------------"
   echo "7. Balanced contribution (from psi) to full ps"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d2d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d2d_contrib (psi_ps) failed with error" $RC
       echo "gen_be_cov3d2d_contrib (psi_ps)" > $RUN_DIR/FAIL
       exit 1
   fi

######  Calculate psi_chi diagnostics:
   export VARIABLE1=chi_u
   export VARIABLE2=ps

   echo "---------------------------------------------------------------"
   echo "8. Balanced contribution (from chi_u) to full ps"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov3d2d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov3d2d_contrib (chi_u_ps) failed with error" $RC
       echo "gen_be_cov3d2d_contrib (chi_u_ps)" > $RUN_DIR/FAIL
       exit 1
   fi

#  Calculate psi_chi diagnostics:
   export VARIABLE1=ps_u
   export VARIABLE2=rh

   echo "---------------------------------------------------------------"
   echo "9. Balanced contribution (from ps_u) to full rh"
   echo "---------------------------------------------------------------"

   $SCRIPTS_DIR/gen_be/gen_be_cov2d3d_contrib.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
       echo "gen_be_cov2d3d_contrib (ps_u_rh) failed with error" $RC
       echo "gen_be_cov2d3d_contrib (ps_u_rh)" > $RUN_DIR/FAIL
       exit 1
   fi


fi

# Preserve the interesting log files
cp $WORK_DIR/*log $RUN_DIR
cp $WORK_DIR/*.dat $RUN_DIR
cp $WORK_DIR/*.bin $RUN_DIR
cp $WORK_DIR/fort.* $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

echo
echo $(date) "Finished"

touch $RUN_DIR/SUCCESS

exit 0
