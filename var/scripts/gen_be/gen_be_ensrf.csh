#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_ensrf.csh
#
# Purpose: To perform an Ensemble Square Root Filter (EnSRF) assimilation
# using an ensemble of WRF output forecasts.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

setenv WRFVAR_DIR /smoke/dmbarker/code/latest/wrfvar

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#Define environment variables:

 if ( ! $?DATE )          setenv DATE          2003010112 # Time of perturbation.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    12         # Forecast range (hours).
 if ( ! $?NUM_MEMBERS )   setenv NUM_MEMBERS   56         # Number of ensemble members (for ENS).
 if ( ! $?COV_INF_FAC )   setenv COV_INF_FAC   1.0        # Covariance Inflation Factor.
 if ( ! $?COV_LOC_RAD_M ) setenv COV_LOC_RAD_M 1500000.0  # Covariance localization radius (m)

 if ( ! $?RELEASE )       setenv RELEASE    WRF_V2.1.2
 if ( ! $?REL_DIR )       setenv REL_DIR    ${HOME}/code/${RELEASE}
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${REL_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR  ${WRFVAR_DIR}/var/da
 if ( ! $?REGION )        setenv REGION     con200
 if ( ! $?EXPT )          setenv EXPT       test 
 if ( ! $?DAT_DIR )       setenv DAT_DIR    ${HOME}/data/${REGION}/${EXPT}
 if ( ! $?RUN_DIR )       setenv RUN_DIR    ${DAT_DIR}/${DATE}
 if ( ! -d ${RUN_DIR} )   mkdir -p ${RUN_DIR}
 if ( ! $?TMP_DIR )       setenv TMP_DIR    ${RUN_DIR}/ensrf
 if ( ! -d ${TMP_DIR} )   mkdir ${TMP_DIR}
 cd $TMP_DIR
 cp ${WRFVAR_DIR}/run/gen_be/observations .

 if ( ! -d U ) mkdir U
 if ( ! -d V ) mkdir V
 if ( ! -d T ) mkdir T
 if ( ! -d QVAPOR ) mkdir QVAPOR
 if ( ! -d PSFC ) mkdir PSFC

 echo "---------------------------------------------------------------------"
 echo "Perform Ensemble Square Root Filter (EnSRF) assimilation at $DATE."
 echo "---------------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 set YYYY = `echo $DATE | cut -c1-4`
 set MM = `echo $DATE | cut -c5-6`
 set DD = `echo $DATE | cut -c7-8`
 set HH = `echo $DATE | cut -c9-10`
 set FILE_DATE = ${YYYY}-${MM}-${DD}_${HH}:00:00

 setenv PREV_DATE `${BUILD_DIR}/da_advance_time.exe $DATE -$FCST_RANGE`
 set FILE = ${DAT_DIR}/${PREV_DATE}/wrfout_d01_${FILE_DATE}

cat > gen_be_ensrf_nl.nl << EOF
  &gen_be_ensrf_nl
    filestub = '${FILE}',
    num_members = ${NUM_MEMBERS},
    cov_inf_fac = ${COV_INF_FAC},
    cov_loc_rad_m = ${COV_LOC_RAD_M} /
EOF

#Run:
 cp ${BUILD_DIR}/gen_be_ensrf.exe .
 ./gen_be_ensrf.exe >&! gen_be_ensrf.out

#Tidy:
# rm -rf tmp* *.exe >&! /dev/null

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

exit(0)

