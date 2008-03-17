#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_ep2.csh
#
# Purpose: To calculate ensemble perturbations for use in WRF-Var to
# represent flow-dependent forecast errors.
#
# This version (gen_be_ep2) calculates perturbations for use with WRF-Var and 
# NL_ALPHACV_METHOD=2: Perturbations in model (xa) space (u, v, t, q, ps).
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

setenv DATE 2003010212
setenv WRFVAR_DIR /smoke/dmbarker/code/latest/wrfvar

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#Define environment variables:

 if ( ! $?DATE )          setenv DATE          2003010112 # Time of perturbation.
 if ( ! $?FCST_RANGE )    setenv FCST_RANGE    12         # Forecast range (hours).
 if ( ! $?NE )            setenv NE            56         # Number of ensemble members (for ENS).

 if ( ! $?RELEASE )       setenv RELEASE    WRF_V2.1.2
 if ( ! $?REL_DIR )       setenv REL_DIR    ${HOME}/code/${RELEASE}
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${REL_DIR}/wrfvar
 if ( ! $?BUILD_DIR )     setenv BUILD_DIR  ${WRFVAR_DIR}/var/da
 if ( ! $?REGION )        setenv REGION     con200
 if ( ! $?EXPT )          setenv EXPT       test
 if ( ! $?DAT_DIR )       setenv DAT_DIR    ${HOME}/data/${REGION}/${EXPT}
 if ( ! $?RUN_DIR )       setenv RUN_DIR    ${DAT_DIR}/${DATE}/ep2
 if ( ! -d ${RUN_DIR} )   mkdir -p ${RUN_DIR}
 cd $RUN_DIR

 if ( ! -d u ) mkdir u
 if ( ! -d v ) mkdir v
 if ( ! -d t ) mkdir t
 if ( ! -d q ) mkdir q
 if ( ! -d ps ) mkdir ps

 echo "---------------------------------------------------------------------"
 echo "Calculate model-space ensemble perturbations valid at time $DATE."
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

#Run:
 echo "gen_be_ep2: Calculating model space (xa) perturbations at time " $DATE
 cp ${BUILD_DIR}/gen_be_ep2.exe .
 ./gen_be_ep2.exe ${DATE} $NE $FILE >&! gen_be_ep2.out

#Tidy:
 rm -rf tmp* *.exe >&! /dev/null

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

exit(0)

