#!/bin/ksh
#-----------------------------------------------------------------------
# Purpose: To calculate ensemble perturbations in "standard fields".
#          For Stage1 gen_be_gsi
# 
# Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009

#-----------------------------------------------------------------------
#
#
# Note: START_DATE and END_DATE are defined as the times of the first and 
# last perturbation. We derive START_DATE_STAGE0 and END_DATE_STAGE0
# from these using FCST_RANGE.  
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir $RUN_DIR; fi
if [[ ! -d $STAGE0_GSI_DIR ]]; then mkdir $STAGE0_GSI_DIR; fi

#Derive times of initial/final FCST_RANGE forecasts:
export START_DATE_STAGE0=$(${BUILD_DIR}/da_advance_time.exe $START_DATE -$FCST_RANGE1)
export END_DATE_STAGE0=$(${BUILD_DIR}/da_advance_time.exe $END_DATE   -$FCST_RANGE1)
export DATE=$START_DATE_STAGE0

while [[ $DATE -le $END_DATE_STAGE0 ]]; do
   export TMP_DIR=${WORK_DIR}/${DATE}
   rm -rf ${TMP_DIR} 2>/dev/null
   mkdir ${TMP_DIR}  2>/dev/null
   cd ${TMP_DIR}

   #  Create file dates:
   export FCST_TIME=$(${BUILD_DIR}/da_advance_time.exe $DATE $FCST_RANGE1)
   echo "gen_be_stage0_gsi: Calculating standard perturbation fields valid at time " $FCST_TIME

   export YYYY=$(echo $FCST_TIME | cut -c1-4)
   export MM=$(echo $FCST_TIME | cut -c5-6)
   export DD=$(echo $FCST_TIME | cut -c7-8)
   export HH=$(echo $FCST_TIME | cut -c9-10)
   export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
   export FILE=${FC_DIR}/${DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export FILE1=wrfout_d${DOMAIN}_${FILE_DATE}
   export FILE2=wrfout_d${DOMAIN}_${FILE_DATE}.e001
   export FILE3=wrfout_d${DOMAIN}_${FILE_DATE}.e002
   export NEXT_DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $INTERVAL)
   if [[ $BE_METHOD == NMC ]]; then
      ln -sf $FILE $FILE1
      ln -sf $FILE $FILE2
      ln -sf ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE} $FILE3
   fi
   if [[ $BE_METHOD == ENS ]]; then
      typeset -Z3 count
      count=1 
      while [[ $count -le ${NE} ]];do
         ln -sf ${FC_DIR}/${DATE}.e${count}/wrfout_d${DOMAIN}_${FILE_DATE} wrfout_d${DOMAIN}_${FILE_DATE}.e${count}
         (( count += 1 ))
      done
   fi

   if $PROCESS_AERO; then
      ln -sf ${WORK_DIR}/be_for_aero.nl .
   fi

   ln -fs ${BUILD_DIR}/gen_be_stage0_gsi.exe .
   ./gen_be_stage0_gsi.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE1 > gen_be_stage0_gsi.${FCST_TIME}.log 2>&1

   #  Tidy:
   mv pert.${FCST_TIME}* ${STAGE0_GSI_DIR}
   mv gen_be_stage0_gsi.${FCST_TIME}.log ${STAGE0_GSI_DIR}
   cd ${WORK_DIR}
   #hcl rm -rf ${WORK_DIR}/${DATE}  2> /dev/null

   echo $DATE $FILE ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $INTERVAL)

done     # End loop over dates.

exit 0
