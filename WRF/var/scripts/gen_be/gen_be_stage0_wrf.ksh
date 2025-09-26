#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage0_wrf.ksh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#
# Note: START_DATE and END_DATE are defined as the times of the first and 
# last perturbation. We derive START_DATE_STAGE0 and END_DATE_STAGE0
# from these using FCST_RANGE1.  
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir $STAGE0_DIR; fi

#Derive times of initial/final FCST_RANGE forecasts:
DAAT=${BUILD_DIR}/da_advance_time.exe
if [[ ! -x $DAAT ]]; then echo "gen_be_stage0_wrf finds no " $DAAT;exit; fi
export START_DATE_STAGE0=$($DAAT $START_DATE -$FCST_RANGE1)
export END_DATE_STAGE0=$($DAAT $END_DATE   -$FCST_RANGE1)
export DATE=$START_DATE_STAGE0

if [[ $START_DATE_STAGE0 > $END_DATE_STAGE0 ]]; then
   echo "Start date $START_DATE_STAGE0 must be before $END_DATE_STAGE0!"
   exit -1
fi

export CV_OPTIONS=$NL_CV_OPTIONS #Set CV_OPTIONS (either specified by wrapper or defaults)

while [[ $DATE -le $END_DATE_STAGE0 ]]; do
   export TMP_DIR=${WORK_DIR}/${DATE}
   rm -rf ${TMP_DIR} 2>/dev/null
   mkdir ${TMP_DIR}  2>/dev/null
   cd ${TMP_DIR}

   #Find the difference between forecast lead times
   export DIFF_FCST=$(($FCST_RANGE1 - $FCST_RANGE2))

   if [[ $DIFF_FCST -le 0  ]]; then
      echo "Later forecast time FCST_RANGE1 ($FCST_RANGE1) must be later than FCST_RANGE2 ($FCST_RANGE2)!"
      exit -2
   fi

   if [[ $NL_CV_OPTIONS == 7 ]]; then
      for SV in u v t rh ps; do mkdir -p $SV; done
   else
      for SV in psi chi t rh ps; do mkdir -p $SV; done
   fi

   #  Create file dates:
   export FCST_TIME=$($DAAT $DATE $FCST_RANGE1)
   echo "gen_be_stage0_wrf: Calculating standard perturbation fields valid at time " $FCST_TIME

   export YYYY=$(echo $FCST_TIME | cut -c1-4)
   export MM=$(echo $FCST_TIME | cut -c5-6)
   export DD=$(echo $FCST_TIME | cut -c7-8)
   export HH=$(echo $FCST_TIME | cut -c9-10)
      if $NOCOLONS; then
         export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}_00_00
      else
         export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
      fi
   export FILE=${FC_DIR}/${DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export FILE1=wrfout_d${DOMAIN}_${FILE_DATE}
   export NEXT_DATE=$($DAAT $DATE $DIFF_FCST)
   if [[ $BE_METHOD == NMC ]]; then
      export FILE2=wrfout_d${DOMAIN}_${FILE_DATE}.e001
      export FILE3=wrfout_d${DOMAIN}_${FILE_DATE}.e002
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

   ln -fs ${BUILD_DIR}/gen_be_stage0_wrf.exe .
   ./gen_be_stage0_wrf.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE1 ${CV_OPTIONS} > gen_be_stage0_wrf.${FCST_TIME}.log 2>&1

   #  Tidy:
   mv pert.${FCST_TIME}* ${STAGE0_DIR}
   # mv mean.${FCST_TIME}* ${STAGE0_DIR}
   mv gen_be_stage0_wrf.${FCST_TIME}.log ${STAGE0_DIR}
   # rm -rf $TMP_DIR 2> /dev/null

   echo $DATE $FILE ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export DATE=$($DAAT $DATE $INTERVAL)

done     # End loop over dates.

exit 0

