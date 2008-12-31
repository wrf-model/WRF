#!/bin/ksh
#########################################################################
# Script: da_run_suite_verif_obs.ksh
#
#########################################################################
export NL_ANALYSIS_TYPE=verify
export CLEAN=true
export RUN_DIR=$EXP_DIR
export OB_DIR=$FILTERED_OBS_DIR


export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export SUITE_DIR=${SUITE_DIR:-$RUN_DIR}

echo "<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY><H1>$EXPT</H1><PRE>"

echo 'WRFVAR_DIR   <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN

echo "DUMMY        $DUMMY"
echo "CLEAN        $CLEAN"
echo "NUM_PROCS    $NUM_PROCS"
echo "INITIAL_DATE $INITIAL_DATE"
echo "FINAL_DATE   $FINAL_DATE"
echo "CYCLE_PERIOD $CYCLE_PERIOD"
echo 'BE_DIR       <A HREF="file:'$BE_DIR'">'$BE_DIR'</a>'
echo 'FILTERED_OBS_DIR       <A HREF="file:'$FILTERED_OBS_DIR'">'$FILTERED_OBS_DIR'</a>'
echo 'EXP_DIR      <A HREF="file:'..'">'$EXP_DIR'</a>'
echo 'RUN_DIR      <A HREF="file:'.'">'$RUN_DIR'</a>'
echo 
echo $(date) "Start"

export DATE=$INITIAL_DATE

RC=0

while [[ $DATE -le $FINAL_DATE ]] ; do 
   export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
   export YEAR=$(echo $DATE | cut -c1-4)
   export MONTH=$(echo $DATE | cut -c5-6)
   export DAY=$(echo $DATE | cut -c7-8)

   export HOUR=$(echo $DATE | cut -c9-10)

   echo "=========="
   echo $DATE
   echo "=========="

   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
        echo "ANALYSIS_DATE=${ANALYSIS_DATE}"

      export RUN_DIR=$SUITE_DIR/$DATE/wrfvar
      mkdir -p $RUN_DIR

      export DA_FIRST_GUESS=${FC_DIR}/$DATE/wrfinput_d01
      if [[ ${VERIFY_HOUR} != 0 ]]; then
        export PREVF_DATE=`$BUILD_DIR/da_advance_time.exe $DATE -$VERIFY_HOUR 2>/dev/null`
        export DA_FIRST_GUESS=${FC_DIR}/${PREVF_DATE}/${VERIFICATION_FILE_STRING}_d01_${ANALYSIS_DATE}
      fi
      export DA_ANALYSIS=$RUN_DIR/analysis
      ln -sf $FILTERED_OBS_DIR/$DATE/wrfvar/filtered_obs $OB_DIR/$DATE/ob.ascii

      echo "VERIFY_HOUR= ${VERIFY_HOUR}"
      echo "Verify file=${DA_FIRST_GUESS}"
      echo "Verify obs are: "$OB_DIR/$DATE/filtered_obs

      $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
      $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}wrfvar failed with error $RC$END"
         echo wrfvar > FAIL
         break
      fi


   export NEXT_DATE=$($BUILD_DIR/da_advance_time.exe $DATE $CYCLE_PERIOD 2>/dev/null)
   export DATE=$NEXT_DATE
   let CYCLE_NUMBER=$CYCLE_NUMBER+1
done

echo
echo $(date) "Finished"

if [[ $RC == 0 ]]; then
   touch SUCCESS
fi

echo "</PRE></BODY></HTML>"

exit $RC

