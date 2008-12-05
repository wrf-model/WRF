#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_restore_data_rtobs.ksh
#
# Purpose: Restore real time observation files from Jim Bresch's archive
#
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/restore_data_rtobs}
export WORK_DIR=$RUN_DIR/working

if [[ ! -d $DAT_DIR ]]; then mkdir $DAT_DIR; fi
if [[ ! -d $RTOBS_DIR ]]; then mkdir $RTOBS_DIR; fi

echo "<HTML><HEAD><TITLE>$EXPT restore_data_rtobs</TITLE></HEAD><BODY>"
echo "<H1>$EXPT restore_data_rtobs</H1><PRE>"

date

export START_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} $WINDOW_START 2>/dev/null)
export END_DATE=$($BUILD_DIR/da_advance_time.exe ${DATE} $WINDOW_END 2>/dev/null)

echo "DATE       $DATE"
echo "START_DATE $START_DATE"
echo "END_DATE   $END_DATE"
echo "OBS_FREQ   $OBS_FREQ"
echo "MSS_RTOBS_DIR $MSS_RTOBS_DIR"
echo 'RTOBS_DIR  <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'

LOCAL_DATE=$START_DATE

while [[ $LOCAL_DATE -le $END_DATE ]]; do

   YEAR=$(echo $LOCAL_DATE | cut -c1-4)
   MONTH=$(echo $LOCAL_DATE | cut -c5-6)
   DAY=$(echo $LOCAL_DATE | cut -c7-8)
   HOUR=$(echo $LOCAL_DATE | cut -c9-10)

   DIR=${RTOBS_DIR}/${LOCAL_DATE}
   mkdir -p ${DIR}
   
   export FILE=obs.$LOCAL_DATE.gz

   if [[ ! -f ${DIR}/$FILE ]]; then
      echo Retrieving ${MSS_RTOBS_DIR}/${YEAR}${MONTH}/$FILE to $DIR
      if $DUMMY; then
         echo Dummy restore_data > ${DIR}/${FILE}
      else
         msrcp ${MSS_RTOBS_DIR}/${YEAR}${MONTH}/$FILE $DIR
      fi
   else
      echo "File ${DIR}/$FILE exists, skipping"
   fi

   LOCAL_DATE=$($BUILD_DIR/da_advance_time.exe ${LOCAL_DATE} ${OBS_FREQ} 2>/dev/null)
done

date

echo "</BODY></HTML>"

exit 0
