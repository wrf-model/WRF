#!/bin/ksh
#########################################################################
# Script: da_run_wrfvar.ksh
#
# Purpose: Run wrfvar
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/var/scripts/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wrfvar}

export WORK_DIR=$RUN_DIR/working

export WINDOW_START=${WINDOW_START:--3}
export WINDOW_END=${WINDOW_END:-3}

export YEAR=$(echo $DATE | cut -c1-4)
export MONTH=$(echo $DATE | cut -c5-6)
export DAY=$(echo $DATE | cut -c7-8)
export HOUR=$(echo $DATE | cut -c9-10)
export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
export NL_ANALYSIS_DATE=${ANALYSIS_DATE}.0000

export DA_FIRST_GUESS=${DA_FIRST_GUESS:-${RC_DIR}/$DATE/wrfinput_d01}

if $CYCLING; then
   if [[ $CYCLE_NUMBER -gt 0 ]]; then
      export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfinput_d01_${ANALYSIS_DATE}
   fi
fi
export DA_ANALYSIS=${DA_ANALYSIS:-analysis}
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # wrfvar background errors.

# Error tuning namelist parameters
# Assign random seeds

export NL_SEED_ARRAY1=$DATE
export NL_SEED_ARRAY2=$DATE

# Change defaults from Registry.wrfvar which is required to be
# consistent with WRF's Registry.EM
export NL_INTERP_TYPE=${NL_INTERP_TYPE:-1}
export NL_T_EXTRAP_TYPE=${NL_T_EXTRAP_TYPE:-1}
export NL_I_PARENT_START=${NL_I_PARENT_START:-0}
export NL_J_PARENT_START=${NL_J_PARENT_START:-0}
export NL_JCDFI_USE=${NL_JCDFI_USE:-false}
export NL_CO2TF=${NL_CO2TF:-0}
export NL_W_SPECIFIED=${NL_W_SPECIFIED:-true}
export NL_REAL_DATA_INIT_TYPE=${NL_REAL_DATA_INIT_TYPE:-3}

#=======================================================

mkdir -p $RUN_DIR

echo "<HTML><HEAD><TITLE>$EXPT wrfvar</TITLE></HEAD><BODY><H1>$EXPT wrfvar</H1><PRE>"

date

echo 'REL_DIR               <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR            <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
echo "DA_BACK_ERRORS        $DA_BACK_ERRORS"
if [[ -d $BIASCORR_DIR ]]; then
   echo "BIASCORR_DIR          $BIASCORR_DIR"
fi
if [[ -d $OBS_TUNING_DIR ]] ; then
   echo "OBS_TUNING_DIR        $OBS_TUNING_DIR"
fi
echo 'OB_DIR                <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo 'RC_DIR                <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR                <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo 'RUN_DIR               <A HREF="file:'.'">'$RUN_DIR'</a>'
echo 'WORK_DIR              <A HREF="file:'${WORK_DIR##$RUN_DIR/}'">'$WORK_DIR'</a>'
echo "DA_ANALYSIS           $DA_ANALYSIS"
echo "DATE                  $DATE"
echo "WINDOW_START          $WINDOW_START"
echo "WINDOW_END            $WINDOW_END"

rm -rf ${WORK_DIR}
mkdir -p ${WORK_DIR}
cd $WORK_DIR

START_DATE=$($BUILD_DIR/da_advance_time.exe $DATE $WINDOW_START)
END_DATE=$($BUILD_DIR/da_advance_time.exe $DATE $WINDOW_END)

for INDEX in 01 02 03 04 05 06 07; do
   let H=$INDEX-1+$WINDOW_START
   D_DATE[$INDEX]=$($BUILD_DIR/da_advance_time.exe $DATE $H)
   export D_YEAR[$INDEX]=$(echo ${D_DATE[$INDEX]} | cut -c1-4)
   export D_MONTH[$INDEX]=$(echo ${D_DATE[$INDEX]} | cut -c5-6)
   export D_DAY[$INDEX]=$(echo ${D_DATE[$INDEX]} | cut -c7-8)
   export D_HOUR[$INDEX]=$(echo ${D_DATE[$INDEX]} | cut -c9-10)
done

if $NL_GLOBAL; then
   export NL_NPROC_X=1
fi

export YEAR=$(echo $DATE | cut -c1-4)
export MONTH=$(echo $DATE | cut -c5-6)
export DAY=$(echo $DATE | cut -c7-8)
export HOUR=$(echo $DATE | cut -c9-10)

export NL_START_YEAR=$YEAR
export NL_START_MONTH=$MONTH
export NL_START_DAY=$DAY
export NL_START_HOUR=$HOUR

export NL_END_YEAR=$YEAR
export NL_END_MONTH=$MONTH
export NL_END_DAY=$DAY
export NL_END_HOUR=$HOUR

export START_YEAR=$(echo $START_DATE | cut -c1-4)
export START_MONTH=$(echo $START_DATE | cut -c5-6)
export START_DAY=$(echo $START_DATE | cut -c7-8)
export START_HOUR=$(echo $START_DATE | cut -c9-10)

export END_YEAR=$(echo $END_DATE | cut -c1-4)
export END_MONTH=$(echo $END_DATE | cut -c5-6)
export END_DAY=$(echo $END_DATE | cut -c7-8)
export END_HOUR=$(echo $END_DATE | cut -c9-10)

export NL_TIME_WINDOW_MIN=${NL_TIME_WINDOW_MIN:-${START_YEAR}-${START_MONTH}-${START_DAY}_${START_HOUR}:00:00.0000}
export NL_TIME_WINDOW_MAX=${NL_TIME_WINDOW_MAX:-${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00.0000}

#-----------------------------------------------------------------------
# [2.0] Perform sanity checks:
#-----------------------------------------------------------------------

if [[ ! -r $DA_FIRST_GUESS ]]; then
   echo "${ERR}First Guess file >$DA_FIRST_GUESS< does not exist:$END"
   exit 1
fi

if [[ ! -d $OB_DIR ]]; then
   echo "${ERR}Observation directory >$OB_DIR< does not exist:$END"
   exit 1
fi

if [[ ! -r $DA_BACK_ERRORS ]]; then
   echo "${ERR}Background Error file >$DA_BACK_ERRORS< does not exist:$END"
   exit 1
fi

#-----------------------------------------------------------------------
# [3.0] Prepare for assimilation:
#-----------------------------------------------------------------------

ln -fs $WRFVAR_DIR/run/gribmap.txt .
ln -fs $WRFVAR_DIR/run/*.TBL .
if $DOUBLE; then
   ln -fs $WRFVAR_DIR/run/RRTM_DATA_DBL RRTM_DATA
   ln -fs $WRFVAR_DIR/run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA
else
   ln -fs $WRFVAR_DIR/run/RRTM_DATA .
   ln -fs $WRFVAR_DIR/run/ETAMPNEW_DATA .
fi
ln -fs $WRFVAR_DIR/run/CAM_ABS_DATA .
ln -fs $WRFVAR_DIR/run/CAM_AEROPT_DATA .
ln -fs $WRFVAR_DIR/run/gmao_airs_bufr.tbl .
ln -fs $BUILD_DIR/da_wrfvar.exe .
export PATH=$WRFVAR_DIR/var/scripts:$PATH

ln -fs $DA_FIRST_GUESS fg01
ln -fs $DA_FIRST_GUESS wrfinput_d01
ln -fs $DA_BACK_ERRORS be.dat

for FILE in $DAT_DIR/*.inv; do
   if [[ -f $FILE ]]; then
      ln -fs $FILE .
   fi
done

if [[ -d $BIASCORR_DIR ]]; then
   ln -fs $BIASCORR_DIR biascorr
fi

if [[ -d $OBS_TUNING_DIR ]]; then
   ln -fs $OBS_TUNING_DIR/* .
fi

ln -fs $WRFVAR_DIR/run/radiance_info .

if [[ $NL_NUM_FGAT_TIME -gt 1 ]]; then
      if [[ $DATE -eq $START_DATE ]]; then
         ln -fs $OB_DIR/$DATE/ob.ascii+ ob01.ascii
      else
         ln -fs $OB_DIR/$DATE/ob.ascii  ob01.ascii
      fi
      typeset -i N
      let N=1
      FGAT_DATE=$START_DATE
      # while [[ $FGAT_DATE < $END_DATE || $FGAT_DATE -eq $END_DATE ]] ; do
      until [[ $FGAT_DATE > $END_DATE ]]; do
         if [[ $FGAT_DATE -ne $DATE ]]; then
            let N=$N+1
            if [[ $FGAT_DATE -eq $START_DATE ]]; then
               ln -fs $OB_DIR/$FGAT_DATE/ob.ascii+ ob0${N}.ascii
            elif [[ $FGAT_DATE -eq $END_DATE ]]; then
               ln -fs $OB_DIR/$FGAT_DATE/ob.ascii- ob0${N}.ascii
            else
               ln -fs $OB_DIR/$FGAT_DATE/ob.ascii ob0${N}.ascii
            fi
            FYEAR=$(echo ${FGAT_DATE} | cut -c1-4)
            FMONTH=$(echo ${FGAT_DATE} | cut -c5-6)
            FDAY=$(echo ${FGAT_DATE} | cut -c7-8)
            FHOUR=$(echo ${FGAT_DATE} | cut -c9-10)
            ln -fs ${FC_DIR}/${PREV_DATE}/wrfinput_d01_${FYEAR}-${FMONTH}-${FDAY}_${FHOUR}:00:00 fg0${N}
         fi
         FGAT_DATE=$($BUILD_DIR/da_advance_time.exe $FGAT_DATE $OBS_FREQ)
      done
else
   ln -fs $OB_DIR/${DATE}/ob.ascii  ob01.ascii
   if [[ -s $OB_DIR/${DATE}/ob.ssmi ]]; then
      ln -fs $OB_DIR/${DATE}/ob.ssmi ob01.ssmi
   fi
   if [[ -s $OB_DIR/${DATE}/ob.radar ]]; then
      ln -fs $OB_DIR/${DATE}/ob.radar ob01.radar
   fi
fi

for FILE in $OB_DIR/$DATE/*.bufr; do
   if [[ -f $FILE ]]; then
      ln -fs $FILE .
   fi
done


. $WRFVAR_DIR/inc/namelist_script.inc 

cp namelist.input $RUN_DIR
echo '<A HREF="namelist.input">Namelist.input</a>'

#-------------------------------------------------------------------
#Run WRF-Var:
#-------------------------------------------------------------------
mkdir trace

if $DUMMY; then
   echo Dummy wrfvar
   echo "Dummy wrfvar" > $DA_ANALYSIS
   RC=0
else
   $RUN_CMD ./da_wrfvar.exe
   RC=$?

   if [[ -f fort.9 ]]; then
      cp fort.9 $RUN_DIR/namelist.output
   fi

   if [[ -f statistics ]]; then
      cp statistics $RUN_DIR
   fi

   if [[ -f cost_fn ]]; then 
      cp cost_fn $RUN_DIR
   fi

   if [[ -f grad_fn ]]; then
      cp grad_fn $RUN_DIR
   fi

   if [[ -f gts_omb_oma ]]; then
      cp gts_omb_oma $RUN_DIR
   fi

   if [[ -f ob.etkf.000 ]]; then
      cp ob.etkf.000 $RUN_DIR
   fi

   if [[ -d trace ]]; then
      mkdir -p $RUN_DIR/trace
      mv trace/* $RUN_DIR/trace
   fi
   rm -f unpert_obs.*
   rm -f pert_obs.*
   rm -f rand_obs_error.*
   rm -f gts_omb_oma.*
   rm -f qcstat_*.*
   # No routine to merge these files across processors yet
   # rm -f inv_*.*
   # rm -f oma_*.*
   # rm -f filtered_*.*

   if [[ -f wrfvar_output ]]; then
      if [[ $DA_ANALYSIS != wrfvar_output ]]; then 
         cp wrfvar_output $DA_ANALYSIS
      fi
   fi

   if [[ -d trace ]]; then
      mkdir -p $RUN_DIR/trace
      mv trace/* $RUN_DIR/trace
   fi

   cp $WORK_DIR/namelist.output $RUN_DIR
   echo '<A HREF="namelist.output">Namelist.output</a>'

   if [[ -f rsl.out.0000 ]]; then
      rm -rf $RUN_DIR/rsl
      mkdir -p $RUN_DIR/rsl
      mv rsl* $RUN_DIR/rsl
      cd $RUN_DIR/rsl
      for FILE in rsl*; do
         echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
         echo "<H1>$FILE</H1><PRE>" >> $FILE.html
         cat $FILE >> $FILE.html
         echo "</PRE></BODY></HTML>" >> $FILE.html
         rm $FILE
      done
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'
   fi

   echo '<A HREF="trace/0.html">PE 0 trace</a>'
   echo '<A HREF="trace">Other tracing</a>'
   echo '<A HREF="cost_fn">Cost function</a>'
   echo '<A HREF="grad_fn">Gradient function</a>'
   echo '<A HREF="statistics">Statistics</a>'

   cat $RUN_DIR/cost_fn

   echo $(date +'%D %T') "Ended $RC"
fi

# We never look at core files

for DIR in $WORK_DIR/coredir.*; do
   if [[ -d $DIR ]]; then
      rm -rf $DIR
   fi
done

if $CLEAN; then
   rm -rf $WORK_DIR
fi

echo '</PRE></BODY></HTML>'

exit $RC
