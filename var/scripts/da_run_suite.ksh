#!/bin/ksh
#########################################################################
# Script: da_run_suite.ksh
#
# Purpose: End-to-end testing of the WRF system.
#
# Description:
# The da_run_suite.ksh script is designed for end-to-end real data 
# testing of the following components of the WRF system:
#
# WRF_REAL, OBSPROC, WRFVAR, UPDATE_BC, NDOWN, NUP, ETKF and WRF.
#
# Any stage can be switched on/off via environment variables as
# described below. The da_run_suite.ksh script can also cycle the
# above sequence for multiple times, and so can be used for
# extended period assessment of scientific impact. I have
# successfully run the script for month long periods with 6 
# hourly cycling all the above. 
#
# Before running da_run_suite.ksh, you must do the following:
#
# 1) Compile the executables for the WRF components you wish to 
# test.
# 2) Restore input datasets (e.g. AVN fields, observations, etc).
# A template da_restore_data_mss.ksh script is called from da_run_suite.ksh, 
# which I use when working on machines that have access to NCAR's 
# Mass Store).
# 3) Overwrite default directories, filenames, namelist parameters,
# etc in script da_run_wrf_wrapper.ksh. This is done via environment
# variables. (TO DO: Automate vertical levels ENV variable)
#
# NOTE: The idea is that you overwrite defaults in da_run_suite_wrapper.ksh, 
# NOT da_run_suite.ksh itself. We want to maintain a clean script interface
# (da_run_wrf.ksh) to the WRF system. We cannot support changes made to 
# da_run_wrf.ksh. If you feel you need to modify da_run_wrf.ksh, please 
# email wrfhelp@ucar.edu and we will try to include your ideas. 
#
# Once you have set up your experiment as described above, then run the
# da_run_wrf_wrapper.ksh script.
#
# Thank you, and good luck.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export SUITE_DIR=${SUITE_DIR:-$RUN_DIR}

echo "<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY><H1>$EXPT</H1><PRE>"

echo 'REL_DIR      <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR      <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'WRFVAR_DIR   <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
echo 'WRFPLUS_DIR  <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
echo 'WPS_DIR      <A HREF="file:'$WPS_DIR'">'$WPS_DIR'</a>' $WPS_VN

echo "CYCLING      $CYCLING"
echo "DUMMY        $DUMMY"
echo "CLEAN        $CLEAN"
echo "NUM_PROCS    $NUM_PROCS"
echo "INITIAL_DATE $INITIAL_DATE"
echo "FINAL_DATE   $FINAL_DATE"
echo "CYCLE_PERIOD $CYCLE_PERIOD"
echo "LBC_FREQ     $LBC_FREQ"
echo "OBS_FREQ     $OBS_FREQ"
echo "WINDOW_START $WINDOW_START"
echo "WINDOW_END   $WINDOW_END"
echo 'BE_DIR       <A HREF="file:'$BE_DIR'">'$BE_DIR'</a>'
echo 'GRIB_DIR     <A HREF="file:'$GRIB_DIR'">'$GRIB_DIR'</a>'
echo 'RC_DIR       <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR       <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo 'OB_DIR       <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo 'RTOBS_DIR    <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'
echo 'EXP_DIR      <A HREF="file:'..'">'$EXP_DIR'</a>'
echo 'RUN_DIR      <A HREF="file:'.'">'$RUN_DIR'</a>'
echo 
echo $(date) "Start"

export DATE=$INITIAL_DATE

RC=0

while [[ $DATE -le $FINAL_DATE ]] ; do 
   export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
   export HOUR=$(echo $DATE | cut -c9-10)

   if [[ ! -d $FC_DIR/$DATE ]]; then mkdir -p $FC_DIR/$DATE; fi

   echo "=========="
   echo $DATE
   echo "=========="

   # Decide on length of forecast to run
   export FCST_RANGE=${FCST_RANGE:-$CYCLE_PERIOD}
   if [[ $HOUR -eq $LONG_FCST_TIME_1 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_1; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_2 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_2; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_3 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_3; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_4 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_4; fi

   if $RUN_RESTORE_DATA_GRIB; then
      export RUN_DIR=$SUITE_DIR/$DATE/restore_data_grib
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_restore_data_grib $RUN_DIR
      $SCRIPTS_DIR/da_restore_data_grib.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}restore_data_grib failed with error$RC$END"
         echo restore_data_grib > FAIL
         break
      fi
   fi

   if $RUN_RESTORE_DATA_RTOBS; then
      export RUN_DIR=$SUITE_DIR/$DATE/restore_data_rtobs
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_restore_data_rtobs $RUN_DIR
      $SCRIPTS_DIR/da_restore_data_rtobs.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}restore_data_rtobs failed with error$RC$END"
         echo restore_data_rtobs > FAIL
         break
      fi
   fi
  
   if $RUN_WPS; then
      export RUN_DIR=$SUITE_DIR/$DATE/wps
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_wps $RUN_DIR
      $SCRIPTS_DIR/da_run_wps.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}wps failed with error $RC$END"
         echo wps > FAIL
         break
      fi
      export RUN_GEOGRID=false # Only need to run it once.
   fi

   if $RUN_REAL; then
      export RUN_DIR=$SUITE_DIR/$DATE/real
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_real $RUN_DIR
      $SCRIPTS_DIR/da_run_real.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}real failed with error $RC$END"
         echo real > FAIL
         break
      fi
   fi

   if $RUN_IDEAL; then
      export RUN_DIR=$SUITE_DIR/$DATE/ideal
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_ideal $RUN_DIR
      $SCRIPTS_DIR/da_run_ideal.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}ideal failed with error $RC$END"
         echo ideal > FAIL
         break
      fi
   fi

   if $RUN_OBSPROC; then
      export RUN_DIR=$SUITE_DIR/$DATE/obsproc
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_obsproc $RUN_DIR
      $SCRIPTS_DIR/da_run_obsproc.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}obsproc failed with error $RC$END"
         echo obsproc > FAIL
         break
      fi
   fi

   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00

   if $NL_VAR4D; then
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            if $RUN_UPDATE_BC; then
               export RUN_DIR=$SUITE_DIR/$DATE/update_bc_4dvar
               export PHASE=true
               mkdir -p $RUN_DIR

               export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d01
               export BDYIN=$RC_DIR/$DATE/wrfbdy_d01
               export BDYOUT=$FC_DIR/$DATE/wrfbdy_d01

               $SCRIPTS_DIR/da_trace.ksh da_run_update_bc $RUN_DIR
               $SCRIPTS_DIR/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
               RC=$?
               if [[ $? != 0 ]]; then
        	  echo $(date) "${ERR}update_bc failed with error $RC$END"
                  echo update_bc > FAIL
                  break
               fi
            fi 
         fi
      fi
   fi

   if $RUN_ENS_EP; then
      export RUN_DIR=$SUITE_DIR/$DATE/ep
      mkdir -p $RUN_DIR
      export FCST_RANGE_SAVE=$FCST_RANGE
      export FCST_RANGE=$CYCLE_PERIOD

      $SCRIPTS_DIR/da_trace.ksh run_ens_ep $RUN_DIR
      $SCRIPTS_DIR/da_run_ens_ep.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $? != 0 ]]; then
         echo $(date) "${ERR}run_ens_ep failed with error $RC$END"
         echo etkf > FAIL
         break
      fi
      
      export FCST_RANGE=$FCST_RANGE_SAVE
   fi

   if $RUN_WRFVAR; then
      export RUN_DIR=$SUITE_DIR/$DATE/wrfvar
      mkdir -p $RUN_DIR

      export DA_FIRST_GUESS=${RC_DIR}/$DATE/${FILE_TYPE}_d01
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE}
         fi
      fi
      export EP_DIR=$FC_DIR/$DATE/ep
      export DA_ANALYSIS=$FC_DIR/$DATE/${FILE_TYPE}_d01
      
      if [[ ${DA_VARBC_IN:+1} = 1 ]]; then
         if [[ -f $DA_VARBC_IN ]]; then
#            if $CYCLING; then
               if  [[ -s ${SUITE_DIR}/${PREV_DATE}/wrfvar/working/VARBC.out ]]; then
	          export DA_VARBC_IN=${SUITE_DIR}/${PREV_DATE}/wrfvar/working/VARBC.out
	       fi
#	    fi
         fi
      fi

      $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
      $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}wrfvar failed with error $RC$END"
         echo wrfvar > FAIL
         break
      fi

      if [[ $NL_MULTI_INC == 1 ]] ; then

         export NL_MULTI_INC=2

         export OB_DIR_TMP=$OB_DIR
         export RC_DIR_TMP=$RC_DIR
         export BE_DIR_TMP=$BE_DIR
         export NL_E_WE_TMP=$NL_E_WE
         export NL_E_SN_TMP=$NL_E_SN
         export NL_DX_TMP=$NL_DX
         export NL_DY_TMP=$NL_DY
         export NL_TIME_STEP_TMP=$NL_TIME_STEP

         export OB_DIR=$OB_DIR_LOW
         export RC_DIR=$RC_DIR_LOW
         export BE_DIR=$BE_DIR_LOW
         export NL_E_WE=$NL_E_WE_LOW
         export NL_E_SN=$NL_E_SN_LOW
         export NL_DX=$NL_DX_LOW
         export NL_DY=$NL_DY_LOW
         export NL_TIME_STEP=$NL_TIME_STEP_LOW
         export DA_BACK_ERRORS=$BE_DIR/be.dat
         
         $SCRIPTS_DIR/da_run_wrfvar.ksh >> $RUN_DIR/index.html 2>&1

         export OB_DIR=$OB_DIR_TMP
         export RC_DIR=$RC_DIR_TMP
         export BE_DIR=$BE_DIR_TMP
         export NL_E_WE=$NL_E_WE_TMP
         export NL_E_SN=$NL_E_SN_TMP
         export NL_DX=$NL_DX_TMP
         export NL_DY=$NL_DY_TMP
         export NL_TIME_STEP=$NL_TIME_STEP_TMP
         export DA_BACK_ERRORS=$BE_DIR/be.dat

         export NL_MULTI_INC=1

      fi

      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR} 4dvar multi-inc stage 2 failed with error $RC$END"
         echo wrfvar_2 > FAIL
         break
      fi
      export WRF_INPUT=$DA_ANALYSIS
   else     
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE}
         fi
      fi
   fi

   if $RUN_ETKF; then
      export RUN_DIR=$EXP_DIR/run/$DATE/run_etkf
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh gen_be_etkf $RUN_DIR
      $SCRIPTS_DIR/da_run_etkf.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $? != 0 ]]; then
         echo $(date) "${ERR}etkf failed with error $RC$END"
         echo etkf > FAIL
         break
      fi
   fi

   if $RUN_UPDATE_BC; then
      if [[ $NUM_MEMBERS -gt 0 ]]; then
         export MEM=1
         export JOB=1

         while [[ $MEM -le $NUM_MEMBERS ]]; do
            export CMEM=e$MEM
            if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
            if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

            export RUN_DIR=$EXP_DIR/run/$DATE/update_bc.${CMEM}
            export PHASE=false
            mkdir -p $RUN_DIR

            export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d01
            export BDYIN=$RC_DIR/${DATE}.${CMEM}/wrfbdy_d01.wpb
            export DA_ANALYSIS=$FC_DIR/${DATE}.${CMEM}/${FILE_TYPE}_d01
            export BDYOUT=$FC_DIR/${DATE}.${CMEM}/wrfbdy_d01

            $SCRIPTS_DIR/da_trace.ksh da_run_update_bc $RUN_DIR
            $SCRIPTS_DIR/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1 &
            RC=$?
            if [[ $? != 0 ]]; then
               echo $(date) "${ERR}update_bc failed with error $RC$END"
               echo update_bc > FAIL
               break 2
            fi

            let MEM=$MEM+1
            let JOB=$JOB+1

            if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
               export JOB=1
               wait # Wait for current jobs to finish
            fi
            sleep 1 # Leave 1s gap between job start
         done
      else
         export RUN_DIR=$EXP_DIR/run/$DATE/update_bc
         export PHASE=false
         mkdir -p $RUN_DIR

         export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d01
         export BDYIN=$RC_DIR/$DATE/wrfbdy_d01
         export DA_ANALYSIS=$FC_DIR/$DATE/${FILE_TYPE}_d01
         export BDYOUT=$FC_DIR/$DATE/wrfbdy_d01

         $SCRIPTS_DIR/da_trace.ksh da_run_update_bc $RUN_DIR
         $SCRIPTS_DIR/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
         RC=$?
         if [[ $? != 0 ]]; then
            echo $(date) "${ERR}update_bc failed with error $RC$END"
            echo update_bc > FAIL
            break
         fi
      fi
   fi

   if $RUN_NDOWN; then
      export RUN_DIR=$SUITE_DIR/$DATE/ndown
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_ndown $RUN_DIR
      $SCRIPTS_DIR/da_run_ndown.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}ndown failed with error $RC$END"
         echo ndown > FAIL
         break
      fi
   fi

   if $RUN_NUP; then
      export RUN_DIR=$SUITE_DIR/$DATE/nup
      mkdir -p $RUN_DIR

      $SCRIPTS_DIR/da_trace.ksh da_run_nup $RUN_DIR
      $SCRIPTS_DIR/da_run_nup.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}nup failed with error $RC$END"
         echo nup > FAIL
         break
      fi
   fi

   if $RUN_WRF; then
      if [[ $NUM_MEMBERS -gt 0 ]]; then
         export MEM=1
         export JOB=1

         while [[ $MEM -le $NUM_MEMBERS ]]; do
            export CMEM=e$MEM
            if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
            if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

            export RUN_DIR=$EXP_DIR/run/$DATE/wrf.${CMEM}
            mkdir -p $RUN_DIR

            export WRF_INPUT_DIR=$RC_DIR/${DATE}.$CMEM
            if [[ $CYCLE_NUMBER -gt 0 ]] && $CYCLING; then
               export WRF_INPUT_DIR=$FC_DIR/$DATE.$CMEM
            fi
#            if [[ -f $FC_DIR/$DATE/${FILE_TYPE}_d01 ]]; then
#               export WRF_INPUT_DIR=$FC_DIR/$DATE
#            fi

            $SCRIPTS_DIR/da_trace.ksh da_run_wrf $RUN_DIR
            $SCRIPTS_DIR/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1 &
            RC=$?
            if [[ $RC != 0 ]]; then
               echo $(date) "${ERR}wrf failed with error $RC$END"
               echo wrf > FAIL
               break 2
            fi

            let MEM=$MEM+1
            let JOB=$JOB+1

            if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
               export JOB=1
               wait # Wait for current jobs to finish
            fi
            sleep 1 # Leave 1s gap between job start
         done
      else
         export RUN_DIR=$EXP_DIR/run/$DATE/wrf
         mkdir -p $RUN_DIR

         export WRF_INPUT_DIR=$RC_DIR/$DATE
         if [[ $CYCLE_NUMBER -gt 0 ]] && $CYCLING; then
            export WRF_INPUT_DIR=$FC_DIR/$DATE
         fi
         if [[ -f $FC_DIR/$DATE/${FILE_TYPE}_d01 ]]; then
            export WRF_INPUT_DIR=$FC_DIR/$DATE
         fi

         $SCRIPTS_DIR/da_trace.ksh da_run_wrf $RUN_DIR
         $SCRIPTS_DIR/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1
         RC=$?
         if [[ $RC != 0 ]]; then
            echo $(date) "${ERR}wrf failed with error $RC$END"
            echo wrf > FAIL
            break
         fi
      fi
   fi

   if $RUN_ENSMEAN; then
      export RUN_DIR=$EXP_DIR/run/$DATE/ensmean
      mkdir -p $RUN_DIR
      export FCST_RANGE=$CYCLE_PERIOD

      $SCRIPTS_DIR/da_trace.ksh gen_be_ensmean $RUN_DIR
      $SCRIPTS_DIR/da_run_ensmean.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $? != 0 ]]; then
         echo $(date) "${ERR}ensmean failed with error $RC$END"
         echo ensmean > FAIL
         break
      fi
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

