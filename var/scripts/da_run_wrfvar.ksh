#!/bin/ksh
#########################################################################
# Script: da_run_wrfvar.ksh
#
# Purpose: Run wrfvar
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wrfvar}

export WORK_DIR=$RUN_DIR/working

export WINDOW_START=${WINDOW_START:--3}
export WINDOW_END=${WINDOW_END:-3}
export FGATOBS_FREQ=${FGATOBS_FREQ:-1}

export YEAR=$(echo $DATE | cut -c1-4)
export MONTH=$(echo $DATE | cut -c5-6)
export DAY=$(echo $DATE | cut -c7-8)
export HOUR=$(echo $DATE | cut -c9-10)
export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
export NL_ANALYSIS_DATE=${ANALYSIS_DATE}.0000

export DA_FIRST_GUESS=${DA_FIRST_GUESS:-${RC_DIR}/$DATE/wrfinput_d01}
export EP_DIR=${EP_DIR:-$FC_DIR/$DATE/ep}

if $NL_VAR4D; then
   export DA_BOUNDARIES=${DA_BOUNDARIES:-$RC_DIR/$DATE/wrfbdy_d01}
   #DALE: Boundaries look wrong to me.
fi
if $CYCLING; then
   if [[ $CYCLE_NUMBER -gt 0 ]]; then
      if $NL_VAR4D; then
         export DA_BOUNDARIES=$FC_DIR/$DATE/wrfbdy_d01    # wrfvar boundaries input.
      fi
#cys: exclude WPB case in which DA_FIRST_GUESS is specified outside
#     export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE}
      if ! $RUN_WPB; then
         export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE}
      fi
   fi
fi
if [[ $NL_MULTI_INC == 2 ]]; then
   export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d01
   export DA_BOUNDARIES=${RC_DIR}/$DATE/wrfbdy_d01
fi
export DA_ANALYSIS=${DA_ANALYSIS:-analysis}
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # wrfvar background errors.

export RTTOV=${RTTOV:-$HOME/rttov/rttov87}                            # RTTOV
export DA_RTTOV_COEFFS=${DA_RTTOV_COEFFS:- }
export CRTM=${CRTM:-$HOME/crtm}
export DA_CRTM_COEFFS=${DA_CRTM_COEFFS:- }

# Error tuning namelist parameters
# Assign random seeds

export NL_SEED_ARRAY1=${NL_SEED_ARRAY1:-$DATE}
export NL_SEED_ARRAY2=${NL_SEED_ARRAY2:-$DATE}

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

if [[ $NL_MULTI_INC != 2 ]] ; then
   echo "<HTML><HEAD><TITLE>$EXPT wrfvar</TITLE></HEAD><BODY><H1>$EXPT wrfvar</H1><PRE>"
   if [[ $NL_MULTI_INC == 1 ]] ; then
      echo "============================================================================="
      echo "WRF Multi-incremental Stage I : Calculating High-Resolution Innovations"
      echo "============================================================================="
   fi
else
   echo "================================================================================================================="
   echo "WRF Multi-incremental Stage II : Solve Low-Resolution Minimization Problem With High-Resolution Innovations"
   echo "================================================================================================================="
fi

date

echo 'REL_DIR               <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR            <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
if $NL_VAR4D; then
   echo 'WRFNL_DIR             <A HREF="file:'$WRFNL_DIR'">'$WRFNL_DIR'</a>' $WRFNL_VN
   echo 'WRFPLUS_DIR           <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
fi
echo "DA_BACK_ERRORS        $DA_BACK_ERRORS"
if [[ -d $DA_RTTOV_COEFFS ]]; then
   echo "DA_RTTOV_COEFFS       $DA_RTTOV_COEFFS"
fi
if [[ -d $DA_CRTM_COEFFS ]]; then
   echo "DA_CRTM_COEFFS        $DA_CRTM_COEFFS"
fi
if [[ -d $BIASCORR_DIR ]]; then
   echo "BIASCORR_DIR          $BIASCORR_DIR"
fi
if [[ -d $OBS_TUNING_DIR ]] ; then
   echo "OBS_TUNING_DIR        $OBS_TUNING_DIR"
fi
if [[ -f $DA_VARBC_IN ]]; then
   echo "DA_VARBC_IN          $DA_VARBC_IN"
fi
echo 'OB_DIR                <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo 'RC_DIR                <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR                <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo 'EP_DIR                <A HREF="file:'$EP_DIR'">'$EP_DIR'</a>'
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

if $NL_VAR4D; then

   export NL_START_YEAR=$(echo $START_DATE | cut -c1-4)
   export NL_START_MONTH=$(echo $START_DATE | cut -c5-6)
   export NL_START_DAY=$(echo $START_DATE | cut -c7-8)
   export NL_START_HOUR=$(echo $START_DATE | cut -c9-10)

   export NL_END_YEAR=$(echo $END_DATE | cut -c1-4)
   export NL_END_MONTH=$(echo $END_DATE | cut -c5-6)
   export NL_END_DAY=$(echo $END_DATE | cut -c7-8)
   export NL_END_HOUR=$(echo $END_DATE | cut -c9-10)

fi

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

if [[ $NL_ANALYSIS_TYPE != "VERIFY" ]] ; then
  if [[ ! -r $DA_BACK_ERRORS ]]; then
   echo "${ERR}Background Error file >$DA_BACK_ERRORS< does not exist:$END"
   exit 1
  fi
fi

#-----------------------------------------------------------------------
# [3.0] Prepare for assimilation:
#-----------------------------------------------------------------------

if [[ -d $DA_RTTOV_COEFFS ]]; then
   ln -fs $DA_RTTOV_COEFFS/* .
fi

if [[ -d $DA_CRTM_COEFFS ]]; then
   ln -fs $DA_CRTM_COEFFS crtm_coeffs
fi

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

if $NL_VAR4D; then
   ln -fs $DA_BOUNDARIES wrfbdy_d01
   ln -fs $DA_FIRST_GUESS fg01
fi
ln -fs $DA_FIRST_GUESS fg 
ln -fs $DA_FIRST_GUESS ${FILE_TYPE}_d01
if [[ $NL_ANALYSIS_TYPE != "VERIFY" ]] ; then
  ln -fs $DA_BACK_ERRORS be.dat
fi


for FILE in $DAT_DIR/*.inv; do
   if [[ -f $FILE ]]; then
      ln -fs $FILE .
   fi
done

if [[ -d $EP_DIR ]]; then
   ln -fs $EP_DIR ep
fi

if [[ -d $BIASCORR_DIR ]]; then
   ln -fs $BIASCORR_DIR biascorr
fi

if [[ -d $OBS_TUNING_DIR ]]; then
   ln -fs $OBS_TUNING_DIR/* .
fi

if [[ -f $DA_VARBC_IN ]]; then
   ln -fs $DA_VARBC_IN "VARBC.in"
fi

export RADIANCE_INFO_DIR=${RADIANCE_INFO_DIR:-$WRFVAR_DIR/run/radiance_info}
ln -fs $RADIANCE_INFO_DIR radiance_info

if [[ $NL_NUM_FGAT_TIME -gt 1 ]]; then
   if $NL_VAR4D; then
      # More than one observation file of each type
      ln -fs $OB_DIR/${D_DATE[01]}/ob.ascii+ ob01.ascii
      for I in 02 03 04 05 06; do
         ln -fs $OB_DIR/${D_DATE[$I]}/ob.ascii ob${I}.ascii
      done
      ln -fs $OB_DIR/${D_DATE[07]}/ob.ascii- ob07.ascii

      if [[ -s $OB_DIR/${D_DATE[01]}/ob.ssmi+ ]]; then
         ln -fs $OB_DIR/${D_DATE[01]}/ob.ssmi+ ob01.ssmi
         for I in 02 03 04 05 06; do
            ln -fs $OB_DIR/${D_DATE[$I]}/ob.ssmi ob${I}.ssmi
         done
         ln -fs $OB_DIR/${D_DATE[07]}/ob.ssmi- ob07.ssmi
      fi

      if [[ -s $OB_DIR/${D_DATE[01]}/ob.radar+ ]]; then
         ln -fs $OB_DIR/${D_DATE[01]}/ob.radar+ ob01.radar
         for I in 02 03 04 05 06; do
            ln -fs $OB_DIR/${D_DATE[$I]}/ob.radar ob${I}.radar
         done
         ln -fs $OB_DIR/${D_DATE[07]}/ob.radar- ob07.radar
      fi
   else
      typeset -i N
      let N=0
      FGAT_DATE=$START_DATE
      until [[ $FGAT_DATE > $END_DATE ]]; do
         let N=$N+1
         ln -fs $OB_DIR/$FGAT_DATE/ob.ascii ob0${N}.ascii
         if [[ -s $OB_DIR/$FGAT_DATE/ob.ssmi ]]; then
            ln -fs $OB_DIR/$FGAT_DATE/ob.ssmi ob0${N}.ssmi
         fi
         if [[ -s $OB_DIR/$FGAT_DATE/ob.radar ]]; then
            ln -fs $OB_DIR/$FGAT_DATE/ob.radar ob0${N}.radar
         fi
         FYEAR=$(echo ${FGAT_DATE} | cut -c1-4)
         FMONTH=$(echo ${FGAT_DATE} | cut -c5-6)
         FDAY=$(echo ${FGAT_DATE} | cut -c7-8)
         FHOUR=$(echo ${FGAT_DATE} | cut -c9-10)
         ln -fs ${FC_DIR}/${PREV_DATE}/wrfinput_d01_${FYEAR}-${FMONTH}-${FDAY}_${FHOUR}:00:00 fg0${N}
         FGAT_DATE=$($BUILD_DIR/da_advance_time.exe $FGAT_DATE $FGATOBS_FREQ)
      done
   fi
else
   ln -fs $OB_DIR/${DATE}/ob.ascii  ob.ascii
   if [[ -s $OB_DIR/${DATE}/ob.ssmi ]]; then
      ln -fs $OB_DIR/${DATE}/ob.ssmi ob.ssmi
   fi
   if [[ -s $OB_DIR/${DATE}/ob.radar ]]; then
      ln -fs $OB_DIR/${DATE}/ob.radar ob.radar
   fi
fi

for FILE in $OB_DIR/$DATE/*.bufr; do
   if [[ -f $FILE ]]; then
      ln -fs $FILE .
   fi
done

if $NL_VAR4D; then
   if $NL_JCDFI_USE; then
      export NL_JCDFI_IO=true
   fi
   # Create nl, tl, ad links structures
   mkdir nl tl ad

   # nl

   # Inputs
   export NL_HISTORY_INTERVAL_SAVE=$NL_HISTORY_INTERVAL
   export NL_HISTORY_INTERVAL=9999
   export NL_AUXHIST2_OUTNAME_SAVE=$NL_AUXHIST2_OUTNAME
   export NL_AUXHIST2_OUTNAME='auxhist2_d<domain>_<date>'
   if [[ $NUM_PROCS -gt 1 ]]; then
      export NL_AUXHIST2_OUTNAME='./nl/auxhist2_d<domain>_<date>'
   fi
   export NL_DYN_OPT_SAVE=$NL_DYN_OPT
   export NL_DYN_OPT=2
   export NL_INPUT_OUTNAME_SAVE=$NL_INPUT_OUTNAME
   export NL_INPUT_OUTNAME='nl_d<domain>_<date>'
   if [[ $NUM_PROCS -gt 1 ]]; then
      export NL_INPUT_OUTNAME='./nl/nl_d<domain>_<date>'
   fi
   export NL_INPUTOUT_INTERVAL_SAVE=$NL_INPUTOUT_INTERVAL
   export NL_INPUTOUT_INTERVAL=60
   export NL_AUXHIST2_INTERVAL_SAVE=$NL_INPUTOUT_INTERVAL
   let NL_AUXHIST2_INTERVAL=$NL_TIME_STEP/60
   export NL_FRAMES_PER_AUXHIST2_SAVE=$NL_FRAMES_PER_AUXHIST2
   export NL_FRAMES_PER_AUXHIST2=1

   export NL_TIME_STEP_SOUND_SAVE=$NL_TIME_STEP_SOUND
   export NL_TIME_STEP_SOUND=4
   . $WRFNL_DIR/inc/namelist_script.inc 
   mv namelist.input nl
   export NL_AUXHIST2_OUTNAME=$NL_AUXHIST2_OUTNAME_SAVE
   export NL_FRAMES_PER_AUXHIST2=$NL_AUXHIST2_INTERVAL_SAVE
   ln -fs $WORK_DIR/*.TBL nl
   ln -fs $WORK_DIR/RRTM_DATA nl
   ln -fs $WORK_DIR/wrfbdy_d01 nl
   ln -fs $WORK_DIR/wrfinput_d01 nl/wrfinput_d01
   ln -fs $WRFNL_DIR/main/wrf.exe nl

   # Outputs
   for I in 02 03 04 05 06 07; do
      if [[ $NL_MULTI_INC == 2 ]]; then
         ln -fs nl/nl_d01_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00-thin fg$I
      else
         ln -fs nl/nl_d01_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00 fg$I
      fi
   done

   # tl

   # Inputs

   export NL_DYN_OPT=202
   export NL_INPUT_OUTNAME='tl_d<domain>_<date>'
   export NL_AUXINPUT2_INNAME_SAVE=$NL_AUXINPUT2_INNAME
   if [[ $NL_MULTI_INC == 2 ]]; then
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>-thin'
   else
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>'
   fi
   if [[ $NUM_PROCS -gt 1 ]]; then
      export NL_INPUT_OUTNAME='./tl/tl_d<domain>_<date>'
      if [[ $NL_MULTI_INC == 2 ]]; then
         export NL_AUXINPUT2_INNAME='./nl/auxhist2_d<domain>_<date>-thin'
      else
         export NL_AUXINPUT2_INNAME='./nl/auxhist2_d<domain>_<date>'
      fi
   fi
   export NL_AUXINPUT2_INTERVAL_SAVE=$NL_AUXINPUT2_INTERVAL
   let NL_AUXINPUT2_INTERVAL=$NL_TIME_STEP/60
   unset NL_AUXHIST2_INTERVAL
   export NL_MP_PHYSICS_SAVE=$NL_MP_PHYSICS
   export NL_MP_PHYSICS=0
   export NL_RA_LW_PHYSICS_SAVE=$NL_RA_LW_PHYSICS
   export NL_RA_LW_PHYSICS=0
   export NL_RA_SW_PHYSICS_SAVE=$NL_RA_SW_PHYSICS
   export NL_RA_SW_PHYSICS=0
   export NL_RADT_SAVE=$NL_RADT
   export NL_RADT=0
   export NL_SF_SFCLAY_PHYSICS_SAVE=$NL_SF_SFCLAY_PHYSICS
   export NL_SF_SFCLAY_PHYSICS=0
   export NL_SF_SURFACE_PHYSICS_SAVE=$NL_SF_SURFACE_PHYSICS
   export NL_SF_SURFACE_PHYSICS=0 # Just for tl/ad
   export NL_BL_PBL_PHYSICS_SAVE=$NL_BL_PBL_PHYSICS
   export NL_BL_PBL_PHYSICS=0
   export NL_BLDT_SAVE=$NL_BLDT_SAVE
   export NL_BLDT=0
   export NL_CU_PHYSICS_SAVE=$NL_CU_PHYSICS
   export NL_CU_PHYSICS=0
   export NL_CUDT_SAVE=$NL_CUDT
   export NL_CUDT=0
   export NL_ISFFLX_SAVE=$NL_ISFFLX
   export NL_ISFFLX=0
   export NL_ICLOUD_SAVE=$NL_ICLOUD
   export NL_ICLOUD=0
   export NL_W_DAMPING_SAVE=$NL_W_DAMPING
   export NL_W_DAMPING=0
   export NL_DIFF_OPT_SAVE=$NL_DIFF_OPT
   export NL_DIFF_OPT=1
   export NL_DAMPCOEF_SAVE=$NL_DAMPCOEF
   export NL_DAMPCOEF=0.2
   . $WRFPLUS_DIR/inc/namelist_script.inc
   mv namelist.input tl
   ln -fs $WORK_DIR/*.TBL tl
   ln -fs $WORK_DIR/RRTM_DATA tl
   ln -fs $WORK_DIR/wrfbdy_d01 tl
   ln -fs $WORK_DIR/tl01 tl/wrfinput_d01
   ln -fs $WRFPLUS_DIR/main/wrfplus.exe tl
   mkdir tl/trace

   # Outputs
   for I in 02 03 04 05 06 07; do
      ln -fs tl/tl_d01_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00 tl$I
   done
   if $NL_JCDFI_IO; then
      if [[ $NUM_PROCS -gt 1 ]]; then
         ln -fs auxhist3_d01_${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00 tldf
      else
         ln -fs tl/auxhist3_d01_${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00 tldf
      fi
   fi

   # ad

   # Inputs
   export NL_DYN_OPT=302
   export NL_INPUT_OUTNAME='ad_d<domain>_<date>'
   if [[ $NL_MULTI_INC == 2 ]]; then
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>-thin'
   else
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>'
   fi
   if [[ $NUM_PROCS -gt 1 ]]; then
      export NL_INPUT_OUTNAME='./ad/ad_d<domain>_<date>'
      if [[ $NL_MULTI_INC == 2 ]]; then
         export NL_AUXINPUT2_INNAME='./nl/auxhist2_d<domain>_<date>-thin'
      else
         export NL_AUXINPUT2_INNAME='./nl/auxhist2_d<domain>_<date>'
      fi
   fi
   let NL_AUXINPUT2_INTERVAL=$NL_TIME_STEP/60
   export NL_AUXINPUT3_INNAME_SAVE=$NL_AUXINPUT3_INNAME
   export NL_AUXINPUT3_INNAME='auxinput3_d<domain>_<date>'
   if [[ $NUM_PROCS -gt 1 ]]; then
      export NL_AUXINPUT3_INNAME='./ad/auxinput3_d<domain>_<date>'
   fi
   export NL_AUXINPUT3_INTERVAL_SAVE=$NL_AUXINPUT3_INTERVAL
   export NL_AUXINPUT3_INTERVAL=60
   export NL_AUXHIST3_INTERVAL_SAVE=$NL_AUXHIST3_INTERVAL
   export NL_AUXHIST3_INTERVAL=60
   export NL_INPUTOUT_INTERVAL=60
   . $WRFPLUS_DIR/inc/namelist_script.inc
   mv namelist.input ad
   ln -fs $WORK_DIR/*.TBL ad
   ln -fs $WORK_DIR/RRTM_DATA ad
   ln -fs $WORK_DIR/wrfbdy_d01 ad
   ln -fs $DA_FIRST_GUESS ad/wrfinput_d01
   for I in 01 02 03 04 05 06 07; do
      ln -fs $WORK_DIR/af$I ad/auxinput3_d01_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00
   done
   # JRB
   if $NL_JCDFI_IO; then
      ln -fs $WORK_DIR/auxhist3_d01_${D_YEAR[01]}-${D_MONTH[01]}-${D_DAY[01]}_${D_HOUR[01]}:00:00 ad/auxinput3_d01_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00_dfi
   fi   
   ln -fs $WRFPLUS_DIR/main/wrfplus.exe ad
   mkdir ad/trace

   # Outputs
   ln -fs ad/ad_d01_${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00 gr01

   # Restore values
   export NL_HISTORY_INTERVAL=$NL_HISTORY_INTERVAL_SAVE
   export NL_AUXINPUT2_INTERVAL=$NL_AUXINPUT2_INTERVAL_SAVE
   export NL_AUXINPUT2_INNAME=$NL_AUXINPUT2_INNAME_SAVE
   export NL_AUXINPUT3_INTERVAL=$NL_AUXINPUT3_INTERVAL_SAVE
   export NL_AUXINPUT3_INNAME=$NL_AUXINPUT3_INNAME_SAVE
   export NL_AUXHIST2_INTERVAL=$NL_AUXHIST2_INTERVAL_SAVE
   export NL_AUXHIST3_INTERVAL=$NL_AUXHIST3_INTERVAL_SAVE
   export NL_INPUTOUT_INTERVAL=$NL_INPUTOUT_INTERVAL_SAVE
   export NL_INPUT_OUTNAME=$NL_INPUT_OUTNAME_SAVE
   export NL_FRAMES_PER_AUXHIST2=$NL_FRAMES_PER_AUXHIST2_SAVE
   export NL_DYN_OPT=$NL_DYN_OPT_SAVE
   export NL_MP_PHYSICS=$NL_MP_PHYSICS_SAVE
   export NL_RA_LW_PHYSICS=$NL_RA_LW_PHYSICS_SAVE
   export NL_RA_SW_PHYSICS=$NL_RA_SW_PHYSICS_SAVE
   export NL_SF_SFCLAY_PHYSICS=$NL_SF_SFCLAY_PHYSICS_SAVE
   export NL_BL_PBL_PHYSICS=$NL_BL_PBL_PHYSICS_SAVE
   export NL_BLDT=$NL_BLDT_SAVE
   export NL_CU_PHYSICS=$NL_CU_PHYSICS_SAVE
   export NL_CUDT=$NL_CUDT_SAVE
   export NL_ISFFLX=$NL_ISFFLX_SAVE
   export NL_ICLOUD=$NL_ICLOUD_SAVE
   export NL_SF_SURFACE_PHYSICS=$NL_SF_SURFACE_PHYSICS_SAVE
   export NL_RADT=$NL_RADT_SAVE
   export NL_TIME_STEP_SOUND=$NL_TIME_STEP_SOUND_SAVE
   export NL_W_DAMPING=$NL_W_DAMPING_SAVE
   export NL_DIFF_OPT=$NL_DIFF_OPT_SAVE
   export NL_DAMPCOEF=$NL_DAMPCOEF_SAVE

fi

. $WRFVAR_DIR/inc/namelist_script.inc 

if $NL_VAR4D; then
   cp namelist.input $RUN_DIR/namelist_wrfvar.input
   cp nl/namelist.input $RUN_DIR/namelist_nl.input
   cp tl/namelist.input $RUN_DIR/namelist_tl.input
   cp ad/namelist.input $RUN_DIR/namelist_ad.input
   echo '<A HREF="namelist_wrfvar.input">WRFVAR namelist.input</a>'
   echo '<A HREF="namelist_nl.input">NL namelist.input</a>'
   echo '<A HREF="namelist_tl.input">TL namelist.input</a>'
   echo '<A HREF="namelist_ad.input">AD namelist.input</a>'
else
   cp namelist.input $RUN_DIR
   echo '<A HREF="namelist.input">Namelist.input</a>'
fi

#-------------------------------------------------------------------
#Prepare the multi-incremnet files:
#-------------------------------------------------------------------

if [[ $NL_MULTI_INC == 2 ]] ; then

   mv -f $RUN_DIR/gts_omb.*  .

   if $NL_VAR4D; then
      mv -f $RUN_DIR/auxhist2*-thin $WORK_DIR/nl
      mv -f $RUN_DIR/nl_*-thin $WORK_DIR/nl
   fi

   mv -f $RUN_DIR/wrfinput_d01-thin $WORK_DIR
   ln -fs wrfinput_d01-thin wrfinput_d01
   ln -fs wrfinput_d01-thin fg01
   ln -fs wrfinput_d01-thin fg

fi

#-------------------------------------------------------------------
#Run WRF-Var:
#-------------------------------------------------------------------
mkdir trace

if $DUMMY; then
   echo Dummy wrfvar
   echo "Dummy wrfvar" > $DA_ANALYSIS
   RC=0
else
   if $NL_VAR4D; then
      if [[ $NUM_PROCS -gt 1 ]]; then
         # JRB kludge until we work out what we are doing here
         if [[ $NUM_PROCS -lt 3 ]]; then
            echo "Need at least 3 processors for 4dvar in parallel mode"
            exit 1
         fi
         export NUM_PROCS_VAR=`expr $NUM_PROCS \/ 4`
         export NUM_PROCS_WRF=`expr $NUM_PROCS \/ 4`
         export NUM_PROCS_WRFPLUS=`expr $NUM_PROCS \/ 2`
         export LSF_PTILE=${LSF_PTILE:-$NUM_PROCS}
         export NUM_4DVAR_NODES=`expr $NUM_PROCS \/ $LSF_PTILE`
         if [[ $NUM_4DVAR_NODES -lt 1 ]] ; then
            export NUM_4DVAR_NODES=1
         fi
         export NUM_PROCS_VAR_PER_NODE=`expr $NUM_PROCS_VAR \/ $NUM_4DVAR_NODES`
         export NUM_PROCS_WRF_PER_NODE=`expr $NUM_PROCS_WRF \/ $NUM_4DVAR_NODES`
         export NUM_PROCS_WRFPLUS_PER_NODE=`expr $NUM_PROCS_WRFPLUS \/ $NUM_4DVAR_NODES`
         if [[ $NUM_4DVAR_NODES -gt 1 ]] ; then
            if [[ $NUM_PROCS_VAR_PER_NODE+$NUM_PROCS_WRF_PER_NODE+$NUM_PROCS_WRFPLUS_PER_NODE -ne $LSF_PTILE ]] ; then
               echo $NUM_PROCS_VAR_PER_NODE+$NUM_PROCS_WRF_PER_NODE+$NUM_PROCS_WRFPLUS_PER_NODE
               echo "Wrong processors distribution", $NUM_PROCS_VAR_PER_NODE, $NUM_PROCS_WRF_PER_NODE, $NUM_PROCS_WRFPLUS_PER_NODE, $LSF_PTILE
               exit 1
            fi
         fi
         if [[ $NL_MULTI_INC == 1 ]] ; then
            export NUM_PROCS_VAR=1
            export NUM_PROCS_WRF=`expr $NUM_PROCS - 1`
            export NUM_PROCS_WRFPLUS=0
         fi
         if [[ $NUM_PROCS -eq 3 ]]; then
            export NUM_PROCS_VAR=1
            export NUM_PROCS_WRF=1
            export NUM_PROCS_WRFPLUS=1
         fi
         echo "NUM_PROCS_VAR                $NUM_PROCS_VAR"
         echo "NUM_PROCS_WRF                $NUM_PROCS_WRF"
         echo "NUM_PROCS_WRFPLUS            $NUM_PROCS_WRFPLUS"

         if [[ $SUBMIT == "LSF" ]]; then
            export MP_PGMMODEL=mpmd
            export MP_CMDFILE=poe.cmdfile
            rm -f $MP_CMDFILE

            if [[ $NL_MULTI_INC == 1 ]] ; then
               let I=0
               while [[ $I -lt $NUM_PROCS_VAR ]]; do
                  echo "da_wrfvar.exe" >> $MP_CMDFILE
                  let I=$I+1
               done
               let I=0
               while [[ $I -lt $NUM_PROCS_WRF ]]; do
                  echo "./nl/wrf.exe" >> $MP_CMDFILE
                  let I=$I+1
               done
            else
               let INode=1
               while [[ $INode -le $NUM_4DVAR_NODES ]]; do
                  let I=0
                  while [[ $I -lt $NUM_PROCS_VAR_PER_NODE ]]; do
                     echo "da_wrfvar.exe" >> $MP_CMDFILE
                     let I=$I+1
                  done
                  let I=0
                  while [[ $I -lt $NUM_PROCS_WRFPLUS_PER_NODE ]]; do
                     echo "./ad/wrfplus.exe" >> $MP_CMDFILE
                     let I=$I+1
                  done
                  let I=0
                  while [[ $I -lt $NUM_PROCS_WRF_PER_NODE ]]; do
                     echo "./nl/wrf.exe" >> $MP_CMDFILE
                     let I=$I+1
                  done
                  let INode=$INode+1
               done
            fi

         mpirun.lsf -cmdfile  $MP_CMDFILE
         RC=$?

         fi

         if [[ $SUBMIT == "none" ]]; then
            mpirun -np $NUM_PROCS_VAR ./da_wrfvar.exe : -np $NUM_PROCS_WRFPLUS ./ad/wrfplus.exe : -np $NUM_PROCS_WRF ./nl/wrf.exe
         fi

      else
         $RUN_CMD ./da_wrfvar.exe
         RC=$?
      fi
   else
      # 3DVAR
      $RUN_CMD ./da_wrfvar.exe
      RC=$?
   fi

# temporarily store the high resolution reults in RUN_DIR
   if [[ $NL_MULTI_INC == 1 ]]; then

      mv -f gts_omb.*  $RUN_DIR

      if $NL_VAR4D; then
        cd nl
        ln -fs $WRFPLUS_DIR/main/nupdown.exe .
        ls -l auxhist2* | awk '{print $9}' | sed -e 's/auxhist2/nupdown.exe auxhist2/' -e 's/:00$/:00 -thin 3/' > thin.csh
        ls -la nl_d01* | awk '{print $9}' |sed -e 's/nl/nupdown.exe nl/' -e 's/:00$/:00 -thin 3/' >> thin.csh
        sh thin.csh
        cd ..
        mv -f $WORK_DIR/nl/*-thin $RUN_DIR
      fi

      $WRFPLUS_DIR/main/nupdown.exe wrfinput_d01 -thin 3

      mv -f wrfinput_d01-thin $RUN_DIR

      exit $RC
   fi

   if [[ -f fort.9 ]]; then
      cp fort.9 $RUN_DIR/namelist.output
   fi

   if [[ -f statistics ]]; then
      cp statistics $RUN_DIR
   fi

   for INDEX in 01 02 03 04 05 06 07 08 09 10; do
      if [[ -f rej_obs_conv_$INDEX.000 ]]; then
         cat rej_obs_conv_$INDEX.* > rej_obs_conv_$INDEX
      fi
     cp rej_obs_conv_$INDEX $RUN_DIR

     if [[ -f qcstat_conv_$INDEX ]]; then
     cp qcstat_conv_$INDEX $RUN_DIR
     fi

     if [[ -f gts_omb_oma_$INDEX ]]; then
     cp gts_omb_oma_$INDEX $RUN_DIR
     fi
   done 

   if [[ -f cost_fn ]]; then 
      cp cost_fn $RUN_DIR
   fi

   if [[ -f grad_fn ]]; then
      cp grad_fn $RUN_DIR
   fi

   if [[ -f gts_omb_oma ]]; then
      cp gts_omb_oma $RUN_DIR
   fi

   if [[ -f filtered_obs ]]; then
      cp filtered_obs $RUN_DIR
   fi

   if [[ -f jo ]]; then
      cp jo $RUN_DIR
   fi

   if [[ -f unpert_obs ]]; then
      cp unpert_obs $RUN_DIR
   fi

   if [[ -f check_max_iv ]]; then
      cp check_max_iv $RUN_DIR
   fi

   if [[ -f ob.etkf.000 ]]; then
      cp ob.etkf.000 $RUN_DIR
   fi

# remove intermediate output files

   if [[ $NL_MULTI_INC == 2 ]] ; then

      ncdiff -O -v "U,V,W,PH,T,QVAPOR,MU,MU0" wrfvar_output fg01 low_res_increment

      ${WRFPLUS_DIR}/main/nupdown.exe -down 3 low_res_increment

        
      cp -f ${RC_HIGH_DIR}/${DATE}/wrfinput_d01 ${FC_DIR}/${DATE}/analysis_update
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            cp -f ${FC_DIR}/${PREV_DATE}/wrf_3dvar_input_d01_${ANALYSIS_DATE} ${FC_DIR}/${DATE}/analysis_update
         fi
      fi

      ncflint -A -v "U,V,W,PH,T,QVAPOR,MU,MU0" -w 1,1 low_res_increment-down ${FC_DIR}/${DATE}/analysis_update ${FC_DIR}/${DATE}/analysis_update

#     rm low_res_increment low_res_increment-down
      cp -f ${FC_DIR}/${DATE}/analysis_update $DA_ANALYSIS

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
   rm filtered_obs.*
   # No routine to merge these files across processors yet
   # rm -f inv_*.*
   # rm -f oma_*.*
   # rm -f filtered_*.*

   if [[ $NL_MULTI_INC == 0 ]] ; then
      if [[ -f wrfvar_output ]]; then
         if [[ $DA_ANALYSIS != wrfvar_output ]]; then 
            cp wrfvar_output $DA_ANALYSIS
         fi
      fi
   fi

   if $NL_VAR4D; then
      cp $WORK_DIR/namelist_wrfvar.output $RUN_DIR/namelist_wrfvar.output
      cp $WORK_DIR/nl/namelist.output     $RUN_DIR/namelist_nl.output
      cp $WORK_DIR/tl/namelist.output     $RUN_DIR/namelist_tl.output
      cp $WORK_DIR/ad/namelist.output     $RUN_DIR/namelist_ad.output
      echo '<A HREF="namelist_wrfvar.output">WRFVAR namelist.output</a>'
      echo '<A HREF="namelist_nl.output">NL namelist.output</a>'
      echo '<A HREF="namelist_tl.output">TL namelist.output</a>'
      echo '<A HREF="namelist_ad.output">AD namelist.output</a>'
   else
      cp $WORK_DIR/namelist.output $RUN_DIR
      echo '<A HREF="namelist.output">Namelist.output</a>'
   fi

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
