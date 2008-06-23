#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_run_update_bc.ksh
#
# Purpose: Update WRF lateral boundary conditions to be consistent with 
# WRFVAR analysis.
#
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/update_bc}
export WORK_DIR=$RUN_DIR/working

echo "<HTML><HEAD><TITLE>$EXPT update_bc</TITLE></HEAD><BODY>"
echo "<H1>$EXPT update_bc</H1><PRE>"

date

mkdir -p ${RUN_DIR}

export DA_REAL_OUTPUT=${DA_REAL_OUTPUT:-$RC_DIR/$DATE/wrfinput_d01} # Input (needed only if cycling).
export BDYIN=${BDYIN:-$RC_DIR/$DATE/wrfbdy_d01}       # Input bdy.
if $NL_VAR4D ; then
   if $CYCLING; then
      if [[ $CYCLE_NUMBER -gt 0 ]]; then
         if $PHASE; then
            export YEAR=$(echo $DATE | cut -c1-4)
            export MONTH=$(echo $DATE | cut -c5-6)
            export DAY=$(echo $DATE | cut -c7-8)
            export HOUR=$(echo $DATE | cut -c9-10)
            export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
            export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
            export DA_ANALYSIS=${FC_DIR}/${PREV_DATE}/wrfinput_d01_${ANALYSIS_DATE}
         else
            export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
         fi
      else
         export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
      fi
   fi
else
   export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
fi
export BDYOUT=${BDYOUT:-$FC_DIR/$DATE/wrfbdy_d01}     # Output bdy.

rm -rf ${WORK_DIR}
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}

echo 'REL_DIR        <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR     <A HREF="'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>'
echo "DATE           $DATE"
echo "DA_REAL_OUTPUT $DA_REAL_OUTPUT"
echo "BDYIN          $BDYIN"
echo "DA_ANALYSIS    $DA_ANALYSIS"
echo "BDYOUT         $BDYOUT"
echo 'WORK_DIR       <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

cp -f $DA_REAL_OUTPUT real_output 
cp -f $BDYIN wrfbdy_d01
ln -sf $DA_ANALYSIS wrfvar_output

cat > parame.in << EOF
&control_param
 wrfvar_output_file = 'wrfvar_output'
 wrf_bdy_file       = 'wrfbdy_d01'
 wrf_input          = 'real_output'

 cycling = .${CYCLING}.
 debug   = .true.
 low_bdy_only = .${NL_LOW_BDY_ONLY}. 
 update_lsm = .${NL_UPDATE_LSM}. /
EOF

if $DUMMY; then
   echo "Dummy update_bc"
   echo Dummy update_bc > wrfbdy_d01
else

   ln -fs $BUILD_DIR/da_update_bc.exe .
   ./da_update_bc.exe

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Update_bc failed with error $RC"
      exit 1
   else
      cp wrfbdy_d01 $BDYOUT
   fi
fi

if $CLEAN; then
   rm -rf ${WORK_DIR}
fi

date

echo "</BODY></HTML>"

exit 0
