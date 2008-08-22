#!/bin/ksh
#-------------------------------------------------------------------------
#  Script for getting initial diagnostics for 
#  Observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#-------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/hollingsworth}
export WORK_DIR=$RUN_DIR/working

echo ""
echo "Running da_tune_obs_hollingsworth.ksh"
echo ""

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

echo "WRFVAR_DIR    = $WRFVAR_DIR"
echo "EXP_DIR       = $EXP_DIR"
echo "RUN_DIR       = $RUN_DIR"
echo "START_DATE    = $START_DATE"
echo "END_DATE      = $END_DATE"
  
export DATE=$START_DATE

while [[ $DATE -le $END_DATE ]]; do
   cat ${EXP_DIR}/run/${DATE}/wrfvar/working/gts_omb_oma_${OUTER_ITER} >> hollingsworth1.in
   export DATE=$($BUILD_DIR/da_advance_time.exe $DATE $CYCLE_PERIOD)
done

echo '*end*' >> hollingsworth1.in

$BUILD_DIR/da_tune_obs_hollingsworth1.exe > hollingsworth1.log 2>&1

rm hollingsworth1.in

for FILE1 in *.dat; do
   FILE2=$(basename $FILE1)
   FILE2=${FILE2%%.dat}
   ln -fs $FILE1 fort.35

   $BUILD_DIR/da_tune_obs_hollingsworth2.exe > hollingsworth2_$FILE2.log 2>&1
   if [[ -f fort.30 ]]; then 
      mv fort.30     $FILE2.sigma_o_b
   fi
   mv hollingsworth2.out $FILE2.out
done

mv *.log *.out $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

exit 0
echo ""
echo " da_tune_obs_hollingsworth.ksh completed"
echo ""
