#!/bin/ksh

# da_run_job.ksh

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$RUN}
export WORK_DIR=$RUN_DIR/working

export SCRIPT=${SCRIPT:-$SCRIPTS_DIR/da_run_wrfvar.ksh}

mkdir -p $RUN_DIR
cd $RUN_DIR

if [[ $SUBMIT == "LoadLeveller" ]]; then 
   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1

   cat > job.ksh <<EOF
#!/bin/ksh
# @ job_name         = ${JOBNAME}
# @ total_tasks      = $NUM_PROCS
# @ node             = $NODES
# @ output           = job.output
# @ error            = job.error
# @ wall_clock_limit = $WALLCLOCK
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10
# @ queue            = $QUEUE

$SCRIPT > $RUN_DIR/index.html 2>&1
EOF
elif [[ $SUBMIT == "LSF" ]]; then 
   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB -J ${JOBNAME} 
#BSUB -q $QUEUE 
#BSUB -n $NUM_PROCS              
#BSUB -o job.output               
#BSUB -e job.error   
#BSUB -W $WALLCLOCK       
#BSUB -P $PROJECT        
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10

$SCRIPT > $RUN_DIR/index.html 2>&1

EOF
elif [[ $SUBMIT == "PBS" ]]; then 
   # Rather simplistic node calculation
   export TEMP=$NUM_PROCS
   if [[ $TEMP -gt 4 ]]; then
      TEMP=4
   fi
   typeset -L15 PBS_JOBNAME=${JOBNAME}
   cat > job.ksh <<EOF
#!/bin/ksh
#
# PBS batch script
#
#PBS -N ${PBS_JOBNAME}
##PBS -q $QUEUE
##PBS -l mppe=$NUM_PROCS
#PBS -l ncpus=$NUM_PROCS
#PBS -l walltime=$WALLCLOCK
#PBS -o job.output
#PBS -e job.error
#PBS -V
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10

$SCRIPT > $RUN_DIR/index.html 2>&1

EOF
elif [[ $SUBMIT == none ]]; then
   cat > job.ksh <<EOF
#!/bin/ksh
$SCRIPT > $RUN_DIR/index.html 2>&1
EOF
fi

if $CHECK_SVNVERSION; then
   if [[ -d $WRF_DIR ]]; then
      export WRF_VN=$(svnversion -n $WRF_DIR 2>/dev/null)
   fi
   if [[ -d $WRFNL_DIR ]]; then
      export WRFNL_VN=$(svnversion -n $WRFNL_DIR 2>/dev/null)
   fi
   if [[ -d $WRFVAR_DIR ]]; then
      export WRFVAR_VN=$(svnversion -n $WRFVAR_DIR 2>/dev/null)
   fi
   if [[ -d $WRFPLUS_DIR ]]; then
      export WRFPLUS_VN=$(svnversion -n $WRFPLUS_DIR 2>/dev/null)
   fi
   if [[ -d $WPS_DIR ]]; then
      export WPS_VN=$(svnversion -n $WPS_DIR 2>/dev/null)
   fi
fi

cat >> job.ksh <<EOF
RC=\$?
echo $(date +'%D %T') "Ended $RC"
EOF

chmod +x job.ksh

echo "Running with $NUM_PROCS processors, output to $RUN_DIR"
if [[ $SUBMIT == "LoadLeveller" ]]; then 
   llsubmit $SUBMIT_FLAGS job.ksh
elif [[ $SUBMIT == "LSF" ]]; then 
   bsub $SUBMIT_FLAGS < $PWD/job.ksh
elif [[ $SUBMIT == "PBS" ]]; then 
   qsub $SUBMIT_FLAGS $PWD/job.ksh
else
   ./job.ksh
fi

exit 0
