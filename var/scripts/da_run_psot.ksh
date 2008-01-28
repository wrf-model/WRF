#!/bin/ksh
#########################################################################
# Script: da_run_psot.ksh
#
# Purpose: Run pseudo single observation test
# Description:
#          The suggestion for the single observation location is at the center of the
#          domain and at the following representative levels (approximately):
#          u: eta=0.85 (~850mb), eta=0.25(~250mb)
#          T: eta=0.85 (~850mb), eta=0.50(~500mb)
#          q: eta=0.85 (~850mb)
#          Before running the psot, please set up related directories (e.g., be/,
#          rc/) and namelist as same as the desired real observation data assimilation 
#          run in a wrapper scripts, and please define the following values only for PSOT run:
#          PSEUDO_VAR_SIZE: number of tests.
#          PSEUDO_VAR_LIST: list of the single obervation variable names in each test.
#          PSEUDO_VAL_LIST: list of the single observation values 
#                           (corresponding to each in the VAR_LIST).
#          PSEUDO_ERR_LIST: list of the single observation O-B values
#          PSEUDO_X_LIST: list of the grid indice for the longitudes of 
#                         the single observations.
#          PSEUDO_Y_LIST: list of the grid indice for the latitidues of 
#                         the single observations.
#          PSEUDO_Z_LIST: list of the level indice for the half eta (mass) 
#                         levels of the single observations
#                         (NL_ETA_LEVELS in wrfvar namelist defines the full 
#                          eta levels starting from 1.000. While counting the 
#                          level indice for half eta levels, please skip 
#                          eta=1.0 level from the namelist.)
#          
# Author:  Hui Shao, NCAR DATC 08/27/2007
#########################################################################
#------------------------------------------------------------------------
#Set defaults for required environment variables
#------------------------------------------------------------------------
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

# Experiment
export RUN_WRFVAR=true
export CYCLING=false  #cold start only

# wrfvar namelist
export NL_WRITE_INCREMENTS=${NL_WRITE_INCREMENTS:-true}
export NL_CHECK_RH=0
export NL_NUM_PSEUDO=1                           #single-obs test

#------------------------------------------------------------------------
# convert pseudo list into array
#------------------------------------------------------------------------
ivar=0
for var in $PSEUDO_VAR_LIST; do
   (( ivar=ivar+1 ))
   export PSEUDO_VAR[$ivar]=$var
done
if [[ $ivar != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_VAR_LIST($ivar) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)! "
   exit 1
fi

ival=0
for var in $PSEUDO_VAL_LIST; do
   (( ival=ival+1 ))
   export PSEUDO_VAL[$ival]=$var
done
if [[ $ival != $PSEUDO_VAR_SIZE ]] ; then
   echo "Error: Size of PSEUDO_VAL_LIST($ival) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

ierr=0
for var in $PSEUDO_ERR_LIST; do
   (( ierr=ierr+1 ))
export PSEUDO_ERR[$ierr]=$var
done
if [[ $ierr != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_ERR_LIST($ierr) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

ix=0
for var in $PSEUDO_X_LIST; do
   (( ix=ix+1 ))
   export PSEUDO_X[$ix]=$var
done
if [[ $ix != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_X_LIST($ix) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

iy=0
for var in $PSEUDO_Y_LIST; do
   (( iy=iy+1 ))
   export PSEUDO_Y[$iy]=$var
done
if [[ $iy != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_Y_LIST($iy) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

iz=0
for var in $PSEUDO_Z_LIST; do
   (( iz=iz+1 ))
   export PSEUDO_Z[$iz]=$var
done
if [[ $iz != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_Z_LIST($iz) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

#------------------------------------------------------------------------
#PSOT: loop for different variables
 cd  ${EXP_DIR}
#------------------------------------------------------------------------
export FC_DIR_SAVE=$FC_DIR
iv=1
while [[ $iv -le $PSEUDO_VAR_SIZE ]]; do
   export NL_PSEUDO_VAR=${PSEUDO_VAR[$iv]}
   export NL_PSEUDO_VAL=${PSEUDO_VAL[$iv]}
   export NL_PSEUDO_ERR=${PSEUDO_ERR[$iv]}
   export   NL_PSEUDO_X=${PSEUDO_X[$iv]}
   export   NL_PSEUDO_Y=${PSEUDO_Y[$iv]}
   export   NL_PSEUDO_Z=${PSEUDO_Z[$iv]}

   echo "Single obs test for $NL_PSEUDO_VAR($NL_PSEUDO_X,$NL_PSEUDO_Y,$NL_PSEUDO_Z) with val=$NL_PSEUDO_VAL and err=$NL_PSEUDO_ERR"
 
   export FC_DIR=${FC_DIR_SAVE}/psot$iv
 
   if test ! -d $DAT_DIR; then mkdir $DAT_DIR; fi
   if test ! -d $REG_DIR; then mkdir $REG_DIR; fi
   if test ! -d $EXP_DIR; then mkdir $EXP_DIR; fi
   if test ! -d $EXP_DIR/run; then mkdir $EXP_DIR/run; fi
   if test ! -d $FC_DIR_SAVE; then mkdir $FC_DIR_SAVE; fi
   if test ! -d $FC_DIR; then mkdir $FC_DIR; fi

   export DATE=$INITIAL_DATE

      if test ! -d $FC_DIR/$DATE; then mkdir -p $FC_DIR/$DATE; fi
 
      export RUN_DIR=$EXP_DIR/run/$DATE/wrfvar_psot$iv
      mkdir -p $RUN_DIR
 
      export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d${DOMAINS}
      export DA_ANALYSIS=$FC_DIR/$DATE/analysis

      #-----------------------------------------------------------------------
      #PSOT: submit job
      #-----------------------------------------------------------------------
      export WRFVAR_VN=$(svnversion -n \$WRFVAR_DIR 2>/dev/null)

      if [[ $SUBMIT != LSF ]]; then
          echo "Only works with LSF queueing system"
          exit 1
      fi


      cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
############BSUB -a mpich_gm
#BSUB -a poe
#BSUB -n $NUM_PROCS
#BSUB -J $EXPT
#BSUB -o $EXPT.out
#BSUB -e $EXPT.err
#BSUB -q $QUEUE
#BSUB -P $PROJECT
#BSUB -W $WALLCLOCK
#BSUB -R "span[ptile=$LSF_PTILE]"
############BSUB -w \"done(${PREV_JOBID})\"

. ${SCRIPTS_DIR}/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

EOF

      chmod +x job.ksh
      bsub -q $QUEUE -n $NUM_PROCS < job.ksh  

      RC=$?
      if test $RC != 0; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi

   echo

   ((iv=iv+1 ))
done #end of loop for variables

exit 0
