#!/bin/ksh
#########################################################################
# Script: da_run_etkf.ksh
#
# Purpose: To perform an Ensemble Transform Kalman Filter (ETKF)
# rescaling of ensemble forecast perturbations.
# The ensemble mean is the WRF-Var analysis.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export NUM_PROCS=1 # will not run parallel
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/etkf}
export WORK_DIR=$RUN_DIR/working

export NL_CHECK_MAX_IV=.false.
export NL_ANALYSIS_TYPE="VERIFY"
export DA_ANALYSIS=analysis_not_used
export ETKF_INPUT_DIR=${ETKF_INPUT_DIR:-$FC_DIR}
export ETKF_OUTPUT_DIR=${ETKF_OUTPUT_DIR:-$FC_DIR}

echo "<HTML><HEAD><TITLE>$EXPT etkf</TITLE></HEAD><BODY><H1>$EXPT etkf</H1><PRE>"

date

#-----------------------------------------------------------------------
# [2] Set up configuration:
#-----------------------------------------------------------------------

mkdir -p $WORK_DIR
cd $WORK_DIR

export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$FCST_RANGE)
export YYYY=$(echo $DATE | cut -c1-4)
export MM=$(echo $DATE | cut -c5-6)
export DD=$(echo $DATE | cut -c7-8)
export HH=$(echo $DATE | cut -c9-10)
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
export DA_FILE=$ETKF_INPUT_DIR/$PREV_DATE/${FILE_TYPE}_d01_${FILE_DATE} #JEFS test uses wrfout

#-----------------------------------------------------------------------
# [3] Create observation files for ETKF:
#-----------------------------------------------------------------------

export RUN_DIR_SAVE=$RUN_DIR

let MEM=1
let JOB=1
while [[ $MEM -le $NUM_MEMBERS ]]; do

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi
   export DA_FIRST_GUESS=${DA_FILE}.${CMEM}

   export RUN_DIR=$WORK_DIR/wrfvar.${CMEM}
   mkdir -p $RUN_DIR
   cd $RUN_DIR
   echo '   <A HREF="working/wrfvar.'$CMEM'">wrfvar run '$CMEM'</a>'
   $SCRIPTS_DIR/da_run_wrfvar.ksh > index.html 2>&1
   cd $WORK_DIR
   let MEM=$MEM+1
   let JOB=$JOB+1

   if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
done

let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
   export RUN_DIR=$WORK_DIR/wrfvar.${CMEM}/working

   wc -l ${RUN_DIR}/ob.etkf.000 > $WORK_DIR/ob.etkf.${CMEM}
   cat ${RUN_DIR}/ob.etkf.000 >> $WORK_DIR/ob.etkf.${CMEM}

   let MEM=$MEM+1
done

cd $WORK_DIR

#-----------------------------------------------------------------------
# [4] Calculate ensemble mean:
#-----------------------------------------------------------------------

ln -s $ETKF_INPUT_DIR/$PREV_DATE/*.e* .

# Initialize ensemble mean and variance with first member
cp ${FILE_TYPE}_d01_${FILE_DATE}.e001 ${FILE_TYPE}_d01_${FILE_DATE}
cp ${FILE_TYPE}_d01_${FILE_DATE}.e001 ${FILE_TYPE}_d01_${FILE_DATE}.vari

cat > gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    filestub = '${FILE_TYPE}_d01_${FILE_DATE}'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV} /
EOF

ln -fs $BUILD_DIR/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

cp gen_be_ensmean.out $RUN_DIR_SAVE
echo
echo '   <A HREF="gen_be_etkf.out">gen_be_ensmean.out</a>'

#-----------------------------------------------------------------------
# [5] Run Ensemble Transform Kalman Filter:
#-----------------------------------------------------------------------

#Prepare ETKF input/output files:

ln -sf ${DA_FILE} etkf_input                     # ETKF input mean (unchanged)
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

   ln -sf ${DA_FILE}.${CMEM} etkf_input.${CMEM}  # ETKF input (unchanged).
   cp ${DA_FILE}.${CMEM} etkf_output.${CMEM}     # ETKF output (overwritten).
   let MEM=$MEM+1
done

#Create namelist and run ETKF:

cat > gen_be_etkf_nl.nl << EOF
  &gen_be_etkf_nl
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV},
    naccumt1 = ${NACCUMT1},
    naccumt2 = ${NACCUMT2},
    nstartaccum1 = ${NSTARTACCUM1},
    nstartaccum2 = ${NSTARTACCUM2},
    nout = ${CYCLE_NUMBER},
    tainflatinput = ${TAINFLATINPUT},
    rhoinput = ${RHOINPUT} /
EOF

ln -fs $BUILD_DIR/gen_be_etkf.exe .
./gen_be_etkf.exe > gen_be_etkf.out 2>&1

cp gen_be_etkf.out $RUN_DIR_SAVE
echo
echo '   <A HREF="gen_be_etkf.out">gen_be_etkf.out</a>'

mkdir -p $ETKF_OUTPUT_DIR/$PREV_DATE
mkdir -p $ETKF_OUTPUT_DIR/$DATE

# Move ensemble mean and variance

mv ${FILE_TYPE}_d01_${FILE_DATE}      $ETKF_OUTPUT_DIR/$PREV_DATE
mv ${FILE_TYPE}_d01_${FILE_DATE}.vari $ETKF_OUTPUT_DIR/$PREV_DATE

# Move ensemble of analyses:

let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
   mv $WORK_DIR/etkf_output.${CMEM} $ETKF_OUTPUT_DIR/$DATE/${FILE_TYPE}_d01_${FILE_DATE}.${CMEM}
   let MEM=$MEM+1
done

if $CLEAN; then
   rm -rf $WORK_DIR
fi

echo '</PRE></BODY></HTML>'

exit 0

